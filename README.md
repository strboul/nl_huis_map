# Choropleth map of total housing types by region in the Netherlands with R

The Netherlands... a tiny, cozy and rainy country. A person coming to that beautiful place will immediately realize a frequent problem: a place to live, or also known as permanent accommodation. To be honest, this problem appears to most of the people who come to the Netherlands. After all, finding a house is hard, and finding a bit better house among the others is harder. At the end of the day, who does not want a *'gezellig'* place?

Today, we are going to make a regional level housing map of the Netherlands in the form of choropleth. Despite the disadvantages of choropleth maps such as giving a false impression on the regional borders where boundaries are abruptly changed, they are nice to present visual communication establishing with shaded regions with regard to a defined key such as gradient color revealing different value ranges<sup>[1](#fn1)</sup>.

Making a regional level map is not a hard task with R, especially with the given cleaned data. As you know that today many platforms offer open data for the public use - as there is an abundance of data, but we cannot say the same for the analysis of data. Though, it’s a good initiative to open data to all, when so many analysts can interpret it and try to turn them something meaningful.

<!--more-->

In this example, the dataset offered from [**cbs.nl**](https://www.cbs.nl/) is used that requires a little bit data cleaning. However there is no parsing or heavy data cleaning done. We extract the house owning and house rental data. Metadata depicts that "huurwoningen" is the rental house whose property owner does not live in. "Eigenwoningen" is the homes whose owner is also the resident or one of the residents.

To create the map, you need the following packages for R :

``` r
#Loading necessary packages
unlist(lapply(c("tidyverse", "sp", "RColorBrewer"), 
              require, character.only = TRUE))
```

I personally prefer to load packages in this way, which looks more cluttered than running the library function in multiple lines.

`tidyverse` is the tidy approach which absolutely changed the destiny of data analysis in R. We require all its connected [**packages**](http://www.tidyverse.org). `sp` package provides the tools we need to acquire and control the spatial data. `RColorBrewer` offers a nice looking color palette, particularly for maps.

## Import and massage the data

Data can be found [**here**](http://statline.cbs.nl/StatWeb/publication/?DM=SLNL&PA=81870NED). As said, data preparation gets the 80% of work. Though, our job is not going to be so long! Let’s import the data first:

``` r
#Houses in the NL, data
huis <- read.csv("~/Downloads/dataset.csv", header=T, sep=";")
```

Now coding the province names from the data, which are actually obtained from the metadata file attached to the same folder of main data document. By the way, the code in this blog post depicts the data from rental column, the value can be changed for analysing different values - if needed.

``` r
#According to metadata file, from PV20 to PV31 belong to provinces.
regions <- dplyr::recode(huis$RegioS, 'PV20  '="Groningen", 'PV21  '="Friesland", 'PV22  '="Drenthe", 'PV23  '="Overijssel", 'PV24  '="Flevoland", 'PV25  '="Gelderland", 'PV26  '="Utrecht", 'PV27  '="Noord-Holland", 'PV28  '="Zuid-Holland", 'PV29  '="Zeeland", 'PV30  '="Noord-Brabant", 'PV31  '="Limburg")
huis <- mutate(huis, id = regions) # We give the name "id" because we will use it later when joining the data frames
huis_sub <- huis[,c(11,13)] #Choosing the BewoondeHuurwoningen (column 11) and id (column 13)
colnames(huis_sub) <- c("value", "id")
```

Now we choose and assign variables from the data frame. It is vital to extract the rows related with the provinces. A simple for loop will do this work.

``` r
#Our region list
region_list <- c("Groningen", "Friesland", "Drenthe", "Overijssel", "Flevoland", "Gelderland", "Utrecht", "Noord-Holland", "Zuid-Holland", "Zeeland", "Noord-Brabant", "Limburg")
data_list <- list() #Initiate a list to save the df.
for(n in region_list){
  y <- huis_sub[which(huis_sub$id==n), ]
  data_list[[n]] <- y
}
huisdata <- dplyr::bind_rows(data_list)
```

After detecting some symbol situated in the values of rows (from `levels(huisdata$value)`), we come back to metadata file to see what should be done.

> -   Symbol Declaration (Verklaring van symbolen):
> -   Nothing (blank): the number can not occur on logical grounds (het cijfer kan op logische gronden niet voorkomen).
> -   . : The grade is unknown, insufficiently reliable or secret (het cijfer is onbekend, onvoldoende betrouwbaar of geheim).
> -   (...)
> -   In this table, all numbers are rounded to hundreds (in deze tabel zijn alle getallen afgerond op honderdtallen).

So before that, we transform the '.' (dot) character into numerical value as zero.

``` r
huisdata$value[huisdata$value=="."] <- 0
```

## Shape the map

Second, we look our map form. [**GADM**](http://www.gadm.org) (Global Administrative Areas) offers spatial data of administrative areas and boundaries of the whole world. There are three levels available, which we use Level 1 that presents the regional areas.

Read `.rds` file downloaded from GADM with `readRDS` function, and transform it into data frames with `fortify` to use in plotting later on.

``` r
nl01 <- readRDS("~/Downloads/NLD_adm1.rds") #NL map (Level 1) ##GADM version 2.8
nl01_df <- ggplot2::fortify(nl01, region="NAME_1") # Region names 1 in data frame
```

As we have data frame, we cannot learn the levels of the id column by using `levels` function. We use `unique` function extracting unique elements. And, there are 14 regions in total, and only 12 provinces are available. 2 provinces which are "Zeeuwse meren" and "IJsselmeer" are out of our context. Well, there are no built housing there yet, as far as I know!

``` r
unique(unlist(nl01_df[ ,6])) #6th column "id"
nl01_map0 <- nl01_df[ !(nl01_df$id) %in% c("Zeeuwse meren", "IJsselmeer"), ]
```

Computing the summary of value column with regards to the regions mentioned in the id column. Then, a little attention should be pointed out here. For merging, the data frames should have the same number of rows, and one common name column between these two data frames.

``` r
huisdata_sum <- aggregate(cbind(value)~id, data=huisdata, FUN=sum)
nl01_map_merged <- merge(nl01_map0, huisdata_sum, by.y = 'id', all.x = TRUE)
```

Find the centroids of provinces, and acquire names and id according to provinces.

``` r
nl01_map_centroids <- data.frame(long = sp::coordinates(nl01)[, 1], lat = sp::coordinates(nl01)[, 2])
nl01_map_centroids[, 'ID_1'] <- nl01@data[,'ID_1']
nl01_map_centroids[, 'NAME_1'] <- nl01@data[,'NAME_1']
```

Here, we don't delete the unwanted rows by numeric order of data frame, as it may be changed in the future and will cost us trouble. We choose `ID_1` property for removing action.

``` r
nl01_map_centroids <- nl01_map_centroids[ !(nl01_map_centroids$ID_1) %in% c(6,13), ] #Removing the rows of "IJsselmeer" and "Zeeuwse meren"
```

## The Viz

`ggplot2` is very convenient to build the graph. Adding continious value to the `fill` is feasible as we used gradient color scales instead of discrete (then, we would need to `cut` it). Following that, the `sourcesign` variable is used for the caption as that the italic font can be emphasized with `expression` function assigned to an independent variable, and called from the `caption` line.

``` r
sourcesign <- expression(paste("Data: cbs.nl"," & ","Source: ", italic("strboul.github.io"), sep="\n")) #For ggplot's caption
ggplot(nl01_map_merged, aes(x = long, y = lat, group = group)) + 
geom_polygon(aes(fill = value)) +
geom_text(data = nl01_map_centroids, aes(label = NAME_1, x = long, y = lat, group = NAME_1), size = 3) +
scale_fill_gradient(high = "#ff8300", low = "#ffe4c9", space = "Lab", na.value = "transparent", guide = "legend", name="Total Number") +
coord_map() + 
theme_bw() +
theme(legend.position = c(0.87, 0.15),
      axis.ticks = element_blank(),
      axis.text.y   = element_blank(),
      axis.text.x   = element_blank(),
      plot.caption = element_text(vjust=8, size=rel(0.8)),
      panel.background = element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "black", fill=NA, size=0.1))+
labs(x="", y="", title = "Residental Homes in the Netherlands", subtitle = "Bewoonde Huurwoningen
", caption=sourcesign)
```

## The Result

Contrary to what is believed, achieving a nice looking visualization without rambling the aim of the message is not an easy task. It may be needed to think more, especially the data, for not creating a dull and redundant map. At least, the tutorial has reached its purpose.

<img src="https://raw.githubusercontent.com/strboul/_pic_blog/master/pic/nlmap_eigen.png" width="325" height="425"> <img src="https://raw.githubusercontent.com/strboul/_pic_blog/master/pic/nlmap_huur.png" width="325" height="428">

*Voilà!*

------------------------------------------------------------------------

### Remarks and Reference

Check the full tutorial [**here**](https://strboul.github.io/2017/08/choropleth-map-housing-netherlands.html). Check the source code in [**my GitHub repository**](https://github.com/strboul/nl_huis_map).

<a name="fn1">[1]</a>: Choropleth maps. (2017, January 27). *Barcelona Field Studies Centre*. Retrieved from <http://geographyfieldwork.com/DataPresentationMappingTechniques.htm>

