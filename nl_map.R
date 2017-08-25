#Loading necessary packages
unlist(lapply(c("tidyverse", "sp", "RColorBrewer"), 
              require, character.only = TRUE))

#Houses in the NL, data
huis <- read.csv("~/Google Drive/map_project_1/Dataset_docs/81870NED_UntypedDataSet_28072017_203507.csv", header=T, sep=";")

#According to metadata file, from PV20 to PV31 belong to provinces.
regions <- dplyr::recode(huis$RegioS, 'PV20  '="Groningen", 'PV21  '="Friesland", 'PV22  '="Drenthe", 'PV23  '="Overijssel", 'PV24  '="Flevoland", 'PV25  '="Gelderland", 'PV26  '="Utrecht", 'PV27  '="Noord-Holland", 'PV28  '="Zuid-Holland", 'PV29  '="Zeeland", 'PV30  '="Noord-Brabant", 'PV31  '="Limburg")
huis <- mutate(huis, id = regions) # We give the name "id" because we will use it later when joining the data frames
huis_sub <- huis[,c(11,13)] #Choosing the BewoondeHuurwoningen (column 11) and id (column 13)
colnames(huis_sub) <- c("value", "id")

#Our region list
region_list <- c("Groningen", "Friesland", "Drenthe", "Overijssel", "Flevoland", "Gelderland", "Utrecht", "Noord-Holland", "Zuid-Holland", "Zeeland", "Noord-Brabant", "Limburg")
data_list <- list() #Initiate a list to save the df.
for(n in region_list){
  y <- huis_sub[which(huis_sub$id==n), ]
  data_list[[n]] <- y
}
huisdata <- dplyr::bind_rows(data_list)

huisdata$value[huisdata$value=="."] <- 0

nl01 <- readRDS("~/Google Drive/map_project_1/Dataset_docs/NLD_adm1.rds") #NL map (Level 1) ##GADM version 2.8
nl01_df <- ggplot2::fortify(nl01, region="NAME_1") #Region names in data frame

unique(unlist(nl01_df[ ,6])) #6th column "id"
nl01_map0 <- nl01_df[ !(nl01_df$id) %in% c("Zeeuwse meren", "IJsselmeer"), ]

huisdata_sum <- aggregate(cbind(value)~id, data=huisdata, FUN=sum)
nl01_map_merged <- merge(nl01_map0, huisdata_sum, by.y = 'id', all.x = TRUE)

nl01_map_centroids <- data.frame(long = sp::coordinates(nl01)[, 1], lat = sp::coordinates(nl01)[, 2])
nl01_map_centroids[, 'ID_1'] <- nl01@data[,'ID_1']
nl01_map_centroids[, 'NAME_1'] <- nl01@data[,'NAME_1']

nl01_map_centroids <- nl01_map_centroids[ !(nl01_map_centroids$ID_1) %in% c(6,13), ] #Removing the rows of "IJsselmeer" and "Zeeuwse meren"

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
