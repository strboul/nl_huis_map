#Loading necessary packages
unlist(lapply(c("tidyverse", "sp", "RColorBrewer"), 
              require, character.only = TRUE))

#Houses in the NL, data
huis <- read.csv("~/Google Drive/map_project_1/Dataset_docs/81870NED_UntypedDataSet_28072017_203507.csv", header=T, sep=";")
