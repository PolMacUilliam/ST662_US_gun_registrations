# options(max.print=25000)
# sink("installed.packages.csv")
# installed.packages()
# sink()
# sink("r-colours.csv")
# colours(distinct = FALSE)
# sink()

# install.packages('purrr')
# install.packages('lazyeval')
library(purrr)
library(tidyverse) # attaches packages: ggplot2, tibble, tydr, readr
library(ggplot2)
library(tibble)
library(reshape2)

#install.packages('dplyr')
library(dplyr)

#install.packages('GGally')
library(GGally)

library(lubridate)

setwd("C:\\Users\\PAUL\\Documents\\A_Maynooth HDip\\ST662 - Topics in Data Analytics\\Gun registration project")

# us_states_polygons<- read.csv("E:\\Maynooth University\\semester 2\\topics in data analytics\\project\\fireaArms\\data\\us_states_polygons.csv",header = T)
us_states_polygons<- read.csv("us_states_polygons.csv",header = T)

View(us_states_polygons)


merged_datasets_with_ratios_2014_2017<- read.csv("merged_datasets_with_ratios_2014_2017.csv",header = T)
View(merged_datasets_with_ratios_2014_2017)


merged_datasets_with_ratios_and_polygons <- merge(x = us_states_polygons, y = merged_datasets_with_ratios_2014_2017,
             by.x="name",
             by.y="state",
             all.x=T, all.y=T)
View(merged_datasets_with_ratios_and_polygons)
write.csv(merged_datasets_with_ratios_and_polygons, file="merged_datasets_with_ratios_and_polygons.csv", row.names=FALSE, quote=FALSE)


library(tidyverse)
library(sf)

?read_rds
