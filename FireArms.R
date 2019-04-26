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

# fire_arms<- read.csv("E:\\Maynooth University\\semester 2\\topics in data analytics\\project\\fireaArms\\data\\firearms.csv",header = T)
fire_arms<- read.csv("nics-firearm-background-checks.csv",header = T)

View(fire_arms)
names(fire_arms)
dim(fire_arms)

#############################################################################################################
# see if filtering works

# feb2019<-fire_arms %>% filter(month == '2019-02')
# View(feb2019)
# 
# # data of handguns
# feb2019_handgun<-feb2019 %>% select(contains('handgun'))
# View(feb2019_handgun)
# 
# # handgun sum
# feb2019_handgun_sum<-colSums(feb2019_handgun)
# View(feb2019_handgun_sum)
# 
# # data of LongGuns
# feb2019_longgun<-feb2019 %>% select(contains('Long'))
# View(feb2019_longgun)
# 
# # LongGuns sum
# feb2019_longgun_sum<-colSums(feb2019_longgun)
# View(feb2019_longgun_sum)

#############################################################################################################

# there are total 55 states
# now picking the top 5 and bottom 5 states

# convert 'month' col to char, creating new year and month cols: year and month stripped from orig 'month' col
fire_arms$month <- as.character(fire_arms$month)
fire_arms$year_val <- as.numeric(substr(fire_arms$month,1,4))
fire_arms$month_val <- as.numeric(substr(fire_arms$month,6,7))
#str(fire_arms)
View(fire_arms)

# grouping by statewise for the entire period
statewise <- fire_arms %>% group_by(state) %>% summarise(gun_reg_total=sum(totals)) %>% arrange(desc(gun_reg_total))
View(statewise)

head(statewise,5)
tail(statewise,5)

# grouping by months
#fire_arms_monthwise<-fire_arms %>% group_by(month) %>% summarise(mean(totals))
#View(fire_arms_monthwise)
# there are 244 months

# Look what can be done with 2010 populations...

# # import pop data
# us_state_pops<- read.csv("us_state_populations.csv",header = T)
# us_state_pops[,1] <- NULL # remove 'rank' col from orig csv imported
# View(us_state_pops)
# 
# # append pop data onto statewise dataframe aligned by state and state.name cols
# statewise_with_pops = merge(statewise, us_state_pops, by.x=1, by.y=1, all.x=T)
# View(statewise_with_pops)
# 
# # get per capita vals
# statewise_with_pops$per_capita <- statewise_with_pops$gun_reg_total/statewise_with_pops$Population.2010
# View(statewise_with_pops)


#############################################################################################################

# original fire-arms grouped by state, years and months
fire_arms_year_month_state_wise<-fire_arms %>% group_by(state, year_val, month_val) %>%
  summarise(gun_reg_total=sum(totals)) %>%
  arrange(year_val)
View(fire_arms_year_month_state_wise)

# original fire-arms grouped by state and years
fire_arms_year_state_wise<-fire_arms %>% group_by(state, year_val) %>%
  summarise(gun_reg_total=sum(totals)) %>%
  arrange(year_val)
View(fire_arms_year_state_wise)

#############################################################################################################

# gun violence data
gun_violence_data <- read.csv("gun_violence_monthly_aggregate_bystate.csv",header = T)
View(gun_violence_data)

gun_violence_year_state_wise<-gun_violence_data %>% group_by(state, year) %>%
  summarise(incident_count_total=sum(incident_count)) %>%
  arrange(year)
View(gun_violence_year_state_wise)

#############################################################################################################

# filtered fire_arms data (2014-2017) to be merged
fire_arms_year_state_wise_2014_2017<-fire_arms_year_state_wise %>% filter(year_val > 2013 & year_val < 2018)
View(fire_arms_year_state_wise_2014_2017)

# this 2014-2017 monthly data is used for heatmap below
fire_arms_year_month_state_wise_2014_2017<-fire_arms_year_month_state_wise %>% filter(year_val > 2013 & year_val < 2018)
View(fire_arms_year_month_state_wise_2014_2017)

# filtered gun violence data (2014-2017) to be merged
gun_violence_data_2014_2017 <- gun_violence_year_state_wise %>% filter(year > 2013 & year < 2018)
View(gun_violence_data_2014_2017)

merged_datasets_2014_2017 <- merge(fire_arms_year_state_wise_2014_2017, gun_violence_data_2014_2017,
                         by.x=c("state", "year_val"),
                         by.y=c("state", "year"), all.x=T, all.y=T)
View(merged_datasets_2014_2017)
write.csv(merged_datasets_2014_2017, file="merged_gun_registr_and_gun_violence_bystate_2014_2017.csv", row.names=FALSE, quote=FALSE)

#############################################################################################################

# get state populations from 2014-2017
pops_2014_2017_nst <- read.csv("pops_2010-2017_nst-est2017-01.csv",header = T)[,c(1,6:9)] # years 2014-2017

colnames(pops_2014_2017_nst)[1] <- 'State'
pops_2014_2017_nst$State <- sub('.', '', pops_2014_2017_nst$State) # remove '.' char from orig data imported
colnames(pops_2014_2017_nst)[2] <- 2014 # rename column
colnames(pops_2014_2017_nst)[3] <- 2015 # rename column
colnames(pops_2014_2017_nst)[4] <- 2016 # rename column
colnames(pops_2014_2017_nst)[5] <- 2017 # rename column
View(pops_2014_2017_nst)

melted_populations_2014_2017 <- melt(pops_2014_2017_nst, id='State', measure.vars = c(2:5)) # pop col called 'variable after melt
View(melted_populations_2014_2017)

#############################################################################################################

# merge 2014-2017 population data into 2014-2017 gun&incident dataframe
merged_datasets_with_pops <- merge(merged_datasets_2014_2017, melted_populations_2014_2017,
                                   by.x=c("state", "year_val"),
                                   by.y=c("State", "variable"),
                                   all.x=T, all.y=T)

colnames(merged_datasets_with_pops)[5] <- 'population' # rename 'value' column after merging

# re-order data according to state and year
merged_datasets_with_pops <- merged_datasets_with_pops[order(merged_datasets_with_pops[,1],
                                                             merged_datasets_with_pops[,2]),]

View(merged_datasets_with_pops)


# copy merged datasets to new dataframe
merged_datasets_with_ratios <- merged_datasets_with_pops


# add per capita cols to merged dataframe

# guns per capita
merged_datasets_with_ratios$per_capita_guns <- merged_datasets_with_ratios$gun_reg_total/
  as.numeric(merged_datasets_with_ratios$population)

# incidents per capita
merged_datasets_with_ratios$per_capita_incidents <- merged_datasets_with_ratios$incident_count/
  as.numeric(merged_datasets_with_ratios$population)

# incidents per guns
merged_datasets_with_ratios$incidents_per_gun <- merged_datasets_with_ratios$incident_count/
  merged_datasets_with_ratios$gun_reg_total

# guns per incidents -  VERY INTERESTING
merged_datasets_with_ratios$guns_per_incident <- merged_datasets_with_ratios$gun_reg_total/
  merged_datasets_with_ratios$incident_count

View(merged_datasets_with_ratios)
write.csv(merged_datasets_with_ratios, file="merged_datasets_with_ratios_2014_2017.csv", row.names=FALSE, quote=FALSE)

#############################################################################################################

# # for top 5/10 and bot 5/10 calculations remove any rows with NAs
# merged_datasets_with_ratios_no_NAs <- merged_datasets_with_ratios[rowSums(!is.na(merged_datasets_with_ratios)) >= 9, ]
# View(merged_datasets_with_ratios_no_NAs)
# 
# # re-order data in numerical order of the values
# top10_merged <- tail(merged_datasets_with_ratios_no_NAs[order(merged_datasets_with_ratios_no_NAs[,3],
#                                                              merged_datasets_with_ratios_no_NAs[,9]),], 20)
# 
# bot10_merged <- head(merged_datasets_with_ratios_no_NAs[order(merged_datasets_with_ratios_no_NAs[,3],
#                                                              merged_datasets_with_ratios_no_NAs[,9]),], 20)
# 
# top10_and_bot10 <- rbind(top10_merged, bot10_merged)
# View(top10_and_bot10)
# 
# 
# #############################################################################################################
# 
# # 'melt' the count cols (measure.vars) into one 'variable' col and one 'values' col
# melted_top10_and_bot10 <- melt(top10_and_bot10, id=c('state','year_val'), measure.vars = c(3,4,9)) # vars cols merged into 'variable' col post melt
# View(melted_top10_and_bot10)
# 
# melted_top10_and_bot10_summary <- melted_top10_and_bot10 %>% group_by(state, year_val, variable) %>% summarize(total = sum(value))
# View(melted_top10_and_bot10_summary)
# 
# 
# # create a new col with scaled version of the values col
# melted_top10_and_bot10_summary$value_scaled <- scale(melted_top10_and_bot10_summary$total, FALSE, TRUE)
# 
# # plot all states
# ggplot(melted_top10_and_bot10_summary,aes(x= year_val,y= total, fill=variable)) +
#   geom_col(position="dodge") #+
#   #geom_text(aes(x=state))
# 
# # stat="identity", position="dodge"
# 
# #############################################################################################################
# 
# # create a state filter e.g. filter for just Alabama
# # melt data first
# melted_merged_datasets_with_ratios <- melt(merged_datasets_with_ratios,
#                                            id=c('state','year_val'),
#                                            measure.vars = c(4,9)) # variables col called 'variable' after melt
# # filter by state
# filtered_data <- melted_merged_datasets_with_ratios %>% filter(state == 'Alabama')
# ggplot(filtered_data,aes(state,value_scaled,fill=variable)) + geom_col(stat="identity", position="dodge")
# View(filtered_data)


# fire_arms_year_month_state_wise_2014_2017

#######################################################################################
# used in heatmap below
merged_datasets_monthly_data_with_pops <- merge(fire_arms_year_month_state_wise_2014_2017, melted_populations_2014_2017,
                                                by.x=c("state", "year_val"),
                                                by.y=c("State", "variable"),
                                                all.x=T, all.y=T)

colnames(merged_datasets_monthly_data_with_pops)[5] <- 'population' # rename 'value' column after merging

# re-order data according to state and year
merged_datasets_monthly_data_with_pops <- merged_datasets_monthly_data_with_pops[order(merged_datasets_monthly_data_with_pops[,1],
                                                                                       merged_datasets_monthly_data_with_pops[,2]),]
View(merged_datasets_monthly_data_with_pops)

# copy merged datasets to new dataframe
merged_datasets_monthly_data_with_pops_with_ratios <- merged_datasets_monthly_data_with_pops

# guns per capita
merged_datasets_monthly_data_with_pops_with_ratios$per_capita_guns <- merged_datasets_monthly_data_with_pops_with_ratios$gun_reg_total/
  as.numeric(merged_datasets_monthly_data_with_pops_with_ratios$population)

# # incidents per capita
# merged_datasets_monthly_data_with_pops_with_ratios$per_capita_incidents <- merged_datasets_monthly_data_with_pops_with_ratios$incident_count/
#   as.numeric(merged_datasets_monthly_data_with_pops_with_ratios$population)
# 
# # incidents per guns
# merged_datasets_monthly_data_with_pops_with_ratios$incidents_per_gun <- merged_datasets_monthly_data_with_pops_with_ratios$incident_count/
#   merged_datasets_monthly_data_with_pops_with_ratios$gun_reg_total
# 
# # guns per incidents -  VERY INTERESTING
# merged_datasets_monthly_data_with_pops_with_ratios$guns_per_incident <- merged_datasets_monthly_data_with_pops_with_ratios$gun_reg_total/
#   merged_datasets_monthly_data_with_pops_with_ratios$incident_count

View(merged_datasets_monthly_data_with_pops_with_ratios)

merged_datasets_monthly_data_with_pops_with_ratios_2014 <- filter(merged_datasets_monthly_data_with_pops_with_ratios, year_val==2014)
ggplot(data = merged_datasets_monthly_data_with_pops_with_ratios_2014, aes(x = state, y = paste(year_val,"-",month_val))) +
  geom_tile(aes(fill = per_capita_guns)) + coord_flip()

merged_datasets_monthly_data_with_pops_with_ratios_2015 <- filter(merged_datasets_monthly_data_with_pops_with_ratios, year_val==2015)
ggplot(data = merged_datasets_monthly_data_with_pops_with_ratios_2015, aes(x = state, y = paste(year_val,"-",month_val))) +
  geom_tile(aes(fill = per_capita_guns)) + coord_flip()

merged_datasets_monthly_data_with_pops_with_ratios_2016 <- filter(merged_datasets_monthly_data_with_pops_with_ratios, year_val==2016)
ggplot(data = merged_datasets_monthly_data_with_pops_with_ratios_2016, aes(x = state, y = paste(year_val,"-",month_val))) +
  geom_tile(aes(fill = per_capita_guns)) + coord_flip()

merged_datasets_monthly_data_with_pops_with_ratios_2017 <- filter(merged_datasets_monthly_data_with_pops_with_ratios, year_val==2017)
ggplot(data = merged_datasets_monthly_data_with_pops_with_ratios_2017, aes(x = state, y = paste(year_val,"-",month_val))) +
  geom_tile(aes(fill = per_capita_guns)) + coord_flip()

fire_arms
view(fire_arms)
ggplot(data = fire_arms, aes(x = state, y = month)) +
  geom_tile(aes(fill = totals)) + coord_flip()

# original firearms

fire_arms_1998 <- filter(fire_arms,year_val==1998)
fire_arms_2003 <- filter(fire_arms,year_val==2003)
fire_arms_2008 <- filter(fire_arms,year_val==2008)
fire_arms_2013 <- filter(fire_arms,year_val==2013)
fire_arms_2018 <- filter(fire_arms,year_val==2018)

ggplot(data = fire_arms_1998, aes(x = state, y = month)) +
  geom_tile(aes(fill = totals)) + coord_flip()

ggplot(data = fire_arms_2003, aes(x = state, y = month)) +
  geom_tile(aes(fill = totals)) + coord_flip()

ggplot(data = fire_arms_2008, aes(x = state, y = month)) +
  geom_tile(aes(fill = totals)) + coord_flip()

ggplot(data = fire_arms_2013, aes(x = state, y = month)) +
  geom_tile(aes(fill = totals)) + coord_flip()

ggplot(data = fire_arms_2018, aes(x = state, y = month)) +
  geom_tile(aes(fill = totals)) + coord_flip()













