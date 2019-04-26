# set working directory
setwd("C:\\Users\\PAUL\\Documents\\A_Maynooth HDip\\ST662 - Topics in Data Analytics\\Gun registration project")

# load library
require(lubridate)

# import dataset
gun_violence_data <- read.csv("gun-violence-data_01-2013_03-2018.csv",header=TRUE, sep=",")

# check structure and actual data
# str(gun_violence_data)
# head(gun_violence_data)

# trim off cols not needed
gun_violence_data_trim <- gun_violence_data[,c(1,2,3,4,6,7,11,12,13,15,17,18,20,21,22,24,25,28,29)]
head(gun_violence_data_trim)
tail(gun_violence_data_trim)

# convert dates stored as string to actual date format
gun_violence_data_trim$date <- ymd(gun_violence_data_trim$date)
# check structure and actual data
str(gun_violence_data_trim)

# check date/months data
# months.Date(head(gun_violence_data_trim$date,100))

# create new 'incident_count' column, this is used below to get number of incidents by month and year
gun_violence_data_trim$incident_count <- 1
# check structure and actual data
str(gun_violence_data_trim)

# get aggregate daily/monthly/annual incident counts
gun_violence_daily_aggregate <- aggregate(incident_count ~ format(gun_violence_data_trim$date, "%d") +
                                            format(gun_violence_data_trim$date, "%m") +
                                            format(gun_violence_data_trim$date, "%G"),
                                            data = gun_violence_data_trim, FUN = 'sum')

gun_violence_monthly_aggregate <- aggregate(incident_count ~ format(gun_violence_data_trim$date, "%m") +
                                            format(gun_violence_data_trim$date, "%G"),
                                            data = gun_violence_data_trim, FUN = 'sum')

gun_violence_annual_aggregate <- aggregate(incident_count ~ format(gun_violence_data_trim$date, "%G"),
                                            data = gun_violence_data_trim, FUN = 'sum')


# check overall total is still the same
sum(gun_violence_daily_aggregate$incident_count)
sum(gun_violence_monthly_aggregate$incident_count)
sum(gun_violence_annual_aggregate$incident_count)


# get aggregate daily/monthly/annual incident counts BY STATE

# daily
gun_violence_daily_aggregate_bystate <- aggregate(incident_count ~ format(gun_violence_data_trim$date, "%d") +
                                            format(gun_violence_data_trim$date, "%m") +
                                            format(gun_violence_data_trim$date, "%G") +
                                            gun_violence_data_trim$state,
                                            data = gun_violence_data_trim, FUN = 'sum')

# write daily file with derived dataset
colnames(gun_violence_daily_aggregate_bystate) <- c("date","month","year","state","incident_count")
write.csv(gun_violence_daily_aggregate_bystate, file="gun_violence_daily_aggregate_bystate.csv", row.names=FALSE, quote=FALSE)


# monthly
gun_violence_monthly_aggregate_bystate <- aggregate(incident_count ~ gun_violence_data_trim$state +
                                              format(gun_violence_data_trim$date, "%G") +
                                              format(gun_violence_data_trim$date, "%m"),
                                              data = gun_violence_data_trim, FUN = 'sum')

# write monthly file with derived dataset
colnames(gun_violence_monthly_aggregate_bystate) <- c("state","year","month","incident_count")
write.csv(gun_violence_monthly_aggregate_bystate, file="gun_violence_monthly_aggregate_bystate.csv", row.names=FALSE, quote=FALSE)


# annual
gun_violence_annual_aggregate_bystate <- aggregate(incident_count ~ format(gun_violence_data_trim$date, "%G") +
                                             gun_violence_data_trim$state,
                                             data = gun_violence_data_trim, FUN = 'sum')

# write annual file with derived dataset
colnames(gun_violence_annual_aggregate_bystate) <- c("year","state","incident_count")
write.csv(gun_violence_annual_aggregate_bystate, file="gun_violence_annual_aggregate_bystate.csv", row.names=FALSE, quote=FALSE)


# check overall total is still the same
sum(gun_violence_daily_aggregate_bystate$incident_count)
sum(gun_violence_monthly_aggregate_bystate$incident_count)
sum(gun_violence_annual_aggregate_bystate$incident_count)



#########################################################################################################
# Alternative to grouping and aggregating
# gun_violence_daily_count <- xtabs(incident_count ~ format(gun_violence_data_trim$date, "%d") + 
#                                     format(gun_violence_data_trim$date, "%m") +
#                                     format(gun_violence_data_trim$date, "%G"),
#                                     data=gun_violence_data_trim)
# 
# gun_violence_monthly_count <- xtabs(incident_count ~ format(gun_violence_data_trim$date, "%m") +
#                                       format(gun_violence_data_trim$date, "%G"),
#                                       data=gun_violence_data_trim)
# 
# gun_violence_annual_count <- xtabs(incident_count ~ format(gun_violence_data_trim$date, "%G"),
#                                      data=gun_violence_data_trim)

