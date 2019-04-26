us_state_pops2010<- read.csv("us_state_populations2010.csv",header = T)
us_state_pops2010[,1] <- NULL # remove 'rank' col from orig csv imported
View(us_state_pops2010)

us_pops_2019<- read.csv("us_pops_2019.csv",header = T)
write.csv(us_pops_2019, file="us_pops_2019.csv", row.names=FALSE, quote=FALSE)

View(us_pops_2019)

us_pops_2010_2019 = merge(us_state_pops2010, us_pops_2019, by.x=1, by.y=1, all.x=T)
View(us_pops_2010_2019)


us_pops_1900_2015<- t(read.csv("us_pops_1900_2015.csv",header = T)) # transposed input because of original file format
colnames(us_pops_1900_2015) <- us_pops_1900_2015[1,] # use row 1 vals as new col names
us_pops_1900_2015 <- us_pops_1900_2015[-1,] # delete row 1 (col names)
colnames(us_pops_1900_2015)[1] <- 'StateCode'
View(us_pops_1900_2015)
write.csv(us_pops_1900_2015, file="NEW_us_pops_1900_2015.csv", row.names=FALSE, quote=FALSE)


states_names_and_codes <- read.csv("state_abbreviations.csv",header = T)
View(states_names_and_codes)

us_pops_1900_2015_with_stateNames <- merge(us_pops_1900_2015, states_names_and_codes, by.x = 'StateCode', by.y = 'Postal.Code', all.x=T, all.y=T)
colnames(us_pops_1900_2015_with_stateNames)[1] <- 'State'

View(us_pops_1900_2015_with_stateNames)

write.csv(us_pops_1900_2015_with_stateNames, file="NEW_us_pops_1900_2015.csv", row.names=FALSE, quote=FALSE)
us_pops_1998_2013 <- us_pops_1900_2015_with_stateNames[,c(118,1,100:115)]
colnames(us_pops_1998_2013)[2] <- 'StateCode'
colnames(us_pops_1998_2013)[1] <- 'State'

View(us_pops_1998_2013)
write.csv(us_pops_1998_2013, file="us_pops_1998_2013.csv", row.names=FALSE, quote=FALSE)



pops_2014_2017_nst <- read.csv("pops_2010-2017_nst-est2017-01.csv",header = T)[,c(1,6:9)] # years 2014-2017
colnames(pops_2014_2017_nst)[1] <- 'State'
pops_2014_2017_nst$State <- sub('.', '', pops_2014_2017_nst$State) # remove '.' char from orig data imported
colnames(pops_2014_2017_nst)[2] <- '2014_pop' # rename column
colnames(pops_2014_2017_nst)[3] <- '2015_pop' # rename column
colnames(pops_2014_2017_nst)[4] <- '2016_pop' # rename column
colnames(pops_2014_2017_nst)[5] <- '2017_pop' # rename column

View(pops_2014_2017_nst)

