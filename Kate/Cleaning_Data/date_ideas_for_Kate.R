#code to extract parts of dates

#read in some data
rm(list = ls())
library(lubridate)#manipulates dates
library(readr)
DF <- read_csv("JoinedData1.csv")

#note that we have "Date_Out" and "Date_Checked" as character.

#convert to YY/MM/DD via lubridate
DF$Date_Out<-ymd(DF$Date_Out)

#strip just year from the date (could also do hr/m/sec)
DF$Year_Out<-year(DF$Date_Out)

#check here for info https://r4ds.had.co.nz/dates-and-times.html

#The key issue is that the data that come in need to be include the hr/min/sec if you are going to parse it.

