###################################################
#CLEANING AND PREPARING MESSY MARKETING DATA 
#BEFORE PERFORMING MARKETING MIX MODELLING

#VIEW THE POST ON MEDIUM: 
# https://towardsdatascience.com/cleaning-and-preparing-marketing-data-in-r-prior-to-machine-learning-or-analysis-ec1a12079f1
###################################################




##################################################
#IMPORT DATA
##################################################

#Get marketing data
marketing_df <- read.csv("MarketingReportCoreCSV.csv", fileEncoding="UTF-8-BOM", head = TRUE)

#Lets look at it
View(marketing_df)

#Yuck. Needs semi colon seperators
marketing_df <- read.csv("MarketingReportCoreCSV.csv", sep = ';',  fileEncoding="UTF-8-BOM", head = TRUE)

#Check again
View(marketing_df)


#K. Now do the same for transactions
orders_df <- read.csv("WebTransactionsCSV.csv", sep = ';', fileEncoding="UTF-8-BOM", head = TRUE)

#Check again
View(orders_df)


##################################################
#PROCESS MARKETING DATA
##################################################

#Keep only the columns we care about

desired_columns <-c(1, 4, 7)
marketing_df_clean <- marketing_df[desired_columns]

#Check
View(marketing_df_clean)

#check class is still data frame
class(marketing_df_clean)

#rename columns names
colnames(marketing_df_clean) <- c("date", "channel", "spend")

#Check
str(marketing_df_clean)


#force lowercase channel character column
marketing_df_clean$channel <- tolower(marketing_df_clean$channel)

#check all unique channel names specified in the channel column
unique(marketing_df_clean$channel)

#rename "not tracked" to "direct"
marketing_df_clean$channel <- gsub("not tracked","direct",marketing_df_clean$channel)

#rename "unpaid" to "organic"
marketing_df_clean$channel <- gsub("unpaid","organic",marketing_df_clean$channel)

#rename "silverpop" to "email"
marketing_df_clean$channel <- gsub("silverpop","email",marketing_df_clean$channel)

#shorten facebookbusinessadsmanager to just "facebook" as there are no other FB activity in here
marketing_df_clean$channel <- gsub("facebookbusinessadsmanager","facebook",marketing_df_clean$channel)

#check
View(marketing_df_clean)


#set dates to year-month-day using ymd() from lubridate library
library(lubridate)
marketing_df_clean$date <- ymd(marketing_df_clean$date)

#now check class. must be date
class(marketing_df_clean$date)

# check all column formats
str(marketing_df_clean)

#spend column is string for some reason. 
#There are commas in there which should be turned into decimals.
marketing_df_clean$spend <- as.numeric(gsub("," , ".", marketing_df_clean$spend))

#change all other integer column to numeric for consistency
#marketing_df_clean[,4:7] <- sapply(marketing_df_clean[,4:7], as.numeric)

# check column formats
str(marketing_df_clean)

#check again. 
View(marketing_df_clean)

#looks like lots of duplicates. 
#Some of these rows have been segmented by the columns we've dropped previously (e.g. segment channel by mobile and desktop). 
#so we just need to remember to sum spend by date and group by channel

marketing_df_clean <- aggregate(spend ~ date + channel, data = marketing_df_clean, sum)



##################################################
#PROCESS ORDERS DATA
##################################################

#we've loaded this previously. remind ourselves what it looks like
View(orders_df)

#Keep only the columns we care about

needed_columns <-c(2, 4, 18)
orders_df_clean <- orders_df[needed_columns]

#check class is still data frame
class(orders_df_clean)

#check
View(orders_df_clean)

#rename columns names
colnames(orders_df_clean) <- c("date", "channel", "revenue")

#force lowercase channel character column
orders_df_clean$channel <- tolower(orders_df_clean$channel)

#check all unique channel names specified in the channel column
unique(orders_df_clean$channel)

#rename "notset" to "direct"
orders_df_clean$channel <- gsub("notset", "direct", orders_df_clean$channel)

#rename "silverpop" to "email"
orders_df_clean$channel <- gsub("silverpop", "email", orders_df_clean$channel)

#check values are correct
head(orders_df_clean)

#date values have "T00:00:00" appended. This is unnecessary lets clean this out.
orders_df_clean$date <- gsub("T00:00:00" , "", orders_df_clean$date)

#now set the date column to ymd
orders_df_clean$date <- ymd(orders_df_clean$date)

#check format is correct
str(orders_df_clean)

#revenue needs to be numeric for consistency
orders_df_clean$revenue <- as.numeric(orders_df_clean$revenue)

#check format is correct
str(orders_df_clean)

#check the data
View(orders_df_clean)

#looks like lots of duplicates. 
#Some of these rows have been segmented by the columns we've dropped previously (e.g. segment channel by mobile and desktop). 
#so we just need to remember to sum revenue by date and group by channel
orders_df_clean <- aggregate(revenue ~ date + channel, data = orders_df_clean, sum)

#check the data
View(orders_df_clean)



##################################################
#TURN DAILY DATA INTO WEEKLY
##################################################

#Sorting out the marketing data first

#create new column called week, assign same values as date and then convert it into week of the year. 
# e.g. 2020-03-15 falls on week 11 of year 2020. https://www.timeanddate.com/date/weeknumber.html
marketing_df_clean$week = lubridate::week(marketing_df_clean$date)

#create new column called month year, assign same values as date and then convert it to month year
marketing_df_clean$monthyear = format(as.Date(marketing_df_clean$date), "%m-%Y")

#save to a new df that we will prepare for weekly data
marketing_df_weekly <- marketing_df_clean

#reorder columns for sanity
marketing_df_weekly  <- marketing_df_weekly [, c(4, 5, 1, 2, 3)]


#view it
View(marketing_df_weekly)


#Now prep the orders data

#create new column called week, assign same values as date and then convert it into  week of the year.
#e.g. 2020-03-15 falls on week 11 of year 2020. https://www.timeanddate.com/date/weeknumber.html
orders_df_clean$week = lubridate::week(orders_df_clean$date)

#create new column called month year, assign same values as date and then convert it to month year
orders_df_clean$monthyear = format(as.Date(orders_df_clean$date), "%m-%Y")

#save to a new df that we will prepare for weekly data
orders_df_weekly <- orders_df_clean

#reorder columns for sanity
orders_df_weekly  <- orders_df_weekly [, c(4, 5, 1, 2, 3)]

#check figures
View(orders_df_weekly)


#NOW MERGE!
weekly_df <- merge(marketing_df_weekly,orders_df_weekly)
View(weekly_df)


#get spend per week by channel and save it in its own dataframe
weekly_spend_df <- aggregate(spend ~ week + channel, data = weekly_df, sum)
colnames(weekly_spend_df)[3] <- "weeklyspend"
View(weekly_spend_df)

#get revenue per week and save it in its own dataframe
weekly_rev_df <- aggregate(revenue ~ week, data = weekly_df, sum)
#rename columns names
colnames(weekly_rev_df)[2] <- "totalrevenueforweek"
View(weekly_rev_df)


#just want dates and save to data frame called weekly_df_dates
keep_dates <- c(1,3)
weekly_df_dates <- weekly_df[keep_dates]
View(weekly_df_dates)

#remove duplicate rows
weekly_df_dates <- weekly_df_dates[!duplicated(weekly_df_dates[c("week")]),]
colnames(weekly_df_dates)[2] <- "weekdatestart"
View(weekly_df_dates)


#begin merge
#merge() only allows joining 2 dataframes at a time. 

#so here's the first merge
weekly_df_updated <- merge(weekly_spend_df, weekly_rev_df, by="week" )
View(weekly_df_updated)

#now the 2nd merge
weekly_df_updated <- merge(weekly_df_dates, weekly_df_updated, by="week" )

#drop week column. we no longer need it
weekly_df_updated <-weekly_df_updated[,-1]
View(weekly_df_updated)

str(weekly_df_updated)

#now view it
View(weekly_df_updated)


#finally turn long data to wide
weekly_reshaped_channel <- reshape(weekly_df_updated, idvar = c("weekdatestart","totalrevenueforweek"), timevar = "channel", direction = "wide")
View(weekly_reshaped_channel)


#view the data and check for NA or missing values
print(weekly_reshaped_channel)

#if found, replace any NA with 0
weekly_reshaped_channel[is.na(weekly_reshaped_channel)] <- 0

str(weekly_reshaped_channel)
View(weekly_reshaped_channel)

#data is now FINALLY READY!!

