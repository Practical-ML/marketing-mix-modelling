###################################################
#BUILDING A BASIC MARKETING MIX MODEL USING R

#VIEW THE POST ON MEDIUM: 
# https://towardsdatascience.com/building-a-marketing-mix-model-in-r-3a7004d21239
###################################################



##################################################
#LOAD DATASET
##################################################
library(datarium)

#Load marketing data from the datarium package, assign it to a dataframe called "sampledf".
data(marketing)
sampledf <- marketing

#View it
str(sampledf)
View(sampledf)

##################################
#Checking correlation
##################################

library(PerformanceAnalytics)
chart.Correlation(sampledf, histogram = TRUE, pch=19)


##################################
#DEFINING ADSTOCK
#Applying Gabriel Mohanna's maximum period decay method
##################################

#set adstock fb rate
set_rate_fb <- 0.1
set_memory <- 2
get_adstock_fb <- rep(set_rate_fb, set_memory+1) ^ c(0:set_memory)

#set adstock youtube rate
set_rate_yt <- 0.15
set_memory <- 2
get_adstock_youtube <- rep(set_rate_yt, set_memory+1) ^ c(0:set_memory)

#set adstock news rate
set_rate_news <- 0.25
set_memory <- 2
get_adstock_news <- rep(set_rate_news, set_memory+1) ^ c(0:set_memory)


#adstocked fb
ads_fb <- stats::filter(c(rep(0, set_memory), sampledf$facebook), get_adstock_fb, method="convolution")
ads_fb <- ads_fb[!is.na(ads_fb)]
#plot
plot(seq(1,length(sampledf$facebook)), sampledf$facebook, type="h", 
main = "Adstocked Facebook",
xlab="Time (Weeks)", ylab="Facebook", 
ylim=c(0, max(c(sampledf$facebook, ads_fb))), 
frame.plot=FALSE)
lines(ads_fb, col="blue")


#adstocked youtube
ads_youtube <- stats::filter(c(rep(0, set_memory), sampledf$youtube), get_adstock_youtube, method="convolution")
ads_youtube <- ads_youtube[!is.na(ads_youtube)]
#plot
plot(seq(1,length(sampledf$youtube)), sampledf$youtube, type="h", 
main = "Adstocked Youtube", 
xlab="Time (Weeks)", ylab="Youtube", 
ylim=c(0, max(c(sampledf$youtube, ads_youtube))), 
frame.plot=FALSE)
lines(ads_youtube, col="blue")


#adstocked newpaper
ads_news <- stats::filter(c(rep(0, set_memory), sampledf$newspaper), get_adstock_news, method="convolution")
ads_news <- ads_news[!is.na(ads_news)]
#plot
plot(seq(1,length(sampledf$newspaper)), sampledf$newspaper, type="h", 
main = "Adstocked Newspaper",
xlab="Time (Weeks)", ylab="Newspaper", 
ylim=c(0, max(c(sampledf$newspaper, ads_news))), 
frame.plot=FALSE)
lines(ads_news, col="blue")

##################################
#BUILD MARKETING MIX MODEL USING MULTIPLE REGRESSION
##################################

#We are specifying sales as the dependent variable in the lm() function
mmm_1 <- lm(sampledf$sales ~ ads_youtube + ads_fb + ads_news)
summary(mmm_1)

#Check for multicollinearity using VIFs
library(mctest)
imcdiag(mmm_1, method = "VIF")

#or use jtools
#library(jtools)
#summ(mmm_1, vifs=TRUE)

#check for heteroscedasticity
#first, plot the model out and review the siduals vs fitted plot and the Sclae-Location plot
par(mfrow=c(2,2)) # put all 4 charts into 1 page
plot(mmm_1)

#Confirm with an objective test for heteroscedasticity using Breusch Pagan test and NCV test
library(lmtest)
lmtest::bptest(mmm_1)

library(car)
car::ncvTest(mmm_1)

#h0: the variance in the model is homoskedastic (what we want).
#Both returned p-values higher than significance level 0.05, 
#so we can't reject the null and can say that there are no major issues with heteroscedasticity.

##################################
#BUILD MARKETING MIX MODEL WITH TREND AND SEASONALITY USING TIMESERIES
##################################

#Create timeseries
library(forecast)

#frequency is 52 to denote weekly as there are about 52 weeks in a year. 
#ts() needs a minimum of 2 periods (52 x 2 = 104 weeks), 
#our data has observations from 200 weeks so this should be sufficient
ts_sales <- ts(sampledf$sales, start = 1, frequency = 52)

#check class. should state "ts"
class(ts_sales)

#decompose to get the individual components for trends, seasonality, etc
ts_sales_comp <- decompose(ts_sales)

#plot out
plot(ts_sales_comp)

#we use tslm() for our regression. 
#this is just  a timeseries wrapper for lm() but allows trend and seasons on the fly from the data 
#https://www.rdocumentation.org/packages/forecast/versions/8.16/topics/tslm
#just specify "trend" and "season" and tslm() will automatically generate values based on the ts() object you have specified.

#fit the model
mmm_2 <- tslm(ts_sales ~ trend + season + ads_youtube + ads_fb + ads_news)
summary(mmm_2)


#we want to forecast using our model
#i.e. if we were to spend x1 in youtube, x2 in facebook and 0 in newspaper for the next period what would this look like?
#we first need to create a dataframe containing new figures

#we want to get newspaper spend
news_spend <- as.data.frame(ads_news)
names(news_spend)[1] <- "ads_news"

#and give 40% to Youtube.
#this is added to the current youtube spend budget (assuming we are keeping the youtube budget 
#for the next period is the same as it has been for the previous period)
yt_spend <- as.data.frame(ads_youtube)
names(yt_spend)[1] <- "ads_youtube"
yt_spend$ads_youtube <- yt_spend$ads_youtube + (news_spend$ads_news*0.4)
yt_spend


#and give the remainder 60% of newspaper spend to facebook.
#this is added to the current fb spend budget (assuming we are keeping the fb budget 
#for the next period is the same as it has been for the previous period)
fb_spend <- as.data.frame(ads_fb)
names(fb_spend)[1] <- "ads_fb"
fb_spend$ads_fb <- fb_spend$ads_fb + (news_spend$ads_news*0.6)
fb_spend


#leaving nothing for newspapers (we are swtiching it off for the next period)
final_news_spend <-  as.data.frame(news_spend*0)
names(final_news_spend)[1] <- "ads_news"
final_news_spend

#now put these new values all into a dataframe. 
#We'll use the model to predict sales for the next period based on these new budget allocation values
new_spends <- cbind(yt_spend, fb_spend,final_news_spend)
new_spends


library(ggfortify)
par(mfrow=c(1,1)) # reset to 1 chart per page
set.seed(9999)

#what performance looks like with no change
forecast_unchanged <- forecast(mmm_2, h=200)
ggplot2::autoplot(forecast_unchanged, ts.colour = 'black', size= 0.7, predict.size = 0.7, predict.colour = 'red', conf.int = TRUE, conf.int.fill = 'red', main = "Forecasted", predict.linetype='dashed') 

#forecast with budget changes
forecast_new_spends <- forecast(mmm_2, newdata=new_spends)
ggplot2::autoplot(forecast_new_spends, ts.colour = 'black', size= 0.7, predict.size = 0.7, predict.colour = 'blue', conf.int = TRUE, conf.int.fill = 'blue', main = "Forecasted")


#overlaying them together using autolayer()
forecast_unchanged <- forecast(mmm_2, h=200)
ggplot2::autoplot(forecast_unchanged, ts.colour = 'black', size= 0.7, predict.size = 0.7, predict.colour = 'red', conf.int = TRUE, conf.int.fill = 'red', main = "Forecasted", predict.linetype='dashed') + forecast::autolayer(forecast_new_spends, col = 'blue')

#Get fitted values
#Finally, you can access fitted values from the model by quering forecast_new_spends$fitted



##################################
#THE END!!
##################################
