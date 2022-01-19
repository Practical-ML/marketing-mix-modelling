###################################################
#BUILDING A BASIC MARKETING MIX MODEL USING R

#VIEW THE POST ON MEDIUM: 
# https://practicalmachinelearning.medium.com/building-a-marketing-mix-model-in-r-3a7004d21239
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
#Our data has observations from 200 weeks
#and ts() needs a minimum of 2 periods (52 x 2 = 104 weeks), 
#so our data should be sufficient
ts_rev <- ts(sampledf$sales, start = 1, frequency = 52)

#check class. should state "ts"
class(ts_rev)

#decompose to get the individual components for trends, seasonality, etc
ts_rev_comp <- decompose(ts_rev)

#plot out
plot(ts_rev_comp)

#we use tslm() for our regression. 
#this is just  a timeseries wrapper for lm() but allows trend and seasons on the fly from the data 
#https://www.rdocumentation.org/packages/forecast/versions/8.16/topics/tslm
#just specify "trend" and "season" and tslm() will automatically generate values based on the ts() object you have specified.

#fit the model
mmm_2 <- tslm(ts_rev ~ trend + season + ads_youtube + ads_fb + ads_news)
summary(mmm_2)

#we want to forecast using our model
#i.e. if we were to spend x1 in youtube, x2 in facebook and 0 in newspaper, 
#what would the predicted spend be for the next 52 weeks?
#we first need to create a dataframe containing new figures for next 52 weeks

#we get this from the last 52 weeks of newspaper spend
getnewsspend <- as.data.frame(ads_news)
getnewsspend <- as.data.frame(getnewsspend[149:200, ])
names(getnewsspend)[1] <- "getnewsspend"


#and give 40% to Youtube.
#this is added to the current youtube spend budget (assuming we are keeping the youtube budget 
#for the next 52 weeks the same as it has been for the past 52 weeks)
getyoutubespend <- as.data.frame(ads_youtube)
getyoutubespend <- as.data.frame(getyoutubespend[149:200, ])
names(getyoutubespend)[1] <- "ads_youtube"
getyoutubespend$ads_youtube <- getyoutubespend$ads_youtube + (getnewsspend$getnewsspend*0.4)
getyoutubespend


#and give the remainder 60% of newspaper spend to facebook.
#this is added to the current fb spend budget (assuming we are keeping the fb budget 
#for the next 52 weeks the same as it has been for the past 52 weeks)
getfbspend <- as.data.frame(ads_fb)
getfbspend <- as.data.frame(getfbspend[149:200, ])
names(getfbspend)[1] <- "ads_fb"
getfbspend$ads_fb <- getfbspend$ads_fb + (getnewsspend$getnewsspend*0.6)
getfbspend


#leaving nothing for newspapers (we are swtiching it off for the next 52 weeks)
finalnewsspend <-  as.data.frame(getnewsspend*0)
names(finalnewsspend)[1] <- "ads_news"
finalnewsspend

#now put these new values all into a dataframe. 
#We'll use the model to predict sales for the next 52 weeks based on these new budget allocation values
newbudgets <- cbind(getyoutubespend, getfbspend,finalnewsspend)
newbudgets

#what performance looks like with no change
library(ggfortify)

set.seed(9999)
forecast_nochange <- forecast(mmm_2, h=200)
ggplot2::autoplot(forecast_nochange, ts.colour = 'black', size= 0.7, predict.size = 0.7, predict.colour = 'red', conf.int = TRUE, conf.int.fill = 'red', main = "Forecasted", predict.linetype='dashed') 


#forecast with budget changes
get_forecastedsales <- forecast(mmm_2, newdata=newbudgets)
ggplot2::autoplot(get_forecastedsales, ts.colour = 'black', size= 0.7, predict.size = 0.7, predict.colour = 'blue', conf.int = TRUE, conf.int.fill = 'blue', main = "Forecasted")


#overlaying them together, and looking at just the periods in blue by limiting x axis with xlim and using autolayer()
forecast_nochange <- forecast(mmm_2, h=200)
ggplot2::autoplot(forecast_nochange, ts.colour = 'black', size= 0.7, predict.size = 0.7, predict.colour = 'red', conf.int = TRUE, conf.int.fill = 'red', main = "Forecasted", xlim=c(1,5.5), predict.linetype='dashed') + forecast::autolayer(get_forecastedsales, col = 'blue')

#Get fitted values
#Finally, you can access fitted values from the model by quering get_forecastedsales$fitted



##################################
#THE END!!
##################################
