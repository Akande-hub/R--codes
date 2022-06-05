##########################################################
##                       Group Members                    #
#                                                         #
##                  1. AKANDE Oluwatosin Adetoye          #
##                  2. Ange Clement Akazan                #
##                  3. Jeanne NIYONTEZE                   #
##                  4. Redempta Blandine ISHIME           #
##                  5. Enock MWIZERWA                     #
##                  6. Mahamat Azibert ABDELWAHAB         #
###########################################################
###################---------------------------###################
#################                                 ###############
#                       Time Series Analysis                    #
#              Autocorrelation and Cross correlation            #
#################                                 ###############
###################---------------------------###################

rm(list = ls()) 
##setwd('/home/alabi/Review_Phase/Block 3/Climate statistics') # please change  to your 
                                                             # preferred working directory

# please install psych package by removing the # before the install commands below
#install.packages('forecast')
#install.packages('TSA')
#install.packages('biwavelet') 
#install.packages("dplyr")
#install.packages("zoo")
library(forecast)
library(ggplot2)
library(dplyr)
library(zoo)
library(forecast)
library(ggplot2)
library(forecast)
library(TSA)
library(biwavelet)
library(dplyr)

## Data importation
lagos<-read.csv('Lagos1.csv')

## Data preparation
class(lagos)
dim(lagos)
names(lagos)
str(lagos)
anyNA(lagos)
View(lagos)
summary(lagos)

needed_data <-lagos[, c(3,7)] # precipitation and temperature


### converting data to time series
#png('timeseries_plot.#png')
prep <- ts(needed_data$PRED, start = c(1960,1), end = c(2010,12), frequency = 12)
temp <- ts(needed_data$TMPD, start = c(1960,1), end = c(2010,12), frequency = 12)
#dev.off()

#png('timeseries_plot.#png')
par(mfcol = c(2,1))
plot(temp, ylab = 'Monthly Temperature', main = 'Time Series plot of Temperature', col='black')
abline(reg = lm(temp~time(temp)), col="red")
plot(prep, ylab = 'Monthly Precipitation', main = 'Time Series plot of Precipitation', col='black')
abline(reg = lm(prep~time(prep)), col="red")
#dev.off()

############# De-trending the data set
## ploting the series, Note: always Zoom to see the chart clearly
#png('timeseries_plotDetrended.#png')
trend_p <- lm(prep ~c(1:length((prep))))
detr_prep <- residuals(trend_p)
trend_t <- lm(temp ~c(1:length((temp))))
detr_temp <-  residuals(trend_t)

par(mfcol = c(2,1))
plot.ts(detr_temp,main='Time Series plot of Detrended Temperature')
plot.ts(detr_prep,main='Time Series plot of Detrended Precipitation')
#dev.off()

### Autocorrelation plot
#png('Auto_correlation.#png')
par(mfcol = c(1,2))
forecast::Acf(detr_prep, lag.max = 36,main='Auto-corr of Detrended Prep')
forecast::Acf(detr_temp, lag.max = 36, main='Auto-corr of Detrended Temp')
#dev.off()


### cross-correlation plot
#png('cross_correlation.#png')
par(mfcol = c(1,1))
forecast::Ccf(detr_prep,detr_temp,main='Cross-correlation of Detrended Prep and TempD')
#dev.off()

###################---------------------------###################
#################                                 ###############
#                       Time Series Analysis                    #
#                         Fourier Analysis                      #
#################                                 ###############
###################---------------------------###################
##png('periodogram.#png')
par(mfcol = c(1,1))
p1 <- periodogram(detr_prep,main='Periodogram of Detrended Precipitation')
#p <- periodogram(temp)
##dev.off()

## converting the frequency to periods
#png('frequency_gram.#png')
#period_Temp <- 1/p$freq
period_Prep <- 1/p1$freq
par(mfcol = c(1,1))
#plot(period_Temp, p$spec, type = 'b', xlim = c(2,13), xlab = 'period', ylab = 'periodogram',
     #main = 'Temperature')
plot(period_Prep, p1$spec, type = 'b', xlim = c(2,13), xlab = 'period', ylab = 'periodogram',
     main = 'Precipitation in Peroids')
#dev.off()
####################################################
autoplot(ts(detr_prep), series="Data") +
  autolayer(ma(ts(detr_prep),6), series="6 - moving average") +
  xlab("Year") + ylab("Detrended Precipitation") +
  ggtitle("6 moving average for precipitation") +
  scale_colour_manual(values=c("Data"="grey50","6 - moving average"="red"),
                      breaks=c("Data","6 - moving average"))

###################---------------------------###################
#################                                 ###############
#                       Time Series Analysis                    #
#             Wavelet Analysis and Wavelet Coherence            #
#################                                 ###############
###################---------------------------###################

## Wavelet
timelong=seq(1960, 2011-1/12, 1/12)  # indexing
wavelet=wt(cbind(timelong,detr_prep),dj=0.1,mother="paul",max.scale=16) #wavelet
wavelet1=wt(cbind(timelong,detr_temp),dj=0.1,mother="morlet",max.scale=16) #wavelet

## plot of wavelet
##png('waveletpaul.#png')
par(mfcol = c(1,1))
par(oma=c(0, 0, 0, 1), mar=c(5, 4, 4, 5) + 0.1)
xx=plot(wavelet,  plot.cb = TRUE,lwd.coi = 1,col.coi="white",alpha.coi=0.5,col.sig="black",
        lwd.sig = 2, ncol = 768, tol = 0.95, plot.phase=F,ylab="Period(Year)", xlab = "Time(Year)",
        main='Precipitation')

par(oma=c(0, 0, 0, 1), mar=c(5, 4, 4, 5) + 0.1)
yy=plot(wavelet1,  plot.cb = TRUE,lwd.coi = 1,col.coi="white",alpha.coi=0.5,col.sig="black",
        lwd.sig = 2, ncol = 768, tol = 0.95, plot.phase=F,ylab="Period(Year)", xlab = "Time(Year)",
        main='Temperature')
#dev.off()


### wavelet coherence calculation
wcoh=wtc(cbind(timelong,prep),cbind(timelong,temp),dj=0.1,mother="morlet",max.scale=16) 

## plot of wavelet coherence
#png('wavelet_coherence.#png')
par(mfcol = c(1,1))
par(oma=c(0, 0, 0, 1), mar=c(5, 4, 4, 5) + 0.1) #To allow for colour bar to be included
plot(wcoh,  plot.cb = TRUE,lwd.coi = 1,col.coi="white",alpha.coi=0.5,col.sig="black",
     lwd.sig = 2, ncol = 768, tol = 0.95, plot.phase=T,ylab="Period(Year)", xlab = "Time(Year)",
     main='Wavelet Coherence') #plotting
#dev.off()



# Arrows pointing to the right mean that x and y are in phase.

#Arrows pointing to the left mean that x and y are in anti-phase.

#Arrows pointing up mean that y leads x by π/2.

#Arrows pointing down mean that x leads y by π/2. 
