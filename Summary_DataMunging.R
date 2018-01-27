
######################################################################################################################################################################################
######################################################################################################################################################################################

# Taking baby steps in exploring the data. 


######################################################################################################################################################################################
######################################################################################################################################################################################


library(ggplot2)
library(dplyr)

Quakes <- read.csv('quakes.csv')

head(Quakes)

glimpse(Quakes)

summary(Quakes)

str(Quakes)


#boxplot to find the extreme values

ggplot(data=Quakes,aes(x="",y=depth,colour=mag)) + geom_boxplot() + geom_point(position="jitter",alpha=0.2) +  scale_colour_gradientn(colours = terrain.colors(5))


#histograms to understand distribution

ggplot(data=Quakes,aes(x=mag)) + geom_histogram(fill="purple",binwidth = 0.1) +stat_function(fun = dnorm, args = list(mean = mean(Quakes$mag), sd = sd(Quakes$mag)))

#histogram with normal distribution to observe right/left skew in data

ggplot(data=Quakes,aes(x=mag)) + stat_function(fun = dnorm, args = list(mean = mean(Quakes$mag), sd = sd(Quakes$mag))) +  geom_histogram(aes(y =..density..),
                                                                                                                                         breaks = seq(4, 6, by = 0.1), 
                                                                                                                                         colour = "black", 
                                                                                                                                         fill = "green",alpha=0.2)


#scatterplot to understand data

ggplot(data=Quakes,aes(x=mag,y=depth,color=stations)) + geom_point(position="jitter",alpha=0.35) +  scale_colour_gradientn(colours = terrain.colors(5))


######################################################################################################################################################################################
######################################################################################################################################################################################

# Imputing Missing Values


######################################################################################################################################################################################
######################################################################################################################################################################################

airquality <- read.csv("airquality.csv")

data <- airquality

data[4:10,3] <- rep(NA,7)
data[1:5,4] <- NA

data <- data[-c(5,6)]
summary(data)

#percentage of missing function 
pmiss <- function(x){sum(is.na(x))/length(x)*100}

apply(data,2,pmiss)
apply(data,1,pmiss)


#how many are missing exactly
library(mice)
md.pattern(data)

#visualize what's happening
library(VIM)
aggr_plot <- aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))


marginplot(data[c(1,2)])


### Imputing the missing data with M.I.C.E
# m = 5, number of imputed datasets, method = predictive mean matching


tempData <- mice(data,m=5,method = "pmm",maxit=50,seed=500)


## explore other kinds of imputations with mice

methods(mice)

#completion of data
completeData <- complete(tempData,1)

#comparison of original with imputed
xyplot(tempData,Ozone~Wind+Solar.R,pch=18,cex=1)


# What we would like to see is that the shape of the magenta points
# (imputed) matches the shape of the blue ones (observed). 
# The matching shape tells us that the imputed values are indeed 
# "plausible values".

#more ways of visualizing the difference
densityplot(tempData)

#strip plot
stripplot(tempData, pch = 20, cex = 1.2)


#How do we pick the best data set from these sets of imputed values

#call pool to take the average of all the imputed datasets
modelFit1 <- with(tempData,lm(Wind~ Ozone+Solar.R))
summary(pool(modelFit1))

