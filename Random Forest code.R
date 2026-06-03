library(caTools)
library(randomForest)
library(tidyverse)
library(readxl)
library(dplyr)
library(ggpubr)
library(readxl)
library(readr)
library(learnr)
library(tidyverse)#data manipulation
library(readxl)
library(agricolae)
library(daewr)
library(kernlab)
library(ROCR)
library(caret)
library(purrr)
library(fields)
library(RColorBrewer)
library(datasets)
library(doParallel)
library(MASS)
library(pROC)
library(kableExtra)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)
library(spatialRF)
library(sf)
library(ggplot2)
library(tidycensus)
library(ggspatial)
library(leaflet)
library(raster)
library(sp)
library(rgdal)
library(raster)
library(sp)
library(rgdal)
library(stars)
library(sf)
library(tmap)
library(gstat)
library(ggmap)
library(leaflet)
library(sf)
library(maps)
library(PerformanceAnalytics)
library(reshape2) 
library(e1071)
install.packages('car')        # To check multicollinearity 
install.packages("quantmod")
install.packages("MASS")
install.packages("corrplot")   # plot correlation plot

library(caTools)
library(car)
library(quantmod)
library(MASS)
library(corrplot)
setwd("D:/BUAN/Research/New Idea/Materials and Methods/FINAL data")

set.seed(342)


data<-read.csv("Final_data.csv")
data$LONGITUDE<-NULL
data$LATITUDE<-NULL
data$CID<-NULL
data$ ACQ_DATE<-NULL
data$ACQ_TIME<-NULL
head(data)
tail(data)
data$TYPE=as.factor(data$TYPE)
str(data)

###Splitting data set
set.seed(342)
split <- sample.split(data, SplitRatio = 0.7)# Have seperate testing and training data
split
Training <- subset(data, split == "TRUE")#True is for Training data set
Testing <- subset(data, split == "FALSE")#False is testing
set.seed(342)
split2 <- sample.split(Random_forest_data, SplitRatio = 0.7)
split2
Testing <- subset(Random_forest_data, split2 == "FALSE")
Training <- subset(Random_forest_data, split2 == "TRUE")
summary(Random_forest_data)
###Remaining data will be used for Logistic regression
set.seed(342)
split3 <- sample.split(Another, SplitRatio = 0.5)
Testing2 <- subset(Another, split3 == "TRUE")
Testing3 <- subset(Another, split3 == "FALSE")
Testing$Class=as.factor(Testing$Class)
Testing2$Class=as.factor(Testing3$Class)
Testing3$Class=as.factor(Testing3$Class)

str(Training)
##Correlation chart
chart.Correlation(data[-5],col=data$Class)
chart.Correlation(data[-5],col=data$Class)
split <- sample.split(data, SplitRatio = 0.7)# Have seperate testing and training data
split
##Check correlation
corr=cor(data[as.numeric(which(sapply(data,class)=="numeric"))])
corr
check_collinearity(rf)
if (require("see")) {
  x <- check_collinearity(rf)
  plot(x)
}



##Training model
set.seed(342)
rf<-randomForest(TYPE~.,data = Remaining_data, 
                 ntree = 1000,plot=TRUE,
                 importance=TRUE,confusion=TRUE,
                 na.action = na.roughfix,
                 replace=TRUE,
                 trcontrol=trainControl(method = 'cv',number = 4))

rf1<-randomForest(TYPE~DMP+SM,data = Training, 
                  ntree = 900,plot=TRUE,
                  importance=TRUE,confusion=TRUE,
                  na.action = na.roughfix,
                  replace=TRUE,
                  trcontrol=trainControl(method = 'cv',number = 4))
remove(rf1)

rf2<-randomForest(TYPE~LST+LFMC+DFMC,data = Training, 
                  ntree = 900,mtry=3,plot=TRUE,
                  importance=TRUE,confusion=TRUE,
                  na.action = na.roughfix,
                  replace=TRUE,
                  trcontrol=trainControl(method = 'cv',number = 4))

rf<-randomForest(TYPE~.,data = Remaining_data, 
                 ntree = 900,mtry=3,plot=TRUE,
                 importance=TRUE,confusion=TRUE,
                 na.action = na.roughfix,
                 replace=TRUE,max.depth = 8,probability = TRUE,
                 trcontrol=trainControl(method = 'cv',number = 4))

rf
rf1
rf2
summary(rf)
plot(rf)
varImp(rf)
varImpPlot(rf)
importance(rf)
help("plot")

###Model Testing
Fires_pred=predict(rf1,Testing)
confusionMatrix(Testing$TYPE, Fires_pred)

Fires_pred2=predict(rf,Random_forest_data)
confusionMatrix(Random_forest_data$TYPE, Fires_pred2)


##2
Fires_pred2=predict(rf,Testing2)
confusionMatrix(Testing2$Class, Fires_pred2)

##3
Fires_pred3=predict(rf,Testing3)
confusionMatrix(Testing3$Class, Fires_pred3)