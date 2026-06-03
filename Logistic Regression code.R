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
library(caTools)
library(car)
library(quantmod)
library(MASS)
library(corrplot)
data<-read.csv("LR_data.csv")
data$LONGITUDE<-NULL
data$LATITUDE<-NULL
data$CID<-NULL
data$ ACQ_DATE<-NULL
data$ACQ_TIME<-NULL
head(data)
tail(data)
data$TYPE=as.factor(data$TYPE)
str(data)
set.seed(342)
split3 <- sample.split(Remaining_data, SplitRatio = 0.7)
LRTraining <- subset(Remaining_data, split3 == "TRUE")
LRTesting <- subset(Remaining_data, split3 == "FALSE")

data<-read.csv("validation.csv")
str(LRTraining)
summary(LRTraining$TYPE)
LRTraining$LONGITUDE<-NULL
LRTraining$X<-NULL
LRTraining$X.1<-NULL
LRTraining$LATITUDE<-NULL
LRTraining$CID<-NULL
LRTraining$ ACQ_DATE<-NULL
LRTraining$ACQ_TIME<-NULL
head(LRTraining)
tail(LRTraining)
data$TYPE=as.factor(LRTraining$TYPE)
set.seed(342)
split <- sample.split(data, SplitRatio = 0.7)# Have seperate testing and training data
split
Training <- subset(data, split == "TRUE")#True is for Training data set
Testing <- subset(data, split == "FALSE")#False is testing
#####Multi-collineality_test####
data_x <- data[,2:6]                          # independent variables 

var <- cor(data_x)                          # independent variables correlation matrix 

var_inv <- ginv(var)              # independent variables inverse correlation matrix 

colnames(var_inv) <- colnames(data_x)  # rename the row names and column names
rownames(var_inv) <- colnames(data_x)

corrplot(var_inv,method='number',is.corr = F)     # visualize the multicollinearity


# Training model
set.seed(763)
logistic_model <- glm(TYPE ~DMP+LST+NDVI, 
                      data = Training, 
                      family = "binomial")
logistic_model

# Summary
summary(logistic_model)

###Correllation Test
vif_values <- vif(logistic_model)           #create vector of VIF values

barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value

abline(v = 5, lwd = 3, lty = 2)    #add vertical line at 5 as after 5 there is severe correlation

# Predict test data based on model
predict_reg <- predict(logistic_model, 
                       Testing, type = "response")
###predict_reg  

# Changing probabilities
predict_reg <- ifelse(predict_reg >0.45, 1, 0)

# Evaluating model accuracy
# using confusion matrix
table(Testing$TYPE, predict_reg)

missing_classerr <- mean(predict_reg != Testing$TYPE)
print(paste('Accuracy =', 1 - missing_classerr))

# ROC-AUC Curve
ROCPred <- prediction(predict_reg, Testing$TYPE) 
ROCPer <- performance(ROCPred, measure = "tpr", 
                      x.measure = "fpr")

auc <- performance(ROCPred, measure = "auc")
auc <- auc@y.values[[1]]
auc

## Goodness of fit test
with(logistic_model,
     pchisq(null.deviance-deviance,
            df.null-df.residual,lower.tail = F))



# Plotting curve
plot(ROCPer)
plot(ROCPer, colorize = TRUE, 
     print.cutoffs.at = seq(0.1, by = 0.1), 
     main = "ROC CURVE")
abline(a = 0, b = 1)

auc <- round(auc, 4)
legend(.6, .4, auc, title = "AUC", cex = 1)