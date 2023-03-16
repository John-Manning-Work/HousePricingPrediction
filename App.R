#=============================================================(INSTALL PACKAGES)======================================================#|
#Implementing all packages into system                                                                                                #|  
                                                                                                                                      #|
#install.packages("shiny")                                                                                                            #|
#install.packages("ggplot2")                                                                                                          #|
#install.packages("class")                                                                                                            #|
#install.packages("caret")                                                                                                            #|
#install.packages("e107")                                                                                                             #|
#install.packages("tidyverse")                                                                                                        #|
#install.packages("statsr")                                                                                                           #|
#install.packages("dplyr")                                                                                                            #|
#install.packages("BAS")                                                                                                              #|
#install.packages("corrplot")                                                                                                         #|
#install.packages("GGally")                                                                                                           #|
#install.packages("MASS")                                                                                                             #|
#install.packages("roxygen2")                                                                                                         #|
                                                                                                                                      #|
#=============================================================(ADDING LIBRARYS)=======================================================#|
#Adding Librarys To Program                                                                                                           #|                                              
                                                                                                                                      #|
library(ggplot2)                                                                                                                      #|
library(caret)                                                                                                                        #|
library(e1071)                                                                                                                        #|
library(statsr)                                                                                                                       #|
library(dplyr)                                                                                                                        #|
library(BAS)                                                                                                                          #|
library(corrplot)                                                                                                                     #|
library(GGally)                                                                                                                       #|
library(MASS)                                                                                                                         #|
                                                                                                                                      #|
#Adding Scipen Options For Graphs To Display Numbers                                                                                  #|
options(scipen = 9999)                                                                                                                #|
                                                                                                                                      #|
#==========================================================(SETTING UP WORKSPACE)=====================================================#|
#Setting up workspace & Reading in the database & sourcing into connect.R for database connections                                    #|
source("Connection.R")                                                                                                                #|
                                                                                                                                      #|
#Turning all 0's in the database to 1 & then splitting the data into training and test data for the prediction                        #|
#'Splitting data up into training and test data                                                                                       #|
#'@param database                                                                                                                     #|
#'a database                                                                                                                          #|
#'Selecting the amount of data wanted in each training and test[1:1000] in training and [1001:1460] in test                           #|
#'@param ProjectDataTraining                                                                                                          #|
#'@param ProjectDataTest                                                                                                              #|
#'Making two different dataframes to work with                                                                                        #|
#'@return Two sets of data to use for machine learning model                                                                          #|
#'@examples                                                                                                                           #|
#'Data <- data[1:100,]                                                                                                                #|
#'data <- data[1000:2000,]                                                                                                            #|
#'@export       
database <- getData("root", "password", "iowa_db", "127.0.0.1", "backup")    
database
database[database=="0"] <- 1                                                                                                          #|
ProjectDataTraining <- database[1:1000,]                                                                                              #|
ProjectDataTest <- database[1001:1460,]                                                                                               #|
                                                                                                                                      #|
#=============================================================(START OF MAIN)=========================================================#|
#Testing data in plots/graphs to see revelence                                                                                        #|
#Finding All Missing Values                                                                                                           #|
                                                                                                                                      #|
k <- colSums(is.na(ProjectDataTraining))                                                                                              #|
missingval <- sort(k, decreasing = TRUE)[1:20]                                                                                        #|
barplot(missingval, main = "Missing Value", las = 2)                                                                                  #|
                                                                                                                                      #|
#Finding the Average Price for a house                                                                                                #|
                                                                                                                                      #|
hist(ProjectDataTraining$SalePrice, main = " House price", xlab = "price", ylab = "Amount", col = "green")                            #|
                                                                                                                                      #|
#===============================================================(PLOTTING)============================================================#|
#Plotting different values to find what affects the price of houses                                                                   #|
#Finding the Number of properties with specific areas                                                                                 #|
                                                                                                                                      #|
hist(log(ProjectDataTraining$LotArea), main = "Log:AREA", xlab = "area", col = "red")                                                 #|
                                                                                                                                      #|
#Finding the number of houses built around what year                                                                                  #|
hist(ProjectDataTraining$YearBuilt, main = "Year of building", col = "blue")                                                          #|
                                                                                                                                      #|
#Box plotting Sale price Vs overall Condition of the property to find price difference of quality to price                            #|
                                                                                                                                      #|
boxplot(ProjectDataTraining$SalePrice~ProjectDataTraining$OverallCond, main = "Overall condition", xlab = "condition", col = "Yellow")#|
                                                                                                                                      #|
#Plotting a histogram to get the quanity of each overall condition                                                                    #|
                                                                                                                                      #|
hist(ProjectDataTraining$OverallCond)                                                                                                 #|
                                                                                                                                      #|
#Plotting a table of overall condition to get exact amount                                                                            #|
table(ProjectDataTraining$OverallCond)                                                                                                #|
                                                                                                                                      #|
#Finding the median and mean of saleprice grouped by overall condition                                                                #|
overall <- ProjectDataTraining %>%                                                                                                    #|
filter(OverallCond !=5)%>%                                                                                                            #|
group_by(OverallCond)%>%                                                                                                              #|
summarise(median = median(SalePrice), mean = mean(SalePrice))                                                                         #|
                                                                                                                                      #|
#Plotting the results found previously                                                                                                #|
                                                                                                                                      #|
plot(overall, col = "red")                                                                                                            #|
                                                                                                                                      #|
#Plotting Area, year and overall condition vs price                                                                                   #|
plot(ProjectDataTraining$SalePrice~ProjectDataTraining$LotArea, main = "Area Vs Price", xlab = "Area", col = "Red")                   #|
plot(ProjectDataTraining$SalePrice~ProjectDataTraining$YearBuilt, main = "Year of building vs price", col = "blue")                   #|
plot(ProjectDataTraining$SalePrice~ProjectDataTraining$OverallCond, main = "Overall condition vs price", xlab = "Condition",          #|
                                                                                                             col = "yellow")          #|
                                                                                                                                      #|
#==========================================================(ADDING TEST MODELS)=======================================================#|
#Plotting different models vs SalePrice with training data                                                                            #|
                                                                                                                                      #|
model.lm=lm(log(SalePrice) ~ OverallCond + log(LotArea) + YearBuilt + GarageArea + TotalBsmtSF + GarageCars + FullBath + HalfBath +   #|
              BedroomAbvGr + X1stFlrSF + X2ndFlrSF + log(LotArea) + CentralAir, data = ProjectDataTraining)                           #|
                                                                                                                                      #|
#Plotting two models with the previous summary data                                                                                   #|
                                                                                                                                      #|
model.bic = bas.lm(log(SalePrice) ~ OverallCond + log(LotArea) + YearBuilt + GarageArea + TotalBsmtSF + GarageCars + FullBath +       #|
              HalfBath + BedroomAbvGr + X1stFlrSF + X2ndFlrSF + log(LotArea) + CentralAir, prior = "BIC", method = "MCMC",            #|
              modelprior = uniform(), data = ProjectDataTraining)                                                                     #|
                                                                                                                                      #|
model.aic = bas.lm(log(SalePrice) ~ OverallCond + log(LotArea) + YearBuilt + GarageArea + TotalBsmtSF + GarageCars + FullBath +       #|
              HalfBath + BedroomAbvGr + X1stFlrSF + X2ndFlrSF + log(LotArea) + CentralAir, prior = "AIC", method = "BAS",             #|
              modelprior = uniform(), data = ProjectDataTraining)                                                                     #|
                                                                                                                                      #|
#Creating a model without GarageArea, FullBath, HalfBath, X1stFlrSF & X2ndFlrSF                                                       #|
                                                                                                                                      #|
model.bic.full = bas.lm(log(SalePrice) ~ OverallCond + log(LotArea) + YearBuilt + TotalBsmtSF + GarageCars + BedroomAbvGr +           #|
              log(LotArea) + CentralAir, prior = "BIC", method = "MCMC", modelprior = uniform(), data = ProjectDataTraining)          #|
                                                                                                                                      #|
lm.bic = lm(log(SalePrice) ~ OverallCond + log(LotArea) + YearBuilt + TotalBsmtSF + GarageCars + BedroomAbvGr + log(LotArea) +        #|
              CentralAir, data = ProjectDataTraining)                                                                                 #|
                                                                                                                                      #|
#Adding in FullBath, HalfBath & X2ndFlrSF                                                                                             #|
                                                                                                                                      #|
model.aic.full = bas.lm(log(SalePrice) ~ OverallCond + log(LotArea) + YearBuilt + TotalBsmtSF + GarageCars + FullBath + HalfBath +    #|
              BedroomAbvGr + X2ndFlrSF + log(LotArea) + CentralAir, prior = "AIC", method = "BAS", modelprior = uniform(),            #|
              data = ProjectDataTraining)                                                                                             #|
                                                                                                                                      #|
lm.aic = lm(log(SalePrice) ~ OverallCond + log(LotArea) + YearBuilt + FullBath + HalfBath + BedroomAbvGr + X2ndFlrSF + log(LotArea) + #|
              CentralAir, data = ProjectDataTraining)                                                                                 #|
                                                                                                                                      #|
#=========================================================(DISPLAY TEST MODELS)=======================================================#|
#Display Summary/Image Models                                                                                                         #|
                                                                                                                                      #|
summary(model.lm)                                                                                                                     #|
image(model.bic)                                                                                                                      #|
image(model.aic)                                                                                                                      #|
image(model.bic.full)                                                                                                                 #|
image(model.aic.full)                                                                                                                 #|
                                                                                                                                      #|
#======================================================(PREDICTING TRAINING DATA)=====================================================#|
#Predicting BIC train data                                                                                                            #|
                                                                                                                                      #|
predict.full.train.bic <- exp(predict(lm.bic, ProjectDataTraining))                                                                   #|
resid.full.train.bic <- ProjectDataTraining$SalePrice - predict.full.train.bic                                                        #|
rmse.full.train.bic <- sqrt(mean(resid.full.train.bic^2, na.rm = TRUE))                                                               #|
rmse.full.train.bic                                                                                                                   #|
                                                                                                                                      #|
#Predicting AIC train data                                                                                                            #|
predict.full.train.aic <- exp(predict(lm.aic, ProjectDataTraining))                                                                   #|
resid.full.train.aic <- ProjectDataTraining$SalePrice - predict.full.train.aic                                                        #|
rmse.full.train.aic <- sqrt(mean(resid.full.train.aic^2, na.rm = TRUE))                                                               #|
rmse.full.train.aic                                                                                                                   #|
                                                                                                                                      #|
#========================================================(PREDICTING TEST DATA)=======================================================#|
#Predicting test data                                                                                                                 #|
                                                                                                                                      #|
predict.full.test<-exp(predict(lm.bic, ProjectDataTest))                                                                              #|
resid.full.test<- ProjectDataTest$SalePrice - predict.full.test                                                                       #|
rmse.full.test.bic <- sqrt(mean(resid.full.test^2, na.rm = TRUE))                                                                     #|
rmse.full.test.bic                                                                                                                    #|
rmse.full.train.bic > rmse.full.test.bic                                                                                              #|
                                                                                                                                      #|
#=======================================================(BUILDING VARIABLE MODEL)=====================================================#|
#Step One                                                                                                                             #|
                                                                                                                                      #|
allvar.model.step1 = lm(SalePrice ~ LotArea + OverallCond + YearBuilt + YearRemodAdd + Heating + CentralAir + BedroomAbvGr +          #|
                        KitchenAbvGr + KitchenQual + TotRmsAbvGrd + Fireplaces + GarageYrBlt + GarageArea + PoolArea,                 #|
                        data = ProjectDataTraining)                                                                                   #|
                                                                                                                                      #|
Train_normal <- ProjectDataTraining%>%                                                                                                #|
  filter(SaleCondition == "Normal")%>%                                                                                                #|
  dplyr::select(SalePrice, LotArea, OverallCond, YearBuilt, YearRemodAdd, Heating, CentralAir, BedroomAbvGr, KitchenAbvGr,            #|
                KitchenQual, TotRmsAbvGrd, Fireplaces, GarageYrBlt, GarageArea, PoolArea)                                             #|
                                                                                                                                      #|
#Step Two                                                                                                                             #|
                                                                                                                                      #|
allvar.model.step2 = lm(log(SalePrice) ~ log(LotArea) + OverallCond + YearBuilt + YearRemodAdd + Heating + CentralAir + BedroomAbvGr +#| 
                        KitchenAbvGr + KitchenQual + TotRmsAbvGrd + Fireplaces + GarageYrBlt + log(GarageArea) + PoolArea,            #|
                        data = ProjectDataTraining)                                                                                   #|
                                                                                                                                      #|
Train_normal_2 <- ProjectDataTraining%>%                                                                                              #|
  filter(SaleCondition == "Normal")%>%                                                                                                #|
  dplyr::select(SalePrice, LotArea, OverallCond, YearBuilt, YearRemodAdd, Heating, CentralAir, BedroomAbvGr, KitchenAbvGr,            #|
                KitchenQual, Fireplaces, GarageArea)                                                                                  #|
                                                                                                                                      #|
#Step Three                                                                                                                           #|
                                                                                                                                      #|
allvar.model.step3 = lm(log(SalePrice) ~ ., data = Train_normal_2)                                                                    #|
                                                                                                                                      #|
Train_normal_3 <- ProjectDataTraining%>%                                                                                              #|
  filter(SaleCondition == "Normal")%>%                                                                                                #|
  dplyr::select(SalePrice, LotArea, OverallCond, YearBuilt, CentralAir, KitchenAbvGr, KitchenQual, Fireplaces)                        #|
                                                                                                                                      #|
#Step Four                                                                                                                            #|
                                                                                                                                      #|
Allvar.model.step4 = lm(log(SalePrice) ~ log(LotArea)+ OverallCond+ YearBuilt + CentralAir+KitchenAbvGr+KitchenQual+Fireplaces,       #|
                      data = Train_normal_3)                                                                                          #|
                                                                                                                                      #|
#===========================================================(BUILDING MODEL)==========================================================#|
#Setting up model from last 4 steps                                                                                                   #|
                                                                                                                                      #|
model.step4.bas = bas.lm(log(SalePrice) ~ log(LotArea) + OverallCond + YearBuilt + CentralAir + KitchenQual + Fireplaces,             #|
                         prior = "BIC", method = "MCMC", modelprior = uniform(), data = Train_normal_3)                               #|
                                                                                                                                      #|
#========================================================(BUILDING FINAL MODEL)=======================================================#|
#Building last model                                                                                                                  #|
                                                                                                                                      #|
final.model = lm(log(SalePrice) ~ log(LotArea) + OverallCond + YearBuilt + CentralAir + KitchenQual + Fireplaces,                     #|
                 data = Train_normal_3)                                                                                               #|
                                                                                                                                      #|
#Adding Dataframe                                                                                                                     #|
                                                                                                                                      #|
model.train <- mean(exp(predict(final.model, ProjectDataTraining)))                                                                   #|
model.test <- mean(exp(predict(final.model, ProjectDataTest)))                                                                        #|
model.train > model.test                                                                                                              #|
                                                                                                                                      #|
#Predicting Final Model                                                                                                               #|
                                                                                                                                      #|
predict.train <- exp(predict(final.model, ProjectDataTraining, interval = "prediction"))                                              #|
coverage.prob.train <- mean(ProjectDataTraining$SalePrice > predict.train[,"lwr"] &                                                   #|
                              ProjectDataTraining$SalePrice < predict.train[,"upr"])                                                  #|
                                                                                                                                      #|
predict.full <- exp(predict(final.model, ProjectDataTest, interval = "prediction"))                                                   #|
coverage.prob.full <- mean(ProjectDataTest$SalePrice > predict.full[,"lwr"] &                                                         #|
                             ProjectDataTest$SalePrice < predict.full[,"upr"])                                                        #|
                                                                                                                                      #|
#===================================================(SUMMARY FOR MODELS STEP 1 TO 4)==================================================#|
#Summary for models 1 - 4                                                                                                             #|
                                                                                                                                      #|
summary(allvar.model.step1)                                                                                                           #|
summary(allvar.model.step2)                                                                                                           #|
summary(allvar.model.step3)                                                                                                           #|
summary(Allvar.model.step4)                                                                                                           #|
                                                                                                                                      #|
#======================================================(PAIRING COLUMNS TOGETHER)=====================================================#|
#GGPAIRS plots on Train_Normal_2 - Plotting Columns together                                                                          #|
                                                                                                                                      #|
ggpairs(Train_normal_2, columns = c(5,6), main = "Years of remodeling and building")                                                  #|
ggpairs(Train_normal_2, columns = c(7,8), main = "Heating and air")                                                                   #|
ggpairs(Train_normal_2, columns = c(2, 8, 9, 12), main = "Areas")                                                                     #|
ggpairs(Train_normal_2, columns = c(10, 2), main = "Kitchen vs area")                                                                 #|
                                                                                                                                      #|
#=======================================================(sTEP 4 SUMMARY & IMAGE)======================================================#|
#Summary & image of model 4 - Limited to rank 5                                                                                       #|
                                                                                                                                      #|
summary(model.step4.bas)                                                                                                              #|
image(model.step4.bas, top.models = 5)                                                                                                #|
                                                                                                                                      #|
#=====================================================(PROBABILITY OF TRAIN & TEST)===================================================#|
#Accuracy of training and test data                                                                                                   #|
                                                                                                                                      #|
coverage.prob.train                                                                                                                   #|
coverage.prob.full                                                                                                                    #|
                                                                                                                                      #|
#========================================================(PLOTTING FINAL MODEL)=======================================================#|
#Plotting Final Model                                                                                                                 #|
                                                                                                                                      #|
hist(final.model$residuals)                                                                                                           #|
qqnorm(final.model$residuals)                                                                                                         #|
qqline(final.model$residuals)                                                                                                         #|
                                                                                                                                      #|
write.csv(Train_normal_3,'TrainNormal.csv')                                                                                           #|
                                                                                                                                      #|
ProjectAmend <- read.table("TrainNormal.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",")                                     #|
                                                                                                                                      #|
ProjectAmend2 <- read.table("TrainNormal2.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",")                                   #|
                                                                                                                                      #|
prediction <- predict(final.model, Train_normal_3)                                                                                    #|
exp(prediction)                                                                                                                       #|             
                                                                                                                                      #|
#=====================================================(PREDICTING INPUT FROM USER)====================================================#|
#'Making a function to implement the prediction                                                                                       #|
#'Data can be mended together in order to use it through the model to create the prediction                                           #|
#'@param predictions                                                                                                                  #|
#'creating a function called predcitions so when its called upon it will run everything inside the function                           #|
#'@param setwd                                                                                                                        #|
#'Setting the workspace that the files will be sitting in in order to read the files in                                               #|
#'@param traindata3                                                                                                                   #|
#'reading in the file trainormal and saving it to the name traindata3 in order to use                                                 #|
#'@param inputdata                                                                                                                    #|
#'reading in data that is drawn from the UI in order to mend the data together                                                        #|
#'@param finaldata                                                                                                                    #|
#'Using this name as a final outcome from both traindata and inputdata                                                                #|
#'@param rbind                                                                                                                        #|
#'using Rbind in order to combine the data from input and move it onto traindata                                                      #|
#'@param pd                                                                                                                           #|
#'using the final model and finaldata in order to make a prediction                                                                   #|
#'@param res                                                                                                                          #|                             
#'taking the data and final model and outputting the prediction                                                                       #|
#'@param print                                                                                                                        #|
#'printing the predciton to the console                                                                                               #|
#'@return                                                                                                                             #|
#'The input datas price prediction                                                                                                    #|
#'@examples                                                                                                                           #|
#'data <- read.csv/read.table header = "?" sep = "?"                                                                                  #|
#'outputdata <- rbind(data,data)                                                                                                      #|
#'prediction <- predict(model, data[1000,])                                                                                           #|
                                                                                                                                      #| 
predictions <- function(){                                                                                                            #|
                                                                                                                                      #|
write.csv(Train_normal_3, file = "TrainNormal.csv")                                                                                   #|
traindata3 <- read.table(file = "TrainNormal.csv", header = TRUE, sep = ",")                                                          #|
traindata3 <- traindata3[,-1]                                                                                                         #|
                                                                                                                                      #|
inputdata <- read.csv("TrainNormal2.csv", header = TRUE, sep = ",")                                                                   #|
inputdata <- inputdata[,-1]                                                                                                           #|
                                                                                                                                      #|
finaldata <- rbind(traindata3, inputdata)                                                                                             #|
pd <- predict(final.model, finaldata[814,])                                                                                           #|
                                                                                                                                      #|
res <- exp(pd)                                                                                                                        #|
print(paste("Prediction Result is ", res))                                                                                            #|
                                                                                                                                      #|
write.csv(res, file = "resdata.csv")                                                                                                  #|
                                                                                                                                      #|
}                                                                                                                                     #|
predictions()                                                                                                                         #|
                                                                                                                                      #|
#===========================================================(PLOTTING GRAPHS)=========================================================#|
#Plotting graphs. Commented out so UI runs smoothly                                                                                   #|
                                                                                                                                      #|
#plot(model.bic.full)                                                                                                                 #|
#plot(final.model)                                                                                                                    #|
                                                                                                                                      #|
#=======================================================(UNUSED/NOT WORKING CODE)=====================================================#|
#Code not being used. Either Not working or may come in useful                                                                        #|
#                                                                                                                                     #|
#ProjectData$MSSubClass <- factor(ProjectData$MSSubClass, levels = c(20, 30, 40, 45, 50, 60, 70, 75, 80, 85, 90, 120, 150,            #|
#                                 160, 180, 190),                                                                                     #|
#                                 labels = c("1-STORY 1946 & NEWER ALL STYLES", "1-STORY 1945 & OLDER",                               #|
#                                 "1-STORY W/ FINISHED ATTIC ALL AGES",                                                               #|
#                                 "1-1/2 STORY - UNFINISHED ALL AGES", "1-1/2 STORY FINISHED ALL AGES",                               #|
#                                 "2-STORY 1946 & NEWER",                                                                             #|
#                                 "2-STORY 1945 & OLDER", "2-1/2 STORY ALL AGES","SPLIT OR MULTI-LEVEL",                              #|
#                                 "SPLIT FOYER", "DUPLEX - ALL STYLES AND AGES","1-STORY PUD 1946 & NEWER",                           #|
#                                 "1-1/2 STORY PUD - ALL AGES", "2 STORY PUD - 1946 & NEWER",                                         #|
#                                 "PUD - MULTILEVEL INCL SPLIT LEV/FOYER", "2 FAMILY CONVERSION - ALL STYLES AND AGES"))              #|
#ProjectRemoveID <- ProjectData[,-1]                                                                                                  #|
#ProjectRemoveStreet <- ProjectRemoveID[,-5]                                                                                          #|
#ProjectRemoveAlley <- ProjectRemoveStreet[,-5]                                                                                       #|
#ProjectRemoveRoofStyle <- ProjectRemoveAlley[,-19]                                                                                   #|
#ProjectData <- ProjectData %>%                                                                                                       #|
#mutate(ProjectData$Alley = replace(ProjectData$Alley, is.na(Projectdata),"unavailable"))                                             #|
#ProjectData$Id <- NULL                                                                                                               #|
#ProjectData$Street <- NULL                                                                                                           #|
#ProjectData$Alley <- NULL                                                                                                            #|
#ProjectData$RoofStyle <- NULL                                                                                                        #|
#ProjectData$MasVnrArea <- NULL                                                                                                       #|
#ProjectData$BsmtUnfSF <- NULL                                                                                                        #|
#plot(ProjectData$MSZoning, ProjectData$SalePrice)                                                                                    #|
#plot(ProjectData$Fence, ProjectData$SalePrice)#Removed                                                                               #|
#plot(ProjectData$LotFrontage, ProjectData$SalePrice)#Removed                                                                         #|
#plot(ProjectData$LotArea, ProjectData$SalePrice)#Removed                                                                             #|
#plot(ProjectData$LotShape, ProjectData$SalePrice)                                                                                    #|
#plot(ProjectData$LandContour, ProjectData$SalePrice)                                                                                 #|
#plot(ProjectData$LandSlope, ProjectData$SalePrice)                                                                                   #| 
#(lm(ProjectData$LotArea ~ ProjectData$SalePrice),data = ProjectData)                                                                 #|
#title <- c()                                                                                                                         #|
#rvalue <- c()                                                                                                                        #|
#pvalue <- c()                                                                                                                        #|
#fvalue <- c()                                                                                                                        #|
#for (i in seq(1:80)){                                                                                                                #|
#themodel <- lm(as.numeric(ProjectData[,i]) ~ ProjectData$SalePrice, data = ProjectData)                                              #|
#summary <- summary(themodel)                                                                                                         #|
#title<-c(title,colnames(ProjectData)[i])                                                                                             #|
#rvalue<- c(rvalue,as.numeric(summary$r.squared))                                                                                     #|
#pvalue<-c(pvalue,as.numeric(anova(themodel)$'Pr(>F)'[1]))                                                                            #|
#fvalue <- c(fvalue,as.numeric(summary$fstatistic[1]))                                                                                #|
#}                                                                                                                                    #|
#Project <- data.frame(title, rvalue, pvalue, fvalue)                                                                                 #|
#ProjectData$MSZoning <- factor(ProjectData$MSZoning, levels = c("A", "C", "FV", "I", "RH", "RL", "RP", "RM"),                        #|
#labels = c("AGRICULTURE","COMMERCIAL","FLOATING VILLAGE RESIDENTIAL",                                                                #|
#"INDUSTRIAL","RESIDENTIAL HIGH DENSITY",                                                                                             #|
#"RESIDENTIAL LOW DENSITY","RESIDENTIAL LOW DENSITY PARK",                                                                            #|
#"RESIDENTIAL MEDIUM DENSITY"))                                                                                                       #|
#ProjectData$Street <- factor(ProjectData$Street, levels = c("Grvl","Pave"),                                                          #|
#labels = c("Gravel","Paved"))                                                                                                        #|
#ProjectData$LotShape <- factor(ProjectData$LotShape, levels = c("Reg","IR1","IR2","IR3"),                                            #|
#labels = c("Regular","Slightly Irregular",                                                                                           #|
#"Moderately Irregular","Irregular"))                                                                                                 #|
#ProjectData$LandContour <- factor(ProjectData$LandContour, levels = c("Lvl","Bnk","Hls","Low"),                                      #|
#labels = c("Near/Flat Level","Banked, Significant rise from street","Hillside - Significant slope","Depression"))                    #|
#ProjectData$Utilities <- factor(ProjectData$Utilities, levels = c("AllPub","NoSwer","NoSeWa","ELO"),                                 #|
#labels = c("All public utilities","Electricity, gas and water","Electricity and gas","Electricity only"))                            #|
#ProjectData$LotConfig <- factor(ProjectData$LotConfig, levels = c("Inside", "Corner","CulDSac","FR2","FR3"),                         #|
#labels = c("Inside lot","Corner lot","Cul-de-sac","Frontage on 2 sides","Frontage on 3 sides"))                                      #|
#ProjectData$LandSlope <- factor(ProjectData$LandSlope, levels = c("Gtl","Mod","Sev"),                                                #|
#labels = c("Gente slope","Moderate slope","Severe Slope"))                                                                           #|
#ProjectData$Neighborhood <- factor(ProjectData$Neighborhood, levels = c("Blmngtn","Blueste","BRDale","BrkSide","ClearCr","CollegCr", #|
#"Crawfor","Edwards","Gilbert","IDOTRR","MeadowV","Mitchel","Names","NoRidge","NPKVill","NridgHt","NWAmes","OldTown","SWISU",         #|
#"Sawyer","SawyerW","Somerst","StoneBr","Timber","Veenker"),                                                                          #|
#labels = c("Bloomington Heights","Bluestem","Briardale","Brookside","Clear Creek","College Creek","Crawford","Edwards","Gilbert",    #|
#"Iowa DOT and rail road","Meadow Village","Mitchell","North ames","Northridge","Northpark villa","Northridge heights",               #|
#"Northwest ames","Old town","South & west of iowa","Sawyer","Sawyer west","Somerset","stone brook","Timberland","Veenker"))          #|
#ProjectData$Condition1 <- factor(ProjectData$Condition1, levels = c("Artery","Feedr","Norm","RRNn","RRAn","PosN","PosA","RRNe",      #|
#'"RRAe"),                                                                                                                            #|
#labels = c("Adjacent to arterial street","Ajacent to feeder street","Normal","Within 200' of north-south railroad",                  #|
#"Adjacent to north-south railroad","Near positive offsite feature","Adjacent to positive offsite feature",                           #|
#"Within 200' of east-west railroad","Adjacent to east-west railroad"))                                                               #|
#ProjectData$Condition2 <- factor(ProjectData$Condition2, levels = c(),                                                               #|
#labels = c())                                                                                                                        #|
#database[database==""] <- NA                                                                                                         #|
#database[database=="a"] <- NA                                                                                                        #|
# ProjectDataTraining[ProjectDataTraining==""] <-"NA"                                                                                 #|
# ProjectDataTraining[ProjectDataTraining=="0"] <- "NA"                                                                               #|
# ProjectDataTest[ProjectDataTest==""] <-"NA"                                                                                         #|
# ProjectDataTest[ProjectDataTest=="0"] <- "NA"                                                                                       #|
# setwd("C:/Users/Admin/Desktop/RStudio&Files/Github")                                                                                #|
# #Reading in CSV files & Splitting Data Into Two Files                                                                               #|
# ProjectData <- read.table("train.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",")                                          #|
# ProjectDataTes <- read.table("test.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",")                                        #|
# ProjectDataTraining <- ProjectData[1:1000,]                                                                                         #|
# ProjectDataTest <- ProjectData[1001:1460,]                                                                                          #|
#=====================================================================================================================================#|