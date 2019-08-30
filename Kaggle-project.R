##PART A) CLEAN DATA
##1) Combine testing and training together
library(readr)
library(dplyr)
library(ggplot2)
library(ggcorrplot)
testing <- read.csv("HTestLastNoY.csv")
training <- read.csv("HTrainLast.csv")
training <- training[-which(is.na(training$affordabilitty) == TRUE), ]
testing$affordabilitty <- NA
training <- training %>% mutate_if(is.character, as.factor)
testing <- testing %>% mutate_if(is.character, as.factor)
alldata <- rbind(testing,training)


sapply(alldata, function(x) sum(is.na(x)) ) # count of all NAs in each column

colnames(alldata)[44] <- "FirstFlrSF"
colnames(alldata)[45] <- "SecondFlrSF"
colnames(alldata)[70] <- "ThreeSsnPorch"


##2) Create functions
# impute the median into NAs
impute.median <- function(x) {
  z <- median(x, na.rm = TRUE)
  x[is.na(x)] <- z
  return(x)
}

# impute the mode into NAs 
# MODE function: 
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


##3) Type Conversion
#Make these numerically listed variables into factors
alldata$MSSubClass <- factor(alldata$MSSubClass)
alldata$OverallCond <- factor(alldata$OverallCond)
alldata$OverallQual <- factor(alldata$OverallQual)
alldata$BedroomAbvGr <- factor(alldata$BedroomAbvGr)
alldata$KitchenAbvGr <- factor(alldata$KitchenAbvGr)
alldata$Fireplaces <- factor(alldata$Fireplaces)
alldata$MoSold <- factor(alldata$MoSold)
alldata$Fireplaces <- factor(alldata$Fireplaces)
alldata$YrSold <- factor(alldata$YrSold)
alldata$TotRmsAbvGrd <- factor(alldata$TotRmsAbvGrd)
alldata$GarageCars <- factor(alldata$GarageCars)

#Graph to show MSSubClass after type conversion
ggplot(alldata, aes(x=alldata$MSSubClass, y=..count../sum(count))) +
  geom_bar(cex=.75, fill="dark blue")


##4) Replace NAs
#Replace NA values with the mode (most frequently appeared value)
alldata$MSZoning[is.na(alldata$MSZoning)] <- getmode(alldata$MSZoning)
alldata$Exterior1st[is.na(alldata$Exterior1st)] <- getmode(alldata$Exterior1st)
alldata$Exterior2nd[is.na(alldata$Exterior2nd)] <- getmode(alldata$Exterior2nd)
alldata$Electrical[is.na(alldata$Electrical)] <- getmode(alldata$Electrical)
alldata$KitchenQual[is.na(alldata$KitchenQual)] <- getmode(alldata$KitchenQual)
alldata$Functional[is.na(alldata$Functional)] <- getmode(alldata$Functional)
alldata$SaleType[is.na(alldata$SaleType)] <- getmode(alldata$SaleType)


#Replace NA values with the median
alldata$LotFrontage <- impute.median(alldata$LotFrontage)


#Replace NA values with a factor level None
alldata$Alley <- as.character(alldata$Alley)
alldata$Alley[is.na(alldata$Alley)] <- "None"
alldata$Alley <- as.factor(alldata$Alley)

alldata$MasVnrType <- as.character(alldata$MasVnrType)
alldata$MasVnrType[is.na(alldata$MasVnrType)] <- "None"
alldata$MasVnrType <- as.factor(alldata$MasVnrType)

alldata$BsmtQual <- as.character(alldata$BsmtQual)
alldata$BsmtQual[is.na(alldata$BsmtQual)] <- 'None'
alldata$BsmtQual <- as.factor(alldata$BsmtQual)

alldata$BsmtCond <- as.character(alldata$BsmtCond)
alldata$BsmtCond[is.na(alldata$BsmtCond)] <- 'None'
alldata$BsmtCond <- as.factor(alldata$BsmtCond)

alldata$BsmtExposure <- as.character(alldata$BsmtExposure)
alldata$BsmtExposure[is.na(alldata$BsmtExposure)] <- 'None'
alldata$BsmtExposure <- as.factor(alldata$BsmtExposure)

alldata$BsmtFinType1 <- as.character(alldata$BsmtFinType1)
alldata$BsmtFinType1[is.na(alldata$BsmtFinType1)] <- 'None'
alldata$BsmtFinType1 <- as.factor(alldata$BsmtFinType1)

alldata$BsmtFinType2 <- as.character(alldata$BsmtFinType2)
alldata$BsmtFinType2[is.na(alldata$BsmtFinType2)] <- 'None'
alldata$BsmtFinType2 <- as.factor(alldata$BsmtFinType2)

alldata$FireplaceQu <- as.character(alldata$FireplaceQu)
alldata$FireplaceQu[is.na(alldata$FireplaceQu)] <- 'None'
alldata$FireplaceQu <- as.factor(alldata$FireplaceQu)

alldata$GarageType <- as.character(alldata$GarageType)
alldata$GarageType[is.na(alldata$GarageType)] <- 'None'
alldata$GarageType <- as.factor(alldata$GarageType)

alldata$GarageYrBlt <- as.character(alldata$GarageYrBlt)
alldata$GarageYrBlt[is.na(alldata$GarageYrBlt)] <- 'None'
alldata$GarageYrBlt <- as.factor(alldata$GarageYrBlt)

alldata$GarageFinish <- as.character(alldata$GarageFinish)
alldata$GarageFinish[is.na(alldata$GarageFinish)] <- 'None'
alldata$GarageFinish <- as.factor(alldata$GarageFinish)

alldata$GarageQual <- as.character(alldata$GarageQual)
alldata$GarageQual[is.na(alldata$GarageQual)] <- 'None'
alldata$GarageQual <- factor(alldata$GarageQual)

alldata$GarageCond <- as.character(alldata$GarageCond)
alldata$GarageCond[is.na(alldata$GarageCond)] <- 'None'
alldata$GarageCond <- as.factor(alldata$GarageCond)

alldata$PoolQC <- as.character(alldata$PoolQC)
alldata$PoolQC[is.na(alldata$PoolQC)] <- 'None'
alldata$PoolQC <- as.factor(alldata$PoolQC)

alldata$Fence <- as.character(alldata$Fence)
alldata$Fence[is.na(alldata$Fence)] <- 'None'
alldata$Fence <- as.factor(alldata$Fence)

alldata$MiscFeature <- as.character(alldata$MiscFeature)
alldata$MiscFeature[is.na(alldata$MiscFeature)] <- 'None'
alldata$MiscFeature <- as.factor(alldata$MiscFeature)


#Replace NA values with 0
alldata$MasVnrArea[is.na(alldata$MasVnrArea)] <- 0
alldata$BsmtFinSF1[is.na(alldata$BsmtFinSF1)] <- 0
alldata$BsmtFinSF2[is.na(alldata$BsmtFinSF2)] <- 0
alldata$BsmtUnfSF[is.na(alldata$BsmtUnfSF)] <- 0
alldata$TotalBsmtSF[is.na(alldata$TotalBsmtSF)] <- 0
alldata$BsmtFullBath[is.na(alldata$BsmtFullBath)] <- 0
alldata$BsmtHalfBath[is.na(alldata$BsmtHalfBath)] <- 0
alldata$GarageArea[is.na(alldata$GarageArea)] <- 0
alldata$GarageCars[is.na(alldata$GarageCars)] <- 0



##5) Combine variables
#Create AgeofHouse
alldata$AgeofHouse <- alldata$YearRemodAdd-alldata$YearBuilt #not correct
alldata$AgeofHouse <- 2010 - alldata$YearBuilt #fixed version
#Combine BsmtFullBath and BsmtHalfBath into BsmtBath
alldata$BsmtBath <- alldata$BsmtFullBath + .5*alldata$BsmtHalfBath
alldata$BsmtBath <- factor(alldata$BsmtBath)
#Combine FullBath and HalfBath
alldata$Bath <- alldata$FullBath + .5*alldata$HalfBath
alldata$Bath <- factor(alldata$Bath)
#Exclude YearBuilt, BsmtBath, BsmtFullBath, FullBath, HalfBath from model


##6) Removal of variables
#Remove variables with near zero variance
library(caret)
alldata[,nearZeroVar(alldata)] %>% colnames()
#REMOVE Street, Alley, LandContour, Utilities, LandSlope, Condition2, RoofMatl, MasVnrArea, BsmtCond, BsmtFinType2
#REMOVE BsmtFinSF2, Heating, LowQualFinSF, KitchenAbvGr, Functional, WoodDeckSF, OpenPorchSF, EnclosedPorch, ThreeSsnPorch, 
#REMOVE ScreenPorch, PoolArea, PoolQC, MiscFeature, MiscVal

#Graphical example of a variable with infrequently occurring levels
alldata$RoofMatl %>% table() %>% barplot(horiz=TRUE, col=c(1:7), names.arg=c("ClyTile", "CompShg", "Membran", "Roll","Tar&Grv","WdShake", "WdShngl"), las=1)


#Separate numerical and categorical variables into two data frames
variabletype <- lapply(alldata, is.numeric) %>% unlist
numerical <- alldata[,which(variabletype=="TRUE")]
categorical <- alldata[,which(variabletype=="FALSE")]
categorical_tabled <- apply(categorical, 2, table)


#Check for highly correlated variables
numerical <- numerical[,-1]
numerical <- numerical[,-5]
numerical <- numerical[,-6]
numerical <- numerical[,-10]
numerical <- numerical[,-c(12:18)]
numerical <- numerical[,-3]
numerical <- numerical[,-3]
numerical <- numerical[,-11]
numerical$GarageYrBlt <- as.numeric(alldata$GarageYrBlt)
numerical$TotRmsAbvGrd <- as.numeric(alldata$TotRmsAbvGrd)
numerical$GarageCars <- as.numeric(alldata$GarageCars)
cor(numerical)

#Correlation matrix graph
ggcorrplot(cor(numerical)) +
  theme(axis.text.x = element_text(size=10))

#REMOVE TotRmsAbvGrd, GarageYrBlt, GarageCars, TotalBsmtSF because of high correlation
cor(as.numeric(alldata$GarageYrBlt), alldata$AgeofHouse)
cor(as.numeric(alldata$TotRmsAbvGrd), alldata$GrLivArea)
cor(as.numeric(alldata$GarageCars), alldata$GarageArea)
cor(alldata$FirstFlrSF, alldata$TotalBsmtSF)

#Graph for correlation
ggplot(training, aes(x=as.numeric(training$TotRmsAbvGrd), y= training$GrLivArea)) +
  geom_point(cex=.75, col="dark blue")


#Looking at density plots for numerical variables
ggplot(training[which(training$LotFrontage < 200),], aes(LotFrontage, col=as.factor(affordabilitty))) +
  geom_density()  + theme(text = element_text(size=15), legend.justification = c(1, 1), legend.position = c(1, 1))
ggplot(training[which(training$LotArea < 50000),], aes(LotArea, col=as.factor(affordabilitty))) +
  geom_density() 
ggplot(training[which(training$BsmtFinSF1 != 0),], aes(BsmtFinSF1, col=as.factor(affordabilitty))) +
  geom_density() 
ggplot(training, aes(BsmtUnfSF, col=as.factor(affordabilitty))) +
  geom_density() 
ggplot(training[which(training$TotalBsmtSF != 0),], aes(TotalBsmtSF, col=as.factor(affordabilitty))) +
  geom_density() 
ggplot(training, aes(FirstFlrSF, col=as.factor(affordabilitty))) +
  geom_density() 
ggplot(training[which(training$SecondFlrSF != 0),], aes(SecondFlrSF, col=as.factor(affordabilitty))) +
  geom_density() + theme(text = element_text(size=12), legend.justification = c(1, 1), legend.position = c(1, 1))
ggplot(training, aes(GrLivArea, col=as.factor(affordabilitty))) +
  geom_density() + theme(text = element_text(size=17), legend.justification = c(1, 1), legend.position = c(1, 1))
ggplot(training, aes(GarageArea, col=as.factor(affordabilitty))) +
  geom_density() 
ggplot(training, aes(AgeofHouse, col=as.factor(affordabilitty))) +
  geom_density() 
#REMOVE based on density plots: LotFrontage, LotArea

#Further exploration on categorical variables to see which to remove for many infrequently occurring levels
categorical_tabled$Neighborhood %>% plot
categorical_tabled$BldgType %>% plot
categorical_tabled$RoofStyle %>% plot
categorical_tabled$CentralAir %>% plot
categorical_tabled$Electrical %>% plot
categorical_tabled$GarageQual %>% plot
categorical_tabled$GarageCond %>% plot
categorical_tabled$PavedDrive %>% plot
categorical_tabled$Fence %>% plot
#REMOVE Neighborhood, BldgType, RoofStyle, CentralAir, Electrical, GarageQual, GarageCond, PavedDrive, Fence


#PART B) TESTING
testing <- alldata[1:1500,]
training <- alldata[1501:4998,]
training$affordabilitty <- factor(training$affordabilitty)
testing <- testing[,-which(colnames(testing)=="affordabilitty")]


set.seed(5)
library(tree)
library(randomForest)
m2 <- randomForest(affordabilitty ~ MSSubClass + MSZoning + LotShape + LotConfig + Condition1 + HouseStyle + OverallQual + 
                     OverallCond + AgeofHouse + Exterior1st + Exterior2nd + MasVnrType + ExterQual + ExterCond + Foundation +
                     BsmtQual + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtUnfSF + HeatingQC + 
                     FirstFlrSF + SecondFlrSF + GrLivArea + BsmtBath + Bath + BedroomAbvGr + KitchenQual + Fireplaces + 
                     FireplaceQu + GarageType + GarageFinish + GarageArea + MoSold + YrSold + SaleType + 
                     SaleCondition, data = training, mtry=8, importance = TRUE)
m2

important <- importance(m2)
important[,3] %>% sort(decreasing=T)
important[,4] %>% sort(decreasing=T)
varImpPlot(m2)
plot(m2)

prediction <- predict(m2,newdata=testing,type="response")
predictedvals <- as.data.frame(prediction)
predictedvals$Ob <- 1:nrow(predictedvals) # create obs column 1:1500
predictedvals <- subset(predictedvals, select=c(2,1))
colnames(predictedvals)[2] <- "affordabilitty"
write.csv(predictedvals, "Kaggle_predictions_Dec9.csv", row.names = FALSE)


#Tree
m5 <- tree(factor(affordabilitty) ~ MSSubClass + MSZoning + Neighborhood + LotShape + LotConfig + Condition1 + HouseStyle + OverallQual + 
             OverallCond + AgeofHouse + Exterior1st + Exterior2nd + MasVnrType + ExterQual + ExterCond + Foundation +
             BsmtQual + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtUnfSF + TotalBsmtSF + HeatingQC + 
             FirstFlrSF + SecondFlrSF + GrLivArea + BsmtBath + Bath + BedroomAbvGr + KitchenQual + Fireplaces + 
             FireplaceQu + GarageType + GarageFinish + GarageArea + MoSold + YrSold + SaleType + 
             SaleCondition, data = training)
preds <- predict(m5, newdata=training, type="class")
table(preds, training$affordabilitty)
misclass_test_error <- mean(preds != training$affordabilitty)
misclass_test_error

cv.train <- cv.tree(m5,FUN=prune.misclass)
plot(cv.train$dev~cv.train$size, type="b")
pruned.tree <- prune.misclass(m5,best=4)
plot(pruned.tree)
text(pruned.tree,pretty=TRUE)
summary(pruned.tree)


#Ridge Regression
training <- training[, -10]
testing <- testing[, -10]
library(glmnet)
x <- model.matrix(affordabilitty ~., data = training)
xtest <- testing
xn <- model.matrix(~., data = xtest)
y <- training$affordabilitty
cv <- cv.glmnet(x,y,alpha = 0, family = "binomial")
bestlam <- cv$lambda.min
ridge <- glmnet(x,y, alpha = 0, lambda = bestlam, family = "binomial")
fit.ridge.pred <- predict(ridge, xn, type = "response")
pred.logit <- rep(0, length(fit.ridge.pred))
pred.logit[fit.ridge.pred >= 0.5] <- 1
predval <- as.data.frame(pred.logit)
predval$Ob <- 1:nrow(predval)
predval <- subset(predval, select = c(2,1))
predval$affordabilitty <- ifelse(predval$pred.logit == 1, 'Unaffordable', "Affordable")
predval <- predval[, -2]
#write.csv(predval, "Ridge.csv", row.names = FALSE)


#Lasso
x <- model.matrix(affordabilitty ~., data = training)
xtest <- testing
xn <- model.matrix(~., data = xtest)
y <- training$affordabilitty
cv <- cv.glmnet(x,y, alpha = 1, family = "binomial")
bestlam <- cv$lambda.min
ridge <- glmnet(x,y, alpha = 1, lambda = bestlam, family = "binomial")
fit.ridge.pred <- predict(ridge, xn, type = "response")
pred.logit <- rep(0, length(fit.ridge.pred))
pred.logit[fit.ridge.pred >= 0.5] <- 1
predval <- as.data.frame(pred.logit)
predval$Ob <- 1:nrow(predval)
predval <- subset(predval, select = c(2,1))
predval$affordabilitty <- ifelse(predval$pred.logit == 1, 'Unaffordable', "Affordable")
predval <- predval[, -2]
#write.csv(predval, "Lasso.csv", row.names = FALSE)



#Logistic Regression
model <- glm(affordabilitty ~ MSSubClass + MSZoning + LotShape + LotConfig + Condition1 + HouseStyle + OverallQual + 
               OverallCond + AgeofHouse + Exterior1st + Exterior2nd + MasVnrType + ExterQual + ExterCond + Foundation +
               BsmtQual + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtUnfSF + TotalBsmtSF + HeatingQC + 
               FirstFlrSF + SecondFlrSF + GrLivArea + BsmtBath + Bath + BedroomAbvGr + KitchenQual + Fireplaces + 
               FireplaceQu + GarageType + GarageFinish + GarageArea + MoSold + YrSold + SaleType + 
               SaleCondition, data=training, family=binomial)
summary(model)
prediction <- predict(model, data=training, type="response")
pred.logit <- rep(0,length(prediction))
pred.logit[prediction>=0.5] <- 1
table(pred.logit, training$affordabilitty)


## Boosting
library(gbm)
training$affordabilitty <- ifelse(training$affordabilitty == 'Affordable', 1,0)

boost.train <- gbm(affordabilitty~.,data=training, distribution="bernoulli",n.trees =1000 , interaction.depth =4)
summary(boost.train)
