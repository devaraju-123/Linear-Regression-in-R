head(cars)
scatter.smooth(x=cars$speed, y=cars$dist, main="Dist ~ Speed")

#divide graph area in 2 coloumns
par(mfrow=c(1,2))
boxplot(cars$speed,main="Speed",sub=paste("Outlier rows: ", boxplot.stats(cars$speed)$out))###boxsplot of speed
##box plot for distance
boxplot(cars$dist, main="Distance",sub=paste("Outlier rows: ",boxplot.stats(cars$dist)$out))

##calculated correlation b/w speed and distance
cor(cars$speed, cars$dist)

#build leaner model

linerMod <- lm(dist ~ speed, data = cars) #build liner regression model on full data
print(linerMod)

######Liner Regaression model Diagnostics

summary(linerMod)# model summary

modelSummary <- summary(linerMod)# capture model summary as an object
modelCoeffs <- modelSummary$coefficients# model coefficients
beta.estimate <- modelCoeffs["speed","Estimate"]#get beta estimate for speed
std.error <- modelCoeffs["speed", "Std. Error"] #get std.error for speed

t_value <- beta.estimate/std.error#calc t statistic
p_value <- 2*pt(-abs(t_value), df=nrow(cars)-ncol(cars))  #cal p vlaue

f_statistic <- linerMod$fstatistic[1]  #fstatistic
f <- summary(linerMod)$fstatistic  #parameters for model p-value calc
model_p <- pf(f[1],f[2],f[3], lower=FALSE)


###STEP1: Create the training (development) and test(validaton) data samples form original data
#creat training and test dasta

set.seed(100) #setting seed to reproduce results of random sampling

trainingRowIndex <- sample(1:nrow(cars), 0.8*nrow(cars)) #row indices for  data
trainingData <- cars[trainingRowIndex, ]# model training data
testData <- cars[-trainingRowIndex, ] #test data


#STEP2: Develop the model on training data and use it to predic the distanc on the test

#Build the model on training data
lmMod <- lm(dist ~ speed, data=trainingData) #Build the model
distPred <-predict(lmMod, testData) #predict distance

##STEP3: Review diagnostic measures
summary(lmMod)
AIC (lmMod)
### STEP4:  calulate prediction accuracy ad error rates
actuals_preds <- data.frame(cbind(actuals=testData$dist, predicteds=distPred)) #make actuals_prodicteds data frame
correlation_accracy <- cor(actuals_preds)
head(actuals_preds)

##lets calu the Min Max accuracy and MAPE
min_max_accuracy <- mean(apply(actuals_preds, 1, min)/ apply(actuals_preds,1,max))
mape <- mean(abs((actuals_preds$predicteds -actuals_preds$actuals))/actuals_preds$actuals)



