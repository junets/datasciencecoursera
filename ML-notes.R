library(caret)
library(kernlab)
data("spam")
inTrain <- createDataPartition(y = spam$type, p = 0.75, list = FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)

# data slicing
set.seed(32343)
modelFit <- train(type ~ ., data = training, method = 'glm')
modelFit

predictions <- predict(modelFit, newdata = testing)
predictions
confusionMatrix(predictions, testing$type)

folds <- createFolds(y = spam$type, k = 10, 
                     list = TRUE, returnTrain = TRUE)
folds <- createFolds(y = spam$type, k = 10, 
                     list = T, returnTrain = F)
folds <- createResample(y = spam$type, times = 10, 
                     list = TRUE)
tme <- 1:1000
folds <- createTimeSlices(y = tme, initialWindow = 20, horizon = 10)

names(folds)

folds$train[[1]]
folds$test[[1]]
folds
sapply(folds, length)

library(ISLR)
data(Wage)
summary(Wage)
library(ggplot2)
inTrain <- createDataPartition(y = Wage$wage, p = .7, list = FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training);dim(testing)

featurePlot(x = training[,c('age', 'education', 'jobclass')],
            y = training$wage,
            plot = 'pairs')

qplot(age, wage, data = training)
qplot(age, wage, colour = jobclass, data = training)
qq <- qplot(age, wage, colour = jobclass, data = training)
qq + geom_smooth(method = 'lm', formula = y ~ x)
library(Hmisc)
cutWage <- cut2(training$wage, g = 3)
table(cutWage)

p1 <- qplot(cutWage, age, data = training, fill = cutWage, geom = 'boxplot')
p1
p2 <- qplot(cutWage, age, data = training, fill = cutWage, geom = c('boxplot', 'jitter'))
p2
grid.arrange(p1,p2,ncol=2)

t1 <- table(cutWage, training$jobclass)
t1
prop.table(t1,1)
qplot(wage, colour=education, data = training, geom = 'density')

# preprocess
library(caret); library(kernlab) ; data(spam)
inTrain <- createDataPartition(y = spam$type, p = 0.75, list = FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
hist(training$capitalAve, main="", xlab = "ave. capital run length")
mean(training$capitalAve)
sd(training$capitalAve)
trainCapAve <- training$capitalAve
# standardizing
trainCapAveS <- (trainCapAve - mean(trainCapAve))/sd(trainCapAve)
mean(trainCapAveS)
sd(trainCapAveS)

preObj <- preProcess(training[,-58], method = c('center','scale'))
trainCapAveS <- predict(preObj, training[,-58])$capitalAve
mean(trainCapAveS)
sd(trainCapAveS)

testCapAveS <- predict(preObj, testing[,-58])$capitalAve
mean(testCapAveS)
sd(testCapAveS)

set.seed(32343)
modelFit <- train(type ~ ., data = training, preProcess = c('center','scale'), method = 'glm')
modelFit

# box-cox transforms
preObj <- preProcess(training[,-58], method = c('BoxCox'))
trainCapAveS <- predict(preObj, training[,-58])$capitalAve
par(mfrow= c(1,2))
hist(trainCapAveS); qqnorm(trainCapAveS)

# imputing data
set.seed(13343)
# make some values NA
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1], size = 1, prob = 0.05) == 1
training$capAve[selectNA] <- NA
# imput and standardize
preObj <- preProcess(training[,-58], model = "knnImpute")
capAve <- predict(preObj, training[,-58])$capAve
# standardize true values
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth - mean(capAveTruth)) / sd(capAveTruth)

quantile(capAve - capAveTruth)
quantile((capAve - capAveTruth)[selectNA])
quantile((capAve - capAveTruth)[!selectNA])

# covariate creation
library(kernlab);data(spam)
spam$capitalAveSq <- spam$capitalAve^2

# covariate creation
library(ISLR);library(caret); data(Wage)
inTrian <- createDataPartition(y = Wage$wage, p = .7, list = FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]
# dummy variables
table(training$jobclass)
dummies <- dummyVars(wage ~ jobclass, data = training)
head(predict(dummies, newdata = training))

# removing zero covariates
nsw <- nearZeroVar(training, saveMetrics = TRUE)
nsw # remvoe the vars which is TRUE in zeroVar

# Spline basis
library(splines)
bsBasis <- bs(training$age, df = 3)
bsBasis

# fitting curves with splines
lml = lm(wage ~ bsBasis, data = training)
plot(training$age, training$wage, pch = 19, cex = 0.5)
points(training$age, predict(lml , newdata = training), col = 'red', pch = 19, cex = .5)

# splines on the test set
predict(bsBasis, age = testing$age)

# preprocessing with principal components analysis

## correlated predictors
library(caret);library(kernlab);data(spam)
inTrain = createDataPartition(y = spam$type, p = .75, list = FALSE)
training = spam[inTrain,]
testing = spam[-inTrain,]
M = abs(cor(training[,-58]))
diag(M) = 0
which(M > .8, arr.ind = TRUE)

names(spam)[c(34,32)]
plot(spam[,34], spam[,32])

smallSpam <- spam[,c(34,32)]
prComp <- prcomp(smallSpam)
plot(prComp$x[,1],prComp$x[,2])
prComp$rotation

typeColor <- ((spam$type == 'spam') * 1 + 1)
prComp <- prcomp(log10(spam[,-58] + 1))
plot(prComp$x[,1], prComp$x[,2], col = typeColor, xlab = 'PC1', ylab = 'PC2')

# PCA with caret
preProc <- preProcess(log10(spam[,-58]+1), method = 'pca', pcaComp = 2)
spamPC <- predict(preProc, log10(spam[,-58]+1))                      
plot(spamPC[,1], spamPC[,2], col = typeColor)

# preprocessing with PCA
preProc <- preProcess(log10(training[,-58]+1), method = 'pca', pcaComp = 2)
trainPC <- predict(preProc, log10(training[,-58]+1))
modelFit <- train(training$type ~ . , method = 'glm', data = trainPC)
testPC <- predict(preProc, log10(testing[,-58]+1))
confusionMatrix(testing$type, predict(modelFit, testPC))


library(caret); data(faithful); set.seed(333)
inTrain <- createDataPartition(y = faithful$waiting, p = .5, list = FALSE)
trainFaith <- faithful[inTrain,]
testFaith <- faithful[-inTrain,]
head(trainFaith)
plot(trainFaith$waiting, trainFaith$eruptions, pch = 19, col = 'blue', xlab = 'waiting', ylab = 'duration')
lines(trainFaith$waiting, lm1$fitted, lwd = 3)

lm1 = lm(eruptions ~ waiting, data = trainFaith)
summary(lm1)

# predict new variable
coef(lm1)[1] + coef(lm1)[2] * 80
newdata <- data.frame(waiting = 80)
predict(lm1, newdata = newdata)

plot(trainFaith$waiting, trainFaith$eruptions, pch = 19, col = 'blue', xlab = 'Waiting', ylab = 'Duration')
lines(trainFaith$waiting, predict(lm1), lwd =3)

plot(testFaith$waiting, testFaith$eruptions, pch = 19, col = 'blue', xlab = 'Waiting', ylab = 'Duration')
lines(testFaith$waiting, predict(lm1, newdata = testFaith), lwd = 3)

# calculate MSE on training
sum((lm1$fitted - trainFaith$eruptions)^2)
# MSE on test
sum((predict(lm1,newdata = testFaith) - testFaith$eruptions)^2)

# prediction intervals
pred1 <- predict(lm1, newdata = testFaith, interval = 'prediction')
ord <- order(testFaith$waiting)
plot(testFaith$waiting, testFaith$eruptions, pch = 19, col = 'blue')
matlines(testFaith$waiting[ord], pred1[ord,], type = 'l',col = c(1,2,2), lty = c(1,1,1), lwd = 3)

modFit <- train(eruptions ~ waiting, data = trainFaith, method = 'lm')
summary(modFit$finalModel)

library(ISLR)
library(ggplot2)
library(caret)
data(Wage)
Wage <- subset(Wage, select = -c(logwage))
summary(Wage)

inTrain <- createDataPartition(y = Wage$wage, p = .7, list = FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training)
dim(testing)

featurePlot(x = training[,c('wage', 'education', 'jobclass')],
            y = training$wage,
            plot = 'pairs')

qplot(age, wage, data = training)
qplot(age, wage, colour = jobclass, data = training)
qplot(age, wage, colour = education, data = training)
unique(Wage$education)
unique(Wage$jobclass)
modFit <- train(wage ~ age + factor(jobclass) + factor(education), method = 'lm', data = training)
finMod <- modFit$finalModel
print(modFit)
plot(finMod, 1, pch = 19, cex = .5, col = '#00000010')
qplot(finMod$fitted, finMod$residuals, colour = race, data = training)

plot(finMod$residuals, pch = 19)

pred <- predict(modFit, testing)
qplot(wage, pred, colour = year, data = testing)

modFitAll <- train(wage ~ . , data = training, method = 'lm')
pred <- predict(modFitAll, testing)
qplot(wage, pred, data = testing)

# Q4
library(caret)  
library(AppliedPredictiveModeling)  
set.seed(3433)data(AlzheimerDisease)  
adData = data.frame(diagnosis,predictors)  
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]  
testing = adData[-inTrain,]

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

trainingIL <- training[,grep("^IL", names(training))]
procTrain <- preProcess(trainingIL, method = "pca", thresh = 0.8 )
procTrain

# grep all columns with IL and diagnosis in the traning and testing set
trainingIL <- training[,grep("^IL|diagnosis", names(training))]
testingIL <- testing[,grep("^IL|diagnosis", names(testing))]

# non-PCA
model <- train(diagnosis ~ ., data = trainingIL, method = "glm")
predict_model <- predict(model, newdata= testingIL)
matrix_model <- confusionMatrix(predict_model, testingIL$diagnosis)
matrix_model$overall[1]

# PCA
modelPCA <- train(diagnosis ~., data = trainingIL, method = "glm", preProcess = "pca",trControl=trainControl(preProcOptions=list(thresh=0.8)))
matrix_modelPCA <- confusionMatrix(testingIL$diagnosis, predict(modelPCA, testingIL))
matrix_modelPCA$overall[1]

# predicting with trees
data(iris)
library(ggplot2)
names(iris)
table(iris$Species)

inTrain <- createDataPartition(y = iris$Species,
                               p = .7, list = FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training)
dim(testing)
qplot(Petal.Width, Sepal.Width, colour = Species, data = training)

library(caret)
modFit <- train(Species ~ ., method = 'rpart', data = training)
print(modFit$finalModel)

plot(modFit$finalModel, uniform = T,
     main = 'Classfication Tree')
text(modFit$finalModel, use.n = T, all = T, ccex = .8)
install.packages('rattle')
library(rattle)
fancyRpartPlot(modFit$finalModel)
predict(modFit, mewdata = testing)

# bootstrap aggregating (bagging)
library(ElemStatLearn)
data(ozone, package = 'ElemStatLearn')

# random forests
data("iris")
library(ggplot2)
inTrain <- createDataPartition(y = iris$Species, p = .7, list = F)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
modFit <- train(Species ~. , data = training, method ='rf', prox =T)
modFit
library(randomForest)
getTree(modFit$finalModel, k =2)

irisP <- classCenter(training[,c(3,4)], training$Species, modFit$finalModel$prox)
irisP <- as.data.frame(irisP)
irisP$Species <- rownames(irisP)
p <- qplot(Petal.Width, Petal.Length, col = Species, data = training)
p + geom_point(aes(x = Petal.Width, y = Petal.Length, col = Species), size = 5, shape = 4, data = irisP)

pred <- predict(modFit, testing)
testing$predRight <- pred == testing$Species
table(pred, testing$Species)
qplot(Petal.Width, Petal.Length, colour = predRight, data = testing, main = 'newdata Predictions')

# Boosting ~ weight the weak predictors and add them up
library(ISLR)
data(Wage)
library(ggplot2)
library(caret)
Wage <- subset(Wage, select = -c(logwage))
inTrain <- createDataPartition(y = Wage$wage, p = .7, list = F)
training <- Wage[inTrain,]
testing <- Wage[inTrain,]
modFit <- train(wage ~., method = 'gbm', data = training, verbose = F)
print(modFit)
qplot(predict(modFit, testing), wage, data = testing)

# model based prediction
inTrain <- createDataPartition(y = iris$Species, p = .7, list = F)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training)
dim(testing)
modlda = train(Species ~ ., data = training, method = 'lda')
modnb = train(Species ~ . , data = training, method = 'nb')
plda = predict(modlda, testing)
pnb = predict(modnb, testing)
table(plda, pnb)
equalPrediction = (plda == pnb)
qplot(Petal.Width, Sepal.Width, colour = equalPrediction, data = testing)

# 3 - q1
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

packageDescription("AppliedPredictiveModeling")$Version
packageDescription("caret")$Version
packageDescription("ElemStatLearn")$Version
packageDescription("pgmm")$Version

table(segmentationOriginal$Case)
training <- subset(segmentationOriginal, Case=="Train")
testing <-  subset(segmentationOriginal, Case=="Test")
## Classification and Regression Trees
set.seed(125)
modFit <- train(Class~., method="rpart", data=training[-c(1,2)])
print(modFit$finalModel)
library(rattle)
fancyRpartPlot(modFit$finalModel)

# q3
library(pgmm)
data(olive)
olive = olive[,-1]
newdata = as.data.frame(t(colMeans(olive)))
modFit <- train(Area ~ ., method = "rpart", data = olive)
print(modFit$finalModel)
predict(modFit, newdata = newdata)
fancyRpartPlot(modFit$finalModel)

# q4
suppressMessages(library(ElemStatLearn))
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

# regularized regression

# combining predictors
library(ISLR)
data(Wage)
library(ggplot2)
library(caret)
Wage <- subset(Wage, select = -c(logwage))
# create a building data set and validation set
inBuild <- createDataPartition(y = Wage$wage, p =.7, list = F)
validation <- Wage[-inBuild,]
buildData <- Wage[inBuild,]

inTrain <- createDataPartition(y = buildData$wage, p = .7, list = F)
training <- buildData[inTrain,]
testing <- buildData[-inTrain,]
dim(training)
dim(testing)
dim(validation)

mod1 <- train(wage ~. ,method = 'glm', data = training)
mod2 <- train(wage ~. ,method = 'rf', data = training, trControl =trainControl(method = 'cv'), number = 3)
pred1 <- predict(mod1, testing)
pred2 <- predict(mod2, testing)
qplot(pred1, pred2, colour = wage, data = testing)

## fit model that combines predictors
predDF <- data.frame(pred1, pred2, wage = testing$wage)
combModFit <- train(wage ~. , method = 'gam', data = predDF)
combPred <- predict(combModFit, predDF)

## testing errors
sqrt(sum((pred1 - testing$wage)^2))
sqrt(sum((pred2 - testing$wage)^2))
sqrt(sum((combPred - testing$wage)^2))

## predict on validation data set
pred1V <- predict(mod1, validation)
pred2V <- predict(mod2, validation)
predVDF <- data.frame(pred1 = pred1V, pred2 = pred2V)
combPredV <- predict(combModFit, predVDF)

## evaluate on validation
sqrt(sum((pred1V - testing$wage)^2))
sqrt(sum((pred2V - testing$wage)^2))
sqrt(sum((combPredV - testing$wage)^2))

# forecasting
## time series data
library(quantmod)
from.dat <- as.Date("01/01/08", format = "%m/%d/%y")
to.dat <- as.Date("12/31/13", format = "%m/%d/%y")
getSymbols("GOOG", src = 'yahoo', from = from.dat, to = to.dat)
head(GOOG)
mGoog <- to.monthly(GOOG)
googOpen <- Op(mGoog)
ts1 <- ts(googOpen, frequency = 12)
plot(ts1, xlab = "Years+1", ylab = "GOOG")
plot(decompose(ts1) , xlab = "Years + 1")
ts1Train <- window(ts1, start = 1, end = 5)
ts1Test <- window(ts1, start = 5, end = (7 - 0.01))
ts1Train
plot(ts1Train)
library(forecast)
lines(ma(ts1Train, order = 3), col = 'red')
ets1 <- ets(ts1Train, model = 'MMM')
fcast <- forecast(ets1)
plot(fcast)
lines(ts1Test, col = 'red')
accuracy(fcast, ts1Test)

# unsupervised prediction
data(iris)
library(ggplot2)
inTrain <- createDataPartition(y = iris$Species, p =.7, list = F)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training)
dim(testing)
kMeans1 <- kmeans(subset(training, select =  -c(Species)), centers = 3)
training$clusters <- as.factor(kMeans1$cluster)
qplot(Petal.Width, Petal.Length, colour = clusters, data = training)
table(kMeans1$cluster, training$Species)
modFit <- train(clusters ~ . , data = subset(training, select = -c(Species)), method = 'rpart')
table(predict(modFit, training), training$Species)
## apply on test set
testClusterPred <- predict(modFit, testing)
table(testClusterPred, testing$Species)
?train

# week 4 Q2
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)
modRF2 <- train(diagnosis ~ ., data=training, method="rf") #, trControl=trainControl("cv"), number=3)
modBoost2 <- train(diagnosis ~ ., data=training, method="gbm", verbose=FALSE)
modLDA2 <- train(diagnosis ~ ., data=training, method="lda", verbose=FALSE)

predRF2 <- predict(modRF2, testing)
predBoost2 <- predict(modBoost2, testing)
predLDA2 <- predict(modLDA2, testing)

dataCombined <- data.frame(predRF2, predBoost2, predLDA2, diagnosis=testing$diagnosis)
modCombined <- train(diagnosis ~ ., data=dataCombined, method="rf", verbose=FALSE)

predCombined <- predict(modCombined, dataCombined)

cfmRF2 <- confusionMatrix(testing$diagnosis, predRF2)
cfmBoost2 <- confusionMatrix(testing$diagnosis, predBoost2)
cfmLDA2 <- confusionMatrix(testing$diagnosis, predLDA2)
cfmCombined <- confusionMatrix(testing$diagnosis, predCombined)

cfmRF2$overall["Accuracy"]
cfmBoost2$overall["Accuracy"]
cfmLDA2$overall["Accuracy"]
cfmCombined$overall["Accuracy"]

# Q3
set.seed(3523)
library(AppliedPredictiveModeling)
library(elasticnet)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[inTrain,]
testing = concrete[-inTrain,]

set.seed(233)
lassomod <- train(CompressiveStrength ~ . , data = training, method = 'lasso')
plot.enet(lassomod$finalModel,  xvar="penalty", use.color=TRUE)

# Q4
library(lubridate) # For year() function below

dat = read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv")

training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

tsmod <- bats(tstrain)
forecastObj <- forecast(tsmod, level=95, h=nrow(testing))
betweenVal <- sum(testing$visitsTumblr > forecastObj$lower &  testing$visitsTumblr < forecastObj$upper)
betweenVal / nrow(testing) * 100

# q5
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(325)
library('e1071')
modSvm <- svm(CompressiveStrength ~ ., data = training)
predSvm <- predict(modSvm, testing)
accSvm <- accuracy(predSvm, testing$CompressiveStrength)
data.frame(accSvm)["RMSE"]
