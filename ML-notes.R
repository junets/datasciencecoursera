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
