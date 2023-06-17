#logistic regression

data <- read.csv("C:/Users/OWUSU/Desktop/ML n DL/pima-indians-diabetes.csv",header = TRUE,sep = ",")
data

split = sample.split(data,SplitRatio = 0.8)
train <- subset(data,split=="TRUE")
test <- subset(data,split=="FALSE")

model <- glm(X1~.-X50,train,family="binomial")
summary(model)

res <- predict(model,test,type="response")
confusionMatrix(test$X1,res)
table(actuaValue=test$X1,predictedvalue=res>0.3)

library(ROCR)
ROCRpred <- prediction(res,train$X1)
ROCRref <-performance(ROCRpred,"tpr","fpr")
plot(ROCRref,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))


#linear regression
library(MASS)
data("Boston")
View(Boston)
?Boston #for data description

library(caTools)
set.seed(2)
split <- sample.split(Boston$medv,SplitRatio = 0.7)
train <- subset(Boston,split == "TRUE")
test <- subset(Boston,split == "FALSE")

plot(Boston$crim,Boston$medv,cex=0.5,xlab="crime_rate",ylab="Price")
cr <- cor(Boston)

#creating a scatterplot matrix
attach(Boston)
library(lattice)
splom(~Boston[c(1:6,14)],groups = NULL,data = Boston,axis.line.tck=0,axis.text.alpha=0)
splom(~Bonston[c(7:14)],groups = NULL,data=Boston,axis.line.tck=0,axis.text.alphaa=0)

#studying rm and medv
plot(rm,medv)
abline(lm(medv~rm),col="red") #regression fit line

#using corrplot to visualize
library(corrplot)
corrplot(cr,type="lower")

#finding multicollinearity
library(caret)
#to exclude medv(output)
Boston_a = subset(Boston,select = -c(medv))
numericData <- Boston_a[sapply(Boston_a,is.numeric)]
descrCor <- cor(numericData)

#vif
install.packages("car")
library(car)
model <- lm(medv~.,data=train)
vif(model)

model <- lm(medv~.,data=train)
summary(model)

#creating model after removing tax
model <- lm(medv~.-tax,data=train)
summary(model)

#after removing indus and age
model <- lm(medv~.-age,-indus,data=train)
summary(model)

predic <- predict(model,test)
predic

#to compare predicted values and actual vaues using plot
plot(test$medv,type = "l",lty=1.8,col="green")
lines(predic,type="l",col="blue")