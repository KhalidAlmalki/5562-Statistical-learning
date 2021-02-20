
#Chapter-4 Lab:
#1.Logistic Regression
#2.LDA
#3.QDA
#4.and KNN

#Dataset: The Stock Market Data

#Load dataset:
library(ISLR)
names(Smarket)
?Smarket
dim(Smarket)
summary(Smarket)

pairs(Smarket)

#produces a matrix that contains all of the pairwise 
#correlations among the predictors in a data set
cor(Smarket[,-9])

#attach(Smarket)
plot(Volume)

#---------------------------------------------------------------------
##1.Logistic Regression:

#To Split dataset into training and testing datasets:
train = (Year<2005)
train_data = Smarket[train, ]

Smarket.2005 = Smarket[!train,]

Direction.2005 = Direction[!train]

dim(Smarket.2005)

#To fit a logistic regression model in order to predict Direction
#using: Lag1 through Lag5 and Volume
#using only the subset of the observations that correspond to dates before 2005

glm.fits = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family = binomial, subset = train)

#Test our model on the testing dataset:
glm.probs = predict(glm.fits, Smarket.2005, type="response")
glm.pred = rep("Down" ,252)
glm.pred[glm.probs >.5]= "Up"
table (glm.pred, Direction.2005)
mean(glm.pred != Direction.2005)

#---------------------------------------------------------------------
#2.LDA
?lda
#We fit an LDA model using the lda() function, which is part of the MASS library
library(MASS)
lda.fit = lda(Direction~Lag1+Lag2, data=Smarket, subset=train)
lda.fit
#The plot() function produces plots of the linear
#discriminants, obtained by computing −0.642 × Lag1 − 0.514 × Lag2 for
#each of the training observations
plot(lda.fit)

lda.pred = predict(lda.fit, Smarket.2005)
lda.class = lda.pred$class

table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)

#---------------------------------------------------------------------
#3.QDA
?qda()
#We fit an QDA model using the qda() function:
qda.fit = qda(Direction~Lag1+Lag2, data=Smarket, subset=train)
qda.fit

qda.pred = predict(qda.fit, Smarket.2005)
qda.class = qda.pred$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005)

#---------------------------------------------------------------------
#4.and KNN
library(class)
?knn()
#A matrix containing the predictors associated with the training data
train.X = cbind(Lag1, Lag2)[train,]
#A matrix containing the predictors associated with the training data
test.X = cbind(Lag1, Lag2)[!train,]
#A vector containing the class labels for the training observations
train.Direction = Direction[train]


#to fit the model
knn.pred = knn(train.X,test.X, train.Direction, k=3)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)
