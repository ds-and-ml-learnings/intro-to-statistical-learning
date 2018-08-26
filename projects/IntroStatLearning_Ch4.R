library(ISLR)

# Lag: The stock market of the 5 prev days
# Today: The % return for the date in question
# Direction: Whether the stock market went up or down
# Volume: The # of shares trades the prev day (billions)
names(Smarket)
attach(Smarket)

dim(Smarket) # 1250 rows, 9 columns

summary(Smarket)

# Correlation Matrix (excluding direction bc its a qualitive variable)
# Like economics, the market cannot be predicted
# Hence, the low correlation btw different days
# The highest correlation is for volume and year
cor(Smarket[, !(colnames(Smarket) == "Direction")])

aggregate(Smarket$Volume, FUN=sum, by=list(Category=Smarket$Year))[2]
# We notices that there's a trend upward, explaining the correlation

#=================================================================
# 4.6.2 Logistic Regression
#=================================================================
glm.fit <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag4+Lag5+Volume,
               data = Smarket, family = binomial)

summary(glm.fit)

# Smallest p-value: Lag1 w/ a neg. coefficient (pvalue is not low enough to be sure)
# Whatever happened yesterday will have the opposite affect for todat

# Info on the cofficients
summary(glm.fit)$coef

# Inisght on how R creates the Y predictor in dummy variables
contrasts(Direction) # the logistic reg will be conducted on the direction being "up"

# Using the predicted function
glm.probs <- predict(glm.fit, type = 'response')

# First 10 cases of the dataset
glm.probs[1:10]

# Changing the variables to classes (uses 0.5 as the threshold) in a vector
glm.pred <- rep("Down", 1250) # 1250 observations (default at down)

# If the prob. is higher than .5, it will change it to up
glm.pred[glm.probs>.5] <- "Up"

table(glm.pred, Direction) # confusion matrix
# (correct vals)/(total vals) is (652/1250) = .5216

# Quicker method to find the corrected val
mean(glm.pred==Direction)

# A pretty bad result for the training error
# We divide the data into subset whihc will display a more accurate measure
# 2005 will be our testing set

training.set <- (Year < 2005)
Smarket.2005 <- Smarket[!training.set, ]
dim(Smarket.2005)

# y-result or the prediction of the testing set
Direction.2005 <- Direction[!training.set]

# Fitting the logistic regression to the training set
glm.fit <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag4+Lag5+Volume,
               data = Smarket, family = binomial, subset = training.set)
glm.probs <- predict(glm.fit, Smarket.2005, type = 'response')
glm.pred <- rep("Down", 252) # 252 observations (default at down)
glm.pred[glm.probs>.5] <- "Up"
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005) # Accuracy decrease to 48%, less than randomnly guessing the direction of stocks

# Error rate
mean(glm.pred!=Direction.2005) # 52%

# Will reimplement the regression with the two lowest p-value predictors (Lag1 and Lag2)
glm.fit <- glm(Direction ~ Lag1+Lag2,
               data = Smarket, family = binomial, subset = training.set)
glm.probs <- predict(glm.fit, Smarket.2005, type = 'response')
glm.pred <- rep("Down", 252) # 252 observations (default at down)
glm.pred[glm.probs>.5] <- "Up"
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005) # Accuracy is 0.5595238

# Error rate
mean(glm.pred!=Direction.2005) # 44%

# Confusion Matrix suggests that when the Direction is UP: 106/(106+76) = 58%
# Confusion Matrix suggests that when the Direction is DOWN: 35/(35+35) = 50%


#=================================================================
# 4.6.3 Linear Discriminant Analysis
#=================================================================

# RECALL: LDA reduces the dimension, creates a linear line (if two classes)
# to seperate two classes
library(MASS)

lda.fit <- lda(Direction ~ Lag1+Lag2, data = Smarket, subset = training.set)

# Priori prob: 0.491984 0.508016 [Down & Up respectively]
# Group mean: A tendency for the previous 2 days’ returns 
# to be negative on days when the market increases, and positive on days when the market declines.
# Coefficients of linear discriminants: −0.642×Lag1−0.514×Lag2 (eqn. that was created)

lda.predict <- predict(lda.fit, Smarket.2005)

names(lda.predict) # "class"     "posterior" "x" 

# Understanding the predictions
lda.class <- lda.predict$class

# Confusion matrix
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005) # similar to logistic: 5595238

# 999   0.082930955 999  0.4901792 0.5098208

sum(lda.predict$posterior[ ,1] >=.5) # prob. the model will guess down, 70
sum(lda.predict$posterior[ ,1] <=.5) # prob. the model will guess up, 182

# Changing the threshold
# Predicting the market will go down with a 90% certainity
sum(lda.predict$posterior[,1]>.9)


#=================================================================
# 4.6.4 Quadratic Discriminant Analysis
#=================================================================

qda.fit <- qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = training.set)
# Similar to the LDA model, but it does not contain coefficients since this model involves a quadratic eqns

qda.class <- predict(qda.fit, Smarket.2005)$class

# Confusion Matrix
table(qda.class, Direction.2005)

# Evaluating it's level of correctness
mean(qda.class == Direction.2005) # 0.5992063

# Notice that this level of correctness is the highest of any model
# 60% for the stock market is quite high and impressive


#=================================================================
# 4.6.5 K-Nearest Neighbors
#=================================================================

# Implementing the knn model using the class library
library(class)

# Creating two matrixes 
train.X <- cbind(Lag1, Lag2)[training.set,]
test.X <- cbind(Lag1, Lag2)[!training.set,]

# Creating the classes for the training set
train.direction <- Direction[training.set]

# Setting up the model
set.seed(1)
knn.pred <- knn(train.X, test.X, train.direction, k = 1)

# Confusion matrix
table(knn.pred, Direction.2005)

# Evaluating it's level of correctness
mean(knn.pred == Direction.2005) # 0.5
# Whenn we use 1 neighbor, the results are great

# Recreating the model with different neighbors

knn.pred <- knn(train.X, test.X, train.direction, k = 3)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005) # 0.5357143

list.of.k <- seq(1, 11, by=1)
largest <- 0
k <- 0
for (i in list.of.k){
  knn.pred <- knn(train.X, test.X, train.direction, k = i)
  table(knn.pred, Direction.2005)

  if (mean(knn.pred == Direction.2005) > largest){
    largest <- mean(knn.pred == Direction.2005)
    k <- i
    }
  }

# Best parameters: 3 neighbors provides an accuracy of 0.531746


#=================================================================
# 4.6.6 An Application to Caravan Insurance Data
#=================================================================

# Understanding if customers purchase the caravan insurance
dim(Caravan) # 5822   86
attach(Caravan)

summary(Purchase) # Only 348 out of 5474 purchased the insurance (or about 6%)

# Since the data is skewed, we need to normalize the data
scale.X <- scale(Caravan[, !(colnames(Caravan) == "Purchase")])

# Creating a testing and training set (testing set will be the first 100 obs)
test <- 1:1000

train.X <- scale.X[-test, ]
test.X <- scale.X[test, ]

train.y <- Purchase[-test]
test.y <- Purchase[test]

set.seed(1)
knn.pred <- knn(train.X, test.X, train.y, k = 3)

table(knn.pred, test.y)
mean(test.y!= knn.pred) # The KNN Error Rate is 0.074 (fairly good)

# However, bc of the structure of the data, if we predicted "No"
# everytime, we would have an error rate of 0.06

# Instead, if we look at the # of correctly predicting people who purchase insurance
# we can get a more accurate insight on how the model will perform for our needs (5/25) or .20


# Let's create a for loop to check for the best # of KNN
list.of.k <- seq(1, 10, by=1)
largest.pos.pred <- 0
k <- 0
for (i in list.of.k){
  knn.pred <- knn(train.X, test.X, train.y, k = i)
  total <- table(knn.pred, test.y)[2] + table(knn.pred, test.y)[4]
  percentage <- table(knn.pred, test.y)[4]/total

  if (percentage > largest.pos.pred){
    largest.pos.pred <- percentage
    k <- i
  }
}

# 9 provides the best result bc it guess yes 1 time correctly, double check for better references!
knn.pred <- knn(train.X, test.X, train.y, k = 9)
table(knn.pred, test.y)






