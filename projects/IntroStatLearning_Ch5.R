#=================================================================
# 5.3.1 The Validation Set Approach
#=================================================================

library(ISLR)
set.seed(1)
training.set <- sample(392, 196) # Creating a vector of 392, choosing 196

# Fitting a linear regression
lm.fit <- lm(mpg~horsepower, data = Auto, subset = training.set)

attach(Auto)

# Predicting the rest of the indexes (not in the training.set)
mean((mpg - predict(lm.fit, Auto))[-training.set]^2) # 26.14142 is our MSE

# Training the model with higher polynomials
lm.fit2 <- lm(mpg~poly(horsepower, 2), data = Auto, subset = training.set)
mean((mpg - predict(lm.fit2, Auto))[-training.set]^2) # 19.82259 is our MSE

# Training the model with higher polynomials
lm.fit3 <- lm(mpg~poly(horsepower, 3), data = Auto, subset = training.set)
mean((mpg - predict(lm.fit3, Auto))[-training.set]^2) # 19.78252 is our MSE

# Models with a higer order have a lower level of MSE


#=================================================================
#5.3.2 Leave-One-Out Cross-Validation
#=================================================================
# We will use the glm function instead of the lm function
# The glm function has a build in cross validation, we must omit the family = 'binomial' portion

library(boot)
glm.fit <- glm(mpg~horsepower, data=Auto)

# Using R build in library
cv.error <- cv.glm(Auto, glm.fit)
cv.error$delta

# Noticed how the variables return two variables
# The first is the raw CV estimate, while the second is the Adjusted CV for not using the LOOCV

cv.error.vector <- rep(0,5)
for (i in 1:5){
  glm.fit <- glm(mpg~poly(horsepower, i), data = Auto)
  cv.error.vector[i] <- cv.glm(Auto, glm.fit)$delta[1]
}


#=================================================================
#5.3.3 k-Fold Cross-Validation
#=================================================================
set.seed(123)
cv.error.10 <- rep(0,10)

# Noticed how we are adding the K variable (to implement the k-fold)
for (i in 1:10){
  glm.fit <- glm(mpg~poly(horsepower, i), data = Auto)
  cv.error.10[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1]
}

# The computation for the k-Fold is quicker the LOOCV


#=================================================================
# 5.3.4 The Bootstrap
#=================================================================

alpha.fn <- function(data, index) {
  X <- data$X[index]
  Y <- data$Y[index]
  return ((var(Y)-cov (X,Y))/(var(X)+var(Y) -2* cov(X,Y)))
}

# The real alpha
alpha.fn(Portfolio ,1:100)

# Randomly select 100 observations from the range 1 to 100, with replacement
set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace = TRUE))

# Creating the bootstrap
boot(Portfolio, alpha.fn, R = 1000)

# Creating the function to return the std. error of the coefficients
boot.fn <- function(data, index)
  return(coef(lm(mpg~horsepower,data = data , subset = index)))

# The actual!
boot.fn(Auto, 1:392) 

# Examples (of a sample)
set.seed(1)
boot.fn(Auto, sample(392, 392, replace = TRUE))

# Performing the bootstrap
boot(Auto, boot.fn, 1000)

# Comparing it to actual dataset
summary(lm(mpg~horsepower, data = Auto))$coef

# Bootstrap std. error Intercept =  0.870261335, horsepower = 0.007528837
# Actual std. error Intercept =  0.717498656, horsepower = 0.006445501

# Results are quite different!
# The reason is that we make the linear function to rely on a linear model 
# However, this is not the case!




