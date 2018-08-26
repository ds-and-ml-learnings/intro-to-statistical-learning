#=================================================================
# 6.5.1 Best Subset Selection
#=================================================================

# We will try to figure how to measure the salary of baseball players
library(ISLR)
fix(Hitters)

names(Hitters)
dim(Hitters) #322  20

sum(is.na(Hitters$Salary)) # there are 59 missing values

# Removing any null values
Hitters <- na.omit(Hitters)
dim(Hitters) #263  20
sum(is.na(Hitters$Salary)) # there are 0 missing values

# Using the leaps library to find the subset predictors (given)
library(leaps)
regfit.full <- regsubsets(Salary~., Hitters)
summary(regfit.full) # The top models preidctors are printed

# The top two-model uses Hits and CRBI

regfit.full <- regsubsets(Salary~., data = Hitters, nvmax = 19) # 19-variable model will be fitted
regfit.full.summary <- summary(regfit.full)
names(regfit.full.summary) # objects in the summary: 
                          # "which"  "rsq"    "rss"    "adjr2"  "cp"     "bic"    "outmat" "obj"   

regfit.full.summary$rsq # R squared increases as we have more values in the function

# Plotting the values (Adjusted R square, cp, etc)
par(mfrow=c(2,2))

plot(regfit.full.summary$rss, xlab = "Number of variables", ylab = "RSS", type = 'b')

# For the adjusted R square, we realize that it peaks!
plot(regfit.full.summary$adjr2, xlab = "Number of variables", ylab = "Adjusted R Square", type = 'b')

# The largest value for Adjusted R square
which.max(regfit.full.summary$adjr2) # 11 value (or index)

# plotting the point where the value is the largest
points(11, regfit.full.summary$adjr2[11], col = 'red3', cex = 2, pch = 20)

# Performing the same steps for C and BIC

# C, min value is 10
plot(regfit.full.summary$cp, xlab = "Number of variables", ylab = "Cp", type = 'b')
points(which.min(regfit.full.summary$cp), 
       regfit.full.summary$cp[which.min(regfit.full.summary$cp)], col = 'red3', cex = 2, pch = 20)

# BIC, min value is 6
plot(regfit.full.summary$bic, xlab = "Number of variables", ylab = "bic", type = 'b')
points(which.min(regfit.full.summary$bic), 
       regfit.full.summary$bic[which.min(regfit.full.summary$bic)], col = 'red3', cex = 2, pch = 20)

# Using the plot.regsubsets
# Error in plot.new() : figure margins too large
# Check what's the issue

# When we use the function, we need to look for the top row and look at the 
# values that are associated (dark box)
par(mfrow=c(1,1))

plot(regfit.full, scale='Cp')
plot(regfit.full, scale='adjr2')
plot(regfit.full, scale='r2')
plot(regfit.full, scale='bic')


#=================================================================
# 6.5.2 Forward and Backward Stepwise Selection
#=================================================================
regfit.fwd <- regsubsets(Salary~., data = Hitters, nvmax = 19, method = 'forward')
summary(regfit.fwd)

regfit.bwd <- regsubsets(Salary~., data = Hitters, nvmax = 19, method = 'backward')
summary(regfit.bwd)

# The forward, backward, and best selector are identical!

# There's a difference in variables the forward, baclward, and best subset selector (7vars)
coef(regfit.full, 7) #   Hits        Walks       CAtBat        CHits       CHmRun    DivisionW      PutOuts
coef(regfit.fwd, 7) #  AtBat         Hits        Walks         CRBI       CWalks    DivisionW      PutOuts
coef(regfit.bwd, 7) #  AtBat         Hits        Walks        CRuns       CWalks    DivisionW      PutOuts


#=================================================================
# 6.5.3 Choosing Among Models Using the Validation Set Approach and Cross-Validation
#=================================================================
set.seed(1)
training.set <- sample(c(TRUE, FALSE), nrow(Hitters), rep=TRUE)
testing.set <- (!training.set)

# Fitting the best subset model to our training set
regfit.best <- regsubsets(Salary~., data = Hitters[training.set, ], nvmax = 19)

# Computating the validation error on the testing set, using the best model for each model size
testing.set.matrix <- model.matrix(Salary~., data = Hitters[testing.set, ])

# Performing a for loop to get the best set for the variables
# Recall that %*% means matrix multiplication
# Hence, we are multiplying the matrix of values with the beta (best beta for each vals)
# error is using the RMSE

val.errors <- rep(NA, 19)
for (i in 1:19){
  coeffi <- coef(regfit.best, id = i)
  pred <- testing.set.matrix[, names(coeffi)]%*%coeffi
  val.errors[i] <- mean((Hitters$Salary[testing.set]-pred)^2)
}

# Vector of the RMSE for the best models
val.errors

# Finding the variable that minimizes the RMSE
which.min(val.errors) # 10 variable model

coef(regfit.best, 10)

# There's no function for the regsubset
# From the textbook, this is the function to use for prediction
predict.regsubset <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id=id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}

# Now, that we have a model, we should use the full dataset to get the coefficeints
# from the full dataset, which will have a larger dataste
# hence, a more accurate description

regfit.best <- regsubsets(Salary~., data = Hitters, nvmax = 19)
coef(regfit.best, 10) # coefficients slightly changed and even some variables changed

# Performing k-fold
k <- 10
set.seed(10)
folds <- sample(1:k, nrow(Hitters), replace = TRUE)
cv.errors <- matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))


# For loop through all the k-fold
# It its not reponsive to the kfold, then it will be used as the training set
# If it is, it will be the testing set
# For each split, we will performing the best variable method (from 1 through 19)
for(j in 1:k){
best.fit <- regsubsets(Salary~., data = Hitters[folds!=j, ], nvmax = 19)

for(i in 1:19) {
  pred <- predict.regsubset(best.fit, Hitters[folds==j,], id = i)
  cv.errors[j,i] <- mean((Hitters$Salary[folds == j]-pred)^2)
  }
}

cv.errors 
# This has given us a 10×19 matrix, of which the (i,j)th element corresponds
# to the test MSE for the ith cross-validation fold for the best j-variable model.

# USing the apply function to find the average
mean.cv.errors <- apply(cv.errors, 2, mean) # 2 indicated columns

par(mfrow = c(1,1))
plot(mean.cv.errors, type='b')

# best model is the 11-variable (index begins at 0 but is model 1)
reg.best <- regsubsets(Salary~., data = Hitters, nvmax = 19)
coef(reg.best, 11)



#=================================================================
# 6.6 Lab 2: Ridge Regression and the Lasso
#=================================================================

# Will be performing the ridge and lasso regression
# Preprocessing the data

# Model matrix is an efficent way to find the dummy variables and fix it, and then 
# use them in a matrix
X <- model.matrix(Salary~., Hitters)[,-1]
y <- Hitters$Salary


#=================================================================
# 6.6.1 Ridge Regression
#=================================================================

# alpha=0: ridge regression, alpha=1: lasso regression
library(glmnet)

# Remember, we need to pass values for the lambda parameter
grid <- 10^seq(10, -2, length = 100)

ridge.mod <- glmnet(X, y, alpha = 0, lambda = grid)

dim(coef(ridge.mod)) # There are 20 rows (one for each predictor) & 100 columns (one for each lambda)

# Values that have a larger lambda -> coefficients will be smaller
# Values that have a smaller lambda -> coefficients will be larger

# Alternative method to split the data
set.seed(1)
training.sample <- sample(1:nrow(X), nrow(X)/2)
testing.sample <- (-training.sample)
y.test <- y[testing.sample]

# lambda is 4?
ridge.mod <- glmnet(X[training.sample,], y[training.sample], 
                    alpha = 0, lambda = grid, thresh = 1e-12)
ridge.predict <- predict(ridge.mod, s = 4, newx = X[testing.sample, ])
mean((ridge.predict - y.test)^2)

# using cross validation to find the number of lambda
set.seed(1)
cv.out <- cv.glmnet(X[training.sample, ], y[training.sample], alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min #211.7416

# What's the MSE associated with the lambda
ridge.predict <- predict(ridge.mod, s = bestlam, newx = X[testing.sample, ])
mean((ridge.predict - y.test)^2) #96015.51

out <- glmnet(X, y, alpha = 0)
predict(out, type = 'coefficients', s = bestlam)[1:20, ]


#=================================================================
# 6.6.2 The Lasso
#=================================================================

lasso.mod <- glmnet(X[training.sample,], y[training.sample], 
                    alpha = 1, lambda = grid)
plot(lasso.mod)

set.seed(1)
cv.out <- cv.glmnet(X[training.sample, ], y[training.sample], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min #16.780166
ridge.predict <- predict(lasso.mod, s = bestlam, newx = X[testing.sample, ])
mean((ridge.predict - y.test)^2) #100743.4

# the lasso model with λ chosen by cross-validation contains only seven variables.
out <- glmnet(X, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = 'coefficients', s = bestlam)[1:20,]

lasso.coef[lasso.coef != 0] # Only 7-variables are chosen for the model


#=================================================================
# 6.7 Lab 3: PCR and PLS Regression
#=================================================================

# 6.7.1 Principal Components Regression
library(pls)
set.seed(2)

# scale=TRUE has the effect of standardizing each predictor
pcr.fit <- pcr(Salary~., data = Hitters, scale = TRUE, validation = 'CV')
summary(pcr.fit)

# I'm guessing the number of components is the CV that will be used with the components used in the PCR

# Plotting the results
validationplot(pcr.fit, val.type='MSEP') # The smallest value is 16...

# Variance is used in PCR: When we look at PCR, we find the value that mostly represents our dataset

# Performing our model on the entire training set
set.seed(1)
pcr.fit <- pcr(Salary~., data = Hitters, subset = training.set, scale = TRUE, validation = 'CV')
summary(pcr.fit) # best model is when comps = 6
validationplot(pcr.fit, val.type='MSEP')

pcr.pred <- predict(pcr.fit, X[testing.sample, ], ncomp = 6)
mean((pcr.pred - y.test)^2)

# Performing the model to the full dataset
pcr.fit <- pcr(y~X, scale = TRUE, ncomp=6)
summary(pcr.fit)


#=================================================================
# 6.7.2 Partial Least Squares
#=================================================================

# PCR suffers from a drawback: there is no guarantee that the directions 
# that best explain the predictors will also be the best directions 
# to use for predicting the response.

set.seed(1)
plsr.fit <- plsr(Salary~., data = Hitters, subset = training.sample, scale = TRUE, validation = 'CV')
summary(plsr.fit) # Lowest is when comps - 2

# Will predict the values for the testing set
plsr.pred <- predict(plsr.fit, X[testing.sample, ], ncomp = 2)
mean((plsr.pred - y.test)^2)

# Performing the model to the full dataset
plsr.fit <- plsr(y~X, scale = TRUE, ncomp=2)
summary(plsr.fit)

# The percentage of variance in Salary that the two-component PLS fit explains, 
# 46.40%, is almost as much as that explained using the
# final seven-component model PCR fit, 46.69 %.

# The pls begins to explain the result t=from the rpedictors and the depdent variable
# while the pcr only eplxains the results from the predictors neglecting the results from the 
# dependent variables!!

