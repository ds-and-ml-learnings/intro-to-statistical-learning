library(MASS)
library (ISLR)


# From the tutorial of Introduction to Statistical Learning, 
# I will be completing the R code 

print(names(Boston))
attach(Boston)

# Basic linear function
lm.fit <- lm(medv~lstat, data=Boston)
summary(lm.fit)

#------------------
# Coefficient
coef(lm.fit)

# Noticed that we get two numbers
# The first is the estimation of the y-intercept
# If lstat was 0, we would expect medv to be 34.5538409
# And for every one unit increase in lstat, medv decreases by -0.9500494
#------------------

#------------------
# Confidence Interval
confint(lm.fit)

# The confidence interval helps us gauge our estimate. We must first decide 
# of the confidence interval (2.5, 5, or 10) and if its one-sided and two-sided 
# confidence interval. 

# In the case, we use a 5% two-sided confidence interval of our y-intercept and slope. Anything
# out of the range can be a signal that its not likely too happen out of pure chance.
#------------------

#------------------
# Prediction (for the confidence interval of the inputted data)

predict(lm.fit,data.frame(lstat=(c(5 ,10 ,15))),
         interval="confidence")

predict(lm.fit,data.frame(lstat=(c(5 ,10 ,15))),
         interval="prediction")

# The confidence interval helps us gauge our estimate. We must first decide 
# of the confidence interval (2.5, 5, or 10) and if its one-sided and two-sided 
# confidence interval. 

# In the case, we use a 5% two-sided confidence interval of our y-intercept and slope. Anything
# out of the range can be a signal that its not likely too happen out of pure chance.
#------------------

#------------------
# Plotting medv and lstat
# pch changes the symbol within the plotting graph

plot(Boston$lstat, Boston$medv, pch="+")
abline(lm.fit) #Including the line of best fit

# Changing the plot lines
abline(lm.fit, lwd=3, col="red")

# Plotting the residuals
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

# Doing this helps us understand if our model is performing a good job
# If the residual is not uniform, then we rethink our model, it might be mean that 
# the information is not linear

# In this example, linear might be the best. We see that the residual decreases as we approach 20
# and then increased exponentially


# To look for values that are not within the normal x range, we can look at the leverage points
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))


#=================================================================
# 3.6.3 Multiple Linear Regression
#=================================================================


lm.fit.2 <- lm(medv~lstat+age, data=Boston)
summary(lm.fit.2)

# Using all the variables
lm.fit.3 <- lm(medv~., data=Boston)
summary(lm.fit.3)

summary(lm.fit)$r.sq
summary(lm.fit)$sigma

# Variance Inflation Factor: Multicollinearity is when there’s correlation 
# between predictors (i.e. independent variables) in a model; 
# it’s presence can adversely affect your regression results. 
# 
# The VIF estimates how much the variance of a regression coefficient 
# is inflated due to multicollinearity in the model.

# VIF of 1.9 tells you that the variance of a particular coefficient is 90% bigger 
# than what you would expect if there was no multicollinearity — 
# if there was no correlation with other predictors.
library(car)
vif(lm.fit.3)

# Mult. Regression subract aging bc of the low age p-value
lm.fit.4 <- lm(medv~.-age,data=Boston)
summary(lm.fit.4)


#=================================================================
# 3.6.4 Interaction Terms
#=================================================================

# The syntax lstat*age simultaneously includes lstat, age,
# and the interaction term lstat×age as predictors

summary(lm(medv~lstat*age, data=Boston))


#=================================================================
# 3.6.5 Non-linear Transformations of the Predictors
#=================================================================

# The function I() is needed since the ^ has a special meaning I()
# in a formula. Thus, we need to wrap it!

lm.fit.non.linear <- lm(medv~lstat+I(lstat^2), data=Boston)
lm.fit <- lm(medv~lstat, data=Boston)
summary(lm.fit.non.linear)

# Looking at the p-value, it decreases when we add a polynomial to the second degree
# Thus, this is a better model than a linear model

# Using the anova table
anova(lm.fit, lm.fit.non.linear)

# When performing the anova table, we are conducting the null hypothesis that 
# both models are the same. Thus, if there's a large f-statistics and low f-value
# we know that the more complicated model is superior (second formula in the anova function)

par(mfrow=c(1,1))
plot(lm.fit.non.linear) # Fits the residual more uniform

# More efficient way to include higher power information
lm.fit.poly.5 <- lm(medv~poly(lstat,5), data=Boston)
summary(lm.fit.poly.5)

summary(lm(medv~log(rm),data=Boston))

# Remember: Log can be useful bc we can a curve that fits better
# Ex: Substantively, sometimes the meaning of a change in a variable is 
# more multiplicative than additive. For example, income. 
# If you make $20,000 a year, a $5,000 raise is huge. 
# If you make $200,000 a year, it is small. 


#=================================================================
# 3.6.6 Qualitative Predictors
#=================================================================

# It remains a bit unclear hwy they use ":" instead of the previous function
names(Carseats)
lm.fit <- lm(Sales~.+Income:Advertising+Price:Age, data=Carseats)
summary(lm.fit)

attach(Carseats)

# Info on dummy variable
contrasts(ShelveLoc)

