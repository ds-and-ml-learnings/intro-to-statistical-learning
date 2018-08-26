#=================================================================
# 7.8 Lab: Non-linear Modeling
#=================================================================

library(ISLR)
attach(Wage)

#=================================================================
# 7.8.1 Polynomial Regression and Step Functions
#=================================================================

# We employ a model that used the wage variable tp the fourth power
# From the looks of it, this does not use all the polynomials measures together
# Rather, it employs each poly individually

fit <- lm(wage ~ poly(age, 4), data = Wage)
coef(summary((fit)))

fit2 <- lm(wage ~ poly(age, 4, raw=T), data = Wage)
coef(summary((fit2)))

# Alternative to perform the fit model
fit2a <- lm(wage ~ age +I(age ^2)+I(age ^3)+I(age ^4) ,data=Wage)
coef(summary((fit2a)))

# Creating values for which we want the predictions
agelim <- range(age)
age.grid <- seq(from = agelim[1], to = agelim[2])
preds <- predict(fit, newdata = list(age=age.grid), se=TRUE)
se.bands <- cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)

# Plotting the data
par(mfrow = c(1,2), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0))
plot(age, wage, xlim = agelim, cex = 0.5, col='darkgrey')
title("Degree -4 Polynomial", outer = T)
lines(age.grid, preds$fit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd =1, col = "blue", lty = 3)


# Calculating the ANOVA btw the complicated and simple models
fit.1 <- lm(wage~age, data=Wage)
fit.2 <- lm(wage~poly(age, 2), data=Wage)
fit.3 <- lm(wage~poly(age, 3), data=Wage)
fit.4 <- lm(wage~poly(age, 4), data=Wage)
fit.5 <- lm(wage~poly(age, 5), data=Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)

# The concept is a bit weird, but the lower p-value is described by the 
# author is not sufficient enough and too large of a value indicates that 
# the p-value cannot be taken serious

# Another method of looking at the pvalues
coef(summary(fit.5))


# Applying the glm method
fit <- glm(I(wage >250) ~ poly(age,4), data=Wage, family=binomial)
preds <- predict(fit, newdata=list(age=age.grid),se=T)


#=================================================================
# 7.8.2 Splines
#=================================================================
library(splines)

# We are breaking the knots at 25, 40, and 60 years 
fit <- lm(wage ~ bs(age, knots=c(25 , 40, 60)), data=Wage)
pred <- predict(fit, newdata=list(age=age.grid),se=T)
plot(age, wage, col="gray")
lines(age.grid, pred$fit ,lwd =2)
lines(age.grid, pred$fit + 2* pred$se, lty="dashed")
lines(age.grid,pred$fit - 2* pred$se, lty="dashed")

# The confusing part is that we are stating that the function above is a cubic function (how?)
# the "bs" function in the linear function create cublic splines by default...

# Finding the dimenisinon of the splines
dim(bs(age, knots=c(25,40,60)))

# We can create a uniform splines
dim(bs(age, df=6))
attr(bs(age, df=6), "knots")

# the bs function allows us to choose the powe we want our splines to be
fit2 <- lm(wage~ns(age, df=4), data=Wage)
pred2 <- predict(fit2, newdata=list(age=age.grid), se=T)
lines(age.grid, pred2$fit, col='red', lwd=2)


#=================================================================
# 7.8.3 GAMS
#=================================================================
# Using gams function helps us run regression across diff data types (quatitive and qualitive)
gam1 <- lm(wage~ns(year,4) + ns(age,5) + education, data=Wage)

library(gam)
gam1.m3 <- gam(wage~s(year,4) + s(age,5) + education, data=Wage)
# The s() function, which is part of the gam library, is used to indicate that 
# we would like to use a smoothing spline.
# Also, the the variable year will use 4 degree of freedom while the "age" variable
# will use 5 degree of freedom

# Since education is qualitative, we leave it as is,
# and it is converted into four dummy variables

par(mfrow=c(1,3))
plot(gam1.m3, se=TRUE, col="blue")

# We know need to figure out if its best to exclude year, include year as a linear, or have it 
# as a spline function

# m1 does not have year
gam.m1 <- gam(wage~s(age, 5) + education, data=Wage)

# m2 includes year a linear function
gam.m2 <- gam(wage~year + s(age, 5) + education, data=Wage)

# m3 includes year a spline function
gam.m3 <- gam(wage~s(year,4) + s(age,5) + education, data=Wage)

# Performing the anova test to check if the results are differernt/better
anova(gam.m1, gam.m2, gam.m3, test='F')

# Result: The linear function has the lowest p-value.

# Summary of the gam fit model
summary(gam.m3)

