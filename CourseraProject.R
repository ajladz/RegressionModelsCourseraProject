
# Executive Summary

# You work for Motor Trend, a magazine about the automobile industry. Looking at a data set of a 
# collection of cars, they are interested in exploring the relationship between a set of variables and 
# miles per gallon (MPG) (outcome). They are particularly interested in the following two questions:
# "Is an automatic or manual transmission better for MPG"?
# "Quantify the MPG difference between automatic and manual transmissions"
# Take the mtcars data set and write up an analysis to answer their question using regression models and 
# exploratory data analyses.

data(mtcars)
mtcars$am <- factor(mtcars$am, labels = c('Automatic', 'Manual'))

# Description
# The data was extracted from the 1974 Motor Trend US magazine, and comprises fuel consumption and 10 
# aspects of automobile design and performance for 32 automobiles (1973-74 models).



## EXPLORATORY DATA ANALYSIS

# First, we'll inspect the data set and see if there are correlations between the regressors. From Figure
# 1 in appendix we see that many variables in our data set mtcars are highly correlated with MPG, as well 
# as with each other. That means that we'll have to be careful when choosing our regression model because 
# inclusion of highly correlated variables might cause increase in standard errors for other regressors.

# Figure 1 - Appendix
library(GGally) # collection of additional tools for ggplot2
library(ggplot2)
g <- ggpairs(mtcars, lower = list(continuous = 'smooth'))
g
# , params = c(method = 'loess')

# Second, we'll inspect the impact of the type of transmission on  MPG graphically. From Figure 2 in appendix 
# we see that automatic transmission significantly increases consumption of gasoline. To check if that
# increase is statistically significant we'll do statistical inference t.test.
# In this case our null hypothesis is that there is no difference in mpg between manual and automatic 
# transmission. 

# Figure 2  - Apendix
g <- ggplot(mtcars, aes(factor(am), mpg))
g <- g + geom_boxplot(aes(fill = factor(am)))
g <- g + labs(title = 'Effect of Transmission Type on Miles per Gallon', 
              x = 'Transmission', y = 'Miles per Gallon')
g

mtcars$am <- factor(mtcars$am, labels = c('Automatic', 'Manual'))
t.test(mtcars$mpg[mtcars$am == 'Manual'], mtcars$mpg[mtcars$am == 'Automatic'])
# As we can see from the output our p-value is 0.001374 which is less than 0.05. We will reject the null 
# hypothesis and conclude that there is a difference in mpg between automatic and manual transmission.
# We conclude that there is a difference in mpg between automatic and manual  transmission and that the 
# difference is between 3.209684 and 11.280194 mpg. 

# Another way of inspecting the relationship between transmission type and gasoline consumption:
fit_am <- lm(mpg ~ am, data = mtcars)
summary(fit_am)$coef
# Here we see that both p values are much smaller  than 0.05 and coefficients are group means. Beta0 is 
# 17.14737 and it represents mean mpg for cars with automatic transmission. Beta1 is  7.244939 and it 
# represents an increase in mean mpg for cars with manual transmission.

# Of course these tests compeletely ignore other factors that might be influencing gasoline consumption.
# As we can see from the Figure 1 in appendix correlation between am and mpg is 0.6 and correlation 
# between am and wt (weight) is -0.692. We also see that am variable is higly correlated with drat - 
# Rear axle ratio  with correlation coefficient of 0.713. This makes us a bit suspicious of our t.test
# results because we failed to take these correlations into account. We cannot be sure that am alone 
# is responsible for the  results of our t.test. For that reason, we'll proceed with further analysis.



## REGRESSION ANALYSIS 

# First I used backward elimination where I started with model that includes all variables in  mtcars as
# predictors and gradually removed those whose p-values were highest (the least significant ones) until 
# all the remaining variables were significant predictors. Using this method I came to conclusion that 
# the best fitted model would be one that includes wt - Weight (lb/1000), qsec - 1/4 mile time and am -
# Transmission as predictors.
fit <- lm(mpg ~ ., data = mtcars)
summary(fit)

fit1 <- update(fit, .~. - cyl)
summary(fit1)

fit2 <- update(fit1, .~ . - vs)
summary(fit2)

fit3 <- update(fit2, . ~ . - gear)
summary(fit3)

fit4 <- update(fit3, . ~ . - carb)
summary(fit4)

fit5 <- update(fit4, .~ . - drat)
summary(fit5)

fit6 <- update(fit5, . ~ . - disp)
summary(fit6)

fit7 <- update(fit6, .~ . - hp)
summary(fit7)



# Using nested likelihood ratio test for model selection
anova(fit_am, fit7, fit)
# From our analysis of variance we can see that adding wt and qsec as regressors to our model that
# consisted only of am as a predictor of mpg improved our model. We would conclude that all of the added 
# Model 2 terms are necessary over Model 1. On the other hand, we see that Model 3 terms are not necesary 
# over Model 2 terms and therefore we would choose Model 2. 

# We can see  that p-value for amManual, an estimate of difference between the automatic and manual 
# transmission, is 0.046716 which is less than 0.05. This means that the difference between the two is 
# statistically significant and we can reject the null hypothesis once more that there is no difference 
# in impact on mpg between automatic and manual transmission.

t <- qt(1 - 0.05/2, length(mtcars$am) - 2)
2.9358 + c(-1, 1) * t * 1.4109
# We can see that the difference in mpg between automatic and manual transmission is between 0.05435779 
# and 5.81724221.


# To be on the safe side I also tried automated stepwise regression with AIC as a criterion for model 
# selection. 

# Backward Elimination method
backward_model <- step(fit, direction = "backward")
summary(backward_model)

# Forward Selection method
forward_model <- step(lm(mpg ~ 1, data = mtcars), direction = 'forward', scope = ~ cyl + disp + hp + 
                              drat + wt + qsec + vs + am + gear + carb)
summary(forward_model)

# Bidirectional Elimination method
bidirectional <- step(fit, direction = 'both')
summary(bidirectional)

# Both Backward Elimination and Bidirectional Elimination procedure produce the model with am, wt and
# qsec as best predictors of mpg.  


# Residual plots 

plot(fit7)

# Leverage is largely measured by one quantity, so called hat diagonals, which can be obtained in R by 
# the function hatvalues. The hat values are necessarily between 0 and 1 with larger values indicating 
# greater (potential for) leverage.
round(hatvalues(fit7), 3)

# To probe for influence:
# dffits - change in the predicted response when the i-th point is deleted in fitting the model.
# dfbetas - change in individual coefficients when the i-th point is deleted in fitting the model.
# cooks.distance - overall change in the coefficients when the i-th point is deleted.

# Leverage measures (hat values) can be useful for diagnosing data entry errors and points that have a 
# high potential for influence. 
# Influence measures get to the bottom line, 'how does deleting or including this point impact a 
# particular aspect of the model.

round(dfbetas(fit7)[1 : 32, 2], 3)
max(round(dfbetas(fit7)[1 : 32, 2], 3))


