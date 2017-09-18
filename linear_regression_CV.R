## Set working directory
## ─────────────────────────

# set the working directory
# setwd("~/Desktop/Rstatistics")
# setwd("C:/Users/Rstatistics")
setwd("..\R work")


getwd() # where am I?
list.files("dataSets") # files in the dataSets folder

## Load the states data
## ────────────────────────

# read the states data
states.data <- readRDS("dataSets/states.rds") 
#get labels
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
#look at last few labels
tail(states.info, 8)

## Linear regression
## ═══════════════════

## Examine the data before fitting models
## ──────────────────────────────────────────

##  Examine the data to check for problems.

# summary of expense and csat columns, all rows
sts.ex.sat <- subset(states.data, select = c("expense", "csat"))
summary(sts.ex.sat)
# correlation between expense and csat
cor(sts.ex.sat)

## Plot the data before fitting models
## ───────────────────────────────────────

##   Plot the data to look for multivariate outliers, non-linear
##   relationships etc.

# scatter plot of expense vs csat
plot(sts.ex.sat)

# Fit linear regression model
sat.mod <- lm(csat ~ expense, # regression formula
              data=states.data) # data set
# Summarize and print the results
summary(sat.mod) # show regression coefficients table

summary(lm(csat ~ expense + percent, data = states.data))

## The lm class and methods
## ────────────────────────────


##   • Examine the model object:

class(sat.mod)
names(sat.mod)
methods(class = class(sat.mod))[1:9]

confint(sat.mod)
# hist(residuals(sat.mod))

##   • Investigate assumptions of OLS regression visually by plotting model:

par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(sat.mod, which = c(1, 2)) # "which" argument optional

## Evaluate models to predict SAT scores over and above expense from congressional voting patterns.
## ────────────────────

# fit a model, adding house and senate as predictors
sat.voting.mod <-  lm(csat ~ expense + house + senate,
                      data = na.omit(states.data))

sat.mod <- update(sat.mod, data=na.omit(states.data))

# compare using the anova() function
anova(sat.mod, sat.voting.mod)
coef(summary(sat.voting.mod))

##   Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). 

##   Examine/plot the data before fitting the model

##examine summary stats for energy and metro
attach(states.data)
summary(energy)
summary(metro)

##examine correlation between metro and energy
cor(metro, energy, use = "complete.obs", method = c("pearson"))

## examine/plot the data before fitting the model
plot(metro, energy)

##The plot reveals that 4 states are outliers on energy consumption: Texas, Wyoming, Alaska, and Louisiana. 
##These may affect the results of the regression model, so it may be worth running the model with and without
## these states to determine whether/how the model is affected. 

##   Print and interpret the model `summary'

energyreg = lm(energy ~ metro, data=states.data)
summary(energyreg)
str(energyreg)
plot(energyreg$residuals)

##Interpretation: As the percentage of a state's residents living in metro areas increases by 1, 
##per capita energy consumption decreases by 2.29. Although this association is
##statistically significant at p < .05, only 12% of the variance in per capita
##energy consumption is explained by the percentage of residents living in metro areas. Thus,
##the magnitude of association between the percentage of residents living in metro areas and
## per capita energy consumption is relatively weak. Because the plot examined earlier indicated that there
## were a few outliers on energy consumption (i.e., energy consumption > 500), I ran some diagnostics to 
##get a better understanding of how these outliers might be affecting the model and then ran the model again with
## these outliers removed. 

##compute cook's distance to determine how outliers are affecting model
cooks.distance(energyreg)

##Interestingly, none of the states have cook's distance > 1, which is the recommended threshold for identifying
##outliers that have a disproportionate amount of influence on model results.

##The effect of outliers on the model can also be examined by running the model with and without the outliers.
##Remove outliers on energy consumption (those > 500) and rerun the model on the remaining states.

states.data.trim <- filter(states.data, energy < 500)
str(states.data.trim)
View(states.data.trim)

energyregtrim = lm(energy ~ metro, data=states.data.trim)
summary(energyregtrim)

##Results: The model fit did improve after the outliers were removed. In this model, per capita energy consumption
##decreases by 1.36 as the percentage of the state's residents living in metro areas increases by 1. This association
##was statistically significant at p = .001, with 22% of the variance in per capita energy consumption explained by 
##the percentage of residents residing in metro areas. 


##   `plot' the model to look for deviations from modeling assumptions

##I also examined the model object for the regression model with all states included and created plots to 
##examine assumptions of linear regression:

class(energyreg)
names(energyreg)
methods(class = class(energyreg))
confint(energyreg)

hist(residuals(energyreg))
par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(energyreg, which = c(1, 2)) # "which" argument optional

##Consistent with assumptions of linear regression, residuals appeared to be approximately normally distributed.
##The outlier states had much higher residuals than other states, indicating that their 
##actual values were fairly discrepant from their predicted values. 

##Conclusion: There was a significant, negative linear relationship between the percentage of a state's residents that reside in 
##metropolitan areas and per capita energy consumption. However, there were a few states (Alaska, Texas, Louisiana, and Wyoming) 
##that had particularly high levels of per capita energy consumption (i.e., > 500), and their energy consumption was
##not predicted very well by metro. 

##Add "income" to the model as a predictor.

##   Examine/plot the data before fitting the model

plot(income, energy)

##examine correlation between income and energy
cor(income, energy, use = "complete.obs", method = c("pearson"))

##In general, there appears to be a weak, negative correlation between income and energy 
##(as income increases, per capita energy consumption decreases).

##   Print and interpret the model `summary'

energyreg2 <- lm(energy ~ metro + income, data=states.data)
summary(energyreg2)

##Test for significance of change in model fit with addition of income to the energy model

anova(energyreg, energyreg2)

##Interpretation: The regression coefficient for income was not significant (Beta = 1.82, std error = 3.82, t = .48, p = .64). Similarly, 
##the amount of variance explained in energy consumption remained steady at 12% (same amount that it was when metro was the only predictor 
##in the model), and, not surprisingly, model fit did not change significantly with the addition of income, F(1, 47) = .23, p = .64.
##Thus, income was not a significant predictor of energy consumption while adjusting for the percentage of residents in metro areas. 

##   `plot' the model to look for deviations from modeling assumptions

##examine residuals to determine whether they conform to the normal distribution
hist(residuals(energyreg2))
##examine multicollinearity of predictors
cor(income, metro, use = "complete.obs", method = c("pearson"))
par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(energyreg, which = c(1, 2)) # "which" argument optional

##The residuals conform roughly to the normal distribution but deviate slightly from it in that they evidence a slight skew to the right. 
##Again, a few states are outliers on energy consumption, and their residuals indicate that the model does not predict
##their energy consumption very well. Metro and income are correlated at .58, which is a moderate strength of association--
##not so high that one would be concerned about multicollinearity.

## Interactions and factors
## ══════════════════════════

## Modeling interactions
## ─────────────────────────

  #Add an interaction to the model
sat.expense.by.percent <- lm(csat ~ expense*income,
                             data=states.data) 
#Show the results
  coef(summary(sat.expense.by.percent)) # show regression coefficients table

## predict SAT scores from region

str(states.data$region)
states.data$region <- factor(states.data$region)
#Add region to the model
sat.region <- lm(csat ~ region,
                 data=states.data) 
#Show the results
coef(summary(sat.region)) # show regression coefficients table
anova(sat.region) # show ANOVA table

# print default contrasts
contrasts(states.data$region)
# change the reference group
coef(summary(lm(csat ~ C(region, base=4),
                data=states.data)))
# change the coding scheme
coef(summary(lm(csat ~ C(region, contr.helmert),
                data=states.data)))

##   Test interaction between metro and density.

energyreg3 <- lm(energy ~ metro*density, data=states.data)
coefficients(summary(energyreg3))

##Results: Metro did not interact significantly with density to predict per capita energy consumption, Beta = .01, std error = .01, p = .15.

##   Add region to the model.

# make sure R knows region is categorical
str(region)
region <- factor(region)

energyreg4 <- lm(energy ~ region, data=states.data)
coefficients(summary(energyreg4))
anova(energyreg4)

##Results: The analysis of variance indicated that, using a conventional alpha level of .05 to 
##determine statistical significance, the omnibus F test was not significant, F(3,46) = 2.43, p = .08. Thus,
##there are not significant regional differences in energy consumption. Although conducting post hoc tests is not truly warranted because 
##the omnibus test was not significant (i.e., to do so increases the risk of a Type 1 error), "peeking" at the post hoc contrasts 
##in the regression model indicated that there appear to be notable differences between the West and Northeast, 
## the regions with the highest and lowest levels of per capita energy consumption, respectively, out of the four regions. 
