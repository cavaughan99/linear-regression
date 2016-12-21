#  Introduction
## ══════════════

#   • Learning objectives:
##     • Learn the R formula interface
##     • Specify factor contrasts to test specific hypotheses
##     • Perform model comparisons
##     • Run and interpret variety of regression models in R

## Set working directory
## ─────────────────────────

##   It is often helpful to start your R session by setting your working
##   directory so you don't have to type the full path names to your data
##   and other files

# set the working directory
# setwd("~/Desktop/Rstatistics")
# setwd("C:/Users/dataclass/Desktop/Rstatistics")
setwd("..\R course")

##   You might also start by listing the files in your working directory

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

##   Start by examining the data to check for problems.

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

## Linear regression example
## ─────────────────────────────

##   • Linear regression models can be fit with the `lm()' function
##   • For example, we can use `lm' to predict SAT scores based on
##     per-pupal expenditures:

# Fit our regression model
sat.mod <- lm(csat ~ expense, # regression formula
              data=states.data) # data set
# Summarize and print the results
summary(sat.mod) # show regression coefficients table

## Why is the association between expense and SAT scores /negative/?
## ─────────────────────────────────────────────────────────────────────

##   Many people find it surprising that the per-capita expenditure on
##   students is negatively related to SAT scores. The beauty of multiple
##   regression is that we can try to pull these apart. What would the
##   association between expense and SAT scores be if there were no
##   difference among the states in the percentage of students taking the
##   SAT?

summary(lm(csat ~ expense + percent, data = states.data))

## The lm class and methods
## ────────────────────────────

##   OK, we fit our model. Now what?
##   • Examine the model object:

class(sat.mod)
names(sat.mod)
methods(class = class(sat.mod))[1:9]

##   • Use function methods to get more information about the fit

confint(sat.mod)
# hist(residuals(sat.mod))

## Linear Regression Assumptions
## ─────────────────────────────────

##   • Ordinary least squares regression relies on several assumptions,
##     including that the residuals are normally distributed and
##     homoscedastic, the errors are independent and the relationships are
##     linear.

##   • Investigate these assumptions visually by plotting your model:

par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(sat.mod, which = c(1, 2)) # "which" argument optional

## Comparing models
## ────────────────────

##   Do congressional voting patterns predict SAT scores over and above
##   expense? Fit two models and compare them:

# fit another model, adding house and senate as predictors
sat.voting.mod <-  lm(csat ~ expense + house + senate,
                      data = na.omit(states.data))

sat.mod <- update(sat.mod, data=na.omit(states.data))

# compare using the anova() function
anova(sat.mod, sat.voting.mod)
coef(summary(sat.voting.mod))

## Exercise: least squares regression
## ────────────────────────────────────────

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). 

##   1. Examine/plot the data before fitting the model

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

##   2. Print and interpret the model `summary'

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


##   3. `plot' the model to look for deviations from modeling assumptions

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

##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?

##Add "income" to the model as a predictor.

##   1. Examine/plot the data before fitting the model

plot(income, energy)

##examine correlation between income and energy
cor(income, energy, use = "complete.obs", method = c("pearson"))

##In general, there appears to be a weak, negative correlation between income and energy 
##(as income increases, per capita energy consumption decreases).

##   2. Print and interpret the model `summary'

energyreg2 <- lm(energy ~ metro + income, data=states.data)
summary(energyreg2)

##Test for significance of change in model fit with addition of income to the energy model

anova(energyreg, energyreg2)

##Interpretation: The regression coefficient for income was not significant (Beta = 1.82, std error = 3.82, t = .48, p = .64). Similarly, 
##the amount of variance explained in energy consumption remained steady at 12% (same amount that it was when metro was the only predictor 
##in the model), and, not surprisingly, model fit did not change significantly with the addition of income, F(1, 47) = .23, p = .64.
##Thus, income was not a significant predictor of energy consumption while adjusting for the percentage of residents in metro areas. 

##   3. `plot' the model to look for deviations from modeling assumptions

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

##   Interactions allow us assess the extent to which the association
##   between one predictor and the outcome depends on a second predictor.
##   For example: Does the association between expense and SAT scores
##   depend on the median income in the state?

  #Add the interaction to the model
sat.expense.by.percent <- lm(csat ~ expense*income,
                             data=states.data) 
#Show the results
  coef(summary(sat.expense.by.percent)) # show regression coefficients table

## Regression with categorical predictors
## ──────────────────────────────────────────

##   Let's try to predict SAT scores from region, a categorical variable.
##   Note that you must make sure R does not think your categorical
##   variable is numeric.

# make sure R knows region is categorical
str(states.data$region)
states.data$region <- factor(states.data$region)
#Add region to the model
sat.region <- lm(csat ~ region,
                 data=states.data) 
#Show the results
coef(summary(sat.region)) # show regression coefficients table
anova(sat.region) # show ANOVA table

##   Again, *make sure to tell R which variables are categorical by
##   converting them to factors!*

## Setting factor reference groups and contrasts
## ─────────────────────────────────────────────────

##   In the previous example we use the default contrasts for region. The
##   default in R is treatment contrasts, with the first level as the
##   reference. We can change the reference group or use another coding
##   scheme using the `C' function.

# print default contrasts
contrasts(states.data$region)
# change the reference group
coef(summary(lm(csat ~ C(region, base=4),
                data=states.data)))
# change the coding scheme
coef(summary(lm(csat ~ C(region, contr.helmert),
                data=states.data)))

##   See also `?contrasts', `?contr.treatment', and `?relevel'.

## Exercise: interactions and factors
## ────────────────────────────────────────

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.

energyreg3 <- lm(energy ~ metro*density, data=states.data)
coefficients(summary(energyreg3))

##Results: Metro did not interact significantly with density to predict per capita energy consumption, Beta = .01, std error = .01, p = .15.

##   2. Try adding region to the model. Are there significant differences
##      across the four regions?

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
