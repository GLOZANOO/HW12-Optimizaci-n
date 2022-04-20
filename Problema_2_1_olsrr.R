# Tarea 12 - Problema 2 subproblema 1 The olsrr Package

# Install release version from CRAN
install.packages("olsrr")

# Install development version from GitHub
# install.packages("devtools")
devtools::install_github("rsquaredacademy/olsrr")


#(a) Introduction to olsrr
ols_regress(mpg ~ disp + hp + wt + qsec, data = mtcars)

#Plot to detect non-linearity, unequal error variances, and outliers.

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_plot_resid_fit(model)

#DFBETAs measure the difference in each parameter estimate with and without the influential observation. dfbetas_panel creates plots to detect influential observations using DFBETAs.

model <- lm(mpg ~ disp + hp + wt, data = mtcars)
ols_plot_dfbetas(model)

#Residual Fit Spread Plot
#Plot to detect non-linearity, influential observations and outliers.

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_plot_resid_fit_spread(model)

#Breusch Pagan test is used to test for herteroskedasticity (non-constant error variance). It tests whether the variance of the errors from a regression is dependent on the values of the independent variables. It is a χ2 test.

model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
ols_test_breusch_pagan(model)

#Collinearity Diagnostics
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_coll_diag(model)

#Stepwise Regression
#Build regression model from a set of candidate predictor variables by entering and removing predictors based on p values, in a stepwise manner until there is no variable left to enter or remove any more.

# stepwise regression
model <- lm(y ~ ., data = surgical)
ols_step_both_p(model)

#plot 
model <- lm(y ~ ., data = surgical)
k <- ols_step_both_p(model)
plot(k)

#Stepwise AIC Backward Regression
#Build regression model from a set of candidate predictor variables by removing predictors based on Akaike Information Criteria, in a stepwise manner until there is no variable left to remove any more.

#Variable Selection
# stepwise aic backward regression
model <- lm(y ~ ., data = surgical)
k <- ols_step_backward_aic(model)
k

#plot
model <- lm(y ~ ., data = surgical)
k <- ols_step_backward_aic(model)
plot(k)



#(b) Variable Selection Methods


#All subset regression tests all possible subsets of the set of potential independent variables. If there are K potential independent variables (besides the constant), then there are 2k distinct subsets of them to be tested. For example, if you have 10 candidate independent variables, the number of subsets to be tested is 210, which is 1024, and if you have 20 candidate variables, the number is 220, which is more than one million.

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_step_all_possible(model)

#The plot method shows the panel of fit criteria for all possible regression methods.

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
k <- ols_step_all_possible(model)
plot(k)

#Best Subset Regression
#Select the subset of predictors that do the best at meeting some well-defined objective criterion, such as having the largest R2 value or the smallest MSE, Mallow’s Cp or AIC.

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_step_best_subset(model)

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
k <- ols_step_best_subset(model)
plot(k)

#Build regression model from a set of candidate predictor variables by entering predictors based on p values, in a stepwise manner until there is no variable left to enter any more. The model should include all the candidate predictor variables. If details is set to TRUE, each step is displayed.

#Variable Selection
# stepwise forward regression
model <- lm(y ~ ., data = surgical)
ols_step_forward_p(model)

model <- lm(y ~ ., data = surgical)
k <- ols_step_forward_p(model)
plot(k)

# stepwise forward regression
model <- lm(y ~ ., data = surgical)
ols_step_forward_p(model, details = TRUE)

#Stepwise Backward Regression
#Build regression model from a set of candidate predictor variables by removing predictors based on p values, in a stepwise manner until there is no variable left to remove any more. The model should include all the candidate predictor variables. If details is set to TRUE, each step is displayed.

#Variable Selection
# stepwise backward regression
model <- lm(y ~ ., data = surgical)
ols_step_backward_p(model)

model <- lm(y ~ ., data = surgical)
k <- ols_step_backward_p(model)
plot(k)

# stepwise backward regression
model <- lm(y ~ ., data = surgical)
ols_step_backward_p(model, details = TRUE)

#Stepwise Regression
#Build regression model from a set of candidate predictor variables by entering and removing predictors based on p values, in a stepwise manner until there is no variable left to enter or remove any more. The model should include all the candidate predictor variables. If details is set to TRUE, each step is displayed.

#Variable Selection
# stepwise regression
model <- lm(y ~ ., data = surgical)
ols_step_both_p(model)

model <- lm(y ~ ., data = surgical)
k <- ols_step_both_p(model)
plot(k)

# stepwise regression
model <- lm(y ~ ., data = surgical)
ols_step_both_p(model, details = TRUE)

#Stepwise AIC Forward Regression
#Build regression model from a set of candidate predictor variables by entering predictors based on Akaike Information Criteria, in a stepwise manner until there is no variable left to enter any more. The model should include all the candidate predictor variables. If details is set to TRUE, each step is displayed.

#Variable Selection



# stepwise aic forward regression
model <- lm(y ~ ., data = surgical)
ols_step_forward_aic(model)


model <- lm(y ~ ., data = surgical)
k <- ols_step_forward_aic(model)
plot(k)

#Detailed outpu
# stepwise aic forward regression
model <- lm(y ~ ., data = surgical)
ols_step_forward_aic(model, details = TRUE)


#Stepwise AIC Backward Regression
#Build regression model from a set of candidate predictor variables by removing predictors based on Akaike Information Criteria, in a stepwise manner until there is no variable left to remove any more. The model should include all the candidate predictor variables. If details is set to TRUE, each step is displayed.

#Variable Selection
# stepwise aic backward regression
model <- lm(y ~ ., data = surgical)
k <- ols_step_backward_aic(model)
k

model <- lm(y ~ ., data = surgical)
k <- ols_step_backward_aic(model)
plot(k)

#Detailed Output
# stepwise aic backward regression
model <- lm(y ~ ., data = surgical)
ols_step_backward_aic(model, details = TRUE)


#Stepwise AIC Regression
#Build regression model from a set of candidate predictor variables by entering and removing predictors based on Akaike Information Criteria, in a stepwise manner until there is no variable left to enter or remove any more. The model should include all the candidate predictor variables. If details is set to TRUE, each step is displayed.

#Variable Selection
# stepwise aic regression
model <- lm(y ~ ., data = surgical)
ols_step_both_aic(model)

model <- lm(y ~ ., data = surgical)
k <- ols_step_both_aic(model)
plot(k)

# stepwise aic regression
model <- lm(y ~ ., data = surgical)
ols_step_both_aic(model, details = TRUE)



#(c) Residual Diagnostics

#Residual QQ Plot
#Graph for detecting violation of normality assumption.

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_plot_resid_qq(model)

#Residual Normality Test
#Test for detecting violation of normality assumption.

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_test_normality(model)

#Correlation between observed residuals and expected residuals under normality.

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_test_correlation(model)

#Residual vs Fitted Values Plot
#It is a scatter plot of residuals on the y axis and fitted values on the x axis to detect non-linearity, unequal error variances, and outliers.

#Characteristics of a well behaved residual vs fitted plot:
###  
#  The residuals spread randomly around the 0 line indicating that the relationship is linear.
#The residuals form an approximate horizontal band around the 0 line indicating homogeneity of error variance.
#No one residual is visibly away from the random pattern of the residuals indicating that there are no outliers.
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_plot_resid_fit(model)

#Residual Histogram
#Histogram of residuals for detecting violation of normality assumption.

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_plot_resid_hist(model)

#(d)) Heteroscedasticity

#Introduction
#One of the assumptions made about residuals/errors in OLS regression is that the errors have the same but unknown variance. This is known as constant variance or homoscedasticity. When this assumption is violated, the problem is known as heteroscedasticity.

#Consequences of Heteroscedasticity
#The OLS estimators and regression predictions based on them remains unbiased and consistent.
#The OLS estimators are no longer the BLUE (Best Linear Unbiased Estimators) because they are no longer efficient, so the regression predictions will be inefficient too.
#Because of the inconsistency of the covariance matrix of the estimated regression coefficients, the tests of hypotheses, (t-test, F-test) are no longer valid.
#olsrr provides the following 4 tests for detecting heteroscedasticity:
  
 # Bartlett Test
#Breusch Pagan Test
#Score Test
#F Test
#Bartlett Test
#Bartlett’s test is used to test if variances across samples is equal. It is sensitive to departures from normality. The Levene test is an alternative test that is less sensitive to departures from normality.

#You can perform the test using 2 continuous variables, one continuous and one grouping variable, a formula or a linear model.

#Use grouping variable

#ols_test_bartlett(hsb, 'read', group_var = 'female')


#ols_test_bartlett(hsb, 'read', 'write')

#Breusch Pagan Test
#Breusch Pagan Test was introduced by Trevor Breusch and Adrian Pagan in 1979. It is used to test for heteroskedasticity in a linear regression model and assumes that the error terms are normally distributed. It tests whether the variance of the errors from a regression is dependent on the values of the independent variables. It is a χ2 test.

#You can perform the test using the fitted values of the model, the predictors in the model and a subset of the independent variables. It includes options to perform multiple tests and p value adjustments. The options for p value adjustments include Bonferroni, Sidak and Holm’s method.

#Use fitted values of the model
model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
ols_test_breusch_pagan(model)

#Use independent variables of the model
model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
ols_test_breusch_pagan(model, rhs = TRUE)

#Use independent variables of the model and perform multiple tests
model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
ols_test_breusch_pagan(model, rhs = TRUE, multiple = TRUE)

#Bonferroni p value Adjustment
model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
ols_test_breusch_pagan(model, rhs = TRUE, multiple = TRUE, p.adj = 'bonferroni')

#Sidak p value Adjustment
model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
ols_test_breusch_pagan(model, rhs = TRUE, multiple = TRUE, p.adj = 'sidak')

#Holm’s p value Adjustment
model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
ols_test_breusch_pagan(model, rhs = TRUE, multiple = TRUE, p.adj = 'holm')

#Score Test
#Test for heteroskedasticity under the assumption that the errors are independent and identically distributed (i.i.d.). You can perform the test using the fitted values of the model, the predictors in the model and a subset of the independent variables.

#Use fitted values of the model
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_test_score(model)

#Use independent variables of the model
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_test_score(model, rhs = TRUE)

#Specify variables
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_test_score(model, vars = c('disp', 'hp'))

#F Test
#F Test for heteroskedasticity under the assumption that the errors are independent and identically distributed (i.i.d.). You can perform the test using the fitted values of the model, the predictors in the model and a subset of the independent variables.

Use fitted values of the model
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_test_f(model)

#Use independent variables of the model
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_test_f(model, rhs = TRUE)

#Specify variables
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_test_f(model, vars = c('disp', 'hp'))

#(e) Measures of Influence

#Introduction
#It is possible for a single observation to have a great influence on the results of a regression analysis. It is therefore important to detect influential observations and to take them into consideration when interpreting the results.

#olsrr offers the following tools to detect influential observations:
  
#  Cook’s D Bar Plot
#Cook’s D Chart
#DFBETAs Panel
#DFFITs Plot
#Studentized Residual Plot
#Standardized Residual Chart
#Studentized Residuals vs Leverage Plot
#Deleted Studentized Residual vs Fitted Values Plot
#Hadi Plot
#Potential Residual Plot
#Cook’s D Bar Plot
#Bar Plot of Cook’s distance to detect observations that strongly influence fitted values of the model. Cook’s distance was introduced by American statistician R Dennis Cook in 1977. It is used to identify influential data points. It depends on both the residual and leverage i.e it takes it account both the x value and y value of the observation.

#Steps to compute Cook’s distance:
  
#  delete observations one at a time.
#refit the regression model on remaining (n−1) observations
#examine how much all of the fitted values change when the ith observation is deleted.
#A data point having a large cook’s d indicates that the data point strongly influences the fitted values.

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_plot_cooksd_bar(model)

#Cook’s D Chart
#Chart of Cook’s distance to detect observations that strongly influence fitted values of the model.

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_plot_cooksd_chart(model)

#DFBETAs Panel
#DFBETA measures the difference in each parameter estimate with and without the influential point. There is a DFBETA for each data point i.e if there are n observations and k variables, there will be n∗k DFBETAs. In general, large values of DFBETAS indicate observations that are influential in estimating a given parameter. Belsley, Kuh, and Welsch recommend 2 as a general cutoff value to indicate influential observations and 2n√ as a size-adjusted cutoff.

model <- lm(mpg ~ disp + hp + wt, data = mtcars)
ols_plot_dfbetas(model)

#DFFITS Plot
#Proposed by Welsch and Kuh (1977). It is the scaled difference between the ith fitted value obtained from the full data and the ith fitted value obtained by deleting the ith observation. DFFIT - difference in fits, is used to identify influential data points. It quantifies the number of standard deviations that the fitted value changes when the ith data point is omitted.

#Steps to compute DFFITs:
  
#  delete observations one at a time.
#refit the regression model on remaining observations
#examine how much all of the fitted values change when the ith observation is deleted.
#An observation is deemed influential if the absolute value of its DFFITS value is greater than:
  
#  2∗(p+1)−−−−−−√(n−p−1)

#where n is the number of observations and p is the number of predictors including intercept.

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_plot_dffits(model)

#Studentized Residual Plot
#Plot for detecting outliers. Studentized deleted residuals (or externally studentized residuals) is the deleted residual divided by its estimated standard deviation. Studentized residuals are going to be more effective for detecting outlying Y observations than standardized residuals. If an observation has an externally studentized residual that is larger than 3 (in absolute value) we can call it an outlier.

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_plot_resid_stud(model)

#Standardized Residual Chart
#Chart for detecting outliers. Standardized residual (internally studentized) is the residual divided by estimated standard deviation.

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_plot_resid_stand(model)

#Studentized Residuals vs Leverage Plot
#Graph for detecting influential observations.

model <- lm(read ~ write + math + science, data = hsb)
ols_plot_resid_lev(model)

#Deleted Studentized Residual vs Fitted Values Plot
#Graph for detecting outliers.

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_plot_resid_stud_fit(model)

#Hadi Plot
#Hadi’s measure of influence based on the fact that influential observations can be present in either the response variable or in the predictors or both. The plot is used to detect influential observations based on Hadi’s measure.

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_plot_hadi(model)

#Potential Residual Plot
#Plot to aid in classifying unusual observations as high-leverage points, outliers, or a combination of both.

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_plot_resid_pot



#(f) Collinearity Diagnostics, Model Fit & Variable Contribu- tion

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_vif_tol(model)

#Condition Index
#Most multivariate statistical approaches involve decomposing a correlation matrix into linear combinations of variables. The linear combinations are chosen so that the first combination has the largest possible variance (subject to some restrictions we won’t discuss), the second combination has the next largest variance, subject to being uncorrelated with the first, the third has the largest possible variance, subject to being uncorrelated with the first and second, and so forth. The variance of each of these linear combinations is called an eigenvalue. Collinearity is spotted by finding 2 or more variables that have large proportions of variance (.50 or more) that correspond to large condition indices. A rule of thumb is to label as large those condition indices in the range of 30 or larger.

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_eigen_cindex(model)

#Collinearity Diagnostics
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_coll_diag(model)

#Model Fit Assessment
#Residual Fit Spread Plot
#Plot to detect non-linearity, influential observations and outliers. Consists of side-by-side quantile plots of the centered fit and the residuals. It shows how much variation in the data is explained by the fit and how much remains in the residuals. For inappropriate models, the spread of the residuals in such a plot is often greater than the spread of the centered fit.

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_plot_resid_fit_spread(model)

#Part & Partial Correlations
#Correlations
#Relative importance of independent variables in determining Y. How much each variable uniquely contributes to R2 over and above that which can be accounted for by the other predictors.

#Zero Order
#Pearson correlation coefficient between the dependent variable and the independent variables.

#Part
#Unique contribution of independent variables. How much R2 will decrease if that variable is removed from the model?
  
#  Partial
#How much of the variance in Y, which is not estimated by the other independent variables in the model, is estimated by the specific variable?
  
  model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_correlations(model)

#Observed vs Predicted Plot
#Plot of observed vs fitted values to assess the fit of the model. Ideally, all your points should be close to a regressed diagonal line. Draw such a diagonal line within your graph and check out where the points lie. If your model had a high R Square, all the points would be close to this diagonal line. The lower the R Square, the weaker the Goodness of fit of your model, the more foggy or dispersed your points are from this diagonal line.

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_plot_obs_fit(model)

#Lack of Fit F Test
#Assess how much of the error in prediction is due to lack of model fit. The residual sum of squares resulting from a regression can be decomposed into 2 components:
  
#  Due to lack of fit
#Due to random variation
#If most of the error is due to lack of fit and not just random error, the model should be discarded and a new model must be built. The lack of fit F test works only with simple linear regression. Moreover, it is important that the data contains repeat observations i.e. replicates for at least one of the values of the predictor x. This test generally only applies to datasets with plenty of replicates.

model <- lm(mpg ~ disp, data = mtcars)
ols_pure_error_anova(model)

#Diagnostics Panel
#Panel of plots for regression diagnostics

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_plot_diagnostics(model)

#Variable Contributions
#Residual vs Regressor Plots
#Graph to determine whether we should add a new predictor to the model already containing other predictors. The residuals from the model is regressed on the new predictor and if the plot shows non random pattern, you should consider adding the new predictor to the model.

model <- lm(mpg ~ disp + hp + wt, data = mtcars)
ols_plot_resid_regressor(model, 'drat')

#Added Variable Plot
#Added variable plot provides information about the marginal importance of a predictor variable Xk, given the other predictor variables already in the model. It shows the marginal importance of the variable in reducing the residual variability.

#The added variable plot was introduced by Mosteller and Tukey (1977). It enables us to visualize the regression coefficient of a new variable being considered to be included in a model. The plot can be constructed for each predictor variable.

#Let us assume we want to test the effect of adding/removing variable X from a model. Let the response variable of the model be Y

#Steps to construct an added variable plot:
  
#  Regress Y on all variables other than X and store the residuals (Y residuals).
#Regress X on all the other variables included in the model (X residuals).
##Construct a scatter plot of Y residuals and X residuals.
#What do the Y and X residuals represent? The Y residuals represent the part of Y not explained by all the variables other than X. The X residuals represent the part of X not explained by other variables. The slope of the line fitted to the points in the added variable plot is equal to the regression coefficient when Y is regressed on all variables including X.

#A strong linear relationship in the added variable plot indicates the increased importance of the contribution of X to the model already containing the other predictors.

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_plot_added_variable(model)

#Residual Plus Component Plot
#The residual plus component plot was introduced by Ezekeil (1924). It was called as Partial Residual Plot by Larsen and McCleary (1972). Hadi and Chatterjee (2012) called it the residual plus component plot.

#Steps to construct the plot:
  
# Regress Y on all variables including X and store the residuals (e).
#Multiply e with regression coefficient of X (eX).
#Construct scatter plot of eX and X
#The residual plus component plot indicates whether any non-linearity is present in the relationship between Y and X and can suggest possible transformations for linearizing the data.

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_plot_comp_plus_resid(model)

