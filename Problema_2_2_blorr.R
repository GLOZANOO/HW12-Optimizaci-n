# Tarea 12 - Problema 2 subproblema 2 The blorr Package

# blorr Overview ---------
    # Tools designed to make it easier for users, 
    # particularly beginner/intermediate R users to build logistic regression models. 
    # Includes comprehensive regression output, variable selection procedures, 
    # model validation techniques and a ‘shiny’ app for interactive model building.

# Install blorr from CRAN
install.packages("blorr")

# Install development version from GitHub
# install.packages("devtools")
devtools::install_github("rsquaredacademy/blorr")

# Install the development version from `rsquaredacademy` universe
install.packages("blorr", repos = "https://rsquaredacademy.r-universe.dev")

library(blorr)
library(magrittr)

# blorr uses consistent prefix blr_* for easy tab completion.

# Bivariate Analysis
blr_bivariate_analysis(hsb2, honcomp, female, prog, race, schtyp)

# Weight of Evidence & Information Value
blr_woe_iv(hsb2, prog, honcomp)

# create model using glm
model <- glm(honcomp ~ female + read + science, data = hsb2,
             family = binomial(link = 'logit'))

# Regression Output
blr_regress(model)

# Model fit Statistics
blr_model_fit_stats(model)

# Confusion matrix
blr_confusion_matrix(model)

# Hosmer Lemesshow test
blr_test_hosmer_lemeshow(model)

# Grains table
blr_gains_table(model)

# Lift chart
model %>%
  blr_gains_table() %>%
  plot()

# ROC Curve
model %>%
  blr_gains_table() %>%
  blr_roc_curve()

# KS Chart
model %>%
  blr_gains_table() %>%
  blr_ks_chart()

# Lorenz curve
blr_lorenz_curve(model)


#(a) A Short Introduction to the blorr Package --------

install.packages("blorr")

library(blorr)
library(magrittr)

# Bivariate Analysis
blr_bivariate_analysis(bank_marketing, y, job, marital, education, default, 
                       housing, loan, contact, poutcome)

# Weight of Evidence & Information Value
blr_woe_iv(bank_marketing, job, y)

# Plot
k <- blr_woe_iv(bank_marketing, job, y)
plot(k)

# Multiple Variables
blr_woe_iv_stats(bank_marketing, y, job, marital, education)

# Model
model <- glm(y ~ ., data = bank_marketing, family = binomial(link = 'logit'))

# Selection summary
blr_step_aic_both(model)

# Plot
model %>%
  blr_step_aic_both() %>%
  plot()

# Regression output
model <- glm(y ~  age + duration + previous + housing + default +
               loan + poutcome + job + marital, data = bank_marketing, 
             family = binomial(link = 'logit'))

blr_regress(model)

# using formua
blr_regress(y ~  age + duration + previous + housing + default +
              loan + poutcome + job + marital, data = bank_marketing)

# model fit statics
blr_model_fit_stats(model)

# Confusion matrix
blr_confusion_matrix(model, cutoff = 0.5)

# hosmer lemeshow test
blr_test_hosmer_lemeshow(model)

# Grains table
blr_gains_table(model)

# Lift chart
model %>%
  blr_gains_table() %>%
  plot()

# ROC Curve
model %>%
  blr_gains_table() %>%
  blr_roc_curve()

# KS Chart
model %>%
  blr_gains_table() %>%
  blr_ks_chart()

# Decile lift chart
model %>%
  blr_gains_table() %>%
  blr_decile_lift_chart()

# Capture by decile
model %>%
  blr_gains_table() %>%
  blr_decile_capture_rate()

# Lorenz curve
blr_lorenz_curve(model)

# Ressiduals
# Influence diagnostic
blr_plot_diag_influence(model)

# Leverage diagnostic
blr_plot_diag_leverage(model)

# Fitted value diagnostic
blr_plot_diag_fit(model)
