rm(list = ls())
###### 1. A function to extract fitted values and residuals across models 
##        Input:  model(or models), and some control parameter
##        Output: A data frame (tall shape) for plot

###### 2. A function to plot it out taking the data frame from (1) 
##        Input:  Data frame from (1), and some control parameter
##        Output: ggplot

library(ggplot2)

ext_from_mod <- function(...){
  mod_name_list <- vapply(match.call(expand.dots = F)$..., deparse, character(1))
  if (all(sapply(mod_name_list, function(x) {all(class(get(x)) == 'lm') })) == FALSE){
    stop('input is not in lm class')
  }
  
  list_mod <- list(...)
  # print(tmp); mod_1$call$formula; mod_1$call$data
  # check_list <- sapply(list_mod, function(x) {
  #   print(x)
  # })
  
  ### start fruther input checking
  check_list <- sapply(list_mod, function(x){
    # print(x)
    data_applied <- deparse(x$call$data)
    # print(data_applied)
    tmp_formula <- unlist(strsplit(gsub(' ', '',
                                        deparse(x$call$formula)), '~'))
    # print(tmp_formula)
    y_applied <- tmp_formula[[1]]
    regMod_applied <- tmp_formula[[2]]
    
    c(data_applied, y_applied, regMod_applied)
  }
  )
  dimnames(check_list) <- list(c('dataUsed', 'yUsed', 'regModUsed'),
                               c('m1', 'm2'))
  check_list <- data.frame(check_list)
  print(check_list)
  # print(length(unique(check_list[1, ] )))
  if ( length(unique(unlist(check_list[1, ])) ) > 1){
    stop('Models are not built on same dataset')
  }
  if ( length(unique(unlist(check_list[2, ])) ) > 1){
    stop('Different Y across models cannot be compared')
  }
  ### Done input checking
  
  res_Y <- unlist(lapply(list_mod, resid))
  fit_X <- lapply(list_mod, fitted)      # Extract fitted values
  num_Rep <- sapply(fit_X, length)
  # print(num_Rep)
  obs_ord_X <- unlist(lapply(num_Rep, function(x) seq(x)))
  # print(mod_name_list)
  model_X <- rep(mod_name_list, num_Rep)
  # print(model_X)
  
  df_group <- data.frame(res_Y, fit_X = unlist(fit_X), 
                         obs_ord_X, model_X)
  # head(df_group)
  # tail(df_group)
} 


diag_plot <- function(consolidated_df, is_fitted_Y = TRUE, is_loess = TRUE){
  if (is_loess == TRUE) {
    fit_type <- 'loess'
  } else {
    fit_type <- 'lm'
  }
  # if (is_fitted_Y == TRUE) {
  #   # x_opt <- as.name('fit_X')
  #   x_opt <- 'fit_X'
  # 
  # } else {
  #   # x_opt <- as.name('obs_ord_X')
  #   x_opt <- 'obs_ord_X'
  # 
  # }
  gg_1 <- ggplot(data = consolidated_df, aes(y = res_Y, shape = model_X, col = model_X))
  
  if (is_fitted_Y == TRUE) {
    gg_1 + geom_point(aes(x = fit_X)) + 
      geom_smooth(aes(x = fit_X), method = fit_type)
  } else {
    gg_1 + geom_point(aes(x = obs_ord_X)) + 
      geom_smooth(aes(x = obs_ord_X), method = fit_type)
  }
}


# ------------------------------------------------------------------------------
# str(mtcars)
mod_1 <- lm(mpg ~ wt, data = mtcars)
mod_2 <- update(mod_1, . ~ . + hp)
# summary(mod_1); summary(mod_2)

ext_from_mod(mod_1, mod_2) # Test 1: valid input (lm) generate expected output?

# install.packages('titanic')
# library(titanic)
# objects('package:titanic')
# str(titanic_train)
# library(dplyr)
# titanic <- as.data.frame(titanic_train) %>% mutate(Sex = as.factor(Sex)) 
# mod_3 <- glm(Survived ~ Sex*Age,
#              family=binomial(link='logit'),data=titanic)
# mod_4 <- glm(Survived ~ Sex,
#              family=binomial(link='logit'),data=titanic)
# mod_5 <- glm(Sex ~ Survived,
#              family=binomial(link='logit'),data=titanic)
# summary(mod_3);summary(mod_4)
# ext_from_mod(mod_3, mod_4) # Test 2: valid input (glm) generate expected output?
# ext_from_mod(mod_1, mod_4) # Test 3: Same dataUsed across the model?
# ext_from_mod(mod_4, mod_5) # Test 4: Same predictors across the model?

# ------------------------------------------------------------------------------
consolidated_df_1 <- ext_from_mod(mod_1, mod_2)
# consolidated_df_2 <- ext_from_mod(mod_3, mod_4)
diag_plot(consolidated_df_1)
diag_plot(consolidated_df_1, is_fitted_Y = TRUE, is_loess = TRUE) # Test 1: valid input (lm) generate expected output?
diag_plot(consolidated_df_1, is_fitted_Y = FALSE, is_loess = TRUE) # Test 1: valid input (lm) generate expected output?
diag_plot(consolidated_df_1, is_fitted_Y = TRUE, is_loess = FALSE) # Test 1: valid input (lm) generate expected output?
diag_plot(consolidated_df_1, is_fitted_Y = FALSE, is_loess = FALSE) # Test 1: valid input (lm) generate expected output?

# diag_plot(consolidated_df_2, is_fitted_Y = TRUE, is_loess = TRUE) # Test 1: valid input (lm) generate expected output?
# diag_plot(consolidated_df_2, is_fitted_Y = FALSE, is_loess = TRUE) # Test 1: valid input (lm) generate expected output?
# diag_plot(consolidated_df_2, is_fitted_Y = TRUE, is_loess = FALSE) # Test 1: valid input (lm) generate expected output?
# diag_plot(consolidated_df_2, is_fitted_Y = FALSE, is_loess = FALSE) # Test 1: valid input (lm) generate expected output?
# 
