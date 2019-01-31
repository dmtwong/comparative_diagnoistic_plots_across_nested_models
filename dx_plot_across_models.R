###### 1. A function to extract fitted values and residuals across models 
##        Input:  model(or models), and some control parameter
##        Output: A data frame (tall shape) for plot

###### 2. A function to plot it out taking the data frame from (1) 
##        Input:  Data frame from (1), and some control parameter
##        Output: ggplot

library(ggplot2)
 
ext_from_mod <- function(...){
  mod_name_list <- vapply(match.call(expand.dots = F)$..., deparse, character(1))
  if (all(sapply(mod_name_list, function(x) {'lm' %in% class(get(x)) })) == FALSE){
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

# ------------------------------------------------------------------------------
# str(mtcars)
mod_1 <- lm(mpg ~ wt, data = mtcars)
mod_2 <- update(mod_1, . ~ . + hp)
# summary(mod_1); summary(mod_2)

ext_from_mod(mod_1, mod_2) # Test 1: valid input (lm) generate expected output?

# install.packages('titanic')
library(titanic)
# objects('package:titanic')
# str(titanic_train)
library(dplyr)
titanic <- as.data.frame(titanic_train) %>% mutate(Sex = as.factor(Sex)) 
mod_3 <- glm(Survived ~ Sex*Age,
             family=binomial(link='logit'),data=titanic)
mod_4 <- glm(Survived ~ Sex,
               family=binomial(link='logit'),data=titanic)
mod_5 <- glm(Sex ~ Survived,
             family=binomial(link='logit'),data=titanic)
# summary(mod_3);summary(mod_4)
ext_from_mod(mod_3, mod_4) # Test 2: valid input (glm) generate expected output?
ext_from_mod(mod_1, mod_4) # Test 3: Same dataUsed across the model?
ext_from_mod(mod_4, mod_5) # Test 4: Same predictors across the model?
