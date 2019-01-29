###### 1. A function to extract fitted values and residuals across models 
##        Input:  model(or models), and some control parameter
##        Output: A data frame (tall shape) for plot

###### 2. A function to plot it out taking the data frame from (1) 
##        Input:  Data frame from (1), and some control parameter
##        Output: ggplot

library(ggplot2)

ext_from_mod <- function(...){
  tmp <- vapply(as.list((match.call()[-1])), deparse, FUN.VALUE = character(1))
  if (all(sapply(tmp, function(x) {'lm' %in% class(get(x)) })) == FALSE){
    stop('input is not in lm class')
  }
  list_mod <- list(...)
  # print(tmp); mod_1$call$formula; mod_1$call$data
  # check_list <- sapply(list_mod, function(x) {
  #   print(x)
  # })
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
