###### 1. A function to extract fitted values and residuals across models 
##        Input:  model(or models), and some control parameter
##        Output: A data frame (tall shape) for plot

###### 2. A function to plot it out taking the data frame from (1) 
##        Input:  Data frame from (1), and some control parameter
##        Output: ggplot

library(ggplot2)

ext_from_mod <- function(...){
  tmp <- list(...)
  tmp
} 

diag_plot <- function(x){
  NULL
}

# ------------------------------------------------------------------------------
str(mtcars)
mod_1 <- lm(mpg ~ wt, data = mtcars)
mod_2 <- update(mod_1, . ~ . + hp)
summary(mod_1); summary(mod_2)

ext_from_mod(mod_1, mod_2)
