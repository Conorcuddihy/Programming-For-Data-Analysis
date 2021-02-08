#Conor Cuddihy 
# 14415362
# CT5102 Assignment 1

#For this assignment we will be using the Linear Regression call:

mod <- lm(eruptions ~ waiting, data=faithful)

# question 1

# This function get_data() accepts the arguments mod and variable name and returns the 
# regression data as vector

get_data <- function(x, y){
  if(y=="waiting"){x$model$waiting}
  else{if(y=="eruptions"){x$model$eruptions}}
}

# Test Code 1

d <- get_data(mod, "waiting")
d[1:20]

# Test Code 2 
e <- get_data(mod, "eruptions")
e[1:20]

# question 2

# This function get_coefficient() accepts the arguments mod and coefficient name 
# and returns the coefficient value

get_coefficient <- function(x, y){
  if(y=="waiting"){x$coefficients[2]}
  else{if(y=="eruptions"){x$coefficients[1]}}
}

# Test Code 1

f <- get_coefficient(mod, "waiting")
f

#Test Code 2

g <- get_coefficient(mod, "eruptions")
g

#Question 3
# This function get_residuals() accepts the argument mod and returns the residuals

get_residuals <- function(x){
  list("data" = as.numeric(resid(x)), "mean" = mean(resid(x)), "number" = length(resid(x)))
}

# Test Code 

r <- get_residuals(mod)
str(r)

# Question 4
# This function mod_summary() accepts the arguments mod returns the call, the coefficients and the dimension. 

mod_summary <- function(x){
  list(call = x$call, coeff = x$coefficients, "DataSize" = as.data.frame(list("rows" = length(x$residuals), "columns" = length(x$coefficients))))
}

# Test Code 

s <- mod_summary(mod) 
s

