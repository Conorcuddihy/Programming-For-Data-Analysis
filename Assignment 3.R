#Conor Cuddihy 
# 14415362
# CT5102 Assignment 3

# The first step is to set the seed to 100, and create a copy of ggplot2::mpg, 
# and select 10 random observations, using the function sample(), and set the 
# cty variable to be -999 for each observation.

set.seed(100) 
mpg <- as.data.frame(ggplot2::mpg)
new_df <- as.data.frame(ggplot2::mpg) 

df_sample <- new_df[sample(nrow(new_df), 10), ]

new_df[row.names(df_sample), "cty"] <- -999

print_df_sample <- function(){
  set.seed(100)
  new_df[sample(nrow(new_df), 10), ]
}
print_df_sample()

# Next, I use lapply (and converting the result to a data frame),I will 
# replace all negative values with the symbol NA

new_df <- as.data.frame(lapply(new_df, function(x){
  ifelse(x < 0, NA, x)
}))

print_df_sample()

# Next, calculate the mean cty for each of the different classes of car 
# (excluding NA values)

mean_by_class <- aggregate( cty ~ class, new_df, mean )
mean_by_class

# Then, for each row with a missing value, replace the NA with the mean of 
# the car class

for (i in which(sapply(new_df, is.numeric))) {
  for (j in which(is.na(new_df[, i]))) {
    new_df[j, i] <- mean(new_df[new_df[, "class"] == new_df[j, "class"], i],  na.rm = TRUE)
  }
}

print_df_sample()

# Now I will confirm that all values are now valid in the data frame by summarising the data.

summary(new_df)

# Now I will show that the two mean values now differ:

mean(mpg$cty)

mean(new_df$cty)  

## All my answers match question sheet, so I am happy with my results 






  

