#Conor Cuddihy 
# 14415362
# CT5102 Assignment 2

#For this assignment the first thing we need to do is set up the raw data as follows with 
# 8 subjects and 25 students:

set.seed(100)
CX101 <- rnorm(25,45,8) 
CX102 <- rnorm(25,65,8) 
CX103 <- rnorm(25,85,10) 
CX104 <- rnorm(25,45,10) 
CX105 <- rnorm(25,60,5) 
CX106 <- rnorm(25,30,20) 
CX107 <- rnorm(25,50,10) 
CX108 <- rnorm(25,90,5) 

#Create a matrix from the raw data 

res <- cbind(CX101,CX102,CX103,CX104,CX105,CX106,CX107,CX108)

# Name the rows of the matrix after the students they represent

rownames(res) <- c("student_1", "student_2","student_3", "student_4", "student_5", 
                 "student_6", "student_7", "student_8","student_9", "student_10","student_11", 
                 "student_12","student_13", "student_14", "student_15", "student_16", 
                 "student_17", "student_18","student_19", "student_20", "student_21", 
                 "student_22","student_23", "student_24", "student_25")

# Produce a summary of this matrix

summary(res) 

# This matches the summary matrix provided on the assignment sheet

res[c(1,2,3,25),]

# We can see that CX103 has a few invalid values (> 100)

res[res[,"CX103"] > 100,]

# Now I will use the apply function to iterate through each column, convert any 
# outliers (< 0 or > 100) to the symbol NA 

res <- apply(res, 2, function(x){
  ifelse(x < 0 | x > 100, NA, x)
})

res[14:20,]

# Next I will us the apply function to replace the NA values with mean of all 
# other results for that subject

for(i in 1:ncol(res)){
  res[is.na(res[,i]), i] <- mean(res[,i], na.rm = TRUE)
}

res[14:20,]

# For each student I calculated the average and the range. 

mean <- apply(res, 1, mean)
range <- apply(res, 1, function(x)diff(range(x)))

# Now bind these as new columns in the matrix

res <- cbind(res, mean, range)
res

# Now I will use subset as a fitler query to display the student with the highest average

res[c(which(res[,"mean"]==max(res[,"mean"]))),, drop = F]  


