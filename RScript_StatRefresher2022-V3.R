#######################
# R Training Day 1-2  for 2022 Stat Refresher
#######################

# Last Updated: August 4, 2022

# Install packages
#install.packages("foreign", dependencies=TRUE) #For data files other than CSV
install.packages("plyr", dependencies=TRUE)
install.packages('epiDisplay')
#install.packages("plyr")

# Enable packages
#library(foreign)
library(plyr)
library(epiDisplay)

# Setting working directory
setwd("~/Desktop/2022-Refreshers/Stat_R_Training")
# Note: You can set in Session>Set Working Directory>Choose Directory

#Get Working Directory
getwd()

####################
# LOADING DATASETS #
####################

# Loading .csv files
lfs17 <- read.csv("2017-AprLFS-edited.csv")

#Another .csv file
test_scores <- read.csv("student_scores2.csv")

#Yet another .csv file
laptop_sales <- read.csv("laptop_sales.csv")

###################
## DESCRIPTIVE STATISTICS ##
#Basic Measures: Max, Min, Central Tendency, Dispersion
###################

#View Variable of Interest
laptop_sales$laptop_sales

#Some Descriptive Statistics
summary(laptop_sales$laptop_sales)
max(laptop_sales$laptop_sales)
min(laptop_sales$laptop_sales)

#Central Tendency: Mean, Median
mean(laptop_sales$laptop_sales)
median(laptop_sales$laptop_sales)

#Store descriptive values as variables
X_max <- max(laptop_sales$laptop_sales)
X_min <- min(laptop_sales$laptop_sales)
X_bar <- mean(laptop_sales$laptop_sales)
X_median <- median(laptop_sales$laptop_salese)

#Measures of Dispersion: Std. Deviation and Variance
sd(laptop_sales$laptop_sales)
var(laptop_sales$laptop_sales)

#Store dispersion measures as variables
X_SD <- sd(laptop_sales$laptop_sales)
X_VAR <- var(laptop_sales$laptop_sales)

#Exercise: Find the basic descriptive statistics (Mean, Median, Variance,
#and Std. Dev) of the age variable in lfs17


###################
## DESCRIPTIVE STATISTICS 2 ##
#With some more complicated functions: Getting the mode, Using functions, and
#Plotting frequency distributions, histograms, and boxplots
###################


#Mode Function (source: https://www.tutorialspoint.com/r/r_mean_median_mode.htm)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

sales_mode <- getmode(laptop_sales$laptop_sales)
print(sales_mode)

# define mode2() function
mode2 = function(v) {
  
  # calculate mode of marks1  
  return(names(sort(-table(v)))[1])
}
# call mode() function
mode2(laptop_sales$laptop_sales)
mode2(test_scores$Score)


#FREQUENCY TABLES AND FREQ. DISTRIBUTIONS###
#Basic Type
table(laptop_sales$laptop_sales)

#Based on plyr
count(laptop_sales$laptop_sales)

#Yet Another One - Based on epiDisplay
tab1(laptop_sales$laptop_sales,sort.group='increasing',cum.percent=TRUE)

####Custom Coding with Class Intervals#####
#Getting the number of classes
number_of_classes <- function(n) {
  for (k in seq(1,n)){
    if (2^k > n){return(k)}
    else {k = k + 1}
  }
}

N <- length(laptop_sales$laptop_sales)
N
K <- number_of_classes(N)
print(K)

#Class Interval
class_interval <- ceiling(((X_max) - (X_min))/K)
bins <- seq(X_min-class_interval, X_max+class_interval, by=class_interval)

#Frequency Table
Sales <- cut(laptop_sales$laptop_sales, bins, right=TRUE)
count(Sales)

freq_table <- transform(table(Sales))
print(freq_table)

#With Relative and Cumulative Table
transform(freq_table, Rel_Freq=prop.table(Freq), Cum_Freq=cumsum(Freq))

#############################
#DESCRIPTIVE STATISTICS 3######
#BASIC PLOTS: BOXPLOT, HISTOGRAM###
#############################

boxplot(laptop_sales$laptop_sales)
hist(laptop_sales$laptop_sales)

#Improved Histogram
hist(laptop_sales$laptop_sales, breaks=bins, main="July 2019 Laptop Sales",
     xlab="Sales",col="red", right=FALSE)

hist(laptop_sales$laptop_sales, breaks=bins, col="slategray3",xlab = "Sales", 
     main="Histogram of Sales", color='blue',border="black", right=FALSE)

#Skewness
((mean(laptop_sales$laptop_sales)-median(laptop_sales$laptop_sales))*3)/sd(laptop_sales$laptop_sales)


#Exercise: Improve the basic box plot using similar arguments with what we did 
#with histograms

boxplot(laptop_sales$laptop_sales,horizontal=TRUE,
        color='gray', xlab='Sales', main='Laptop Sales')

#Exercise: Show summary statistics, and an appropriate plot
#for the age of the labor force in lfs17



#######################################
# CREATING AND MANIPULATING VARIABLES #
#######################################

# Displaying unique variable values
unique(lfs17$lf_partof)

# Creating variables
lfs17$agecube <- 0

# Lets create a variable that says a person is a senior if above 60
lfs17$senior <- ifelse(lfs17$age>=60,1,0)

# Logical operators
# == is "equal"
# != is "not equal"
# < is "less than"
# <= is "less than or equal to"
# > is "greater than"
# >= is "greater than or equal to"
# & is "and"
# | is "or"

# Basic mathematical functions
# (a) *,/,+,- multiplication, division, addition, subtraction
# (b) log(varname) natural logarithm

# Exercise:  Take the variable for age
# (a) take the log
lfs17$lnage <- log(lfs17$age)
# (b) take the cube of age
# (c) add, subtract, multiply and divide by 2


#######################
# SUBSETTING DATA AND RENAMING #
#######################

### Selecting/Keeping/Excluding variables ###

# select variables
selectvars <- c("lf_partof", "female", "age")
newdata1 <- lfs17[selectvars]

# select variables in 5th to 11th column
age_educ_data <- lfs17[c(5:11)]

# Exercise: Keep the following from lfs17
# (a) no college education (use columns 6 to 9)
# (b) no college education (use column names)

# exclude 3rd and 6th variable 
newdata2 <- lfs17[c(-3,-6)]

### Selecting observations ###

# first 5 observations
newdata3 <- lfs17[1:5,]

# based on variable values (males in the labor force)
newdata4 <- lfs17[which(lfs17$male==1 & lfs17$lf_partof==1), ]

# Exercise: Select the following observations from lfs17
# (a) first 1000 observations (use row numbers)
# (b) female college graduates
# (c) male high school graduates


#### RENAMING columns or variable names

colnames(lfs17)
names(lfs17)

# Renaming a specific column

newdata5 <- lfs17
names(newdata5)[3] <- "Male"

newdata6 <- lfs17
names(newdata6)[c(7,9)] <- c("basiced_grad","vocagrad")


# Using the variable name
newdata7 <- lfs17
names(newdata7)[names(newdata7) == "coll_grad"] <- "with_BachelorsDeg"


######################
### SORTING ###
######################

# sort lfs17 by age (descending) then by sex (female then male)

newdata8 <- lfs17[order(-lfs17$age, lfs17$male),]
#or
newdata9 <- lfs17[order(-lfs17$age, -lfs17$female),]


##############################
# MERGING DATASETS #
##############################

## Adding columns (Merging) ##

psei_1 <- read.csv("PSEI_2002-2012.csv")
psei_2 <- read.csv("PSEI_2012-2022.csv")
usdphp_1 <- read.csv("USDPHP_2002-2012.csv")
usdphp_2 <- read.csv("USDPHP_2012-2022.csv")

# merge two data frames by column ID
newdata10 <- merge(psei_1,usdphp_1,by="Month_Index")
newdata11<- merge(psei_2,usdphp_2,by="Month_Index")
#To remove extra date column: newdata10 <- newdata10[c(-4)]

## Adding by rows (Appending) ##
newfindata12 <- rbind(newdata10, newdata11)

names(newfindata12)[3] <- "PSEI_end_of_month"
names(newfindata12)[5] <- "USDPHP_end_of_month"
#Exercise: Clean the newfindata12 dataset using previous lessons on selecting
#rows, dropping columns, and renaming.

####################################
####LINE PLOTS: Time Series Plotting#####
##################################

#Basic Plot Command: Plot(X,Y) where X, Y are the variables per axis
plot(newfindata12$Month_Index,newfindata12$PSEI_end_of_month)

#Convert to Line Graph
options(scipen=5)
plot(newfindata12$Month_Index,newfindata12$PSEI_end_of_month,
     main = "PSEI end-of-month values from 2002 to 2022",
     xlab = "Month Index",
     ylab = "PSEI Index",
     ylim = c(0,9000),
     xlim = c(0,250),
     type='l',lwd=3, col='blue'
)
#Vertical Line Marker
abline(v=80, lty='dotted',lwd=3,col='gray')
#Legend
legend(150,4000,legend=c("PSEI end-of-month"),col='blue',lwd=2,cex=0.5)

#Exercise: Plot USDPHP data using the same methods & commands we used
#for the PSEI.


####################################
####NORMAL PROBABILITY DISTRIBUTIONS#####
####################################
#source: https://r-coder.com/normal-distribution-r/#pnorm_function_example

#Cumulative Probability Density Function for a sequence x
pnorm(x, mean = 0, sd = 1,  #Standard Normal Mean and SD
      lower.tail = TRUE, # If TRUE, probabilities are P(X <= x), or P(X > x) otherwise
      log.p = FALSE)     # If TRUE, probabilities are given as log

#Examples. Feel free to play around with. 
#For Standard Normal, note mean = 0, SD = 1
Mean <- 0
SD <- 1

# X grid for non-standard normal distribution
x <- seq(-3, 3, length = 100) * SD + Mean 

# Density function
y <- dnorm(x, Mean, SD)

#Normal Curve Plot
plot(x, y, type = "l", lwd = 2, col = "blue", ylab = "", xlab = "Weight")
abline(v = Mean) # Vertical line on the mean


#Solving Probabilities Example: P(X < 1100) or P(X <= 1100)
#Run this line by line and check the console for the result.
pnorm(1100, 1000, 100) #= P(X < 1100) = 84.13%
1 - pnorm(1100, 1000, 100, lower.tail =FALSE) # Equivalent to P(X < 1100) 
pnorm(1100, 1000, 100, lower.tail =FALSE)  # P(X > 1100) = 15.87%
      
Mean <- 0
SD <- 1

#Standard Normal: Example: P(Z < 1.96) = 0.9750
pnorm(-2.5, 0, 1) # P(Z < 1.96) =  97.5%
1 - pnorm(1.96, Mean, SD, lower.tail = FALSE) # Equivalent: P(Z < 1.96) =  97.5%

pnorm(1.96, Mean, SD, lower.tail = FALSE) # P(Z > 1.96) = 0.0249
0.5 - pnorm(2.5, Mean, SD, lower.tail = FALSE) #P(0 < Z < 1.96) = 0.04750

