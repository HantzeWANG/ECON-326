library(data.table)
library(ggplot2)
library(tidyverse)
library(haven)
library(stargazer)

# Read data
data <- read_dta('CCHS_Annual_2017_2018_curated_trimmed.dta')

#only keep incdghh, dhhgage, GEN_005, EHG2DVR3, DHHGMS
dataFiltered <- data[,c('incdghh', 'dhhgage', 'GEN_005', 'EHG2DVR3', 'dhhgms', 'GEN_010')]


#remove data with incdghh, dhhgage, GEN_005, EHG2DVR3, dhhgms is NA
dataFiltered <- dataFiltered[complete.cases(dataFiltered),]
#A table of summary statistics including the mean, standard deviation, max, and for each variable
#treating all these variables as dummy variables
#copy dataFiltered to dataFiltered2
dataFiltered2 <- dataFiltered

# find standard deviation of incdghh
sd(dataFiltered$incdghh)
#find sd of dhhgage
sd(dataFiltered$dhhgage)
#find sd of GEN_005
sd(dataFiltered$GEN_005, na.rm=TRUE)
#find sd of EHG2DVR3
sd(dataFiltered$EHG2DVR3, na.rm=TRUE)
#find sd of dhhgms
sd(dataFiltered$dhhgms, na.rm=TRUE)


# for incdghh, if it equals 1, replace the value with 10000,
# if it equals 2, replace the value with 30000, if it equals 3, replace the value with 50000
# if it equals 4, replace the value with 70000, if it equals 5, replace the value with 90000

dataFiltered$incdghh <- ifelse(dataFiltered$incdghh == 1, 10000, dataFiltered$incdghh)
dataFiltered$incdghh <- ifelse(dataFiltered$incdghh == 2, 30000, dataFiltered$incdghh)
dataFiltered$incdghh <- ifelse(dataFiltered$incdghh == 3, 50000, dataFiltered$incdghh)
dataFiltered$incdghh <- ifelse(dataFiltered$incdghh == 4, 70000, dataFiltered$incdghh)
dataFiltered$incdghh <- ifelse(dataFiltered$incdghh == 5, 90000, dataFiltered$incdghh)
summary(dataFiltered$incdghh)
sd(dataFiltered$incdghh)

# for dhhgage, if it equals 1, replace the value with 13
# if it equals 2, replace the value with 16, if it equals 3, replace the value with 18.5
# if it equals 4, replace the value with 22, if it equals 5, replace the value with 27
# if it equals 6, replace the value with 32, if it equals 7, replace the value with 37
# if it equals 8, replace the value with 42, if it equals 9, replace the value with 47
# if it equals 10, replace the value with 52, if it equals 11, replace the value with 57
# if it equals 12, replace the value with 62, if it equals 13, replace the value with 67
# if it equals 14, replace the value with 72, if it equals 15, replace the value with 77
# if it equals 16, replace the value with 82
# write the code
dataFiltered$dhhgage <- ifelse(dataFiltered$dhhgage == 1, 100, dataFiltered$dhhgage)
dataFiltered$dhhgage <- ifelse(dataFiltered$dhhgage == 2, 200, dataFiltered$dhhgage)

dataFiltered$dhhgage <- ifelse(dataFiltered$dhhgage == 3, 18.5, dataFiltered$dhhgage)
dataFiltered$dhhgage <- ifelse(dataFiltered$dhhgage == 4, 22, dataFiltered$dhhgage)
dataFiltered$dhhgage <- ifelse(dataFiltered$dhhgage == 5, 27, dataFiltered$dhhgage)
dataFiltered$dhhgage <- ifelse(dataFiltered$dhhgage == 6, 32, dataFiltered$dhhgage)
dataFiltered$dhhgage <- ifelse(dataFiltered$dhhgage == 7, 37, dataFiltered$dhhgage)
dataFiltered$dhhgage <- ifelse(dataFiltered$dhhgage == 8, 42, dataFiltered$dhhgage)
dataFiltered$dhhgage <- ifelse(dataFiltered$dhhgage == 9, 47, dataFiltered$dhhgage)
dataFiltered$dhhgage <- ifelse(dataFiltered$dhhgage == 10, 52, dataFiltered$dhhgage)
dataFiltered$dhhgage <- ifelse(dataFiltered$dhhgage == 11, 57, dataFiltered$dhhgage)
dataFiltered$dhhgage <- ifelse(dataFiltered$dhhgage == 12, 62, dataFiltered$dhhgage)
dataFiltered$dhhgage <- ifelse(dataFiltered$dhhgage == 13, 67, dataFiltered$dhhgage)
dataFiltered$dhhgage <- ifelse(dataFiltered$dhhgage == 14, 72, dataFiltered$dhhgage)
dataFiltered$dhhgage <- ifelse(dataFiltered$dhhgage == 15, 77, dataFiltered$dhhgage)
dataFiltered$dhhgage <- ifelse(dataFiltered$dhhgage == 16, 82, dataFiltered$dhhgage)
dataFiltered$dhhgage <- ifelse(dataFiltered$dhhgage == 100, 13, dataFiltered$dhhgage)
dataFiltered$dhhgage <- ifelse(dataFiltered$dhhgage == 200, 16, dataFiltered$dhhgage)
summary(dataFiltered$dhhgage)
sd(dataFiltered$dhhgage)



# dhhgms is a dummy with 4 levels, so we need to create 3 dummy variables
#four levels are Married, Common-law, Widowed/Divorced/Separated, Single
#we need to create 3 dummy variables, one for each level
Married <- ifelse(dataFiltered$dhhgms == 1, 1, 0)
Common_law <- ifelse(dataFiltered$dhhgms == 2, 1, 0)
Widowed_Divorced_Separated <- ifelse(dataFiltered$dhhgms == 3, 1, 0)
dataM <- data.frame(Married, Common_law, Widowed_Divorced_Separated)


Less_than_secondary <- ifelse(dataFiltered$EHG2DVR3 == 1, 1, 0)
Post_secondary_university <- ifelse(dataFiltered$EHG2DVR3 == 3, 1, 0)
dataE <- data.frame(Less_than_secondary, Post_secondary_university)
sd(dataE$Less_than_secondary)

# GEN_005 is a dummy with 5 levels, so we need to create 4 dummy variables
excellent <- ifelse(dataFiltered$GEN_005 == 1, 1, 0)
veryGood <- ifelse(dataFiltered$GEN_005 == 2, 1, 0)
good <- ifelse(dataFiltered$GEN_005 == 3, 1, 0)
fair <- ifelse(dataFiltered$GEN_005 == 4, 1, 0)
dataG <- data.frame(excellent, veryGood, good, fair)
# combine dataG, dataM, dataE and dataFiltered
dataFiltered <- cbind(dataFiltered, dataG, dataM, dataE)

# use stargazer to create a linear regression table, dependent variable is GEN_010, independent variables are GEN_005
model1 <- lm(GEN_010 ~ GEN_005, data = dataFiltered)
stargazer(model1, type = "text")

model2 <- lm(GEN_010 ~ GEN_005 + incdghh, data = dataFiltered)
stargazer(model2, type = "text")

model3 <- lm(GEN_010 ~ GEN_005 + incdghh + Married, data = dataFiltered)
stargazer(model3, type = "text")
