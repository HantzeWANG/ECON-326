# Y is GEN_010, which is the general satisfactory
# X is DHH_SEX, which is the gender
library(data.table)
library(ggplot2)
library(tidyverse)
library(haven)

# Read data
data <- read_dta('CCHS_Annual_2017_2018_curated_trimmed.dta')

#only keep incdghh, dhhgage, GEN_005, EHG2DVR3, DHHGMS
dataFiltered <- data[,c('GEN_010', 'dhhgms', 'DHH_SEX')]
#remove data with dhhgms, GEN_010, DHH_SEX is NA
dataFiltered <- dataFiltered[complete.cases(dataFiltered),]

# Using the t.test function in R, estimate the difference in average Yi for
# the two levels of Xi using a model like y ∼ x
t.test(dataFiltered$GEN_010 ~ dataFiltered$DHH_SEX)

# using lm function to estimate the difference in average Yi for different levels of Xi
simple.fit = lm(dataFiltered$GEN_010 ~ dataFiltered$DHH_SEX)
summary(simple.fit)


multi.fit = lm(dataFiltered$GEN_010 ~ dataFiltered$DHH_SEX + dataFiltered$dhhgms)
summary(multi.fit)