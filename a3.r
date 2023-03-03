# Y is GEN_025, which is the perceived stress at work
# X is DHH_SEX, which is the gender
library(data.table)
library(ggplot2)
library(tidyverse)
library(haven)

# Read data
data <- read_dta('CCHS_Annual_2017_2018_curated_trimmed.dta')

#only keep 'GEN_025', 'CMH_015F', 'DHH_SEX'
dataFiltered <- data[,c('GEN_025', 'CMH_015F', 'DHH_SEX')]
#remove data with 'GEN_025', 'CMH_015F', 'DHH_SEX' is NA
dataFiltered <- dataFiltered[complete.cases(dataFiltered),]

#CMH_015F is if consulted mental health professional
# Using the t.test function in R, estimate the difference in average Yi for
# the two levels of Xi using a model like y ∼ x
t.test(dataFiltered$GEN_025 ~ dataFiltered$DHH_SEX)

# using lm function to estimate the difference in average Yi for different levels of Xi
simple.fit = lm(dataFiltered$GEN_025 ~ dataFiltered$DHH_SEX)
summary(simple.fit)


multi.fit = lm(dataFiltered$GEN_025 ~ dataFiltered$DHH_SEX + dataFiltered$CMH_015F)
summary(multi.fit)