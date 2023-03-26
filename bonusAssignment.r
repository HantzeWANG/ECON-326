library(haven)
library(stargazer)
library(dplyr)

data <- read_dta('pwt100.dta')
#filter by year between 2015 and 2019
dataFiltered <- data %>% filter(year >= 2015 & year <= 2019)
# create a new variable, which is GDP per capita, devide rgdpna by pop
dataFiltered <- dataFiltered %>% mutate(gdp_per_capita = rgdpna/pop)

model1 <- lm(gdp_per_capita ~ hc, data = dataFiltered)
model2 <- lm(gdp_per_capita ~ hc + ccon, data = dataFiltered)
model3 <- lm(gdp_per_capita ~ hc + ccon + cda, data = dataFiltered)

# use stargazer to create a linear regression table for model1, model2, model3
stargazer(model1, model2, model3, type = "text")