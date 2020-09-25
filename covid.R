# 09/22/2020
# Hierchical Cluster Analysis with COVID-19 Survey Data

# PRELIMINARY #################################################################

# Libraries
library(dplyr)
library(magrittr)
library(purrr)

# Import data
usa <- read.csv("united-states.csv")

# Manipulation
# subset to week 5+
usa$qweek <- as.numeric(gsub(x = usa$qweek, pattern = "week ", replace = "",
                  fixed = T))

usa <- filter(usa, qweek >= 5)

# remove time from date
usa$endtime <- format(as.POSIXct(usa$endtime, 
                                 format = "%m/%d/%Y %H:%M"),
                      format = "%m/%d/%Y")

# remove NA columns
usa <- usa %>% select(., -matches("(m|v|r|ct|ox)[0-9]")) %>% 
  select(., -matches("i[0-9](a|b)")) %>%
  select(., -matches("i[6-8]")) %>%
  select(., -matches("i14")) %>%
  filter(., CORE_B2_4 != " ") %>%
  filter(., i12_health_1 != " ") %>%
  na.omit(.)

# check that we still have some data!
dim(usa)[1] != 0

# ANALYSIS ####################################################################

# Will attempt logistic regression
# Question: Can we predict if an individual will wear a mask?
# y = i12_health_1 (I have worn a mask outdoors)

# Using book: An Introduction to Categorical Data Analysis (Agestri)
library(gam)

# Create dummy Y variable
unique(usa$i12_health_1) # Want to select Always vs Other
for (row in 1:length(usa$i12_health_1)){
  
  if (isTRUE((usa$i12_health_1[row]) == "Always")) {
    usa$mask_dummy[row] <- 1 }
  
  else {
    usa$mask_dummy[row] <- 0 }
}
# Logistic Regression: i12_health_1 ~ age
plot(jitter(mask_dummy, 0.08) ~ age, data=usa) # scatterplot of y by x=age
gam.fit <- gam(mask_dummy ~ s(age), family = binomial,
               data = usa) # fit generalized additive model 
curve(predict(gam.fit, data.frame(age=x), type="resp"), add=TRUE) # add GAM curve to scatterplot
fit <- glm(mask_dummy ~ age, family=binomial, data=usa) # fit generalized linear model
curve(predict(fit, data.frame(age=x), type="resp"), add=TRUE) # add logistic regression to curve
summary(fit) # print logistic reg summary
confint(fit) # print confidence intervals for logistic regression

# Predict probability of Always wearing a mask using various age inputs
predict(fit, data.frame(age = 21), type="response") # estimated P (x=21)
predict(fit, data.frame(age = 48), type="response") # estimated P (x=48)
predict(fit, data.frame(age = 70), type = "response") # estimated P (x=70)

library(car)
Anova(fit)

