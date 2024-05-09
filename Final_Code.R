#Sara Irshad Final Project
library(MASS)
library(car)
library(data.table)
library(tidyverse)
library(faraway)
library(dplyr)

mydata <- fread("my_data.csv", nrows = 410)
head(mydata)
table(mydata$Marital_status)
#checking how the categorical variables are currently coded
str(mydata)
#I see some of them are incorrectly coded as int
# Convert Marital_status to factor
mydata$Marital_status <- factor(mydata$Marital_status,levels = c(1, 2, 3, 6),
labels = c("single", "in_relationship", "separated_divorced_widow", "other"))
levels(mydata$Marital_status)
# Create a copy of the data excluding the 'other' category - i'm deeming it not really that useful/only 2 data points with "other"
mydata2 <- mydata[mydata$Marital_status != "other", ]
table(mydata2$Marital_status)
#drop unused levels such as 'other'
mydata2$Marital_status <- droplevels(mydata2$Marital_status)
table(mydata2$Marital_status)
str(mydata2)
#converting 'education' into factor
mydata2$Education <- factor(mydata2$Education,levels = c(1, 2, 3, 4, 5),
labels = c("no_education", "compulsory_school","post_compulsory_education", "univ_applied_science_technology_degree", "university"))
str(mydata2$Education)
#same for type of pregnancy
mydata2$Type_pregnancy <- factor(mydata2$Type_pregnancy, levels = c(1, 2),labels = c("single_pregnancy", "twin_pregnancy"))
str(mydata2$Type_pregnancy)

# calculate the total CBTS score
mydata2$total_CBTS <- rowSums(mydata2[, c("CBTS_M_3", "CBTS_M_4", "CBTS_M_5", "CBTS_M_6", "CBTS_M_7", 
                                          "CBTS_M_8", "CBTS_M_9", "CBTS_M_10", "CBTS_M_11", "CBTS_M_12", 
                                          "CBTS_13", "CBTS_14", "CBTS_15", "CBTS_16", "CBTS_17", "CBTS_18", 
                                          "CBTS_19", "CBTS_20", "CBTS_21", "CBTS_22")], na.rm = TRUE)

# Check the first few rows to ensure the total_CBTS score was calculated correctly
head(mydata2$total_CBTS)

# Fit the MLR model
lm1 <- lm(total_CBTS ~ Age + Education + Marital_status + Type_pregnancy + Gestationnal_age, data = mydata2)
summary(lm1)

#checking model assumptions
par(mfrow=c(2, 2), mar=c(4, 4, 2, 1))
plot(lm1)


# let's check the scatterplot matrix

pairs(total_CBTS~Age+Gestationnal_age, data = mydata2)

#relationship does not look linear

#let's look at the categorical variables now

#Boxplot for Education levels against total_CBTS
boxplot(mydata2$total_CBTS ~ mydata2$Education, main="CBTS Score by Education Level", xlab="Education Level", ylab="Total CBTS Score")

#Marital Status against total_CBTS
boxplot(mydata2$total_CBTS ~ mydata2$Marital_status, main="CBTS Score by Marital Status", xlab="Marital Status", ylab="Total CBTS Score")

#Type of Pregnancy against total_CBTS
boxplot(mydata2$total_CBTS ~ mydata2$Type_pregnancy, main="CBTS Score by Pregnancy Type", xlab="Pregnancy Type", ylab="Total CBTS Score")
table(mydata2$Type_pregnancy)
table(mydata2$Education)

par(mfrow=c(1, 2), mar=c(4.5, 4.5, 2, 2))

# Plot standardized residuals vs fitted values
plot(predict(lm1), rstandard(lm1),
     xlab="Fitted Values", ylab="Standardized Residuals")
abline(h=0)

# QQ plot of standardized residuals
qqnorm(rstandard(lm1))
qqline(rstandard(lm1))


par(mfrow=c(2, 3), mar=c(4.5, 4.5, 2, 2))  

# Plot standardized residuals vs Age
plot(mydata2$Age, rstandard(lm1), xlab="Age", ylab="Standardized Residuals")

# Plot standardized residuals vs Gestational_age
plot(mydata2$Gestationnal_age, rstandard(lm1), xlab="Gestational Age", ylab="Standardized Residuals")

# Histograms

# Histogram for Age
hist(mydata2$Age, main="Histogram of Age", xlab="Age", breaks=30)

# Histogram for Gestational Age
hist(mydata2$Gestationnal_age, main="Histogram of Gestational Age", xlab="Gestational Age", breaks=30)

hist(mydata2$total_CBTS)

# Adding a constant to account for zeros in total_CBTS
mydata2$log_total_CBTS <- ifelse(mydata2$total_CBTS == 0, log(mydata2$total_CBTS + 1), log(mydata2$total_CBTS))
# Refit the model using the transformed response variable
lm_transformed <- lm(log_total_CBTS ~ Age + Education + Marital_status + Type_pregnancy + Gestationnal_age, data = mydata2)
# Check the new model's diagnostics
par(mfrow=c(2, 2))
plot(lm_transformed)

#let's check for interactions

# Fit the full model with all two-way interactions
full_model <- lm(total_CBTS ~ (Age + Education + Marital_status + Type_pregnancy + Gestationnal_age)^2, data = mydata2)
summary(full_model)

# Use stepwise selection to find a model that minimizes AIC
step_model <- step(full_model)

# Display the summary of the selected model
summary(step_model)

# Comparing the original model lm1 and the selected stepwise model - don't need to include this in slides
anova(step_model, lm1)

# Check the diagnostic plots of the selected model
par(mfrow=c(2, 2), mar=c(4, 4, 2, 1))
plot(step_model)
summary(step_model)$adj.r.squared
summary(lm1)$adj.r.squared

#multicollinearity

round(vif(lm1),2)

sqrt(vif(lm1))>2 #problem?

table(mydata2$Education)

#organizing the Education variable into 3 categories: basic education, post compulsory, high education
# Create a copy of the data and modify the Education variable
mydata2_copy <- mydata2 %>%
  mutate(Education = case_when(
    Education %in% c("no_education", "compulsory_school") ~ "Basic Education",
    Education %in% c("univ_applied_science_technology_degree", "university") ~ "Higher Education",
    Education %in% c("post_compulsory_education") ~ "Post Compulsory Education"
  ))

# Convert the modified 'Education' variable back to a factor with new levels
mydata2_copy$Education <- factor(mydata2_copy$Education,
                                 levels = c("Basic Education", "Post Compulsory Education", "Higher Education"))

# Check the table
table(mydata2_copy$Education)
table(mydata2$Education)
# Fit the new linear model with the adjusted education variable
lm2_adjusted_copy <- lm(total_CBTS ~ Age + Education + Marital_status + Type_pregnancy + Gestationnal_age, 
                        data = mydata2_copy)

summary(lm2_adjusted_copy)
round(vif(lm2_adjusted_copy),2)

par(mfrow=c(2, 2))
plot(lm2_adjusted_copy)

#box cox

#alternate way since car isnt working

# Adjust the response variable by adding 1 to avoid zeros
mydata2_copy$adjusted_CBTS <- mydata2_copy$total_CBTS + 1

# Fit the model with the adjusted response
lm2 <- lm(adjusted_CBTS ~ Age + Education + Marital_status + Type_pregnancy + Gestationnal_age, 
             data = mydata2_copy)

# Apply the Box-Cox transformation to find the optimal lambda
bc_transform <- boxcox(lm2, plotit = FALSE)

# Find the lambda that maximizes the log-likelihood
lambda <- bc_transform$x[which.max(bc_transform$y)]
lambda

# Applying the Box-Cox transformation to the total CBTS score
mydata2_copy$transformed_CBTS <- (mydata2_copy$total_CBTS + 1)^0.33


lm2_transformed <- lm(transformed_CBTS ~ Age + Education + Marital_status + Type_pregnancy + Gestationnal_age, data = mydata2_copy)


summary(lm2_transformed)


par(mfrow=c(2, 2)) 
plot(lm2_transformed) 

# Shapiro-Wilk normality test on the residuals of the original model
shapiro.test(residuals(lm1))

# Shapiro-Wilk normality test on the residuals of the transformed model
shapiro.test(residuals(lm2_transformed))

#stepwise
final_lm <-step(lm2_transformed)
summary(final_lm)

