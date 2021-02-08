# Initializing packrat
packrat::init()

# Taking a snapshot within packrat
packrat::snapshot()

#Importing Heart Failure data
data <- read.csv("heartFailureMortality.csv", header = TRUE)

# Initial Checks on the data
dim(data) # shows the size of the data
head(data) # Shows first 6 rows


# Installing requisite packages
install.packages("survival")
install.packages("ggplot2")

# Loading requisite packages
library(survival)
library(ggplot2)

# KM Anslysis - extracting important data as independent variables
gender <- factor(data[,"gender"], 
                    levels = c(1,2),
                    labels = c("Male", "Female")) # R calls categorical variables factors
fu_time <- data[,"fu_time"] # continuous variable (numeric) 
death <- data[,"death"] # binary variable (numeric) 

# Running the KM Plot
km_fit <- survfit(Surv(fu_time, death) ~ 1)
plot(km_fit)

# To see the Kaplan Meier survival estimates over time
summary(km_fit, times = c(1:7,30,60,90*(1:10))) 

# Splitting the curve by gender
km_gender_fit <- survfit(Surv(fu_time, death) ~ gender) 
plot(km_gender_fit)

# Log Rank test to compare the two KM plots for each gender
survdiff(Surv(fu_time, death) ~ gender, rho=0)


# Just to be sure you’ve got that, I’d like you to compare survival by broad age
# group: those aged 65 and above versus those aged under 65. To do this, you’ll
# need to dichotomise age, for instance using the “ifelse” command, which was
# covered in the first course in this specialisation.

ageCategory = NULL

for (i in 1:nrow(data)) {
  if (data$age[i] >= 65){
    ageCategory[i] = 1
  } else
  {
    ageCategory[i] = 2
  }  
}

# Change 
# Splitting the curve by AgeCategory
km_ageCategory <- survfit(Surv(fu_time, death) ~ ageCategory) 
plot(km_ageCategory)

# Log Rank test to compare the two KM plots for each age category
survdiff(Surv(fu_time, death) ~ ageCategory, rho=0)

# SOLUTION CODE 
age_65plus <- ifelse(g[,"age"]>=65,1,0) # dichotomise age
table(age_65plus, exclude = NULL) # inspect the numbers - always a good idea
table(age,age_65plus, exclude = NULL) # check - an even better idea...

# Cox's Proportional Hazards

install.packages("survminer")
library(survival)
library(survminer)
cox_age <- coxph(Surv(fu_time,death)~age, data=data)
summary(cox_age)

# Ethnicity as continuous 
cox_test <- coxph(Surv(fu_time, death) ~ ethnicgroup, data = data) # take variables straight from g
summary(cox_test)

# Ethnicity as factor
ethnicgroup <- factor(data[,"ethnicgroup"]) # can also use “as.factor” rather than “factor”
fu_time <- data[,"fu_time"]
death <- data[,"death"]

cox_ethnicity <- coxph(Surv(fu_time, death) ~ ethnicgroup)
summary(cox_ethnicity)