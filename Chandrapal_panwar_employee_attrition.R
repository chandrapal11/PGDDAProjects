
#################  HR Analytics Case Study  #################


################# 1. LOADING DATASETS #################

gen_data <- read.csv("general_data.csv", stringsAsFactors = F)
emp_survey <- read.csv("employee_survey_data.csv", stringsAsFactors = F)
man_survey <- read.csv("manager_survey_data.csv", stringsAsFactors = F)
in_time <- read.csv("in_time.csv", stringsAsFactors = F)
out_time <- read.csv("out_time.csv", stringsAsFactors = F)


################# 2. DATA UNDERSTANDING #################

str(gen_data)     
# It contains demographics and attrition information (which employees left).

str(emp_survey)   
# It contains various feedbacks and ratings of company culture from employees.

str(man_survey)   
# It contains ratings of employees' behaviour and performance from their supervisors.

str(in_time)      
# It contains office coming timestamp of employees 

str(out_time)     
# It contains office leaving timestamp of employees


################# 3. DATA CLEANING #################

# We see that column names for Employee IDs are missing in in_time and out_time data frames
colnames(in_time)[1] <- "EmployeeID"
colnames(out_time)[1] <- "EmployeeID"

length(unique(gen_data$EmployeeID))
length(unique(emp_survey$EmployeeID))
length(unique(man_survey$EmployeeID))
length(unique(in_time$EmployeeID))
length(unique(out_time$EmployeeID))
# Hence, all 4410 Employee IDs are unique in all data frames.

sum(gen_data$EmployeeID - emp_survey$EmployeeID)
sum(gen_data$EmployeeID - man_survey$EmployeeID)
sum(gen_data$EmployeeID - in_time$EmployeeID)
sum(gen_data$EmployeeID - out_time$EmployeeID)
# Hence, same Employee IDs with same index in all data frames.

# in_time and out_time are stored as character. Converting them into time.
in_time <- data.frame(sapply(in_time[,-1],
           function(x) as.POSIXct(x, tz="", format="%Y-%m-%d %H:%M:%S")))

out_time <- data.frame(sapply(out_time[,-1], 
            function(x) as.POSIXct(x, tz="", format="%Y-%m-%d %H:%M:%S")))

# Checking NAs in timestamp datasets
sum(is.na(in_time))
sum(is.na(out_time))
# Both have equal no. of NAs

sum(which(is.na(out_time)) - which(is.na(in_time)))
# Every NA in in_time is at the same index as index of every NA in out_time.
# It means, each NA represents that particular employee did not come on respective date. 

# Converting time difference (in seconds) into working hours of each employee on each date
work_hrs <- (out_time - in_time)/(60 * 60)

sapply(work_hrs, function(x) sum(is.na(x)))
# Several dates have NA's for all 4410 employees. All employees can't be on leave.
# It means that such dates were weekend holidays or public holidays

# Removing such dates
work_hrs <- work_hrs[, sapply(work_hrs, function(x) sum(is.na(x))) < 4410]

# Deriving metrics from timestamp information
work_hrs_tpose <- data.frame(t(work_hrs))

Leaves <- sapply(work_hrs_tpose, function(x) sum(is.na(x)))
Tot_Hrs <- sapply(work_hrs_tpose, sum, na.rm = T)
Avg_Hrs <- sapply(work_hrs_tpose, mean, na.rm = T)


################# 4. MERGING THE DATA #################

employee <- merge(gen_data, emp_survey, by = "EmployeeID")
# No drop in no. of rows. All Employee IDs are same and unique in both datasets.

employee <- merge(employee, man_survey, by = "EmployeeID")
# No drop in no. of rows. All Employee IDs are same and unique in both datasets.

employee <- cbind(employee, Leaves, Tot_Hrs, Avg_Hrs)

str(employee)
sum(duplicated(employee))
# No duplicate rows

# Checking no. of levels in variables
level_counter <- function(my_vect){length(levels(factor(my_vect)))}

sapply(employee, level_counter)
# Some columns contain only 1 factor (i.e. they have same value in all rows)

# All such columns are useless for analysis, thus removing them.
employee <- employee[, sapply(employee, level_counter) > 1]


################# 5. MISSING VALUE TREATMENT #################

sum(is.na(employee))
# 111 NA's in the dataset

sapply(employee, function(x) sum(is.na(x)))
# There are NAs in 5 columns

NA_employee <- employee[which(rowSums(is.na(employee)) > 0),]
View(NA_employee)
# NA's are in random occurance. Cannot be imputed by logic.

nrow(NA_employee)/ nrow(employee)
# There are only 2.5% observations containing NA's.

# So, rather than hypothetical imputation, it is better to drop rows with NAs.
employee <- na.omit(employee)


################# 6. DERIVING NEW METRICS #################

# Years worked outside this company
employee$YearsOutside <- employee$TotalWorkingYears - employee$YearsAtCompany

# Years worked with other than current manager within this company
employee$YearsWithOtherManager <- employee$YearsAtCompany - employee$YearsWithCurrManager


################# 7. DATA MANIPULATION #################

summary(employee)
sapply(employee, level_counter)

# We see that following are clearly categorical variables

# "Attrition"               "BusinessTravel"          "Department"             
# "Education"               "EducationField"          "Gender"                 
# "JobLevel"                "JobRole"                 "MaritalStatus"          
# "StockOptionLevel"        "EnvironmentSatisfaction" "JobSatisfaction"        
# "WorkLifeBalance"         "JobInvolvement"          "PerformanceRating"    

# We should see "NumCompaniesWorked" in detail.
summary(factor(employee$NumCompaniesWorked))

# We see that NumCompaniesWorked = 0 in 570 rows
# An employee cannot work in "0" companies

# Checking details
head(employee[which(employee$NumCompaniesWorked == 0),c(1,14,17,19,30)])

levels(factor(employee$YearsOutside[which(employee$NumCompaniesWorked == 0)]))
# We see that for all rows with NumCompaniesWorked = 0, Total Years - Years At Company = 1.

# It seems that NumCompaniesWorked "0" represents some category of the employees.

# For rows with Years - Years At Company = 0, NumCompaniesWorked should be 1.
summary(factor(employee$NumCompaniesWorked[which(employee$YearsOutside == 1)]))

# Total Yrs - Yrs At Company = 1 for both NumCompaniesWorked 0 & 1 in 570 & 141 rows respectively.

# It is evident that NumCompaniesWorked "0" and "1" represent some sort of categorization.
# But, we cannot impute numeric values due to lack of information on categorization policy.
# Thus, we will convert NumCompaniesWorked into categorical variable "Attitude.

employee$Attitude[which(employee$NumCompaniesWorked == 0)] <- "Specific_Category"
employee$Attitude[which(employee$NumCompaniesWorked == 1)] <- "Loyal_Employee"
employee$Attitude[which(employee$NumCompaniesWorked >= 2 & employee$NumCompaniesWorked < 4)] <- "Norm_Category"
employee$Attitude[which(employee$NumCompaniesWorked >= 4 & employee$NumCompaniesWorked < 6)] <- "Frequent_Switcher"
employee$Attitude[which(employee$NumCompaniesWorked >= 6)] <- "Job_Hopper"

summary(factor(employee$Attitude))

employee$NumCompaniesWorked <- NULL

# Converting categorical variables to factor
fact_employee <- data.frame(sapply(employee[, c(3:5, 7:12, 15, 21:25, 31)], factor))
str(fact_employee)

# Creating dummy variables from factors
dummy_df <- data.frame(sapply(fact_employee, 
            function(x) data.frame(model.matrix(~x, data = fact_employee))[,-1]))

summary(factor(dummy_df$Attrition))
# Attrition which was "Yes" has become = 1 and "No" has become = 0

sum(dummy_df$Attrition) / nrow(dummy_df)
# 16% employees left the company

# Rest are continuous variables
cont_employee <- employee[!colnames(employee) %in% colnames(fact_employee)]


################# 8. PLOTTING AND VISUALIZATION #################

library(corrplot)

cormat <- cor(cont_employee[,-1])

corrplot(cormat, method = "number", order = "hclust", addrect = 4)
# There are many variables having high correlation

library(ggplot2)
library(cowplot)

emp_plot_1 <- ggplot(employee, aes(fill = Attrition)) + ylab("No of Employees") + 
              theme(axis.text.x = element_text(angle = 45, hjust = 1))

emp_plot_2 <- ggplot(employee, aes(fill = Attrition)) + ylab("Attrition Percentage") + 
              theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 1. personal and behaviour related variables
plot_grid(emp_plot_1 + geom_histogram(aes(x = Age), binwidth = 7, col="black"),
          emp_plot_1 + geom_bar(aes(x = MaritalStatus), col="black"),
          emp_plot_1 + geom_bar(aes(x = factor(Attitude)), col="black"),
          emp_plot_2 + geom_histogram(aes(x = Age), binwidth = 7, col="black", position = "fill"),
          emp_plot_2 + geom_bar(aes(x = MaritalStatus), col="black", position = "fill"),
          emp_plot_2 + geom_bar(aes(x = Attitude), col="black", position = "fill"),
          align = "h")

# 1. We see that younger emplyees are most prone to attrition
# 2. Singles most prone to attrition
# 3. Job Hoppers have max. attrition as expected, but surprisingly, 2nd highest are the loyal ones.

# Plot 2. company and Job profile related variables (I)
plot_grid(emp_plot_1 + geom_bar(aes(x = Department), col="black"),
          emp_plot_1 + geom_bar(aes(x = EducationField), col="black"),
          emp_plot_1 + geom_bar(aes(x = BusinessTravel), col="black"),
          emp_plot_2 + geom_bar(aes(x = Department), col="black", position = "fill"),
          emp_plot_2 + geom_bar(aes(x = EducationField), col="black", position = "fill"),
          emp_plot_2 + geom_bar(aes(x = BusinessTravel), col="black", position = "fill"),
          align = "h")

# 1. Ironically, we finad HR department suffers max. attrition
# 2. Again we substantiate that employees with HR education make max. attrition
# 3. We see that as extent of travelling increases, ratio of attrition also increase

# Plot 3. Variables intuitively seeming important
plot_grid(emp_plot_2 + geom_histogram(aes(x = DistanceFromHome), binwidth = 5, col="black", position = "fill"),
          emp_plot_2 + geom_bar(aes(x = Education), col="black", position = "fill"),
          emp_plot_2 + geom_bar(aes(x = Gender), col="black", position = "fill"),
          emp_plot_2 + geom_bar(aes(x = JobLevel), col="black", position = "fill"),
          emp_plot_2 + geom_bar(aes(x = JobRole), col="black", position = "fill"),
          emp_plot_2 + geom_histogram(aes(x = MonthlyIncome), binwidth = 10000, col="black", position = "fill"),
          emp_plot_2 + geom_bar(aes(x = StockOptionLevel), col="black", position = "fill"),
          emp_plot_2 + geom_bar(aes(x = TrainingTimesLastYear), col="black", position = "fill"),
          emp_plot_2 + geom_histogram(aes(x = YearsSinceLastPromotion), binwidth = 2, col="black", position = "fill"),
          align = "h")

# Surprisingly, many variables which seem to be reason of attrition intuitively,
# are showing no patterns (and thus, perhaps are not factors of attrition).
# e.g. - DistanceFromHome, JobLevel, JobRole, MonthlyIncome and YearsSinceLastPromotion.

# Plot 4. company and Job profile related variables (II)
plot_grid(emp_plot_1 + geom_histogram(aes(x = PercentSalaryHike), binwidth = 3, col="black"),
          emp_plot_1 + geom_histogram(aes(x = TotalWorkingYears), binwidth = 10, col="black"),
          emp_plot_1 + geom_histogram(aes(x = YearsWithCurrManager), binwidth = 5, col="black"),
          emp_plot_2 + geom_histogram(aes(x = PercentSalaryHike), binwidth = 3, col="black", position = "fill"),
          emp_plot_2 + geom_histogram(aes(x = TotalWorkingYears), binwidth = 10, col="black", position = "fill"),
          emp_plot_2 + geom_histogram(aes(x = YearsWithCurrManager), binwidth = 5, col="black", position = "fill"),
          align = "h")

# 1. Interesting observation that more the salary hike last year, more happens attrition
# 2. More work experienced employees, in general, have lesser tendency to leave. 
# 3. As intuitive, employees spending more years with their current manager,
#    tend to have good tuning with each other, and thus, keep working with them. 

# Plot 5. Employee Survey Ratings
plot_grid(emp_plot_1 + geom_bar(aes(x = EnvironmentSatisfaction), col="black"),
          emp_plot_1 + geom_bar(aes(x = JobSatisfaction), col="black"),
          emp_plot_1 + geom_bar(aes(x = WorkLifeBalance), col="black"),
          emp_plot_2 + geom_bar(aes(x = EnvironmentSatisfaction), col="black", position = "fill"),
          emp_plot_2 + geom_bar(aes(x = JobSatisfaction), col="black", position = "fill"),
          emp_plot_2 + geom_bar(aes(x = WorkLifeBalance), col="black", position = "fill"),
          align = "h")

# Poorer the ratings of EnvironmentSatisfaction, JobSatisfaction and WorkLifeBalance,
# more is the likelyhood of employee's attrition.

# Plot 6. Manager's impact
plot_grid(emp_plot_1 + geom_bar(aes(x = JobInvolvement), col="black") + facet_grid(~PerformanceRating),
          emp_plot_1 + geom_histogram(aes(x = Leaves), binwidth = 7, col="black"),
          emp_plot_1 + geom_histogram(aes(x = Avg_Hrs), binwidth = 1.5, col="black"),
          emp_plot_2 + geom_bar(aes(x = JobInvolvement), col="black", position = "fill") + facet_grid(~PerformanceRating),
          emp_plot_2 + geom_histogram(aes(x = Leaves), binwidth = 7, col="black", position = "fill"),
          emp_plot_2 + geom_histogram(aes(x = Avg_Hrs), binwidth = 1.5, col="black", position = "fill"))

# 1. Manager's ratings have random pattern.
#    But, employees getting Job Involvement rating "1" are most likely to leave. 
# 2. Employees granted more leaves by their manager are more likely to stay.
# 3. On the other hand, employees being worked for more than 8 daily hours are more prone to attrition.


################# 9. OUTLIERS TREATMENT #################

OTL <- data.frame(sapply(cont_employee[,-1], function(x) quantile(x,seq(0,1,.01))))
summary(OTL)

# Creating a custom function which caps the outliers of a numeric vector.
outlier_capper <- function(vect) {
                  out <- boxplot.stats(vect)$out
                  non_out <- vect[which(!vect %in% out)]
                  vect <- ifelse(vect > max(non_out), max(non_out), vect)
                  vect <- ifelse(vect < min(non_out), min(non_out), vect)
                  return(vect)}

# Checking and capping outliers in all continuous variables
new_cont_employee <- data.frame(sapply(cont_employee, outlier_capper))

No_OTL <- data.frame(sapply(new_cont_employee[,-1], function(x) quantile(x,seq(0,1,.01))))
summary(No_OTL)

# Outliers removed

# Scaling continuous variables
scale_employee <- data.frame(sapply(new_cont_employee[, -1], scale))

final_employee <- cbind(dummy_df, scale_employee)

str(final_employee)
summary(final_employee)
# Dataset is ready for model building


################# 10. MODEL BUILDING #################

library(caTools)

# Creating training and testing datasets
set.seed(100)
indices <- sample.split(final_employee, SplitRatio = 0.7)
train <- final_employee[indices,]
test <- final_employee[!(indices),]

#  Building 1st model with all 62 independent variables
model_1 <- glm(formula = Attrition~.,  data = train, family = "binomial")
summary(model_1)
# AIC = 2056

# Runnning stepAIC
library(MASS)
model_2 <- stepAIC(model_1, direction = "both")

summary(model_2)
# AIC = 2021 (considerably reduced)
# Independent variables reduced to 40

library(car)
sort(vif(model_2), decreasing = T)
# YearsAtCompany has highest VIF and also insignificant. Removing it.

model_3 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + Education.x2 + EducationField.xMarketing + 
                 EducationField.xTechnical.Degree + Gender + JobLevel.x2 + 
                 JobLevel.x5 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 StockOptionLevel.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3 + Attitude.xLoyal_Employee + 
                 Attitude.xNorm_Category + Attitude.xSpecific_Category + Age + 
                 MonthlyIncome + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + Tot_Hrs + 
                 YearsOutside + YearsWithOtherManager, family = "binomial", 
                 data = train)

summary(model_3)
# AIC = 2022 (Negligible increase by 1)

sort(vif(model_3), decreasing = T)
# BusinessTravel.xTravel_Rarely is least significant with VIF ~4. Removing it.

model_4 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 Department.xSales + Education.x2 + EducationField.xMarketing + 
                 EducationField.xTechnical.Degree + Gender + JobLevel.x2 + 
                 JobLevel.x5 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 StockOptionLevel.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3 + Attitude.xLoyal_Employee + 
                 Attitude.xNorm_Category + Attitude.xSpecific_Category + Age + 
                 MonthlyIncome + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + Tot_Hrs + YearsOutside + YearsWithOtherManager, 
                 family = "binomial", data = train)

summary(model_4)
# AIC = 2032 (Slight increase by 10)

sort(vif(model_4), decreasing = T)
# All high VIF's are significant

# Checking correlation of Department.xResearch...Development and Department.xSales
cor(train$Department.xResearch...Development, train$Department.xSales)

# Very high corelation
# Checking AIC by removing them one by one.

# Removing Department.xResearch...Development

model_5.1 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                   Department.xSales + 
                   Education.x2 + EducationField.xMarketing + EducationField.xTechnical.Degree + 
                   Gender + JobLevel.x2 + JobLevel.x5 + JobRole.xLaboratory.Technician + 
                   JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                   JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                   StockOptionLevel.x1 + StockOptionLevel.x3 + EnvironmentSatisfaction.x2 + 
                   EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                   JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                   WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                   JobInvolvement.x3 + Attitude.xLoyal_Employee + Attitude.xNorm_Category + 
                   Attitude.xSpecific_Category + Age + MonthlyIncome + TrainingTimesLastYear + 
                   YearsSinceLastPromotion + YearsWithCurrManager + Tot_Hrs + 
                   YearsOutside + YearsWithOtherManager, family = "binomial", 
                   data = train)

summary(model_5.1)
# AIC = 2057 (Increase by 25)

# Removing Department.xSales
model_5.2 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                   Department.xResearch...Development + 
                   Education.x2 + EducationField.xMarketing + EducationField.xTechnical.Degree + 
                   Gender + JobLevel.x2 + JobLevel.x5 + JobRole.xLaboratory.Technician + 
                   JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                   JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                   StockOptionLevel.x1 + StockOptionLevel.x3 + EnvironmentSatisfaction.x2 + 
                   EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                   JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                   WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                   JobInvolvement.x3 + Attitude.xLoyal_Employee + Attitude.xNorm_Category + 
                   Attitude.xSpecific_Category + Age + MonthlyIncome + TrainingTimesLastYear + 
                   YearsSinceLastPromotion + YearsWithCurrManager + Tot_Hrs + 
                   YearsOutside + YearsWithOtherManager, family = "binomial", 
                   data = train)

summary(model_5.2)
# AIC = 2051 (Increase by 19)
# Proceeding with model_5.2 because of lesser change in AIC.

sort(vif(model_5.2), decreasing = T)
# All high VIF's are significant

# Checking correlation of WorkLifeBalance.x3 and WorkLifeBalance.x3
cor(train$WorkLifeBalance.x3, train$WorkLifeBalance.x2)

# Very high corelation
# Checking AIC by removing them one by one.

# Removing WorkLifeBalance.x3

model_6.1 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                   Department.xResearch...Development + Education.x2 + EducationField.xMarketing + 
                   EducationField.xTechnical.Degree + Gender + JobLevel.x2 + 
                   JobLevel.x5 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                   JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                   MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                   StockOptionLevel.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                   EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                   JobSatisfaction.x4 + WorkLifeBalance.x2 + 
                   WorkLifeBalance.x4 + JobInvolvement.x3 + Attitude.xLoyal_Employee + 
                   Attitude.xNorm_Category + Attitude.xSpecific_Category + Age + 
                   MonthlyIncome + TrainingTimesLastYear + YearsSinceLastPromotion + 
                   YearsWithCurrManager + Tot_Hrs + YearsOutside + YearsWithOtherManager, 
                   family = "binomial", data = train)

summary(model_6.1)
# AIC = 2102 (Increase by 51)

# Removing WorkLifeBalance.x2 

model_6.2 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                   Department.xResearch...Development + Education.x2 + EducationField.xMarketing + 
                   EducationField.xTechnical.Degree + Gender + JobLevel.x2 + 
                   JobLevel.x5 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                   JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                   MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                   StockOptionLevel.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                   EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                   JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                   WorkLifeBalance.x4 + JobInvolvement.x3 + Attitude.xLoyal_Employee + 
                   Attitude.xNorm_Category + Attitude.xSpecific_Category + Age + 
                   MonthlyIncome + TrainingTimesLastYear + YearsSinceLastPromotion + 
                   YearsWithCurrManager + Tot_Hrs + YearsOutside + YearsWithOtherManager, 
                   family = "binomial", data = train)

summary(model_6.2)
# AIC = 2078 (Increase by 27)
# Proceeding with model_6.2 because of lesser change in AIC.

sort(vif(model_6.2), decreasing = T)
# MaritalStatus.xMarried least significant with VIF > 2

model_7 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + Education.x2 + EducationField.xMarketing + 
                 EducationField.xTechnical.Degree + Gender + JobLevel.x2 + 
                 JobLevel.x5 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 StockOptionLevel.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 JobInvolvement.x3 + Attitude.xLoyal_Employee + Attitude.xNorm_Category + 
                 Attitude.xSpecific_Category + Age + MonthlyIncome + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + Tot_Hrs + 
                 YearsOutside + YearsWithOtherManager, family = "binomial", 
                 data = train)

summary(model_7)
# AIC = 2080 (Negligible increase by 2)

sort(vif(model_7), decreasing = T)
# All high VIF's are significant

# Checking correlation of WorkLifeBalance.x3 and WorkLifeBalance.x3
cor(train$YearsOutside, train$Attitude.xLoyal_Employee)

# High correlation
# Checking AIC by removing them one by one.

# Removing YearsOutside

model_8.1 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                   Department.xResearch...Development + Education.x2 + EducationField.xMarketing + 
                   EducationField.xTechnical.Degree + Gender + JobLevel.x2 + 
                   JobLevel.x5 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                   JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                   MaritalStatus.xSingle + StockOptionLevel.x1 + StockOptionLevel.x3 + 
                   EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                   EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                   JobSatisfaction.x4 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                   JobInvolvement.x3 + Attitude.xLoyal_Employee + Attitude.xNorm_Category + 
                   Attitude.xSpecific_Category + Age + MonthlyIncome + TrainingTimesLastYear + 
                   YearsSinceLastPromotion + YearsWithCurrManager + Tot_Hrs + 
                   YearsWithOtherManager, family = "binomial", 
                   data = train)

summary(model_8.1)
# AIC = 2107 (Increase by 27)

# Removing Attitude.xLoyal_Employee
model_8.2 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                   Department.xResearch...Development + Education.x2 + EducationField.xMarketing + 
                   EducationField.xTechnical.Degree + Gender + JobLevel.x2 + 
                   JobLevel.x5 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                   JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                   MaritalStatus.xSingle + StockOptionLevel.x1 + StockOptionLevel.x3 + 
                   EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                   EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                   JobSatisfaction.x4 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                   JobInvolvement.x3 + Attitude.xNorm_Category + 
                   Attitude.xSpecific_Category + Age + MonthlyIncome + TrainingTimesLastYear + 
                   YearsSinceLastPromotion + YearsWithCurrManager + Tot_Hrs + 
                   YearsOutside + YearsWithOtherManager, family = "binomial", 
                   data = train)

summary(model_8.2)
# AIC = 2097 (Increase by 17)
# Proceeding with model_8.2 because of lesser change in AIC.

sort(vif(model_8.2), decreasing = T)
# Only YearsOutside has VIF > 2, but is highly significant.

# So now on, removing variables as per p-value.

# Gender has highest p-value. Removing it.
model_9 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + Education.x2 + EducationField.xMarketing + 
                 EducationField.xTechnical.Degree + JobLevel.x2 + 
                 JobLevel.x5 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + StockOptionLevel.x3 + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 JobInvolvement.x3 + Attitude.xNorm_Category + Attitude.xSpecific_Category + 
                 Age + MonthlyIncome + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + Tot_Hrs + YearsOutside + YearsWithOtherManager, 
                 family = "binomial", data = train)

summary(model_9)
# AIC = 2097 (No change)

# WorkLifeBalance.x4 has highest p-value. Removing it.
model_10 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + Education.x2 + EducationField.xMarketing + 
                  EducationField.xTechnical.Degree + JobLevel.x2 + 
                  JobLevel.x5 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + StockOptionLevel.x1 + StockOptionLevel.x3 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                  JobInvolvement.x3 + Attitude.xNorm_Category + Attitude.xSpecific_Category + 
                  Age + MonthlyIncome + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Tot_Hrs + YearsOutside + YearsWithOtherManager, 
                  family = "binomial", data = train)

summary(model_10)
# AIC = 2096 (Negligible decrease by 1)

# JobLevel.x5 has highest p-value. Removing it.
model_11 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + Education.x2 + EducationField.xMarketing + 
                  EducationField.xTechnical.Degree + JobLevel.x2 + 
                  JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + StockOptionLevel.x1 + StockOptionLevel.x3 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                  JobInvolvement.x3 + Attitude.xNorm_Category + Attitude.xSpecific_Category + 
                  Age + MonthlyIncome + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Tot_Hrs + YearsOutside + YearsWithOtherManager, 
                  family = "binomial", data = train)

summary(model_11)
# AIC = 2096 (No change)

# StockOptionLevel.x3 has highest p-value. Removing it.
model_12 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + Education.x2 + EducationField.xMarketing + 
                  EducationField.xTechnical.Degree + JobLevel.x2 + 
                  JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + StockOptionLevel.x1 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                  JobInvolvement.x3 + Attitude.xNorm_Category + Attitude.xSpecific_Category + 
                  Age + MonthlyIncome + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Tot_Hrs + YearsOutside + YearsWithOtherManager, 
                  family = "binomial", data = train)

summary(model_12)
# AIC = 2096 (No change)

# StockOptionLevel.x1 has highest p-value. Removing it.
model_13 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + Education.x2 + EducationField.xMarketing + 
                  EducationField.xTechnical.Degree + JobLevel.x2 + 
                  JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                  JobInvolvement.x3 + Attitude.xNorm_Category + Attitude.xSpecific_Category + 
                  Age + MonthlyIncome + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Tot_Hrs + YearsOutside + YearsWithOtherManager, 
                  family = "binomial", data = train)

summary(model_13)
# AIC = 2097 (Negligible increase by 1)

# Education.x2 has highest p-value. Removing it. 
model_14 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + EducationField.xMarketing + 
                  EducationField.xTechnical.Degree + JobLevel.x2 + JobRole.xLaboratory.Technician + 
                  JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x3 + JobInvolvement.x3 + Attitude.xNorm_Category + 
                  Attitude.xSpecific_Category + Age + MonthlyIncome + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + Tot_Hrs + 
                  YearsOutside + YearsWithOtherManager, family = "binomial", 
                  data = train)

summary(model_14)
# AIC = 2097 (No change)

# MonthlyIncome has highest p-value. Removing it.
model_15 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + EducationField.xMarketing + 
                  EducationField.xTechnical.Degree + JobLevel.x2 + JobRole.xLaboratory.Technician + 
                  JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x3 + JobInvolvement.x3 + Attitude.xNorm_Category + 
                  Attitude.xSpecific_Category + Age + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + Tot_Hrs + 
                  YearsOutside + YearsWithOtherManager, family = "binomial", 
                  data = train)

summary(model_15)
# AIC = 2097 (No change)

# EducationField.xTechnical.Degree has highest p-value. Removing it.
model_16 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + EducationField.xMarketing + 
                  JobLevel.x2 + JobRole.xLaboratory.Technician + 
                  JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x3 + JobInvolvement.x3 + Attitude.xNorm_Category + 
                  Attitude.xSpecific_Category + Age + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + Tot_Hrs + 
                  YearsOutside + YearsWithOtherManager, family = "binomial", 
                  data = train)

summary(model_16)
# AIC = 2098 (Negligible increase by 1)

# EducationField.xMarketing has highest p-value. Removing it.
model_17 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + 
                  JobLevel.x2 + JobRole.xLaboratory.Technician + 
                  JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x3 + JobInvolvement.x3 + Attitude.xNorm_Category + 
                  Attitude.xSpecific_Category + Age + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + Tot_Hrs + 
                  YearsOutside + YearsWithOtherManager, family = "binomial", 
                  data = train)

summary(model_17)
# AIC = 2100 (Negligible increase by 2)

# Department.xResearch...Development has highest p-value. Removing it.
model_18 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                  JobLevel.x2 + JobRole.xLaboratory.Technician + 
                  JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x3 + JobInvolvement.x3 + Attitude.xNorm_Category + 
                  Attitude.xSpecific_Category + Age + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + Tot_Hrs + 
                  YearsOutside + YearsWithOtherManager, family = "binomial", 
                  data = train)

summary(model_18)
# AIC = 2100 (No change)

# JobLevel.x2 has highest p-value. Removing it.
model_19 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                  JobRole.xLaboratory.Technician + 
                  JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x3 + JobInvolvement.x3 + Attitude.xNorm_Category + 
                  Attitude.xSpecific_Category + Age + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + Tot_Hrs + 
                  YearsOutside + YearsWithOtherManager, family = "binomial", 
                  data = train)

summary(model_19)
# AIC = 2102 (Negligible increase by 2)

# Age has highest p-value. Removing it.
model_20 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                  JobRole.xLaboratory.Technician + 
                  JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x3 + JobInvolvement.x3 + Attitude.xNorm_Category + 
                  Attitude.xSpecific_Category + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + Tot_Hrs + 
                  YearsOutside + YearsWithOtherManager, family = "binomial", 
                  data = train)

summary(model_20)
# AIC = 2105 (Negligible increase by 3)

# TrainingTimesLastYear has highest p-value. Removing it. 
model_21 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                  JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + JobInvolvement.x3 + 
                  Attitude.xNorm_Category + Attitude.xSpecific_Category + 
                  YearsSinceLastPromotion + YearsWithCurrManager + Tot_Hrs + 
                  YearsOutside + YearsWithOtherManager, family = "binomial", 
                  data = train)

summary(model_21)
# AIC = 2109 (Negligible increase by 4)

# JobInvolvement.x3 has highest p-value. Removing it.
model_22 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                  JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                  Attitude.xNorm_Category + Attitude.xSpecific_Category + 
                  YearsSinceLastPromotion + YearsWithCurrManager + Tot_Hrs + 
                  YearsOutside + YearsWithOtherManager, family = "binomial", 
                  data = train)

summary(model_22)
# AIC = 2113 (Negligible increase by 4)

# Attitude.xNorm_Category has highest p-value. Removing it.
model_23 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                  JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                  Attitude.xSpecific_Category + 
                  YearsSinceLastPromotion + YearsWithCurrManager + Tot_Hrs + 
                  YearsOutside + YearsWithOtherManager, family = "binomial", 
                  data = train)

summary(model_23)
# AIC = 2119 (Slight increase by 6)

# JobRole.xSales.Executive has highest p-value. Removing it.
model_24 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                  JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + Attitude.xSpecific_Category + 
                  YearsSinceLastPromotion + YearsWithCurrManager + Tot_Hrs + 
                  YearsOutside + YearsWithOtherManager, family = "binomial", 
                  data = train)

summary(model_24)
# AIC = 2125 (Slight increase by 6)

# JobRole.xLaboratory.Technician has highest p-value. Removing it.
model_25 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                  JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + Attitude.xSpecific_Category + 
                  YearsSinceLastPromotion + YearsWithCurrManager + Tot_Hrs + 
                  YearsOutside + YearsWithOtherManager, family = "binomial", 
                  data = train)

summary(model_25)
# AIC = 2130 (Slight increase by 5)

# JobRole.xResearch.Scientist has highest p-value. Removing it.
model_26 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                  JobRole.xResearch.Director + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + Attitude.xSpecific_Category + 
                  YearsSinceLastPromotion + YearsWithCurrManager + Tot_Hrs + 
                  YearsOutside + YearsWithOtherManager, family = "binomial", 
                  data = train)

summary(model_26)
# AIC = 2132 (Negligible increase by 2)

# JobRole.xResearch.Director has highest p-value. Removing it.
model_27 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + Attitude.xSpecific_Category + 
                  YearsSinceLastPromotion + YearsWithCurrManager + Tot_Hrs + 
                  YearsOutside + YearsWithOtherManager, family = "binomial", 
                  data = train)

summary(model_27)
# AIC = 2137 (Slight increase by 5)

# JobSatisfaction.x2 + 
model_28 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + Attitude.xSpecific_Category + 
                  YearsSinceLastPromotion + YearsWithCurrManager + Tot_Hrs + 
                  YearsOutside + YearsWithOtherManager, family = "binomial", 
                  data = train)

summary(model_28)
# AIC = 2147 (Slight increase by 10)

# Attitude.xSpecific_Category has highest p-value. Removing it.
model_29 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                  YearsSinceLastPromotion + YearsWithCurrManager + Tot_Hrs + 
                  YearsOutside + YearsWithOtherManager, family = "binomial", 
                  data = train)

summary(model_29)
# AIC = 2158 (Slight increase by 11)

# JobSatisfaction.x3 has highest p-value. Removing it.
model_30 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                  YearsSinceLastPromotion + YearsWithCurrManager + Tot_Hrs + 
                  YearsOutside + YearsWithOtherManager, family = "binomial", 
                  data = train)

summary(model_30)
# AIC = 2170 (Decent increase by 12)

sort(vif(model_30), decreasing = T)

# All variables have VIF < 1
# All variables have p-value < 0.0001

final_model <- model_30
# model_30 is our final model with 12 highly significant variables viz.

# YearsSinceLastPromotion        EnvironmentSatisfaction.x4       YearsWithCurrManager 
# EnvironmentSatisfaction.x3     EnvironmentSatisfaction.x2       YearsWithOtherManager 
# YearsOutside                   Tot_Hrs                          MaritalStatus.xSingle 
# JobSatisfaction.x4             WorkLifeBalance.x3               BusinessTravel.xTravel_Frequently 


################# 11. MODEL EVALUATION #################

prediction <- predict(final_model, type = "response", newdata = test[,-1])

# Let's see the summary 
summary(prediction)
actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))

# Calculating the optimal probalility cutoff.

s <- seq(0.01, 0.85, length=100)
OUT <- matrix(0,100,3)
colnames(OUT) <- c("Sensitivity", "Specificity", "Accuracy")

library(caret)

perform_fn <- function(cutoff)
              { predicted_attrition <- factor(ifelse(prediction >= cutoff, "Yes", "No"))
                conf <- confusionMatrix(predicted_attrition, actual_attrition, positive = "Yes")
                acc <- conf$overall[1]
                sens <- conf$byClass[1]
                spec <- conf$byClass[2]
                out <- c(sens, spec, acc) 
                return(out)}

for(i in 1:100) {OUT[i,] <- perform_fn(s[i])}

cutoff <- s[which(abs(OUT[,1]-OUT[,2]) == min(abs(OUT[,1]-OUT[,2])))]
cutoff

# Thus, cutoff value of predicted probability should be 15.42 %

predicted_attrition <- factor(ifelse(prediction >= 0.1542, "Yes", "No"))

con_mat <- confusionMatrix(predicted_attrition, actual_attrition, positive = "Yes")
con_mat

# Thus, our final model is 71% accurate, 71% sensitive and 71% specific.
# with probability cutoff = 15.42 %

################################################################################
