# Load in csv file
dataFile <- "data/CaseStudy2data.csv"
hr_employee_attrition <- read.csv(dataFile)

# Use correlation plot to see which continuous variables are
# highly correlated and need to be focused on
Numeric_Vars <- hr_employee_attrition[,sapply(hr_employee_attrition, is.integer)]

library(dplyr)

Numeric_Vars <- hr_employee_attrition %>%
  select(ï..Age, DailyRate, DistanceFromHome, HourlyRate, MonthlyIncome, MonthlyRate, 
         NumCompaniesWorked, PercentSalaryHike, YearsAtCompany, YearsInCurrentRole,
         YearsSinceLastPromotion, YearsWithCurrManager, TotalWorkingYears,
         TrainingTimesLastYear, StockOptionLevel)

colnames(Numeric_Vars)

library("corrplot")

corrplot(cor(Numeric_Vars), method = "number", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# Use corrplot as starting point / guideline for looking at
# several variables against attrition to see if there is a trend
library(ggplot2)
library(gridExtra)
theme_set(theme_light())

Yrs_Since_Prom <- ggplot(hr_employee_attrition, aes(x = Attrition, y = YearsSinceLastPromotion)) +
                  geom_boxplot() + coord_flip()

Yrs_In_Role <- ggplot(hr_employee_attrition, aes(x = Attrition, y = YearsInCurrentRole)) +
               geom_boxplot() + coord_flip()

Yrs_At_Comp <- ggplot(hr_employee_attrition, aes(x = Attrition, y = YearsAtCompany)) +
               geom_boxplot() + coord_flip()

Yrs_With_Manager <- ggplot(hr_employee_attrition, aes(x = Attrition, y = YearsWithCurrManager)) +
                    geom_boxplot() + coord_flip()

Total_working_Years <- ggplot(hr_employee_attrition, aes(x = Attrition, y = TotalWorkingYears)) +
                       geom_boxplot() + coord_flip()

Age <- ggplot(hr_employee_attrition, aes(x = Attrition, y = ï..Age)) +
       geom_boxplot() + coord_flip()

grid.arrange(Yrs_Since_Prom, Yrs_In_Role,
             Yrs_At_Comp, Yrs_With_Manager,
             Total_working_Years, Age,
             nrow = 3)

# Go through the list of variables
Business_Travel <- ggplot(hr_employee_attrition, aes(x = BusinessTravel, fill = Attrition)) +
                   geom_bar(position = "fill") + coord_flip()

Department <- ggplot(hr_employee_attrition, aes(x = Department, fill = Attrition)) +
              geom_bar(position = "fill") + coord_flip()

Education_Field <- ggplot(hr_employee_attrition, aes(x = EducationField, fill = Attrition)) +
                   geom_bar(position = "fill") + coord_flip()

Gender <- ggplot(hr_employee_attrition, aes(x = Gender, fill = Attrition)) +
          geom_bar(position = "fill") + coord_flip()

Marital_Status <- ggplot(hr_employee_attrition, aes(x = MaritalStatus, fill = Attrition)) +
                  geom_bar(position = "fill") + coord_flip()

Over_Time <- ggplot(hr_employee_attrition, aes(x = OverTime, fill = Attrition)) +
             geom_bar(position = "fill") + coord_flip()

grid.arrange(Business_Travel, Department,
             Education_Field, Gender,
             Marital_Status, Over_Time,
             nrow = 3)

Hourly_Rate <- ggplot(hr_employee_attrition, aes(x = Attrition, y = HourlyRate)) +
               geom_boxplot() + coord_flip()

Daily_Rate <- ggplot(hr_employee_attrition, aes(x = Attrition, y = DailyRate)) +
              geom_boxplot() + coord_flip()

Monthly_Rate <- ggplot(hr_employee_attrition, aes(x = Attrition, y = MonthlyRate)) +
                geom_boxplot() + coord_flip()

Monthly_Income <- ggplot(hr_employee_attrition, aes(x = Attrition, y = log10(MonthlyIncome))) +
                  geom_boxplot() + coord_flip()

Perc_Salary_Hike <- ggplot(hr_employee_attrition, aes(x = Attrition, y = PercentSalaryHike)) +
                    geom_boxplot() + coord_flip()

grid.arrange(Hourly_Rate, Daily_Rate,
             Monthly_Rate, Monthly_Income,
             Perc_Salary_Hike,
             nrow = 3)

Distance_Home_BarPlot <- ggplot(hr_employee_attrition, aes(x = Attrition, y = DistanceFromHome)) +
                         geom_boxplot() + coord_flip()

Distance_Home_DensityPlot <- ggplot(hr_employee_attrition, aes(x = DistanceFromHome, color = Attrition)) +
                             geom_density()

grid.arrange(Distance_Home_BarPlot,
             Distance_Home_DensityPlot,
             nrow = 2)

# Look into education variable. Need to rename numerical factor
# levels to the labels provided under the data definition tab
library(forcats)

Education_Level <- hr_employee_attrition %>% 
  mutate(Education = factor(Education)) %>% 
  mutate(Education = fct_recode(Education, 
                                "Below College" = "1", 
                                "College" = "2", 
                                "Bachelor" = "3", 
                                "Master" = "4", 
                                "Doctor" = "5"))
library(RColorBrewer)
theme_set(theme_light())

ggplot(Education_Level, aes(Education, fill = Attrition)) + 
  geom_bar(stat = "count", aes(y = ..count..), position = position_dodge()) +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5, size = 16), axis.text.x = element_text(angle=90)) + 
  labs(x = "Education Level", y = "Count", title = "Trend of Attrition with Education Level")

ggplot(Education_Level, aes(Education, fill = Attrition)) + 
  geom_bar(stat = "count", aes(y = ..count..), position = position_dodge()) +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, size = 16), axis.text.x = element_text(angle=90)) + 
  labs(x = "Education Level", y = "Count", title = "Education levels and field of education") +
  facet_grid(~ EducationField)

# Compare attrition between different levels of job satisfaction
Job_Satisfaction_Level <- hr_employee_attrition %>% 
  mutate(JobSatisfaction = factor(JobSatisfaction)) %>% 
  mutate(JobSatisfaction = fct_recode(JobSatisfaction,
                                      "Low" = "1",
                                      "Medium" = "2",
                                      "High" = "3",
                                      "Very High" = "4"))

ggplot(Job_Satisfaction_Level, aes(fill = Attrition, x = JobSatisfaction)) + 
  geom_bar(position = 'fill')
  
ggplot(Job_Satisfaction_Level, aes(x = JobSatisfaction, group = Attrition)) + 
  geom_bar(stat = "count", aes(y = ..prop.., fill = factor(..x..))) +
  labs(x = "Job Satisfaction", y = "Percentage", title = "Job Satisfaction Vs Attrition Rates") + 
  facet_wrap(~Attrition) +
  theme(legend.position = "none", plot.title = element_text(hjust=0.5, size=14)) + 
  geom_text(aes(label = scales::percent(..prop..), y= ..prop..), stat = "count", vjust = -0.5)

# Compare job satisfaction among education levels between attrition
# or not attrition groups  
Education_Level %>%
  mutate(JobSatisfaction = factor(JobSatisfaction)) %>% 
  mutate(JobSatisfaction= fct_recode(JobSatisfaction,
                                     "Low" = "1",
                                     "Medium" = "2",
                                     "High" = "3",
                                     "Very High" = "4")) %>%
  ggplot(aes(Education, fill = JobSatisfaction)) + 
  geom_bar(position = 'fill') +
  facet_wrap(~ Attrition)
  
Education_Level %>%
  mutate(JobSatisfaction = factor(JobSatisfaction)) %>% 
  mutate(JobSatisfaction= fct_recode(JobSatisfaction,
                                     "Low" = "1",
                                     "Medium" = "2",
                                     "High" = "3",
                                     "Very High" = "4")) %>%
  ggplot(aes(Education, fill = JobSatisfaction)) + 
  geom_bar(stat = "count", position = position_dodge()) + 
  facet_wrap(~ Attrition) + 
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, size = 16), axis.text.x = element_text(angle = 90, hjust = 0.5)) + 
  labs(x = "Education", y = "Satisfaction Count", title = "Comparing attrition with Education")

# Compare attrition rate based on performance rating
Performance_Rating <- hr_employee_attrition %>% 
  mutate(PerformanceRating = factor(PerformanceRating)) %>% 
  mutate(PerformanceRating = fct_recode(PerformanceRating,
                                        "Low" = "1",
                                        "Good" = "2",
                                        "Excellent" = "3",
                                        "Outstanding" = "4"))

ggplot(Performance_Rating, aes(fill = Attrition, x = PerformanceRating)) + 
  geom_bar(position = 'fill')

# Compare attrition rate based on relationship satisfaction
Relationship_Satisfaction <- hr_employee_attrition %>% 
  mutate(RelationshipSatisfaction = factor(RelationshipSatisfaction)) %>% 
  mutate(RelationshipSatisfaction = fct_recode(RelationshipSatisfaction,
                                               "Low" = "1",
                                               "Medium" = "2",
                                               "High" = "3",
                                               "Very High" = "4"))

ggplot(Relationship_Satisfaction, aes(fill = Attrition, x = RelationshipSatisfaction)) + 
  geom_bar(position = 'fill')

# Compare attrition rate based work-life balance
Work_Life_Balance <- hr_employee_attrition %>% 
  mutate(WorkLifeBalance = factor(WorkLifeBalance)) %>% 
  mutate(WorkLifeBalance = fct_recode(WorkLifeBalance,
                                      "Bad" = "1",
                                      "Good" = "2",
                                      "Better" = "3",
                                      "Best" = "4"))

ggplot(Work_Life_Balance, aes(fill = Attrition, x = WorkLifeBalance)) + 
  geom_bar(position = 'fill')

# Compare attrition rate based on Environment Satisfaction
Environment_Satisfaction <- hr_employee_attrition %>% 
  mutate(EnvironmentSatisfaction = factor(EnvironmentSatisfaction)) %>% 
  mutate(EnvironmentSatisfaction = fct_recode(EnvironmentSatisfaction,
                                              "Low" = "1",
                                              "Medium" = "2",
                                              "High" = "3",
                                              "Very High" = "4"))

ggplot(Environment_Satisfaction, aes(fill = Attrition, x = EnvironmentSatisfaction)) + 
  geom_bar(position = 'fill')

# Compare attrition rate based on Job Involvement
Job_Involvement <- hr_employee_attrition %>% 
  mutate(JobInvolvement = factor(JobInvolvement)) %>% 
  mutate(JobInvolvement = fct_recode(JobInvolvement,
                                     "Low" = "1",
                                     "Medium" = "2",
                                     "High" = "3",
                                     "Very High" = "4"))

ggplot(Job_Involvement, aes(fill = Attrition, x = JobInvolvement)) + 
  geom_bar(position = 'fill')


# Compare attrition rate based overtime
ggplot(hr_employee_attrition, aes(x = OverTime, group = Attrition)) +
  geom_bar(stat = "count", aes(y = ..prop.., fill = factor(..x..))) +
  labs(x = "Overtime", y = "Percentage", title = "Overtime Vs Attrition Rates") +
  facet_wrap(~Attrition) + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 14)) +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop..), stat = "count", vjust = -0.5)

ggplot(hr_employee_attrition, aes(fill = Attrition, x = OverTime)) + 
  geom_bar(position = 'fill')
