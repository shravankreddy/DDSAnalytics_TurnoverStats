library(dplyr)
library(ggpubr)
library(ggsci)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(forcats)
theme_set(theme_light())

# Load in csv file
setwd("C:/Users/Dmitr/Desktop/MSDS 6306 - 402/Project 2/DDSAnalytics_TurnoverStats")
dataFile <- "data/CaseStudy2data.csv"
hr_employee_attrition <- read.csv(dataFile)

# Use correlation plot to see which continuous variables are
# highly correlated and need to be focused on
Numeric_Vars <- hr_employee_attrition %>%
  select(ï..Age, DailyRate, DistanceFromHome, HourlyRate, MonthlyIncome, MonthlyRate, 
         NumCompaniesWorked, PercentSalaryHike, YearsAtCompany, YearsInCurrentRole,
         YearsSinceLastPromotion, YearsWithCurrManager, TotalWorkingYears,
         TrainingTimesLastYear, StockOptionLevel)

class(Numeric_Vars)

colnames(Numeric_Vars)

corrplot(cor(Numeric_Vars), method = "number", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# Use corrplot as starting point / guideline for looking at
# several variables against attrition to see if there is a trend
Yrs_Since_Prom <- ggboxplot(hr_employee_attrition, x = "Attrition", y = "YearsSinceLastPromotion",
                            color = "Attrition", palette = "jco") + 
                            coord_flip() +
                            theme(legend.position = "none") +
                            stat_compare_means(method = "wilcox.test", label = "p.format", vjust = -2.5, hjust = 1.5)

Yrs_In_Role <- ggboxplot(hr_employee_attrition, x = "Attrition", y = "YearsInCurrentRole",
                         color = "Attrition", palette = "jco") + 
                         coord_flip() +  
                         theme(legend.position = "none") +
                         stat_compare_means(method = "wilcox.test", label = "p.format", vjust = -2.5, hjust = 1.5)

Yrs_At_Comp <- ggboxplot(hr_employee_attrition, x = "Attrition", y = "YearsAtCompany",
                         color = "Attrition", palette = "jco") +
                         coord_flip() +
                         theme(legend.position = "none") +
                         stat_compare_means(method = "wilcox.test", label = "p.format", vjust = -2.5, hjust = 1.5)

Yrs_With_Manager <- ggboxplot(hr_employee_attrition, x = "Attrition", y = "YearsWithCurrManager",
                              color = "Attrition", palette = "jco") +
                              coord_flip() +
                              theme(legend.position = "none") +
                              stat_compare_means(method = "wilcox.test", label = "p.format", vjust = -2.5, hjust = 1.5)

Total_working_Years <- ggboxplot(hr_employee_attrition, x = "Attrition", y = "TotalWorkingYears",
                                 color = "Attrition", palette = "jco") +
                                 coord_flip() +
                                 theme(legend.position = "none") +
                                 stat_compare_means(method = "wilcox.test", label = "p.format", vjust = -2.5, hjust = 1.5)

Age <- ggboxplot(hr_employee_attrition, x = "Attrition", y = "ï..Age",
                 color = "Attrition", palette = "jco") +
                 coord_flip() +
                 theme(legend.position = "none") +
                 stat_compare_means(method = "wilcox.test", label = "p.format", vjust = -2.5, hjust = 1.5)

grid.arrange(Yrs_Since_Prom, Yrs_In_Role,
             Yrs_At_Comp, Yrs_With_Manager,
             Total_working_Years, Age,
             nrow = 3)

# Go through the list of variables
Hourly_Rate <- ggboxplot(hr_employee_attrition, x = "Attrition", y = "HourlyRate",
                         color = "Attrition", palette = "jco") +
                         coord_flip() +
                         theme(legend.position = "none") +
                         stat_compare_means(method = "wilcox.test", label = "p.format", vjust = -3, hjust = 1)

Daily_Rate <- ggboxplot(hr_employee_attrition, x = "Attrition", y = "DailyRate",
                        color = "Attrition", palette = "jco") +
                        coord_flip() +
                        theme(legend.position = "none") +
                        stat_compare_means(method = "wilcox.test", label = "p.format", vjust = -3, hjust = 1)

Monthly_Rate <- ggboxplot(hr_employee_attrition, x = "Attrition", y = "MonthlyRate",
                          color = "Attrition", palette = "jco") +
                          coord_flip() +
                          theme(legend.position = "none") +
                          stat_compare_means(method = "wilcox.test", label = "p.format", vjust = -3, hjust = 1)


Monthly_Income <- ggboxplot(hr_employee_attrition, x = "Attrition", y = "log10(MonthlyIncome)",
                            color = "Attrition", palette = "jco") +
                            coord_flip() +
                            theme(legend.position = "none") +
                            stat_compare_means(method = "wilcox.test", label = "p.format", vjust = -3, hjust = 1)


Perc_Salary_Hike <- ggboxplot(hr_employee_attrition, x = "Attrition", y = "PercentSalaryHike",
                              color = "Attrition", palette = "jco") +
                              coord_flip() +
                              theme(legend.position = "none") +
                              stat_compare_means(method = "wilcox.test", label = "p.format", vjust = -3, hjust = 1)

grid.arrange(Hourly_Rate, Daily_Rate,
             Monthly_Rate, Monthly_Income,
             Perc_Salary_Hike,
             nrow = 3)


Distance_Home_BoxPlot <- ggboxplot(hr_employee_attrition, x = "Attrition", y = "DistanceFromHome",
                         color = "Attrition", palette = "jco") +
                         coord_flip() +
                         theme(legend.position = "none") +
                         stat_compare_means(method = "wilcox.test", label = "p.format", vjust = -6, hjust = 1)

Distance_Home_DensityPlot <- ggplot(hr_employee_attrition, aes(x = DistanceFromHome, color = Attrition)) +
                             geom_density() +
                             scale_color_jco() +
                             theme(legend.position = "none")

grid.arrange(Distance_Home_BoxPlot,
             Distance_Home_DensityPlot,
             nrow = 2)


######## Look into categorical variables ##########


# Compare attrition rate with business travel
Business_Travel <- hr_employee_attrition %>% 
  mutate(BusinessTravel = factor(BusinessTravel)) %>% 
  mutate(BusinessTravel = fct_recode(BusinessTravel,
                                     "Travel Frequently" = "Travel_Frequently",
                                     "Travel Rarely" = "Travel_Rarely",
                                     "Non-Travel" = "Non-Travel"))

Business_Travel_Plot <- ggplot(Business_Travel, aes(fill = Attrition, x = BusinessTravel)) + 
  geom_bar(position = 'fill') + coord_flip() + scale_fill_jco()

# Compare attrition rate with marital status
Marital_Status <- hr_employee_attrition %>% 
  mutate(MaritalStatus = factor(MaritalStatus)) %>% 
  mutate(MaritalStatus = fct_recode(MaritalStatus,
                                    "Single" = "Single",
                                    "Married" = "Married",
                                    "Divorced" = "Divorced"))

Marital_Status_Plot <- ggplot(Marital_Status, aes(fill = Attrition, x = MaritalStatus)) + 
  geom_bar(position = 'fill') + coord_flip() + scale_fill_jco()


# Compare attrition rate with gender
Gender <- hr_employee_attrition %>% 
  mutate(Gender = factor(Gender)) %>% 
  mutate(Gender = fct_recode(Gender,
                             "Male" = "Male",
                             "Female" = "Female"))

Gender_Plot <- ggplot(Gender, aes(fill = Attrition, x = Gender)) + 
  geom_bar(position = 'fill') + coord_flip() + scale_fill_jco()

# Look into education variable. Need to rename numerical factor
# levels to the labels provided under the data definition tab
Education_Level <- hr_employee_attrition %>% 
  mutate(Education = factor(Education)) %>% 
  mutate(Education = fct_recode(Education, 
                                "Below College" = "1", 
                                "College" = "2", 
                                "Bachelor" = "3", 
                                "Master" = "4", 
                                "Doctor" = "5"))

Education_Level_Plot <- ggplot(Education_Level, aes(fill = Attrition, x = Education)) + 
  geom_bar(position = 'fill') + coord_flip() + scale_fill_jco()

# Compare attrition rate with education field
Education_Field <- hr_employee_attrition %>% 
  mutate(EducationField = factor(EducationField)) %>% 
  mutate(EducationField = fct_recode(EducationField,
                                     "Technical Degree" = "Technical Degree",
                                     "Other" = "Other",
                                     "Medical" = "Medical",
                                     "Marketing" = "Marketing",
                                     "Life Sciences" = "Life Sciences",
                                     "Human Resources" = "Human Resources"))

Education_Field_Plot <- ggplot(Education_Field, aes(fill = Attrition, x = EducationField)) + 
  geom_bar(position = 'fill') + coord_flip() + scale_fill_jco()


# Compare attrition rate with education level
Department <- hr_employee_attrition %>% 
  mutate(Department = factor(Department)) %>% 
  mutate(Department = fct_recode(Department,
                                 "Sales" = "Sales",
                                 "Research & Development" = "Research & Development",
                                 "Human Resources" = "Human Resources"))

Department_Plot <- ggplot(Department, aes(fill = Attrition, x = Department)) + 
  geom_bar(position = 'fill') + coord_flip() + scale_fill_jco()



# Compare attrition between different levels of job satisfaction
Job_Satisfaction_Level <- hr_employee_attrition %>% 
  mutate(JobSatisfaction = factor(JobSatisfaction)) %>% 
  mutate(JobSatisfaction = fct_recode(JobSatisfaction,
                                      "Low" = "1",
                                      "Medium" = "2",
                                      "High" = "3",
                                      "Very High" = "4"))

Job_Satisfaction_Level_Plot <- ggplot(Job_Satisfaction_Level, aes(fill = Attrition, x = JobSatisfaction)) + 
  geom_bar(position = 'fill') + coord_flip() + scale_fill_jco()

# Compare attrition rate based on performance rating
Performance_Rating <- hr_employee_attrition %>% 
  mutate(PerformanceRating = factor(PerformanceRating)) %>% 
  mutate(PerformanceRating = fct_recode(PerformanceRating,
                                        "Low" = "1",
                                        "Good" = "2",
                                        "Excellent" = "3",
                                        "Outstanding" = "4"))

Performance_Rating_Plot <- ggplot(Performance_Rating, aes(fill = Attrition, x = PerformanceRating)) + 
  geom_bar(position = 'fill') + coord_flip() + scale_fill_jco()

# Compare attrition rate based on relationship satisfaction
Relationship_Satisfaction <- hr_employee_attrition %>% 
  mutate(RelationshipSatisfaction = factor(RelationshipSatisfaction)) %>% 
  mutate(RelationshipSatisfaction = fct_recode(RelationshipSatisfaction,
                                               "Low" = "1",
                                               "Medium" = "2",
                                               "High" = "3",
                                               "Very High" = "4"))

Relationship_Satisfaction_Plot <- ggplot(Relationship_Satisfaction, aes(fill = Attrition, x = RelationshipSatisfaction)) + 
  geom_bar(position = 'fill') + coord_flip() + scale_fill_jco()

# Compare attrition rate based work-life balance
Work_Life_Balance <- hr_employee_attrition %>% 
  mutate(WorkLifeBalance = factor(WorkLifeBalance)) %>% 
  mutate(WorkLifeBalance = fct_recode(WorkLifeBalance,
                                      "Bad" = "1",
                                      "Good" = "2",
                                      "Better" = "3",
                                      "Best" = "4"))

Work_Life_Balance_Plot <- ggplot(Work_Life_Balance, aes(fill = Attrition, x = WorkLifeBalance)) + 
  geom_bar(position = 'fill') + coord_flip() + scale_fill_jco()

# Compare attrition rate based on Environment Satisfaction
Environment_Satisfaction <- hr_employee_attrition %>% 
  mutate(EnvironmentSatisfaction = factor(EnvironmentSatisfaction)) %>% 
  mutate(EnvironmentSatisfaction = fct_recode(EnvironmentSatisfaction,
                                              "Low" = "1",
                                              "Medium" = "2",
                                              "High" = "3",
                                              "Very High" = "4"))

Environment_Satisfaction_Plot <- ggplot(Environment_Satisfaction, aes(fill = Attrition, x = EnvironmentSatisfaction)) + 
  geom_bar(position = 'fill') + coord_flip() + scale_fill_jco()

# Compare attrition rate based on Job Involvement
Job_Involvement <- hr_employee_attrition %>% 
  mutate(JobInvolvement = factor(JobInvolvement)) %>% 
  mutate(JobInvolvement = fct_recode(JobInvolvement,
                                     "Low" = "1",
                                     "Medium" = "2",
                                     "High" = "3",
                                     "Very High" = "4"))

Job_Involvement_Plot <- ggplot(Job_Involvement, aes(fill = Attrition, x = JobInvolvement)) + 
  geom_bar(position = 'fill') + coord_flip() + scale_fill_jco()


# Compare attrition rate with overtime
Over_Time <- hr_employee_attrition %>% 
  mutate(OverTime = factor(OverTime)) %>% 
  mutate(OverTime = fct_recode(OverTime,
                               "Yes" = "Yes",
                               "No" = "No"))

Over_Time_Plot <- ggplot(Over_Time, aes(fill = Attrition, x = OverTime)) + 
  geom_bar(position = 'fill') + coord_flip() + scale_fill_jco()


# Compare attrition rate amongst stock option levels
Stock_Option_Level <- hr_employee_attrition %>% 
  mutate(StockOptionLevel = factor(StockOptionLevel)) %>% 
  mutate(StockOptionLevel = fct_recode(StockOptionLevel,
                                       "0" = "0",
                                       "1" = "1",
                                       "2" = "2",
                                       "3" = "3"))

Stock_Option_Level_Plot <- ggplot(Stock_Option_Level, aes(fill = Attrition, x = StockOptionLevel)) + 
  geom_bar(position = 'fill') + coord_flip() + scale_fill_jco()


# Compare attrition rate amongst training times last year
Training_Times_Last_Year <- hr_employee_attrition %>% 
  mutate(TrainingTimesLastYear = factor(TrainingTimesLastYear)) %>% 
  mutate(TrainingTimesLastYear = fct_recode(TrainingTimesLastYear,
                                            "0" = "0",
                                            "1" = "1",
                                            "2" = "2",
                                            "3" = "3",
                                            "4" = "4",
                                            "5" = "5",
                                            "6" = "6"))

Training_Times_Last_Year_Plot <- ggplot(Training_Times_Last_Year, aes(fill = Attrition, x = TrainingTimesLastYear)) + 
  geom_bar(position = 'fill') + coord_flip() + scale_fill_jco()






grid.arrange(Gender_Plot, 
             Marital_Status_Plot,
             nrow = 2)

grid.arrange(Education_Level_Plot, 
             Education_Field_Plot, 
             Department_Plot,
             nrow = 3)

grid.arrange(Job_Satisfaction_Level_Plot, 
             Work_Life_Balance_Plot, 
             Business_Travel_Plot,
             nrow = 3)

grid.arrange(Job_Involvement_Plot, 
             Performance_Rating_Plot,
             nrow = 2)

grid.arrange(Relationship_Satisfaction_Plot, 
             Environment_Satisfaction_Plot,
             nrow = 2)

grid.arrange(Stock_Option_Level_Plot, 
             Training_Times_Last_Year_Plot,
             nrow = 2)
