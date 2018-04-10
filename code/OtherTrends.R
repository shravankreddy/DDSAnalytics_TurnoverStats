# Load in csv file
dataFile <- "data/CaseStudy2data.csv"
hr_employee_attrition <- read.csv(dataFile)

# Use correlation plot to see which continuous variables are
# highly correlated and need to be focused on
Numeric_Vars <- hr_employee_attrition[,sapply(hr_employee_attrition, is.integer)]

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