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

corrplot(cor(Numeric_Vars), method = "square", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
