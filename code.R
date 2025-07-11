library(ggplot2)
library(dplyr)

df <- readxl::read_excel("C:/Users/user/Downloads/Mandatory Health Insurance.xlsx")
summary(df)
str(df)
df <- df[, -1]  # remove the Timestamp column

colnames(df) <- c(
  "Age",                    # What is your age?
  "Region",                 # Where do you live?
  "ResidenceType",          # Your residence is in a:
  "MonthlyIncome",          # Monthly income
  "DelayedCare",            # Delayed or avoided care
  "DelayReason",            # Reason for delay
  "DoctorVisits",           # Doctor visits in past year
  "HasInsurance",           # Has insurance
  "NoInsuranceReason",      # Reason no insurance
  "YearlySpending",         # Yearly healthcare spending
  "SystemProblem",          # Biggest problem in healthcare system
  "AccessImpact",           # Impact on access if system implemented
  "SupportIncomeBased",     # Support based on income and subsidies
  "SupportEssentials",      # Support for essential coverage
  "WillingToPay",           # % willing to pay for insurance
  "HardshipStory",          # Financial hardship story (open-ended)
  "SystemChanges"           # Suggested healthcare system changes (open-ended)
)
names(df)



#Age Distribution

ggplot(df, aes(x = Age, fill = factor(Age))) +
  geom_bar(color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Age Distribution", x = "Age", y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill = "none")



#Region Distribution

ggplot(df, aes(x = Region, fill = Region)) +
  geom_bar(color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Region Distribution", x = "Region", y = "Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(fill = "none")



#Residence Type Distribution

ggplot(df, aes(x = ResidenceType, fill = ResidenceType)) +
  geom_bar(color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Pastel2") +
  labs(title = "Residence Type Distribution", x = "Residence Type", y = "Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  guides(fill = "none")



#Delayed Care by Age
ggplot(df, aes(x = Age, fill = DelayedCare)) +
  geom_bar(position = "fill", color = "black") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Delayed Care by Age",
    x = "Age Group",
    y = "Proportion",
    fill = "Delayed Care",
    caption = "Note: 60+ group (n=4) may not represent population trends."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )



#Willingness to Pay by Age
ggplot(df, aes(x = Age, fill = WillingToPay)) +
  geom_bar(position = "fill", color = "black") + 
  scale_fill_brewer(palette = "Set2") + 
  labs(title = "Willingness to Pay by Age Group", x = "Age Group", y = "Proportion") +
  theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5) 
  ) 



#Hypothesis Testing for Delayed Care Based on Residence Type (Urban vs Rural)
# We want to test if urban residents are more likely to delay seeking care compared to rural residents. 
# Null Hypothesis (H₀): There is no significant difference in the proportion of delayed care cases between urban and rural residents. 
# Alternative Hypothesis (H₁): There is a significant difference in the proportion of delayed care cases between urban and rural residents. 

urban_data <- subset(df, ResidenceType == "Քաղաքային / Urban")
rural_data <- subset(df, ResidenceType == "Գյուղական / Rural")

# Count the total number of people in each group
urban_count <- nrow(urban_data)
rural_count <- nrow(rural_data)

# Count of delayed care cases in each group
urban_delayed_count <- sum(urban_data$DelayedCare == "Այո / Yes")
rural_delayed_count <- sum(rural_data$DelayedCare == "Այո / Yes")

# Proportions of delayed care
urban_proportion <- urban_delayed_count / urban_count
rural_proportion <- rural_delayed_count / rural_count

# Counts and Proportions for each group
cat("Urban residents:", urban_count, "Total with delayed care:", urban_delayed_count, "Proportion:", urban_proportion, "\n")
cat("Rural residents:", rural_count, "Total with delayed care:", rural_delayed_count, "Proportion:", rural_proportion, "\n")

# Two-proportion Z-test:
p_pool <- (urban_delayed_count + rural_delayed_count) / (urban_count + rural_count)
SE <- sqrt(p_pool * (1 - p_pool) * (1/urban_count + 1/rural_count))
Z <- (urban_proportion - rural_proportion) / SE
p_value <- 2 * (1 - pnorm(abs(Z)))

# Print Z and p-value
cat("Z-statistic:", Z, "\n")
cat("p-value:", p_value, "\n")

# Add CI and effect size calculations post-hoc:
prop_test <- prop.test(x = c(urban_delayed_count, rural_delayed_count), 
                       n = c(urban_count, rural_count))

# Get 95% CI for the difference in proportions
prop_test$conf.int  # Returns: [lower, upper] difference 

# Conclusion: Since the p-value (0.0081) is less than 0.05, we reject the null hypothesis at the 5% significance level. 
# This means there is a statistically significant difference in the proportion of delayed care between urban and rural residents. 
# As Z = (urban_proportion - rural_proportion) / SE and given the negative Z-statistic (-2.65), it suggests that urban_proportion < rural_proportion, 
# i.e. urban residents are less likely to delay seeking care compared to rural residents.




# Hypothesis Testing for Healthcare Spending Between Insured vs. Uninsured
# We want to determine whether there is an association between having health insurance (insured vs. uninsured) 
# and the level of annual healthcare spending (e.g., <100K AMD, 100–200K AMD, ..., >400K AMD)
# H₀ (Null Hypothesis): Insurance status and yearly healthcare spending level are independent (no association)
# H₁ (Alternative Hypothesis): Insurance status and healthcare spending level are associated (dependent)

# Here we need to apply the Chi-Square Test of Independence between the two categorical variables
# Constructing the contingency table
contingency_table1 <- table(df$HasInsurance, df$YearlySpending)
contingency_table1

chi_test_result1 <- chisq.test(contingency_table1)
print(chi_test_result1)

# Conclusion: Since the p-value (0.3684) > α (0.05), we fail to reject the null hypothesis, therefore
# at the 5% significance level, there is no statistically significant association between insurance status and yearly healthcare spending.
# This suggests that, in this sample, whether someone is insured or not does not appear to influence how much they spend annually on healthcare.




#Hypothesis Testing for Reasons for Not Having Health Insurance
# We're interested in understanding why people don't have mandatory health insurance, and based on the survey we suspect that lack of awareness is the primary reason. 
# Null Hypothesis (H₀): More than half of the people who do not have health insurance are not primarily unaware of the available options. 
# Alternative Hypothesis (H₁): More than half of the people who do not have health insurance are primarily unaware of the available options. 

# First, I retrieved the reasons for not having insurance
reasons <- df$NoInsuranceReason

# However, as the question in the survey was a multiple-answers/checkbox question,
# some people chose the "Other" option and left it blank, which resulted in missing values in my dataset.
# That is why, let's remove NA values and empty strings
reasons_clean <- reasons[!is.na(reasons) & reasons != ""]
reasons_data <- table(reasons_clean)

# Identifying the number of respondents who are not aware of the possible options for mandatory health insurance
total_count <- sum(reasons_data)
unaware_count <- reasons_data["Տեղյակ չեմ հնարավոր տարբերակների մասին / Not familiar with options"]
other_reasons_count <- total_count - unaware_count

cat("People unaware of options:", unaware_count, "\n")
cat("Other reasons:", other_reasons_count, "\n")

# Perform one-sample proportion test
prop_test_result <- prop.test(unaware_count, total_count, p = 0.5, alternative = "greater")
prop_test_result

# Conclusion: Since the p-value (0.5642) is greater than the significance level of 0.05, we fail to reject the null hypothesis. 
# This means that there is not enough evidence to conclude that more than half of the people without health insurance are unaware of the available options.
# The confidence interval [0.4213, 1.0000] also includes 0.5, reinforcing that the observed proportion (49%) is not significantly greater than 50%.



#Hypothesis Testing for Support for Mandatory Health Insurance vs Age Group
# Suppose we want to test whether support for mandatory health insurance 
# (based on the understanding that it would cover essential services such as doctor visits, hospitalizations, and basic medications) 
# is associated with respondents’ age group.
# H₀ (Null Hypothesis): Support for mandatory health insurance — based on coverage of essential services — is independent of age group.
# H₁ (Alternative Hypothesis): Support for mandatory health insurance — based on coverage of essential services — is associated with age group.

# Here we need to apply the Chi-Square Test of Independence between the two categorical variables
# Constructing the contingency table
contingency_table2 <- table(df$Age, df$SupportEssentials)
contingency_table2

chi_test_result2 <- chisq.test(contingency_table2)
print(chi_test_result2)

# Conclusion: Since the p-value (0.758) > 0.05, we fail to reject the null hypothesis. 
# Based on the Chi-Square Test of Independence results, we fail to reject the null hypothesis. 
# This suggests that there is no statistically significant association between respondents' age group and their support for mandatory health insurance. 
# In other words, support for mandatory health insurance appears to be independent of age group.

