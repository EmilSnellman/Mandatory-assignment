install.packages("readxl")
install.packages("plm")
library(readxl)
library(plm)

# Read the Excel file (SET OWN FILE LOCATION)
panel_data <- read_excel("")

# Convert the 'Date' column to Date format
panel_data$Date <- as.numeric(panel_data$Date)

# Extract only the year from the Date column
pdata$Year <- format(pdata$Date, "%Y")

# Create a panel data frame
pdata <- pdata.frame(panel_data, index = c("Company", "Date"))

# Define dummy variable years based on the identified bad periods
recession_years <- c(2011, 2012, 2020, 2022)

# Creating the recession dummy 
pdata$recession_dummy <- ifelse(pdata$Date %in% recession_years, 1, 0)

# Creating models for Hausman Test
fixed_model <- plm(Leverage ~ Liquidity + Tangibility + Growth + Profitability + Size + recession_dummy, 
                   data = pdata, model = "within")

random_model <- plm(Leverage ~ Liquidity + Tangibility + Growth + Profitability + Size + recession_dummy, 
                    data = pdata, model = "random")

# Conducting the Hausman test
hausman_result <- phtest(fixed_model, random_model)
print(hausman_result)


# Fixed effects model that will be used in regression
fixed_model <- plm(Leverage ~ Liquidity + Tangibility + Growth + Profitability + Size + recession_dummy, 
                   data = pdata, model = "within")



# Since Robust clustered standard errors is used the regression will be done again with the robust standard errors
summary(fixed_model)

install.packages("lmtest")
library(lmtest)
robust_se_model <- coeftest(fixed_model, vcov = vcovHC(fixed_model, type = "HC1", cluster = "group"))
print(robust_se_model)


#MODEL DIAGNOSTICS 
#VIF
install.packages("car")
library(car)
vif_model <- lm(Leverage ~ Liquidity + Tangibility + Growth + Profitability + Size + recession_dummy, data = pdata)
vif(vif_model)

#Breusch-Pagan test  
install.packages("lmtest")
library(lmtest)
bptest(fixed_model)


#Breusch-Godfrey test 
library(plm)
pbgtest(fixed_model)

#Jarque Bera test 
install.packages("tseries")
library(tseries)

# Extract residuals from the fixed effects model
residuals_fixed_model <- residuals(fixed_model)

# Perform the Jarque-Bera test
jb_test <- jarque.bera.test(residuals_fixed_model)
print(jb_test)
