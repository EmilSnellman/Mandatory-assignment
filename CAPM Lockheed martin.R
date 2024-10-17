install.packages("readxl")
install.packages("ggplot2")

library(ggplot2)
library(readxl)

#data from excel file
data <- read_excel("C:/Users/Sniemava/Downloads/Lockheed Martin returns and market returns.xlsx")

#performing the regression
data$Lockheed_Excess <- data$Lockheed - data$RF
data$Mkt_Excess <- data$`Mkt-RF`

CAPM_model <- lm(Lockheed_Excess ~ Mkt_Excess, data = data)
summary(CAPM_model)

#creating a more visually pleasing table
install.packages("stargazer")
library(stargazer)

stargazer(CAPM_model, type = "text",
          title = "CAPM Regression Results",
          dep.var.labels = "Lockheed Excess Returns",
          covariate.labels = c("Market Excess Returns (Beta)"),
          single.row = TRUE,
          omit.stat = c("ser", "f"),
          digits = 3)


#visualising the relationship between stock and market returns
install.packages("broom")
install.packages("dplyr")

library(broom)
library(dplyr)

#Scatterplot with regression line
data %>%
  mutate(Mkt_Excess = `Mkt-RF`, Lockheed_Excess = Lockheed - RF) %>%
  ggplot(aes(x = Mkt_Excess, y = Lockheed_Excess)) + 
  geom_point(color = "cornflowerblue") +
  geom_smooth(method = "lm", se = FALSE, color = "green", size = .5) +
  ylab("Lockheed Excess Returns") +
  xlab("Market Excess Returns") +
  ggtitle("Compare CAPM Beta Line to Regression Line")

#Fitted vs Actual returns

portfolio_model_augmented <- augment(CAPM_model, data = data) %>%
  mutate(Date = data$Date)

portfolio_model_augmented %>% 
  ggplot(aes(x = Date)) + 
  geom_line(aes(y = Lockheed_Excess, color = "actual returns")) + 
  geom_line(aes(y = .fitted, color = "fitted returns")) +
  scale_colour_manual("", 
                      values = c("fitted returns" = "green", 
                                 "actual returns" = "cornflowerblue")) +
  xlab("Date") + 
  ylab("Returns") +
  ggtitle("Fitted versus Actual Returns")

