##############################
# Econometrics Project - Time Series and Panel Data Analysis
# Author: Konstantinos Chalikias
# Description: Analysis of macroeconomic indicators using time series and panel data models.
##############################
install.packages(c("car", "lmtest", "tseries", "forecast", "rugarch", "readxl", "strucchange", "plm", "AER"))
# Load Libraries
library(car)
library(lmtest)
library(tseries)
library(forecast)
library(rugarch)
library(readxl)
library(strucchange)
library(plm)
library(AER)
library(ggplot2)
library(dplyr)

# Set Working Directory
setwd("...")

# Load Data
data <- read_xlsx("data_econometrics_final.xlsx")
datap <- read_xlsx("data_econometrics_panel_final.xlsx")

##############################
# Time Series Analysis (France)
##############################

# Construct Time Series
y <- ts(as.numeric(data$y1[-(1:3)]), frequency = 4, start = c(1979, 3))
fr <- data.frame(
  y = y,
  x1 = as.numeric(data$x11[-(1:3)]),
  x2 = as.numeric(data$x12[-(1:3)]),
  x3 = as.numeric(data$x13[-(1:3)]),
  x4 = as.numeric(data$x14[-(1:3)]),
  x5 = as.numeric(data$x15[-(1:3)]),
  x6 = as.numeric(data$cf1[-(1:3)]),
  x7 = as.numeric(data$cf2[-(1:3)]),
  x8 = as.numeric(data$cf3[-(1:3)])
)

# Exploratory Analysis
plot(y, type = "l", col = "blue")
adf.test(y)
shapiro.test(y)
qqnorm(y); qqline(y)
plot(density(y))

# Linear Regression
mod1 <- lm(y ~ ., data = fr)
vif(mod1)
summary(mod1)
qqnorm(residuals(mod1)); qqline(residuals(mod1))
shapiro.test(residuals(mod1))

# Stepwise Algorithm
mod1s <- step(mod1)
summary(mod1s)
vif(mod1s)
summary(mod1s)
qqnorm(residuals(mod1s)); qqline(residuals(mod1s))
shapiro.test(residuals(mod1s))

# Autocorrelation Check
dwtest(mod1s)
par(mfrow=c(1,2))
acf(residuals(mod1s)); pacf(residuals(mod1s))
acf(residuals(mod1s)^2); pacf(residuals(mod1s)^2) 

# ARIMA Model
mod1a <- arima(y, xreg = fr[, c("x4", "x7", "x8")], order = c(1, 0, 1))
summary(mod1a)
checkresiduals(mod1a)
acf(residuals(mod1a)); pacf(residuals(mod1a))
acf(residuals(mod1a)^2); pacf(residuals(mod1a)^2) 

# Heteroscedasticity
bptest(lm(residuals(mod1a) ~ x4 + x7 + x8, data = fr))

##############################
# Structural Break Analysis
##############################

bp <- breakpoints(y ~ 1)
summary(bp)
par(mfrow=c(1,1))
plot(bp)
plot(y, type = "l", main = "Breakpoints and Fitted Lines" , col = "blue")
lines(fitted(bp, breaks = 1), col = "red", lwd = 2)

bp <- breakpoints(y ~ x4 + x7 + x8, data = fr)
summary(bp)
plot(bp)

# Chow Tests
chow.test81 <- sctest(y ~ x4 + x7 + x8, type = "Chow", point = 81, data = fr)
chow.test2.31 <- sctest(y[1:87] ~ x4[1:87] + x7[1:87] + x8[1:87], type = "Chow", point = 31,data = fr)
chow.test2.87 <- sctest(y[31:162] ~ x4[31:162] + x7[31:162] + x8[31:162], type = "Chow", point = 87,data = fr)

# Add Breakpoint as Factor
fr$bp <- cut(1:nrow(fr), breaks = c(0, 81, nrow(fr)), labels = FALSE)
mod2 <- lm(y ~ x4 + x7 + x8 + as.factor(bp), data = fr)
summary(mod2)
mod2s <- step(mod2)
summary(mod2s)
plot(y, type = "l", main = "Breakpoints and Fitted Lines" , col = "blue")
lines(fitted(bp, breaks = 1), col = "red", lwd = 2)

# ARIMA with Breakpoint
mod2a <- arima(y, xreg = data.frame(fr$x4, fr$x7, fr$x8, as.factor(fr$bp)), order = c(1, 0, 1))
summary(mod2a)
checkresiduals(mod2a)
plot(y, type = "l", main = "Model forecast with breakpoint on actual GDP growth" , col = "blue")
lines(fitted(mod2a), col = "red", lwd = 2)

# Heteroscedasticity
bptest(lm(residuals(mod2a) ~ x4 + x7 + x8 + as.factor(bp), data = fr))

##############################
# Panel Data Analysis
##############################

y <- as.numeric(datap$DlnGdp[-1])
colnames(datap) <- c(colnames(datap)[1:2], as.character(datap[1, 3:11]))
dataf <- datap[-1, ]
dataf <- within(dataf, {
  y <- as.numeric(y)
  x1 <- as.numeric(x1)
  x2 <- as.numeric(x2)
  x3 <- as.numeric(x3)
  x4 <- as.numeric(x4)
  x5 <- as.numeric(x5)
  x6 <- as.numeric(x6)
  x7 <- as.numeric(x7)
  x8 <- as.numeric(x8)
  country <- as.factor(country)
  time <- as.factor(time)
})
levels(dataf$country) <- c("FR", "GER", "IT", "JP", "UK", "US")


# Visualizations
ggplot(dataf, aes(x = as.numeric(time), y = y, color = country)) +
  geom_line(size = 0.7) +
  scale_color_manual(values = c("mediumblue","black","green4","red3","navy","red4"), guide = "none") +
  facet_wrap(~ country, ncol = 2, scales = "free_y") +
  theme_minimal(base_size = 14) +
  labs(
    title = "GDP growth over Time by Country",
    x = "Time",
    y = "GDP growth"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

boxplot(split(y, dataf$country), names = c("FR", "GER", "IT", "JP", "UK", "US"), 
        col =c("red"))


# Pooled OLS
pool1 <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, data = dataf)
summary(pool1)
summary(step(pool1))

# Pooled Model (plm)
pool2 <- plm(y ~ x1 + x2 + x4 + x7 + x8, data = dataf, model = "pooling")
summary(pool2)

# Fixed Effects
fixed1 <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + country, data = dataf)
summary(fixed1)
summary(step(fixed1))

fixed2 <- plm(y ~ x1 + x2 + x4 + x7 + x8, data = dataf, model = "within", index = c("country", "time"))
summary(fixed2)

# Random Effects
random <- plm(y ~ x1 + x2 + x4 + x7 + x8, data = dataf, model = "random", index = c("country", "time"))
summary(random)

# Model Tests
pFtest(fixed2, pool2)
phtest(fixed2, random)

# Final Model
summary(random)

# Forecast with train and test dataset #
dataf$time <- as.numeric(as.character(dataf$time))
train <- dataf %>% filter(time <= 128)
test  <- dataf %>% filter(time > 128)

random_train <- plm(y ~ x1 + x2 + x4 + x7 + x8, data = train, model = "random", index = c("country", "time"))
summary(random_train)

test$pred <- predict(random_train, newdata = test)

test$errors <- test$y - test$pred
rmse_val <- sqrt(mean(errors^2))
mae_val <- mean(abs(errors))

perf_by_country <- test %>%
  group_by(country) %>%
  summarise(
    RMSE = sqrt(mean(errors^2)),
    MAE  = mean(abs(errors))
  )

plot_df <- test %>%
  as.data.frame() %>%
  select(country, time, y, pred)

ggplot(plot_df, aes(x = time)) +
  geom_line(aes(y = y, color = "Actual"), size = 1) +
  geom_line(aes(y = pred, color = "Predicted"), size = 1, linetype = "dashed") +
  facet_wrap(~ country, scales = "free_y") +
  labs(
    title = "GDP Growth: Actual vs Predicted (Test Period)",
    x = "Time (quarters)",
    y = "GDP Growth",
    color = ""
  ) +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )
