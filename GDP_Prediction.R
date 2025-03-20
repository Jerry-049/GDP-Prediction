# Packages  ---------------------------------------------------------------
library(dplyr)
library(tidyverse)
library(readxl)
library(tidyr)
library(readr)
library(lubridate)
library(ggplot2)
library(ggcorrplot)
library(tseries)
library(lmtest)
library(vars)
library(lmtest)
library(igraph)
library(ggraph)
library(forecast)
library(urca)
library(dynlm)
library(sandwich)
library(forecast)
library(car)
library(MASS)
library(stringr)
library(MuMIn)

# GDP Germany -------------------------------------------------------------
gdp_data <- read_excel("r_course/paper/GDP Germany.xlsx", skip = 1)

colnames(gdp_data) <- c("Quarter_Start", "GDP (in billion)")

gdp_data <- gdp_data %>% 
  mutate(
    Quarter_Start = as.Date(Quarter_Start, format = "%d.%m.%Y"),
    `GDP (in billion)` = as.numeric(`GDP (in billion)`)
  )
gdp_data_filtered <- gdp_data %>% filter(Quarter_Start >= as.Date("1996-04-01"))

View(gdp_data_filtered)

# Inflation Germany -------------------------------------------------------

inflation <- read_excel("~/r_course/paper/ECB Data Portal wide_20250228132641.xlsx")

inflation$DATE <- as.Date(inflation$DATE, format="%Y-%m-%d")

inflation$Inflation <- as.numeric(inflation$Inflation)

# Quarterly aggregation
inflation_quarterly <- inflation %>%
  mutate(Quarter_Start = case_when(
    format(DATE, "%m") %in% c("01", "02", "03") ~ as.Date(paste0(format(DATE, "%Y"), "-01-01")),
    format(DATE, "%m") %in% c("04", "05", "06") ~ as.Date(paste0(format(DATE, "%Y"), "-04-01")),
    format(DATE, "%m") %in% c("07", "08", "09") ~ as.Date(paste0(format(DATE, "%Y"), "-07-01")),
    format(DATE, "%m") %in% c("10", "11", "12") ~ as.Date(paste0(format(DATE, "%Y"), "-10-01"))
  )) %>%
  group_by(Quarter_Start) %>%
  summarise(Inflation = round(mean(Inflation, na.rm = TRUE), 2)) %>%
  filter(Quarter_Start >= as.Date("1996-04-01"))

View(inflation_quarterly)

# DAX Germany -------------------------------------------------------------

dax_data <- read_csv("r_course/paper/DAX_40_2.csv", col_names = FALSE)

dax_data <- dax_data %>%
  mutate(X1 = str_replace_all(X1, "([0-9])\\,([0-9]{3})", "\\1\\2"))  # Entfernt die Kommas innerhalb von Zahlen

dax_data <- dax_data %>%
  separate(X1, into = c("Datum", "Open", "High", "Low", "Close"), sep = ",", convert = TRUE)

dax_data <- dax_data %>%
  mutate(
    Open = as.numeric(Open),
    High = as.numeric(High),
    Low = as.numeric(Low),
    Close = as.numeric(Close)
  )

dax_data <- na.omit(dax_data)

dax_data <- dax_data[, !(names(dax_data) %in% c("Open", "High", "Low"))]


dax_data <- dax_data %>%
  mutate(Datum = as.Date(paste0("01-", Datum), format="%d-%m/%Y")) %>%  # In "01-MM-YYYY" umwandeln
  mutate(Datum = format(Datum, "%Y-%m-%d"))  # Endgültiges Format: "YYYY-MM-01"

dax_data$Close <- as.numeric(dax_data$Close)



dax_data <- dax_data %>% mutate(Datum = as.Date(Datum, origin = "1995-01-01"))

# Quarterly aggregation
dax_quarterly <- dax_data %>%
  mutate(Quarter_Start = case_when(
    month(Datum) %in% c(1, 2, 3) ~ as.Date(paste0(year(Datum), "-01-01")),
    month(Datum) %in% c(4, 5, 6) ~ as.Date(paste0(year(Datum), "-04-01")),
    month(Datum) %in% c(7, 8, 9) ~ as.Date(paste0(year(Datum), "-07-01")),
    month(Datum) %in% c(10, 11, 12) ~ as.Date(paste0(year(Datum), "-10-01"))
  )) %>%
  group_by(Quarter_Start) %>%
  summarise(Quarterly_DAX = round(mean(Close, na.rm = TRUE), 2)) %>%
  filter(Quarter_Start >= as.Date("1996-04-01"))

View(dax_quarterly)



# Oil worldwide -----------------------------------------------------------

oil_data <- read_csv("r_course/paper/Europe_Brent_Spot_Price_FOB.csv", skip = 5, col_names = FALSE)

colnames(oil_data) <- c("Date", "USD per Barrel")

oil_data <- oil_data %>% 
  mutate(
    Date = parse_date_time(Date, orders = "my", tz = "UTC"),
    `USD per Barrel` = as.numeric(`USD per Barrel`)
  )

# Quarterly Aggreation
oil_quarterly <- oil_data %>%
  mutate(Quarter_Start = case_when(
    month(Date) %in% c(1, 2, 3) ~ as.Date(paste0(year(Date), "-01-01")),
    month(Date) %in% c(4, 5, 6) ~ as.Date(paste0(year(Date), "-04-01")),
    month(Date) %in% c(7, 8, 9) ~ as.Date(paste0(year(Date), "-07-01")),
    month(Date) %in% c(10, 11, 12) ~ as.Date(paste0(year(Date), "-10-01"))
  )) %>%
  group_by(Quarter_Start) %>%
  summarise(Quarterly_Oil_Price = round(mean(`USD per Barrel`, na.rm = TRUE), 2)) %>%
  filter(Quarter_Start >= as.Date("1996-04-01"))

View(oil_quarterly)

# Labour Market Germany ---------------------------------------------------

unemployment_rate_data <- read_excel("r_course/paper/Unemployment2.xlsx")

unemployment_rate_data <- unemployment_rate_data %>% 
  tidyr::fill(...1, .direction = "down")

View(unemployment_rate_data)

unemployment_rate_data <- unemployment_rate_data %>% 
  mutate(
    Date = as.Date(paste0("01 ", Date, " ", ...1), format = "%d %B %Y"),
    UnemploymentRate = as.numeric(gsub(",", ".", Unemploymentrate)) # Komma zu Punkt umwandeln
  ) 
unemployment_rate_data <- unemployment_rate_data[, c("Date", "UnemploymentRate")]

# Quarterly aggregation
unemployment_rate_quarterly <- unemployment_rate_data %>%
  mutate(Quarter_Start = case_when(
    month(Date) %in% c(1, 2, 3) ~ as.Date(paste0(year(Date), "-01-01")),
    month(Date) %in% c(4, 5, 6) ~ as.Date(paste0(year(Date), "-04-01")),
    month(Date) %in% c(7, 8, 9) ~ as.Date(paste0(year(Date), "-07-01")),
    month(Date) %in% c(10, 11, 12) ~ as.Date(paste0(year(Date), "-10-01"))
  )) %>%
  group_by(Quarter_Start) %>%
  summarise(Quarterly_UnemploymentRate = round(mean(UnemploymentRate, na.rm = TRUE), 2)) %>%
  filter(Quarter_Start >= as.Date("1996-04-01"))

View(unemployment_rate_quarterly)

# Merge data --------------------------------------------------------------
# Alle gefilterten Datensätze zusammenführen
# Alle gefilterten Datensätze zusammenführen
merged_data <- gdp_data_filtered %>%
  left_join(inflation_quarterly, by = "Quarter_Start") %>%
  left_join(dax_quarterly, by = "Quarter_Start") %>%
  left_join(oil_quarterly, by = "Quarter_Start") %>%
  left_join(unemployment_rate_quarterly, by = "Quarter_Start")

# Ergebnis anzeigen
View(merged_data)


# Plot data --------------------------------------------------------------

# GDP Plot
ggplot(merged_data, aes(x = Quarter_Start, y = `GDP (in billion)`)) +
  geom_line(color = "blue") +
  labs(title =  "Germany Quartely GDP Development", x = "Quarter", y = "German GDP (in bn)") +
  theme_minimal()

# Inflation Plot
ggplot(merged_data, aes(x = Quarter_Start, y = Inflation)) +
  geom_line(color = "red") +
  labs(title = "Inflation rate Quartely", x = "Quarter", y = "Inflation (%)") +
  theme_minimal()

# DAX Plot
ggplot(merged_data, aes(x = Quarter_Start, y = Quarterly_DAX)) +
  geom_line(color = "green") +
  labs(title = "DAX Development Quarterly", x = "Quarter", y = "DAX Index") +
  theme_minimal()

# Oil Price Plot
ggplot(merged_data, aes(x = Quarter_Start, y = Quarterly_Oil_Price)) +
  geom_line(color = "orange") +
  labs(title = "Europe Brent Spot  price Quarterly Development", x = "Quarter", y = "USD per Barrel") +
  theme_minimal()

# Unemployment Plot
ggplot(merged_data, aes(x = Quarter_Start, y = Quarterly_UnemploymentRate)) +
  geom_line(color = "purple") +
  labs(title = "Unemployment Rate Germany Quarterly", x = "Quarter", y = "Unemployment Rate (%)") +
  theme_minimal()

# ADF Test1 ----------------------------------------------------------------
# ADF-Test 
print(adf.test(merged_data$`GDP (in billion)`, alternative = "stationary"))
print(adf.test(merged_data$Inflation, alternative = "stationary"))
print(adf.test(merged_data$Quarterly_DAX, alternative = "stationary"))
print(adf.test(merged_data$Quarterly_Oil_Price, alternative = "stationary"))
print(adf.test(merged_data$Quarterly_UnemploymentRate, alternative = "stationary"))


# First Difference  and Plots -------------------------------------------------------
# GDP First Difference
merged_data2 <- merged_data %>%
  mutate(GDP_diff = c(NA, diff(`GDP (in billion)`)))

# Inflation First Difference
merged_data2 <- merged_data2 %>%
  mutate(Inflation_diff = c(NA, diff(Inflation)))

# DAX First Difference
merged_data2 <- merged_data2 %>%
  mutate(DAX_diff = c(NA, diff(Quarterly_DAX)))

# Oil price First Difference
merged_data2 <- merged_data2 %>%
  mutate(Oil_diff = c(NA, diff(Quarterly_Oil_Price)))

# Unemployment rate First Difference
merged_data2 <- merged_data2 %>%
  mutate(Unemployment_diff = c(NA, diff(Quarterly_UnemploymentRate)))

# results
merged_data2 <- merged_data2 %>% drop_na()
View(merged_data2)


# Plots First Difference

ggplot(merged_data2, aes(x = Quarter_Start, y = GDP_diff)) +
  geom_line(color = "blue") +
  labs(title = "Differentiated BIP", x = "Quarter", y = "Δ BIP") +
  theme_minimal()

ggplot(merged_data2, aes(x = Quarter_Start, y = Inflation_diff)) +
  geom_line(color = "red") +
  labs(title = "Differentiated Inflationrate", x = "Quarter", y = "Δ Inflation") +
  theme_minimal()

ggplot(merged_data2, aes(x = Quarter_Start, y = DAX_diff)) +
  geom_line(color = "green") +
  labs(title = "Differentiated DAX", x = "Quarter", y = "Δ DAX") +
  theme_minimal()

ggplot(merged_data2, aes(x = Quarter_Start, y = Oil_diff)) +
  geom_line(color = "orange") +
  labs(title = "Differentiated Oilprice", x = "Quarter", y = "Δ Oilprice") +
  theme_minimal()

ggplot(merged_data2, aes(x = Quarter_Start, y = Unemployment_diff)) +
  geom_line(color = "purple") +
  labs(title = "Differentiated Unemploymentrate", x = "Quarter", y = "Δ Unemploymentrate") +
  theme_minimal()




# ADF Test2 ---------------------------------------------------------------
# ADF-Tests First Difference Variables
adf_gdp <- adf.test(merged_data2$GDP_diff, alternative = "stationary")
adf_inflation <- adf.test(merged_data2$Inflation_diff, alternative = "stationary")
adf_dax <- adf.test(merged_data2$DAX_diff, alternative = "stationary")
adf_oil <- adf.test(merged_data2$Oil_diff, alternative = "stationary")
adf_unemployment <- adf.test(merged_data2$Unemployment_diff, alternative = "stationary")

print(adf_gdp)
print(adf_inflation)
print(adf_dax)
print(adf_oil)
print(adf_unemployment)

# Augmented Dickey-Fuller Test (ADF)
adf_gdp <- adf.test(merged_data2$GDP_diff, alternative = "stationary")
adf_inflation <- adf.test(merged_data2$Inflation_diff, alternative = "stationary")
adf_dax <- adf.test(merged_data2$DAX_diff, alternative = "stationary")
adf_oil <- adf.test(merged_data2$Oil_diff, alternative = "stationary")
adf_unemployment <- adf.test(merged_data2$Unemployment_diff, alternative = "stationary")

# KPSS-Test
kpss_gdp <- kpss.test(merged_data2$GDP_diff, null = "Level")
kpss_inflation <- kpss.test(merged_data2$Inflation_diff, null = "Level")
kpss_dax <- kpss.test(merged_data2$DAX_diff, null = "Level")
kpss_oil <- kpss.test(merged_data2$Oil_diff, null = "Level")
kpss_unemployment <- kpss.test(merged_data2$Unemployment_diff, null = "Level")

# Show results
print(adf_gdp)
print(kpss_gdp)
print(adf_inflation)
print(kpss_inflation)
print(adf_dax)
print(kpss_dax)
print(adf_oil)
print(kpss_oil)
print(adf_unemployment)
print(kpss_unemployment)




# Granger Causality  ---------------------------------------------------------------

# Tested Variables
variablen <- c("Inflation_diff", "DAX_diff", "Oil_diff", "Unemployment_diff")

granger_results <- list()

# Granger-Causality for Lags 1 to 8
for (var in variablen) {
  for (lag in 1:8) {
    test_result <- grangertest(GDP_diff ~ get(var), order = lag, data = merged_data2)
    granger_results[[paste(var, "Lag", lag)]] <- test_result
  }
}

# Show results
for (key in names(granger_results)) {
  cat("\n", key, "\n")
  print(granger_results[[key]])
}
# OLS and VAR -------------------------------------------------------------

# Possible Lags for the Variables
possible_lags <- list(GDP = 1:8, 
                      Inflation = 1:8, 
                      DAX = 1:8, 
                      Oil = 1:8, 
                      Unemployment = 1:8)

# Combinations of diferent Lags and Variables
lag_combinations <- expand.grid(GDP = possible_lags$GDP, 
                                Inflation = possible_lags$Inflation, 
                                DAX = possible_lags$DAX, 
                                Oil = possible_lags$Oil, 
                                Unemployment = possible_lags$Unemployment)


best_model <- NULL
best_aic <- Inf
best_lags <- NULL

for (i in 1:nrow(lag_combinations)) {
  
  
  current_lags <- lag_combinations[i, ]
  
  
  formula <- as.formula(paste("GDP_diff ~", 
                              paste(paste0("lag(GDP_diff, ", 1:current_lags$GDP, ")"), collapse = " + "), 
                              "+", 
                              paste(paste0("lag(Inflation_diff, ", 1:current_lags$Inflation, ")"), collapse = " + "),
                              "+",
                              paste(paste0("lag(DAX_diff, ", 1:current_lags$DAX, ")"), collapse = " + "),
                              "+",
                              paste(paste0("lag(Oil_diff, ", 1:current_lags$Oil, ")"), collapse = " + "),
                              "+",
                              paste(paste0("lag(Unemployment_diff, ", 1:current_lags$Unemployment, ")"), collapse = " + ")))
  
#Model estimation
  model <- tryCatch(lm(formula, data = merged_data2), error = function(e) NULL)
  
  if (!is.null(model)) {
    model_aic <- AIC(model)
    
    if (model_aic < best_aic) {
      best_model <- model
      best_aic <- model_aic
      best_lags <- current_lags
    }
  }
}

# best Model based on AIC
cat("\n📌 Bestes OLS-Modell basierend auf AIC:\n")
print(best_model)
cat("\n📌 Niedrigster AIC-Wert:", round(best_aic, 4), "\n")
cat("\n📌 Optimale Lag-Kombination:\n")
print(best_lags)

# Training and Test Data
train_data <- merged_data2 %>% filter(Quarter_Start < as.Date("2020-01-01"))
test_data  <- merged_data2 %>% filter(Quarter_Start >= as.Date("2020-01-01"))

# OLS Forecast
ols_forecast <- predict(best_model, newdata = test_data)

#Computation of RMSE and MAE
rmse_ols <- sqrt(mean((ols_forecast - test_data$GDP_diff)^2, na.rm = TRUE))
mae_ols <- mean(abs(ols_forecast - test_data$GDP_diff), na.rm = TRUE)

cat("📌 RMSE (OLS):", round(rmse_ols, 4), "\n")
cat("📌 MAE (OLS):", round(mae_ols, 4), "\n")

test_data <- test_data %>% mutate(OLS_Prediction = ols_forecast)

#Plot Prediction
ggplot(test_data, aes(x = Quarter_Start)) +
  geom_line(aes(y = GDP_diff, color = "Tatsächliche Werte"), size = 1) +
  geom_line(aes(y = OLS_Prediction, color = "OLS-Vorhersage"), linetype = "dashed", size = 1) +
  labs(title = "📊 OLS Prognose für GDP_diff", x = "Zeit", y = "GDP Wachstum (Differenziert)") +
  scale_color_manual(name = "Legende", values = c("Tatsächliche Werte" = "blue", "OLS-Vorhersage" = "red")) +
  theme_minimal()


#VAR Estimation
var_lag_length <- max(best_lags)  # Maximale Lag-Länge aus OLS übernehmen
var_data <- merged_data2[, c("GDP_diff", "Inflation_diff", "DAX_diff", "Oil_diff", "Unemployment_diff")]
var_model <- VAR(var_data, p = var_lag_length, type = "const")

# VAR Forecast
forecast_horizon <- nrow(test_data)  
var_forecast_full <- predict(var_model, n.ahead = forecast_horizon)$fcst$GDP_diff[, 1]


if (length(var_forecast_full) < length(ols_forecast)) {
  var_forecast_full <- c(var_forecast_full, rep(NA, length(ols_forecast) - length(var_forecast_full)))
}

#VAR RMSE and MAE
rmse_ols <- sqrt(mean((ols_forecast - test_data$GDP_diff)^2, na.rm = TRUE))
mae_ols <- mean(abs(ols_forecast - test_data$GDP_diff), na.rm = TRUE)

rmse_var <- sqrt(mean((var_forecast_full - test_data$GDP_diff)^2, na.rm = TRUE))
mae_var <- mean(abs(var_forecast_full - test_data$GDP_diff), na.rm = TRUE)

cat("📌 RMSE (OLS):", round(rmse_ols, 4), "\n")
cat("📌 MAE (OLS):", round(mae_ols, 4), "\n")
cat("📌 RMSE (VAR):", round(rmse_var, 4), "\n")
cat("📌 MAE (VAR):", round(mae_var, 4), "\n")

test_data <- test_data %>% 
  mutate(OLS_Prediction = ols_forecast, 
         VAR_Prediction = var_forecast_full)  

#Plot both Predictons
ggplot(test_data, aes(x = Quarter_Start)) +
  geom_line(aes(y = GDP_diff, color = "Tatsächliche Werte"), size = 1) +
  geom_line(aes(y = OLS_Prediction, color = "OLS-Vorhersage"), linetype = "dashed", size = 1) +
  geom_line(aes(y = VAR_Prediction, color = "VAR-Vorhersage"), linetype = "dotted", size = 1) +
  labs(title = "📊 Vergleich OLS vs. VAR Prognose für GDP_diff", x = "Zeit", y = "GDP Wachstum (Differenziert)") +
  scale_color_manual(name = "Legende", values = c("Tatsächliche Werte" = "blue", "OLS-Vorhersage" = "red", "VAR-Vorhersage" = "green")) +
  theme_minimal()



#Compute RMSE and MAE over the same numbers of Preditions
valid_indices <- which(!is.na(test_data$OLS_Prediction) & !is.na(test_data$VAR_Prediction))

rmse_ols_adjusted <- sqrt(mean((test_data$OLS_Prediction[valid_indices] - test_data$GDP_diff[valid_indices])^2, na.rm = TRUE))
rmse_var_adjusted <- sqrt(mean((test_data$VAR_Prediction[valid_indices] - test_data$GDP_diff[valid_indices])^2, na.rm = TRUE))

mae_ols_adjusted <- mean(abs(test_data$OLS_Prediction[valid_indices] - test_data$GDP_diff[valid_indices]), na.rm = TRUE)
mae_var_adjusted <- mean(abs(test_data$VAR_Prediction[valid_indices] - test_data$GDP_diff[valid_indices]), na.rm = TRUE)

cat("\n📌 RMSE (OLS, angepasster Zeitraum):", round(rmse_ols_adjusted, 4), "\n")
cat("📌 RMSE (VAR, angepasster Zeitraum):", round(rmse_var_adjusted, 4), "\n")
cat("📌 MAE (OLS, angepasster Zeitraum):", round(mae_ols_adjusted, 4), "\n")
cat("📌 MAE (VAR, angepasster Zeitraum):", round(mae_var_adjusted, 4), "\n")

#Prediction Plot for the same time horizon
ggplot(test_data[valid_indices, ], aes(x = Quarter_Start)) +
  geom_line(aes(y = GDP_diff, color = "Actual Data"), size = 1) +
  geom_line(aes(y = OLS_Prediction, color = "OLS Prediction"), linetype = "dashed", size = 1) +
  geom_line(aes(y = VAR_Prediction, color = "VAR Prediction"), linetype = "dotted", size = 1) +
  labs(title = "Comparison of OLS vs. VAR Predictions", x = "Quarter", y = "GDP Germany  (Differenced)") +
  scale_color_manual(name = "Legend", values = c("Actual Data" = "blue", "OLS Prediction" = "red", "VAR Prediction" = "green")) +
  theme_minimal()

# AIC OLS -----------------------------------------------------------------

#All Lag possibilities for the OLS
max_lag <- 8
lag_combinations <- expand.grid(
  GDP = 1:max_lag,
  Inflation = 1:max_lag,
  DAX = 1:max_lag,
  Oil = 1:max_lag,
  Unemployment = 1:max_lag
)


aic_results <- list()

#calculate all possible OLS Models again
for (i in 1:nrow(lag_combinations)) {
  lag_gdp <- lag_combinations$GDP[i]
  lag_inflation <- lag_combinations$Inflation[i]
  lag_dax <- lag_combinations$DAX[i]
  lag_oil <- lag_combinations$Oil[i]
  lag_unemployment <- lag_combinations$Unemployment[i]
  
  formula <- as.formula(paste("GDP_diff ~",
                              paste(paste0("lag(GDP_diff, ", 1:lag_gdp, ")"), collapse = " + "), "+",
                              paste(paste0("lag(Inflation_diff, ", 1:lag_inflation, ")"), collapse = " + "), "+",
                              paste(paste0("lag(DAX_diff, ", 1:lag_dax, ")"), collapse = " + "), "+",
                              paste(paste0("lag(Oil_diff, ", 1:lag_oil, ")"), collapse = " + "), "+",
                              paste(paste0("lag(Unemployment_diff, ", 1:lag_unemployment, ")"), collapse = " + ")))
  
#Again Model estimation (same model as before)
  model <- lm(formula, data = merged_data2)
  
#AIC values
  aic_results[[i]] <- list(
    model = model,
    AIC = AIC(model),
    Lags = c(GDP = lag_gdp, Inflation = lag_inflation, DAX = lag_dax, Oil = lag_oil, Unemployment = lag_unemployment)
  )
}

#show the 10 best Models and the lag structure 
best_models <- aic_results[order(sapply(aic_results, function(x) x$AIC))[1:10]]

for (j in 1:10) {
  cat("\n📌 Modell", j, "mit dem niedrigsten AIC:\n")
  cat("AIC:", round(best_models[[j]]$AIC, 4), "\n")
  cat("Verwendete Lags:\n")
  print(best_models[[j]]$Lags)
}





