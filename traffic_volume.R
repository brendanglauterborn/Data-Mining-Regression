
install.packages(c("tidyverse","psych","GGally"))
library(tidyverse)
library(psych)
library(GGally)
install.packages("randomForest")
library(randomForest)
install.packages("Metrics")
library(Metrics)
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
install.packages("gbm")
library(gbm)
install.packages("DataExplorer")
library(DataExplorer)
install.packages("SmartEDA")
library(SmartEDA)
subset_data <- traffic_data %>%
  select(holiday, temp, rain_1h, snow_1h,clouds_all, weather_main, weather_description, date_time, traffic_volume)
ExpData(data = subset_data, type = 2)

create_report(traffic_data)
install.packages("skimr")
library(skimr)

skim(traffic_data)

#init
traffic_data <- read.csv("Metro_Interstate_Traffic_Volume.csv")

#time
traffic_data <- traffic_data %>%
  mutate(datetime_parsed = ymd_hms(date_time))

traffic_data <- traffic_data %>%
  mutate(hour = factor(hour(datetime_parsed)), weekday = factor(wday(datetime_parsed)), month = factor(month(datetime_parsed)), year = factor(year(datetime_parsed)))
ggplot(data = traffic_data, mapping = aes(x = traffic_volume, y = hour )) + geom_boxplot() + labs(x= "Traffic Volume" , y = "Hour", title = "Traffic Volume by Hour")
ggplot(data = traffic_data, mapping = aes(x = traffic_volume, y = weekday )) + geom_boxplot() + labs(x= "Traffic Volume" , y = "weekday", title = "Traffic Volume by Weekday")
ggplot(data = traffic_data, mapping = aes(x = traffic_volume, y = month )) + geom_boxplot() + labs(x= "Traffic Volume" , y = "Month", title = "Traffic Volume by Month")
ggplot(data = traffic_data, mapping = aes(x = traffic_volume, y = year )) + geom_boxplot() + labs(x= "Traffic Volume" , y = "Year", title = "Traffic Volume by Year")


#box plot traffic vs holiday
ggplot(data = traffic_data, mapping = aes(x = traffic_volume, y = holiday )) + geom_boxplot() + labs(x= "Traffic Volume" , y = "Holiday", title = "Traffic Volume by Holiday")

traffic_data <- traffic_data %>%
  mutate(is_holiday = ifelse(holiday == "None", "Non-Holiday", "Holiday"))
ggplot(data = traffic_data, mapping = aes(x = traffic_volume, y = is_holiday )) + geom_boxplot() + labs(x= "Traffic Volume" , y = "Is a Holiday", title = "Traffic Volume by Holiday")

#box plot weather
ggplot(data = traffic_data, mapping = aes(x = traffic_volume, y = weather_main )) + geom_boxplot() + labs(x= "Traffic Volume" , y = "Weather", title = "Traffic Volume by Weather")


#scatter plots
traffic_data <- traffic_data %>%
  mutate(temp_f = (temp - 273.15) * (9/5) +32)

ggplot(data = traffic_data) + 
  geom_point(mapping = aes(x = traffic_volume, y = temp_f), alpha = 1 / 100) +
  ylim(-200, 200)

ggplot(data = traffic_data) + 
  geom_point(mapping = aes(x = traffic_volume, y = rain_1h)) + scale_y_log10()

ggplot(data = traffic_data) + 
  geom_point(mapping = aes(x = traffic_volume, y = snow_1h)) + scale_y_log10()

ggplot(data = traffic_data) + 
  geom_point(mapping = aes(x = traffic_volume, y = clouds_all)) 

#hist
ggplot(data = traffic_data, aes(x = traffic_volume)) + 
  geom_histogram(bins = 50, fill = 'steelblue', color = 'black') +
  labs(x= 'Traffic Volume', y = 'Count', title = 'Count Traffic Volume')

#hist
ggplot(data = traffic_data, aes(x = clouds_all)) + 
  geom_histogram(bins = 50, fill = 'steelblue', color = 'black') +
  labs(x= 'Traffic Volume', y = 'Count', title = 'Count Clouds')

#qq
qqnorm(traffic_data$traffic_volume)
qqline(traffic_data$traffic_volume, col = 'steelblue')

#qq
qqnorm(log1p(traffic_data$traffic_volume))
qqline(log1p(traffic_data$traffic_volume), col = 'steelblue')

#preprocessing

#2


#four ewual bins
traffic_data <- traffic_data %>%
  mutate(temp_f_bin = cut(
    temp_f,
    breaks = 4,                           
    labels = c("Very Cold","Cold","Warm","Hot")
  ))

table(traffic_data$temp_f_bin)

#range
traffic_data <- traffic_data %>%
  mutate(
    temp_f_bin2 = cut(
      temp_f,
      breaks = c(-Inf,40,60,80,Inf),
      labels = c('Very Cold', 'Cold', 'Warm', 'Very Warm')
    )
  )

#kmeans
set.seed(123)
k <- 4
traffic_data <- traffic_data %>%
  mutate(temp_kmeans = kmeans(temp_f, centers = k)$cluster)

#hist
ggplot(traffic_data, aes(x = temp_f_bin)) + 
  geom_bar(fill = "steelblue", color = "black") +
  labs(x = "Temperature Bins (Equal Frequency)", y = "Count",
       title = "Counts per Bin (Quantile)")

ggplot(traffic_data, aes(x = temp_f_bin2)) + 
  geom_bar(fill = "steelblue", color = "black") +
  labs(x = "Temperature Bins (Custom Thresholds)", y = "Count",
       title = "Counts per Bin (Custom)")

ggplot(traffic_data, aes(x = temp_kmeans)) + 
  geom_bar(fill = "steelblue", color = "black") +
  labs(x = "Temperature Clusters (K-means)", y = "Count",
       title = "Counts per Bin (K-means)")




table(traffic_data$temp_f_bin2)
#3
traffic_data <- traffic_data %>%
  mutate(
    traffic_log = log1p(traffic_volume),          
    traffic_sqrt = sqrt(traffic_volume),          
    traffic_inv_sqrt = 1 / sqrt(traffic_volume+1) 
  )

plot(density(traffic_data$traffic_volume, na.rm = TRUE),
     main = "Density of Traffic Volume",
     xlab = "Traffic Volume",
     col = "blue")

par(mfrow = c(2,2)) 
qqnorm(traffic_data$traffic_volume, main = 'Normal'); qqline(traffic_data$traffic_volume, col = 'steelblue')
qqnorm(traffic_data$traffic_log, main = 'Log'); qqline(traffic_data$traffic_log, col = 'steelblue')
qqnorm(traffic_data$traffic_sqrt, main = 'Sqrt'); qqline(traffic_data$traffic_sqrt, col = 'steelblue')
qqnorm(traffic_data$traffic_inv_sqrt, main = 'Inv Sqrt'); qqline(traffic_data$traffic_inv_sqrt, col = 'steelblue')
par(mfrow = c(1,1))


minMax <- function(x) {
  (x-min(x)) / (max(x) - min(x))
}

  
zScore <- function(x) {
  ((x - mean(x)) / sd(x))
}

decimalScale <- function(x){
  max_abs = max(abs(x))
  decimal_shift <- nchar(as.integer(max_abs))
  return (x / 10^decimal_shift)
}

traffic_data <- traffic_data %>%
  mutate(
    across(c(traffic_volume, temp_f, rain_1h, snow_1h,clouds_all),
           minMax,
           .names = "{.col}_minmax"),
    across(c(traffic_volume, temp_f, rain_1h, snow_1h,clouds_all),
           zScore,
           .names = "{.col}_z"),
    across(c(traffic_volume, temp_f, rain_1h, snow_1h,clouds_all),
           decimalScale,
           .names = "{.col}_dec")
  )
summary(select(traffic_data, ends_with("_minmax")))

summary(select(traffic_data, ends_with("_z")))

summary(select(traffic_data, ends_with("_dec")))

library(gridExtra)
library(ggplot2)

norm_table <- traffic_data %>%
  select(
    traffic_volume_minmax,
    temp_f_minmax,
    rain_1h_minmax,
    snow_1h_minmax,
    clouds_all_minmax
  ) %>%
  head(10)

table_plot <- gridExtra::tableGrob(norm_table)
gridExtra::grid.arrange(table_plot)

norm_table <- traffic_data %>%
  select(
    traffic_volume_z,
    temp_f_z,
    rain_1h_z,
    snow_1h_z,
    clouds_all_z
  ) %>%
  head(10)

table_plot <- gridExtra::tableGrob(norm_table)
gridExtra::grid.arrange(table_plot)

norm_table <- traffic_data %>%
  select(
    traffic_volume_dec,
    temp_f_dec,
    rain_1h_dec,
    snow_1h_dec,
    clouds_all_dec
  ) %>%
  head(10)

table_plot <- gridExtra::tableGrob(norm_table)
gridExtra::grid.arrange(table_plot)








#regression test/train
n <- nrow(traffic_data)
train_idx <- sample(seq_len(n), size = 0.7*n)
train <- traffic_data[train_idx, ]
test  <- traffic_data[-train_idx, ]

#regression 
#ordinary least squares

#train
lm_model <- lm(traffic_volume ~ temp_f + rain_1h + snow_1h + clouds_all + hour + weekday + month + is_holiday,
             data = train)
#test
pred_lm <- predict(lm_model, newdata=test)

#get how far predictions are off
rmse_lm <- rmse(test$traffic_volume, pred_lm)

r2_lm <- 1 - sum((test$traffic_volume - pred_lm)^2) / sum((test$traffic_volume - mean(test$traffic_volume))^2)



summary (lm_model)
library(ggplot2)
ggplot(data = NULL, aes(x= pred_lm, y = test$traffic_volume)) +
  geom_point(alpha = .2, color = 'steelblue')+ 
  geom_abline(slope=1, intercept = 0, color ='red', linetype = 'dashed') +
  labs(x="Predicted Traffic Volume",
       y="Actual Traffiv Volume",
       title='Predicted vs Actual Traffic Volume')
ggplot(data = NULL, aes(x = pred_lm, y = test$traffic_volume - pred_lm)) +
  geom_point(alpha = 0.2, color = 'steelblue') +
  geom_hline(yintercept = 0, color = 'red', linetype = 'dashed') +
  labs(
    x = "Predicted Traffic Volume",
    y = "Residuals",
    title = "Residuals vs Predicted"
  )

dt_model <- rpart(traffic_volume ~ temp_f + rain_1h + snow_1h + clouds_all + hour + weekday + month + is_holiday,
                  data = train, method = 'anova')
rpart.plot(dt_model, main = "Decision Tree for Traffic Volume")

dtpred <- predict(dt_model, newdata = test)

rmse <- function(a,p) sqrt(mean((a-p)^2))
rmse_tree <- rmse(test$traffic_volume, dtpred)

#how far predictions are from actual
sse <- sum((test$traffic_volume - dtpred)^2)

#how far each actual value is from the mean of all actual values
sst <- sum((test$traffic_volume - mean(test$traffic_volume))^2)
r2_tree <- 1 - (sse/sst)

ggplot(data = NULL, aes(x= dtpred, y = test$traffic_volume)) +
  geom_point(alpha = .2, color = 'steelblue')+ 
  geom_abline(slope=1, intercept = 0, color ='red', linetype = 'dashed') +
  labs(x="Predicted Traffic Volume",
       y="Actual Traffic Volume",
       title='Predicted vs Actual Traffic Volume')

ggplot(data = NULL, aes(x = dtpred, y = test$traffic_volume - dtpred)) +
  geom_point(alpha = 0.2, color = 'steelblue') +
  geom_hline(yintercept = 0, color = 'red', linetype = 'dashed') +
  labs(
    x = "Predicted Traffic Volume",
    y = "Residuals",
    title = "Residuals vs Predicted"
  )


rf_model <- randomForest(traffic_volume ~ temp_f + rain_1h + snow_1h + clouds_all + hour + weekday + month + is_holiday,
                         data = train, importance = TRUE)

rf_pred <- predict(rf_model, newdata = test)

rmse_rf <- sqrt(mean((test$traffic_volume - rf_pred)^2))

sse <- sum((test$traffic_volume - rf_pred)^2)
sst <- sum((test$traffic_volume - mean(test$traffic_volume))^2)

r2_rf <- 1 - (sse/sst)

plot(rf_model)
ggplot(data = NULL, aes(x= rf_pred, y = test$traffic_volume)) +
  geom_point(alpha = .2, color = 'steelblue')+ 
  geom_abline(slope=1, intercept = 0, color ='red', linetype = 'dashed') +
  labs(x="Predicted Traffic Volume",
       y="Actual Traffic Volume",
       title='Predicted vs Actual Traffic Volume')

ggplot(data = NULL, aes(x = rf_pred, y = test$traffic_volume - rf_pred)) +
  geom_point(alpha = 0.2, color = 'steelblue') +
  geom_hline(yintercept = 0, color = 'red', linetype = 'dashed') +
  labs(
    x = "Predicted Traffic Volume",
    y = "Residuals",
    title = "Residuals vs Predicted"
  )
train$is_holiday <- as.factor(train$is_holiday)
test$is_holiday  <- as.factor(test$is_holiday)

# Then re-run the gbm
library(gbm)

gbm_model <- gbm(traffic_volume ~ temp_f + rain_1h + snow_1h + clouds_all + hour + weekday + month + is_holiday,
                 data = train,
                 distribution = 'gaussian',
                 n.trees = 100,
                 interaction.depth = 5,
                 shrinkage = .05,
                 cv.folds = 5,
                 verbose = FALSE)

best_iter <- gbm.perf(gbm_model, method = 'cv')

gbm_pred <- predict(gbm_model, newdata = test, n.trees = best_iter)

rmse_gbm <- sqrt(mean((test$traffic_volume - gbm_pred)^2))

sse <- sum((test$traffic_volume - gbm_pred)^2)
sst <- sum((test$traffic_volume - mean(test$traffic_volume))^2)

r2_gbm <- 1 - (sse/sst)

ggplot(data = NULL, aes(x= gbm_pred, y = test$traffic_volume)) +
  geom_point(alpha = .2, color = 'steelblue')+ 
  geom_abline(slope=1, intercept = 0, color ='red', linetype = 'dashed') +
  labs(x="Predicted Traffic Volume",
       y="Actual Traffic Volume",
       title='Gradient Boosting: Predicted vs Actual Traffic Volume')

ggplot(data = NULL, aes(x = gbm_pred, y = test$traffic_volume - gbm_pred)) +
  geom_point(alpha = 0.2, color = 'steelblue') +
  geom_hline(yintercept = 0, color = 'red', linetype = 'dashed') +
  labs(
    x = "Predicted Traffic Volume",
    y = "Residuals",
    title = "Residuals vs Predicted"
  )
model_results <- data.frame(
  Model = c("Linear Regression", "Decision Tree", "Random Forest", "Gradient Boosting"),
  R2    = c(r2_lm, r2_tree, r2_rf, r2_gbm),
  RMSE  = c(rmse_lm, rmse_tree, rmse_rf, rmse_gbm)
)
library(knitr)
kable(model_results, digits = 3, caption = "Model Performance (Test Set)")




