
# Build Knn Regression model on dataset to predict income of a person . 
rm(list=ls(all=TRUE))
install.packages("RCurl")
install.packages("vegan")
install.packages("dummies")
install.packages("caTools")
install.packages("FNN")
install.packages("DMwR")
install.packages("caret")

library("RCurl")
data_full=read.table(text = getURL("https://raw.githubusercontent.com/rajsiddarth119/Datasets/master/Bank_dataset.csv"), header=T, sep=',',
                col.names = c('ID', 'age', 'exp', 'inc', 
                              'zip', 'family', 'ccavg', 'edu', 
                              'mortgage', 'loan', 'securities', 
                              'cd', 'online', 'cc'))

str(data_full)
#Remove ID,exp and zip
data=subset(data_full,select = -c(ID,zip,exp))

#Converting age,inc,CCavg,Mortgage,family to numeric and standardizing
#Income as dependent variable
dep_atr=c("inc")
num_atr=c("age","ccavg","mortgage","family")
categ_atr=setdiff(names(data),c(num_atr,"inc"))

num_data=data.frame(sapply(data[num_atr], function(x){as.numeric(x)}))
library(vegan)
num_data=decostand(num_data,method = "range")

#Converting categ attributes to dummy variables
# Загрузите пакет caret
library(caret)

categ_data=data.frame(sapply(data[categ_atr], as.factor))

# Создайте фиктивные переменные с использованием функции dummyVars
dummy_data <- dummyVars(" ~ .", data = categ_data, sep = "_")
categ_data <- predict(dummy_data, newdata = categ_data)

# Объедините числовые и категориальные данные
final_data <- cbind(num_data, categ_data, "inc" = data$inc)

# Выведите структуру окончательных данных
str(final_data)
# categ_data=dummy.data.frame(categ_data,sep="_")

# final_data=cbind(num_data,categ_data,"inc"=data$inc)
# str(final_data)

# Divide the data into training,testing and eval data
set.seed(123)
library(caTools)
rowids = 1:nrow(final_data)
train_index =  sample(rowids, length(rowids)*0.6)
test_index = sample(setdiff(rowids, train_index), length(rowids)*0.2)
eval_index = setdiff(rowids, c(train_index, test_index))

train_data=final_data[train_index,]
test_data=final_data[test_index,]
eval_data=final_data[eval_index,]

# Checking how records are split with respect to target attribute.
summary(final_data$inc)
summary(train_data$inc)
summary(test_data$inc)
summary(eval_data$inc)

ind_variables=setdiff(names(final_data),"inc")

# Using Knn algorithm to predict income variable
#install.packages("FNN")
library(FNN)

# .........k = 1........................#
# Предсказание на обучающих данных
pred1_train = knn.reg(train = train_data[,ind_variables], test = train_data[,ind_variables],
                      y = train_data$inc, k = 1)

# Предсказание на тестовых данных
pred1_test = knn.reg(train = train_data[,ind_variables], test = test_data[,ind_variables],
                     y = train_data$inc, k = 1)

# Метрики ошибок для регрессии

#-------- Train_1 ---------

cat("Метрики ошибок на обучающих данных для k=1\n")

# Создание данных для использования в train
train_data_caret_1 <- data.frame(actual = train_data[,"inc"], predicted = pred1_train$pred)

# Оценка модели с использованием функции train и линейной регрессии
model_1 <- train(actual ~ predicted, data = train_data_caret_1, method = "lm")

# Вывод результатов оценки
summary(model_1)

# Функция для расчета MAPE
calculateMAPE <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual)) * 100
}

# Рассчет MAPE для k=1 на обучающих данных
mape_train_k1 <- calculateMAPE(train_data[,"inc"], pred1_train$pred)

# Вывод результата
cat("MAPE для k=1 на обучающих данных: ", round(mape_train_k1, 4), "%\n")

#------- Test_1 -------

cat("\nМетрики ошибок на тестовых данных для k=1\n")

test_data_caret_1 <- data.frame(actual = test_data[,"inc"], predicted = pred1_test$pred)

# Оценка модели с использованием функции train и линейной регрессии
model_1 <- train(actual ~ predicted, data = train_data_caret_1, method = "lm")

# Предсказания на тестовых данных
predictions <- predict(model_1, newdata = test_data_caret_1)
predictions

# Рассчет MAPE для оценки производительности модели на тестовых данных
MAPE <- calculateMAPE(test_data[,"inc"], pred1_test$pred)

# Вывод результатов
cat("MAPE для k=1 на тестовых данных: ", round(MAPE, 4), "%\n")


# Загрузите пакет ggplot2
library(ggplot2)

# Создайте данные для графика
plot_data_1 <- data.frame(actual = test_data_caret_1$actual, predicted = predictions)

# Создайте график с точечным разбросом (scatter plot)
ggplot(plot_data_1, aes(x = actual, y = predicted)) +
  geom_point(color = "darkgreen", size = 2, alpha = 0.5)+
  geom_smooth(method = "loess", color = "green", se = FALSE, size = 1.5) +
  geom_abline(intercept = 0, slope = 1, color = "darkorange", linetype = "dashed",  size = 1.5) +
    labs(title = "Actual vs Predicted, k = 1",
       x = "Actual Income",
       y = "Predicted Income") +
  theme_minimal()

# График плотности
ggplot(plot_data_1, aes(x = actual, y = predicted)) +
  geom_point() +
  geom_hex(bins = 30) +
  geom_abline(intercept = 0, slope = 1, color = "darkorange", linetype = "dashed", size = 1.5) +
  labs(title = "Actual vs Predicted (k = 1)",
       x = "Actual Income",
       y = "Predicted Income") +
  theme_minimal() +
  xlim(c(0, max(plot_data_1$actual, plot_data_1$predicted))) +
  ylim(c(0, max(plot_data_1$actual, plot_data_1$predicted)))


# ---------- k = 3 -------------#
#Predicting on train data

pred3_train = knn.reg(train = train_data[,ind_variables],test = train_data[,ind_variables],
                      y = train_data$inc, k = 3)

pred3_test = knn.reg(train = train_data[,ind_variables],test = test_data[,ind_variables],
                     y = train_data$inc, k = 3)

#Error metrics for regression

#---------- Train_3 ------------

cat("Error metrics on train data for k=3")

# Создайте данные для использования в train
train_data_caret_3 <- data.frame(actual = train_data[,"inc"], predicted = pred3_train$pred)

# Оцените модель с использованием функции train и метода k-ближайших соседей
model_k3 <- train(actual ~ predicted, data = train_data_caret_3, method = "knn", trControl = trainControl(method = "cv", number = 3))

# Выведите результаты оценки
summary(model_k3)

# Рассчитайте MAPE для k=3 на тренировочных данных
mape_train_k3 <- calculateMAPE(train_data[,"inc"], pred3_train$pred)

# Выведите результат
cat("MAPE for k=3 on train is", round(mape_train_k3, 4), "%\n")


#----------- Test_3 -------------

cat("Error metrics on test data for k=3")
# Создайте данные для использования в test
test_data_caret_3 <- data.frame(actual = test_data[,"inc"], predicted = pred3_test$pred)

# Оцените модель с использованием функции train и метода k-ближайших соседей
model_3 <- train(actual ~ predicted, data = test_data_caret_3, method = "knn", trControl = trainControl(method = "cv", number = 3))

# Предсказания на тестовых данных
predictions_3 <- predict(model_3, newdata = test_data_caret_3)
predictions_3

# Рассчитайте MAPE для оценки производительности модели на тестовых данных
MAPE_3 <- calculateMAPE(test_data[,"inc"], pred3_test$pred)

# Вывод результатов
cat("MAPE for k=3 on test is", round(MAPE_3, 4), "%\n")


# Создайте данные для графика
plot_data_3 <- data.frame(actual = test_data_caret_3$actual, predicted = predictions_3)
# Создайте график с точечным разбросом

ggplot(plot_data_3, aes(x = actual, y = predicted)) +
  geom_point(color = "darkgreen", size = 2, alpha = 0.5) +
  geom_smooth(method = "loess", color = "green", se = FALSE, size = 1.5) +
  geom_abline(intercept = 0, slope = 1, color = "darkorange", linetype = "dashed", size = 1.5) +
  labs(title = "Actual vs Predicted (k=3)",
       x = "Actual Income",
       y = "Predicted Income") +
  theme_minimal() +
  xlim(c(0, max(plot_data_3$actual, plot_data_3$predicted))) +
  ylim(c(0, max(plot_data_3$actual, plot_data_3$predicted)))

# График плотности
ggplot(plot_data_3, aes(x = actual, y = predicted)) +
  geom_point() +
  geom_hex(bins = 30) +
  geom_abline(intercept = 0, slope = 1, color = "darkorange", linetype = "dashed", size = 1.5) +
  labs(title = "Actual vs Predicted (k=3)",
       x = "Actual Income",
       y = "Predicted Income") +
  theme_minimal() +
  xlim(c(0, max(plot_data_3$actual, plot_data_3$predicted))) +
  ylim(c(0, max(plot_data_3$actual, plot_data_3$predicted)))


#--------- k = 5 -----------#
#Predicting on train data

pred5_train = knn.reg(train = train_data[,ind_variables],test = train_data[,ind_variables],
                      y = train_data$inc, k = 5)

pred5_test = knn.reg(train = train_data[,ind_variables],test = test_data[,ind_variables],
                     y = train_data$inc, k = 5)

#Error metrics for regression
# --------- Train_5 ----------

cat("Error metrics on train data for k=5")
# Создайте данные для использования в train
train_data_caret_5 <- data.frame(actual = train_data[,"inc"], predicted = pred5_train$pred)

# Оцените модель с использованием функции train и метода k-ближайших соседей
model_k5 <- train(actual ~ predicted, data = train_data_caret_5, method = "knn", trControl = trainControl(method = "cv", number = 3))

# Выведите результаты оценки
summary(model_k5)

# Рассчитайте MAPE для k=5 на тренировочных данных
mape_train_k5 <- calculateMAPE(train_data[,"inc"], pred5_train$pred)

# Выведите результат
cat("MAPE for k=5 on train is", round(mape_train_k5, 4), "%\n")

# --------- Test_5 ---------

cat("Error metrics on test data for k=5")
# Создайте данные для использования в test
test_data_caret_5 <- data.frame(actual = test_data[,"inc"], predicted = pred5_test$pred)

# Оцените модель с использованием функции train и метода k-ближайших соседей
model_5 <- train(actual ~ predicted, data = test_data_caret_5, method = "knn", trControl = trainControl(method = "cv", number = 3))

# Предсказания на тестовых данных
predictions_5 <- predict(model_5, newdata = test_data_caret_5)
predictions_5

# Рассчитайте MAPE для оценки производительности модели на тестовых данных
MAPE_5 <- calculateMAPE(test_data[,"inc"], pred5_test$pred)

# Вывод результатов
cat("MAPE for k=5 on test is", round(MAPE_5, 4), "%\n")


# Создайте данные для графика
plot_data_5 <- data.frame(actual = test_data_caret_5$actual, predicted = predictions_5)

# Создайте график с точечным разбросом
ggplot(plot_data_5, aes(x = actual, y = predicted)) +
  geom_point(color = "darkgreen", size = 2, alpha = 0.5) +
  geom_smooth(method = "loess", color = "green", se = FALSE, size = 1.5) +
  geom_abline(intercept = 0, slope = 1, color = "darkorange", linetype = "dashed", size = 1.5) +
  labs(title = "Actual vs Predicted (k=5)",
       x = "Actual Income",
       y = "Predicted Income") +
  theme_minimal() +
  xlim(c(0, max(plot_data_5$actual, plot_data_5$predicted))) +
  ylim(c(0, max(plot_data_5$actual, plot_data_5$predicted)))


# График плотности_5
ggplot(plot_data_5, aes(x = actual, y = predicted)) +
  geom_point() +
  geom_hex(bins = 30) +
  geom_abline(intercept = 0, slope = 1, color = "darkorange", linetype = "dashed", size = 1.5) +
  labs(title = "Actual vs Predicted (k=5)",
       x = "Actual Income",
       y = "Predicted Income") +
  theme_minimal() +
  xlim(c(0, max(plot_data_5$actual, plot_data_5$predicted))) +
  ylim(c(0, max(plot_data_5$actual, plot_data_5$predicted)))


# ---------- k = 7 ------------#
#Predicting on train data

pred7_train = knn.reg(train = train_data[,ind_variables],test = train_data[,ind_variables],
                      y = train_data$inc, k = 7)

pred7_test = knn.reg(train = train_data[,ind_variables],test = test_data[,ind_variables],
                     y = train_data$inc, k = 7)

#Error metrics for regression
#---------- Train_7 ---------

cat("Error metrics on train data for k=7")
# Создайте данные для использования в train
train_data_caret_7 <- data.frame(actual = train_data[,"inc"], predicted = pred7_train$pred)

# Оцените модель с использованием функции train и метода k-ближайших соседей
model_k7 <- train(actual ~ predicted, data = train_data_caret_7, method = "knn", trControl = trainControl(method = "cv", number = 3))
model_k7 <- train(actual ~ predicted, data = train_data_caret_7, method = "lm")

# Выведите результаты оценки
summary(model_k7)

# Рассчитайте MAPE для k=7 на тренировочных данных
mape_train_k7 <- calculateMAPE(train_data[,"inc"], pred7_train$pred)

# Выведите результат
cat("MAPE for k=7 on train is", round(mape_train_k7, 4), "%\n")

#----------- Test_7 -----------
cat("Error metrics on test data for k=7")
# Создайте данные для использования в test
test_data_caret_7 <- data.frame(actual = test_data[,"inc"], predicted = pred7_test$pred)

# Оцените модель с использованием функции train и метода k-ближайших соседей
model_k7 <- train(actual ~ predicted, data = test_data_caret_7, method = "knn", trControl = trainControl(method = "cv", number = 3))
model_k7 <- train(actual ~ predicted, data = test_data_caret_7, method = "lm")

# Выведите результаты оценки
summary(model_k7)

# Предсказания на тестовых данных
predictions_k7 <- predict(model_k7, newdata = test_data_caret_7)
predictions_k7

# Рассчитайте MAPE для оценки производительности модели на тестовых данных
MAPE_k7 <- mean(abs((test_data_caret_7$actual - predictions_k7) / test_data_caret_7$actual)) * 100

# Выведите результат
cat("MAPE for k=7 on test is", round(MAPE_k7, 4), "%\n")


# Создайте данные для графика
plot_data_k7 <- data.frame(actual = test_data_caret_7$actual, predicted = predictions_k7)

# Создайте график с точечным разбросом
ggplot(plot_data_k7, aes(x = actual, y = predicted)) +
  geom_point(color = "darkgreen", size = 2, alpha = 0.5) +
  geom_smooth(method = "loess", color = "green", se = FALSE, size = 1.5) +
  geom_abline(intercept = 0, slope = 1, color = "darkorange", linetype = "dashed", size = 1.5) +
  labs(title = "Actual vs Predicted (k=7)",
       x = "Actual Income",
       y = "Predicted Income") +
  theme_minimal() +
  xlim(c(0, max(plot_data_k7$actual, plot_data_k7$predicted))) +
  ylim(c(0, max(plot_data_k7$actual, plot_data_k7$predicted)))


# График плотности_7
ggplot(plot_data_k7, aes(x = actual, y = predicted)) +
  geom_point() +
  geom_hex(bins = 30) +
  geom_abline(intercept = 0, slope = 1, color = "darkorange", linetype = "dashed", size = 1.5) +
  labs(title = "Actual vs Predicted (k=7)",
       x = "Actual Income",
       y = "Predicted Income") +
  theme_minimal() +
  xlim(c(0, max(plot_data_k7$actual, plot_data_k7$predicted))) +
  ylim(c(0, max(plot_data_k7$actual, plot_data_k7$predicted)))


#----------- ----------------
# Testing the final model performance on evaluation data 
pred_eval = knn.reg(train = train_data[,ind_variables],test = eval_data[,ind_variables],
                    y = train_data$inc, k = 3)

cat("Error metrics on eval data for k=3")
# Создайте данные для использования в eval
eval_data_caret <- data.frame(actual = eval_data[,"inc"], predicted = pred_eval$pred)

# Оцените модель с использованием функции train и метода k-ближайших соседей
model_k3_eval <- train(actual ~ predicted, data = eval_data_caret, method = "knn", trControl = trainControl(method = "cv", number = 3))

# Выведите результаты оценки
summary(model_k3_eval)

# Предсказания на eval данных
predictions_k3_eval <- predict(model_k3_eval, newdata = eval_data_caret)
predictions_k3_eval

# Рассчитайте MAPE для оценки производительности модели на eval данных
MAPE_k3_eval <- mean(abs((eval_data_caret$actual - predictions_k3_eval) / eval_data_caret$actual)) * 100

# Выведите результат
cat("MAPE for k=3 on eval is", round(MAPE_k3_eval, 4), "%\n")

# Создайте данные для графика
plot_data_k3_eval <- data.frame(actual = eval_data_caret$actual, predicted = predictions_k3_eval)

# Создайте график с точечным разбросом
ggplot(plot_data_k3_eval, aes(x = actual, y = predicted)) +
  geom_point(color = "darkgreen", size = 2, alpha = 0.5) +
  geom_smooth(method = "loess", color = "green", se = FALSE, size = 1.5) +
  geom_abline(intercept = 0, slope = 1, color = "darkorange", linetype = "dashed", size = 1.5) +
    labs(title = "Actual vs Predicted (k=3, Eval Data)",
       x = "Actual Income",
       y = "Predicted Income") +
  theme_minimal() +
  xlim(c(0, max(plot_data_k3_eval$actual, plot_data_k3_eval$predicted))) +
  ylim(c(0, max(plot_data_k3_eval$actual, plot_data_k3_eval$predicted)))

# График плотности eval
ggplot(plot_data_k3_eval, aes(x = actual, y = predicted)) +
  geom_point() +
  geom_hex(bins = 30) +
  geom_abline(intercept = 0, slope = 1, color = "darkorange", linetype = "dashed", size = 1.5) +
  labs(title = "Actual vs Predicted (k=3, Eval Data)",
       x = "Actual Income",
       y = "Predicted Income") +
  theme_minimal() +
  xlim(c(0, max(plot_data_k3_eval$actual, plot_data_k3_eval$predicted))) +
  ylim(c(0, max(plot_data_k3_eval$actual, plot_data_k3_eval$predicted)))

