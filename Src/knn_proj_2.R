rm(list=ls(all=TRUE))
install.packages("caret")
install.packages("plotrix")
install.packages("rlang", dependencies=TRUE)
install.packages("vctrs")
install.packages("confusionMatrix")
install.packages('e1071', dependencies=TRUE)
install.packages('caret', dependencies=TRUE)
#library(vctrs)

# Загрузите библиотеку
library(plotrix)
library("RCurl")
data_full=read.table(text = getURL("https://raw.githubusercontent.com/rajsiddarth119/Datasets/master/Bank_dataset.csv"), header=T, sep=',',
                col.names = c('ID', 'age', 'exp', 'inc', 
                              'zip', 'family', 'ccavg', 'edu', 
                              'mortgage', 'loan', 'securities', 
                              'cd', 'online', 'cc'))
str(data)
#Remove ID,exp and zip
data=subset(data_full,select = -c(ID,exp,zip))

#Converting age,inc,CCavg,Mortgage,family to numeric and standardizing
num_atr=c("age","inc","ccavg","mortgage","family")
dep_atr=c("loan")
categ_atr=setdiff(names(data),c(num_atr,dep_atr))

num_data=data.frame(sapply(data[num_atr], function(x){as.numeric(x)}))
library(vegan)
num_data=decostand(num_data,method = "range")

# Создаем фиктивные переменные для категориальных атрибутов
dummy_variables <- dummyVars(~., data = data[categ_atr])
categ_data_caret <- predict(dummy_variables, newdata = data)

# Создаем фактор для переменной "loan"
loan <- as.factor(data$loan)

# Объединяем числовые, фиктивные и факторные переменные в один набор данных
final_data <- cbind(num_data, categ_data_caret, loan)
str(final_data)

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

#Classification using K nearest neighbour
#install.packages("class")
# Указываем метрику расстояния
distance_metric <- "Euclidean"
library(class)
library(caret)
library(pROC)

#Uses euclidean distance metric
# ...........Using k = 1....................#
#Predicting on train data set itslef
ind_variables=setdiff(names(final_data),"loan")
pred1_train = knn(train_data[,ind_variables],train_data[,ind_variables],
                  cl = train_data$loan,k = 1)

cmatrix1_train = table("predicted"=pred1_train, "actual"=train_data$loan)
cmatrix1_train
accu_train1= sum(diag(cmatrix1_train))/sum(cmatrix1_train)
cat("accuracy on train data for k=1 is",round(accu_train1*100,2),"%")

#Predicting on test data
pred1_test = knn(train_data[,ind_variables],test_data[,ind_variables],
                 cl = train_data$loan,k=1)

cmatrix1_test = table("predicted"=pred1_test,"actual"= test_data$loan)
cmatrix1_test
accu_test1= sum(diag(cmatrix1_test))/sum(cmatrix1_test)
cat("accuracy on test data for k=1 is",round(accu_test1*100,2),"%")

# ------------- График_1 -------------
# Преобразуйте матрицу в датафрейм
df_cmatrix_1 <- as.data.frame(cmatrix1_test)

# Постройте график
ggplot(df_cmatrix_1, aes(x = actual, y = Freq, fill = predicted)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Confusion histogram, Using k = 1",
       x = "Actual",
       y = "Count") +
  scale_fill_manual(values = c("darkgreen", "darkorange")) +
  geom_text(aes(label = Freq), position = position_dodge(width = 0.9), vjust = -0.5)


#.......... k = 3.......................# 
pred3_train = knn(train_data[,ind_variables],train_data[,ind_variables],
                  cl = train_data$loan,k = 3)

cmatrix3_train = table("predicted"=pred3_train, "actual"=train_data$loan)
accu_train3= sum(diag(cmatrix3_train))/sum(cmatrix3_train)
cat("accuracy on train data for k=3 is",round(accu_train3*100,2),"%")

#Predicting on test data
pred3_test = knn(train_data[,ind_variables],test_data[,ind_variables],
                 cl = train_data$loan,k=3)

cmatrix3_test = table("predicted"=pred3_test,"actual"= test_data$loan)
accu_test3= sum(diag(cmatrix3_test))/sum(cmatrix3_test)
cat("accuracy on test data for k=3 is",round(accu_test3*100,2),"%")

# ------------- График_3 -------------
# Преобразуйте матрицу в датафрейм
df_cmatrix_3 <- as.data.frame(cmatrix3_test)

# Постройте график
ggplot(df_cmatrix_3, aes(x = actual, y = Freq, fill = predicted)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Confusion Matrix, k=3",
       x = "Actual",
       y = "Count") +
  scale_fill_manual(values = c("darkgreen", "orange")) +
  geom_text(aes(label = Freq), position = position_dodge(width = 0.9), vjust = -0.5)



#.......... k = 5.................# 
pred5_train = knn(train_data[,ind_variables],train_data[,ind_variables],
                  cl = train_data$loan,k = 5)

cmatrix5_train = table("predicted"=pred5_train, "actual"=train_data$loan)
accu_train5= sum(diag(cmatrix5_train))/sum(cmatrix5_train)
cat("accuracy on train data for k=5 is",round(accu_train5*100,2),"%")

#Predicting on test data
pred5_test = knn(train_data[,ind_variables],test_data[,ind_variables],
                 cl = train_data$loan,k=5)

cmatrix5_test = table("predicted"=pred5_test,"actual"= test_data$loan)
accu_test5= sum(diag(cmatrix5_test))/sum(cmatrix5_test)
cat("accuracy on test data for k=5 is",round(accu_test5*100,2),"%")

# ------------- График_5 -------------
# Преобразуйте матрицу в датафрейм
df_cmatrix_5 <- as.data.frame(cmatrix5_test)

# Постройте график
ggplot(df_cmatrix_5, aes(x = actual, y = Freq, fill = predicted)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Confusion Matrix",
       x = "Actual",
       y = "Count") +
  scale_fill_manual(values = c("darkgreen", "orange")) +
  geom_text(aes(label = Freq), position = position_dodge(width = 0.9), vjust = -0.5)

#.......... k = 7.................# 
pred7_train = knn(train_data[,ind_variables],train_data[,ind_variables],
                  cl = train_data$loan,k = 7)

cmatrix7_train = table("predicted"=pred7_train, "actual"=train_data$loan)
accu_train7= sum(diag(cmatrix7_train))/sum(cmatrix7_train)
cat("accuracy on train data for k=7 is",round(accu_train7*100,2),"%")

#Predicting on test data
pred7_test = knn(train_data[,ind_variables],test_data[,ind_variables],
                 cl = train_data$loan,k=7)

cmatrix7_test = table("predicted"=pred7_test,"actual"= test_data$loan)
accu_test7= sum(diag(cmatrix7_test))/sum(cmatrix7_test)
cat("accuracy on test data for k=7 is",round(accu_test7*100,2),"%")

# ------------- График_7 -------------
# Преобразуйте матрицу в датафрейм
df_cmatrix_7 <- as.data.frame(cmatrix7_test)

# Постройте график
ggplot(df_cmatrix_7, aes(x = actual, y = Freq, fill = predicted)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Confusion Matrix",
       x = "Actual",
       y = "Count") +
  scale_fill_manual(values = c("darkgreen", "orange")) +
  geom_text(aes(label = Freq), position = position_dodge(width = 0.9), vjust = -0.5)



#After checking accuracies finally decided on k=5 .Did not use cross validation.
# Using Condensation to find the points that define decision surfaces

final_points = condense(train_data, train_data$loan)
length(final_points)

#Using k=5 and condensed points 
prediction = knn(train_data[final_points,ind_variables],test_data[,ind_variables], 
                 train_data$loan[final_points], k=5)
conf_matrix= table(prediction, test_data$loan)
conf_matrix
accuracy=sum(diag(conf_matrix))/sum(conf_matrix)

cat("Accuracy on test data for k=5 and using condensed points is",round(accuracy*100,2),"%")

# Testing model performance on evaluation data 
predict_eval = knn(train_data[final_points,ind_variables], eval_data[,ind_variables], 
                   train_data$loan[final_points], k=5)
cmatrix= table("predicted"=predict_eval, "actual"=eval_data$loan)
cmatrix
accuracy=sum(diag(cmatrix))/sum(cmatrix)
cat("Accuracy on evaluation data for k=5 and using condensed points is",round(accuracy*100,2),"%")

# Преобразуйте матрицу в датафрейм
df_cmatrix_eval <- as.data.frame(cmatrix)

# Постройте график
ggplot(df_cmatrix_eval, aes(x = actual, y = Freq, fill = predicted)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Confusion Matrix",
       x = "Actual",
       y = "Count") +
  scale_fill_manual(values = c("darkgreen", "orange")) +
  geom_text(aes(label = Freq), position = position_dodge(width = 0.9), vjust = -0.5)

