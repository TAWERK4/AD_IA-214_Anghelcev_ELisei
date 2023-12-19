# Загрузка необходимых библиотек
rm(list=ls(all=TRUE))
install.packages("corrplot")
install.packages("vctrs")
install.packages("dplyr")


library(tidyverse)
library(corrplot)
library(dplyr)

data_full=read.table(text = getURL("https://raw.githubusercontent.com/rajsiddarth119/Datasets/master/Bank_dataset.csv"), header=T, sep=',',
                     col.names = c('ID', 'age', 'exp', 'inc', 
                                   'zip', 'family', 'ccavg', 'edu', 
                                   'mortgage', 'loan', 'securities', 
                                   'cd', 'online', 'cc'))

#Remove ID,exp and zip
data=subset(data_full,select = -c(ID,zip))


# Посмотрим на структуру данных
str(data)

# Первые несколько строк данных
head(data)

# Основные статистические показатели
summary(data)

# Распределение целевой переменной
table(data$loan)

# ++ Корреляционная матрица
cor_matrix <- cor(data[, c("age", "exp", "inc", "family", "ccavg", 'edu', 
                           'mortgage', 'loan', 'securities', 
                           'cd', 'online', 'cc')])
corrplot::corrplot(cor_matrix, method = "circle")

# Диаграмма рассеяния для дохода и расходов при выплачивании кредита 
ggplot(data, aes(x = inc, y = ccavg)) +
  geom_point(aes(color = factor(loan)), alpha = 0.5) +
  labs(title = "Диаграмма рассеяния для дохода и расходов при выплачивании кредита", x = "Доход", y = "Средние траты по кредитной карте") +
  scale_color_manual(values = c("0" = "darkgreen", "1" = "darkorange")) +  # Устанавливаем цвета для значений 0 и 1
  theme_minimal()

# ++ Диаграмма рассеяния для возраста и опыта работы
ggplot(data, aes(x = age, y = exp)) +
  geom_point(aes(color = factor(loan)), alpha = 0.7) +
  labs(title = "Диаграмма рассеяния для возраста и опыта работы", x = "Возраст", y = "Опыт работы") +
  theme_minimal()


# ++ Построение ящика с усами для дохода в зависимости от уровня образования (без выбросов)
ggplot(filtered_data, aes(x = factor(edu), y = inc, group = factor(edu), fill = factor(edu))) +
  geom_boxplot(color = "blue", alpha = 0.7) +
  geom_jitter(position = position_jitter(width = 0.2), aes(color = factor(cd)), size = 2, alpha = 0.5) +
  scale_fill_manual(values = c("1" = "lightblue", "2" = "lightgreen", "3" = "lightpink"),
                    name = "Уровень образования",
                    labels = c("1" = "Студент бакалавра", "2" = "Выпускник", "3" = "Профессиональный")) +
  scale_color_manual(values = c("0" = "darkorange", "1" = "darkgreen"),
                     name = "Наличие депозита",
                     labels = c("0" = "Нет", "1" = "Есть")) +
  labs(title = "Доход относительно уровня образования", x = "Уровень образования", y = "Доход") +
  theme_minimal()



# ++ Гистограмма размера семьи и уровня образования
ggplot(data, aes(x = family, fill = factor(edu))) +
  geom_bar(position = "stack") +
  labs(title = "Гистограмма размера семьи и уровня образования", x = "Размер семьи", y = "Количество", fill = "Уровень образования") +
  theme_minimal()


# ++График для распределения заработка и возраста в зависимости от стоимости ипотеки (не учитывать значения = 0)
ggplot(data[data$mortgage > 0, ], aes(x = mortgage, y = inc, color = age)) +
  geom_point() +
  scale_color_gradient(low = "darkgreen", high = "darkorange") +
  labs(title = "Заработок и возраст в зависимости от стоимости ипотеки",
       x = "Цена ипотеки",
       y = "Доход",
       color = "Возраст") +
  theme_minimal()


# ++ Диаграмма для использования онлайн-банкинга в зависимости от возраста
ggplot(data, aes(x = age, fill = factor(online))) +
  geom_bar(position = "stack") +
  labs(title = "Использование онлайн-банкинга в зависимости от возраста", x = "Возраст", y = "Количество", fill = "Online Banking") +
  scale_fill_manual(values = c("darkgreen", "darkorange")) +
  theme_minimal()


# ++ Ящик с усами для распределения ccavg (от 0 до 5.5) в зависимости от использования кредитных карт
ggplot(data, aes(x = factor(cc), y = ccavg)) +
  geom_boxplot(fill = "lightyellow", color = "darkorange") +
  geom_jitter(data = subset(data, mortgage > 0), aes(x = factor(cc), y = ccavg, color = "Mortgage"), 
              position = position_jitter(width = 0.2), size = 2, alpha = 0.7) +
  labs(title = "Средние затраты людей с наличием кред.карты и ипотеки", 
       x = "Наличие кредитной карты", 
       y = "Средние траты по кредитной карте", 
       color = "Категория") +
  scale_color_manual(values = c("Mortgage" = "darkgreen"), name = "Категория") +
  theme_minimal() +
  ylim(0, 10)




# Удаление строк с NaN и Inf в столбце ccavg
data_cleaned <- data[is.finite(data$ccavg), ]

# График точек и ящиков средних трат по кредитной карте в зависимости от использования кредитных карт
ggplot(data_cleaned, aes(x = factor(cc), y = ccavg, color = factor(online))) +
  geom_boxplot(fill = "lightyellow", color = "darkorange", alpha = 0.7) +
  geom_jitter(position = position_jitter(width = 0.2), size = 2, alpha = 0.3) +
  labs(title = "BoxPlot и Strip Plot средних трат по кредитной карте в зависимости от использования кредитных карт и онлайн-банка",
       x = "Использование кредитной карты",
       y = "Средние траты по кредитной карте",
       color = "Использ. интернет-банка") +
  scale_color_manual(values = c("darkorange", "darkgreen")) +
  theme_minimal()


# График точек средних трат по кредитной карте в зависимости от уровня образования и статуса ценных бумаг
ggplot(data_cleaned, aes(x = factor(edu), y = exp, color = factor(securities))) +
  geom_boxplot(fill = "lightyellow", color = "darkorange", alpha = 0.7) +
  geom_jitter(position = position_jitter(width = 0.2), size = 2, alpha = 0.4) +
  labs(title = "BoxPlot и Strip Plot средних трат по кредитной карте в зависимости от уровня образования и наличия ценных бумаг",
       x = "Уровень образования",
       y = "Опыт профессиональной работы",
       color = "Наличие ценных бумаг") +
  scale_color_manual(values = c("darkorange", "darkgreen")) +
  theme_minimal()



# Совмещенный график boxplot и stripplot с прозрачными серыми точками для переменных "edu" и "inc"
ggplot(data_cleaned, aes(x = factor(edu), y = inc, fill = factor(family))) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.shape = NA) +
  geom_jitter(position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.2), aes(color = factor(family)), size = 2, alpha = 0.4) +
  labs(title = "Совмещенный график Boxplot и Stripplot для ежегодного дохода по уровню образования и размеру семьи",
       x = "Уровень образования",
       y = "Ежегодный доход",
       fill = "Размер семьи") +
  theme_minimal() +
  scale_fill_manual(values = c("lightpink", "lightgreen", "lightblue", "lightyellow")) +
  scale_color_manual(values = c("darkred", "darkgreen", "darkblue", "yellow")) +
  guides(fill = guide_legend(title = "Размер семьи"), color = guide_legend(title = "Размер семьи")) +
  theme(legend.position = "bottom")


# Совмещенный график boxplot и stripplot с прозрачными серыми точками для переменных "edu" и "ccavg"
ggplot(data_cleaned, aes(x = factor(edu), y = ccavg, fill = factor(family))) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.shape = NA) +
  geom_jitter(position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.2), aes(color = factor(family)), size = 2, alpha = 0.2) +
  labs(title = "Совмещенный график Boxplot и Stripplot для средних затрат по уровню образования и размеру семьи",
       x = "Уровень образования",
       y = "Средние затраты по кредитной карте",
       fill = "Размер семьи") +
  theme_minimal() +
  scale_fill_manual(values = c("lightpink", "lightgreen", "lightblue", "lightyellow")) +
  scale_color_manual(values = c("darkred", "darkgreen", "darkblue", "yellow")) +
  guides(fill = guide_legend(title = "Размер семьи"), color = guide_legend(title = "Размер семьи")) +
  theme(legend.position = "bottom")
