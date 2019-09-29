# Данные из работы
# Secher et al. (1987), European Journal of Obstetrics, Gynecology, and Reproductive Biology, 24: 1–11.
#

# Данные представлены в пакете {ISwR}

# На УЗИ у плода измеряли  межтеменной и абдоминальный диаметр.
# Вопрос:
#   Можно ли предсказать вес новорожденного по данным УЗИ?
#
#
#
#
# Задание:
#   1. Постройте модель, описывающую связь веса новорожденного, с размером межтеменного диаметра
#
#
# Переменные
# bwt – вес новорожденного (г)
# bpd – Межтеменной диаметр (мм)



library(readxl)
library(ggplot2)



baby <- read_excel("data/secher.xls")
head(baby)


# Строим модель

baby_M1

# Результаты





# Визуализация модели без использования geom_smooth()

# Создаем искусственный датафрейм, в котором будут все возможные (не только измеренные) значения предиктора

MyData <- data.frame(bpd = seq(min(baby$bpd), max(baby$bpd), by = 0.1) )


# Вычисляем для всех возможных значений предиктора величину зависимой переменной, в соответствии с моделью

MyData$Predicted <- predict(baby_M1, newdata = MyData, se.fit = TRUE )$fit

# Вычисляем значения стандартной ошибки для каждой точки
MyData$SE <- predict(baby_M1, newdata = MyData, se.fit = TRUE)$se.fit


# Рисуем линию, предсказанную моделью

Pl_predicted <- ggplot(MyData, aes(x = bpd, y = Predicted)) + geom_line(size = 2, color = "blue")

Pl_predicted

# Наносим на рисунок линии, соотвествующие доверительному интервалу

Pl_predicted_2 <- Pl_predicted + geom_line(aes(y = Predicted - 1.96*SE), linetype = 2, color = "red") + geom_line(aes(y = Predicted + 1.96*SE), linetype = 2, color = "red")

Pl_predicted_2

# Вписываем в рисунок исходные данные




# Рисуем диапазон предсказания

baby_predict_diap <- predict(baby_M1, newdata = MyData, interval="prediction")
baby_predict_diap <- as.data.frame(baby_predict_diap) [ , -1]

MyData <- cbind(MyData, baby_predict_diap)


Pl_predicted_3 + geom_ribbon(data = MyData, aes(ymin = lwr, ymax = upr, fill = "Conf. area for prediction"), alpha = 0.2, fill = "green")


######################################################
#          Здания для работы по группам              #
######################################################

#
# 1. Подберите модель, описывающую связь между зависимой переменной и предложенным предиктором.
# 2. Выведите числовые результаты подбора модели.
# 3. Глядя на числовые результаты, опишите словами, как изменяется переменная отклика в зависимости  от предиктора
# 4. Визуализируйте полученную модель, изобразив на графике первичные данные, линию регрессии, доверительный интервал регрессии и интервал предсказаний.

#
# Группа № 1
# датасет iris для вида setosa
# переменная отклка: Sepal.Length
# предиктор: Petal.Length


# Группа № 2
# датасет diamonds для группы Good
# переменная отклка: price
# предиктор: carat



# Группа № 3
# датасет cats {MASS} для группы M
# переменная отклка: Hwt
# предиктор: Bwt


# Группа № 4
# датасет crabs {MASS} для группы sp = B & sex = M
# переменная отклка: CW
# предиктор: CL



# Группа № 5
# датасет geyser {MASS}
# переменная отклка: duration
# предиктор: waiting

