# "Введение в регрессионный анализ"
# Вадим Хайтов
# Каф. Зоологии беспозвоночных, СПбГУ
# Линейные модели на R, осень 2015


# Вам понадобятся пакеты ggplot2, ppcor
# install.packages("ggplot2")
# install.packages("ppcor")

options(digits=3)
brain <- read.csv("IQ_brain.csv", header = T)

#Здесь должен появиться Ваш код для корреляционного анализа





# Вычисляем частные корреляции
library(ppcor)
brain_complete <- brain[complete.cases(brain),]
pcor.test(brain_complete$PIQ, brain_complete$MRINACount, brain_complete$Height, )



# Подбираем модель для зависимост IQ от размера головного мозга
brain_model <- lm(PIQ ~ MRINACount, data = brain)
brain_model

#Здесь должен появиться Ваш код для ответа на следующие вопросы
# 1. Чему равны угловой коэффициент и свободный член полученной модели brain_model?



# 2. Какое значеие IQ-теста предсказывает модель для человека с объемом  мозга равным 900000




# 3. Чему равно значение остатка от модели для человека с порядковым номером 10






# Проводим более детальный анализ модели
summary(brain_model)

# Графическое представление результатов. Постройте график при разных значениях параметра level
library(ggplot2)
pl_brain + geom_smooth(method="lm", level=0.99)

confint(brain_model)

# Оцениваем ожидаемое значение IQ для человека с заданным размером головного мозга

newdata <- data.frame(MRINACount = 900000)

predict(brain_model, newdata, interval = "confidence", level = 0.95, se = TRUE)


# Отражаем на графике область значений, в которую попадут 95% предсказанных величин IQ

# Подготавлваем данные
brain_predicted <- predict(brain_model, interval="prediction")
brain_predicted <- data.frame(brain, brain_predicted)
head(brain_predicted)

# Отражаем на графике область значений, в которую попадут 95% предсказанных величин IQ
pl_brain +
  geom_smooth(method = "lm", aes(fill = "Conf.interval"), alpha = 0.4) +
  geom_ribbon(data = brain_predicted,  aes(ymin = lwr, ymax = upr, fill = "Conf. area for prediction"), alpha = 0.2) +
  scale_fill_manual(name = "Intervals", values = c("green", "gray")) +
  ggtitle("Confidence interval \n and confidence area for prediction")
# Комментарии к графику:
# 1) geom_smooth - Линия регрессии и ее дов. интервал
# Если мы указываем fill внутри aes() и задаем фиксированное значение - появится соотв. легенда с названием.
# alpha - задает прозрачность
# 2) geom_ribbon - Интервал предсказаний создаем при помощи геома ribbon ("лента")
# Данные берем из другого датафрейма - из brain_predicted
# ymin и ymax - эстетики геома ribbon, которые задают нижний и верхний край ленты в точках с заданным x (x = MRINACount было задано в ggplot() при создании pl_brain, поэтому сейчас его указывать не обязательно)
# 3) scale_fill_manual - Вручную настраиваем цвета заливки при помощи шкалы fill_manual.
# Ее аргумент name - название соотв. легенды, values - вектор цветов
# 4) ggtitle - Название графика
