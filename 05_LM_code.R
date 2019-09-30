# ---
# title: "Линейная регрессия"
# subtitle: "Линейные модели..."
# author: "Вадим Хайтов, Марина Варфоломеева"

## Читаем данные
brain <- read.csv("data/IQ_brain.csv", header = TRUE)
head(brain)


## Находим строки с пропущенными значениями
sum(!complete.cases(brain))

colSums(is.na(brain))

brain[!complete.cases(brain), ]



#Вычисление матрицы корреляций





library(ggplot2)
## Строим график

pl_brain <-

pl_brain





## Подгоняем модель с помощью функции lm()
brain_model <-





## Прогнозируем величину IQ для человека с размером мозга 900000




#Находим остатки



##Находим доверительные интервалы для параметров




##Рисуем графики для разных уровней значимости
pl_alpha1 <- pl_brain + geom_smooth(method="lm", level=0.8) + ggtitle(bquote(alpha==0.2))

pl_alpha2 <- pl_brain + geom_smooth(method="lm", level=0.95) + ggtitle(bquote(alpha==0.05))

pl_alpha3 <- pl_brain + geom_smooth(method="lm", level=0.999) + ggtitle(bquote(alpha==0.01))


grid.arrange(pl_alpha1, pl_alpha2, pl_alpha3, ncol=3)


## Вычисляем 95%  зону предсказания

newdata <- data.frame(MRINACount = 900000)

predict(brain_model, newdata, interval = "prediction", level = 0.95, se = TRUE)$fit


## Строим график с зоной педсказаия
brain_predicted <- predict(brain_model, interval="prediction")
brain_predicted <- data.frame(brain, brain_predicted)
head(brain_predicted)

pl_brain +

  # 1) Линия регрессии и ее дов. интервал
  # Если мы указываем fill внутри aes() и задаем фиксированное значение - появится соотв. легенда с названием.
  # alpha - задает прозрачность
  geom_smooth(method = "lm", aes(fill = "Conf.interval"), alpha = 0.4) +
  # 2) Интервал предсказаний создаем при помощи геома ribbon ("лента")
  # Данные берем из другого датафрейма - из brain_predicted
  # ymin и ymax - эстетики геома ribbon, которые задают нижний и верхний край ленты в точках с заданным x (x = MRINACount было задано в ggplot() при создании pl_brain, поэтому сейчас его указывать не обязательно)
  geom_ribbon(data = brain_predicted,  aes(ymin = lwr, ymax = upr, fill = "Conf. area for prediction"), alpha = 0.2) +

  # 3) Вручную настраиваем цвета заливки при помощи шкалы fill_manual.
  # Ее аргумент name - название соотв. легенды, values - вектор цветов
  scale_fill_manual(name = "Intervals", values = c("green", "gray")) +

  # 4) Название графика
  ggtitle("Confidence interval \n and confidence area for prediction")
