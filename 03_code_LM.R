######################################
#Введение в регрессионый анализ
#Линейные модели на R
#Вадим Хайтов, Марина Варфоломеева
######################################


## Читаем данные
brain <- read.csv("data/IQ_brain.csv", header = TRUE)
head(brain)


## Находим строки с пропущенными значениями
sum(!complete.cases(brain))

sapply(brain, function(x) sum(is.na(x)))

brain[!complete.cases(brain), ]


#Вычисление матрицы корреляций


## Строим график
pl_brain <- ggplot(brain, aes(x = MRINACount, y = PIQ)) + geom_point() +   xlab("Brain size") + ylab("IQ test")
pl_brain

## Меняем настройки графика
pl_brain + theme_dark() +
  geom_point(aes(color = Gender), size = 4) +
  scale_color_manual(values =  c("red", "blue"))


## Подгоняем модель с помощью функции lm()





## Прогнозируем величину IQ для человека с размером мозга 900000


#Находим остатки
brain$PIQ[10] - fitted(brain_model)[10]
residuals(brain_model)[10]


##Находим доверительные интервалы для параметров

confint(brain_model)

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

pl_brain +  geom_smooth(method = "lm", aes(fill = "Conf.interval"), alpha = 0.4) +
geom_ribbon(data = brain_predicted,  aes(ymin = lwr, ymax = upr, fill = "Conf. area for prediction"), alpha = 0.2) + scale_fill_manual(name = "Intervals", values = c("green", "gray")) +   ggtitle("Confidence interval \n and confidence area for prediction")

