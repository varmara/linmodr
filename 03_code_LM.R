# ---
# title: "Линейная регрессия"
# subtitle: "Линейные модели..."
# author: "Вадим Хайтов, Марина Варфоломеева"

## Читаем данные
brain <- read.csv("data/IQ_brain.csv", header = TRUE)
head(brain)
str(brain)

## Находим строки с пропущенными значениями
sum(!complete.cases(brain))

sapply(brain, function(x) sum(is.na(x)))

brain[!complete.cases(brain), ]


#Вычисление матрицы корреляций

cor(brain$Weight, brain$PIQ, use =  "pairwise.complete.obs" )

cor(brain[,-1], use =  "pairwise.complete.obs" )

cor.test(brain[,2], brain[,3])

library(ggplot2)
## Строим график
pl_brain <- ggplot(brain, aes(x = MRINACount, y = PIQ)) + geom_point() +   xlab("Brain size") + ylab("IQ test")
pl_brain

## Меняем настройки графика
pl_brain + theme_dark() +
  geom_point(aes(color = Gender), size = 4) +
  scale_color_manual(values =  c("red", "blue"))

cor.test(brain$PIQ, brain$MRINACount)

## Подгоняем модель с помощью функции lm()
Mod <- lm(PIQ ~ MRINACount, data = brain)

lm(PIQ ~ 1, data = brain)



## Прогнозируем величину IQ для человека с размером мозга 900000
1.7437570 +  0.0001203 * 900000

coef(Mod)[1] + coef(Mod)[2]* 900000


#Находим остатки
brain$PIQ[10] - fitted(Mod)[10]
residuals(Mod)[10]

str(Mod)

summary(Mod)

pl_brain + geom_smooth(method = "lm", fill = "red")

##Находим доверительные интервалы для параметров

confint(Mod)

##Рисуем графики для разных уровней значимости
library(gridExtra)
pl_alpha1 <- pl_brain + geom_smooth(method="lm", level=0.8, fill = "red") + ggtitle(bquote(alpha==0.2))

pl_alpha2 <- pl_brain + geom_smooth(method="lm", level=0.95 , fill = "red") + ggtitle(bquote(alpha==0.05))

pl_alpha3 <- pl_brain + geom_smooth(method="lm", level=0.999,  fill = "red") + ggtitle(bquote(alpha==0.01))


grid.arrange(pl_alpha1, pl_alpha2, pl_alpha3, ncol=3)


## Вычисляем 95%  зону предсказания
ND <- data.frame(MRINACount = 900000)

predict(Mod, newdata = ND , interval = "prediction", level = 0.95, se = T)$fit


## Строим график с зоной педсказаия
brain_predicted <- predict(Mod, interval="prediction")
brain_predicted <- data.frame(brain, brain_predicted)
head(brain_predicted)

pl_brain +  geom_smooth(method = "lm", aes(fill = "Conf.interval"), alpha = 0.4) +
geom_ribbon(data = brain_predicted,  aes(ymin = lwr, ymax = upr, fill = "Conf. area for prediction"), alpha = 0.3) + scale_fill_manual(name = "Intervals", values = c("black", "yellow")) +   ggtitle("Confidence interval \n and confidence area for prediction")

summary(Mod)


anova(Mod)
