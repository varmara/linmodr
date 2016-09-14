---
title       : Построение моделей при нестабильности дисперсии остатков
subtitle    : Линейные модели, осень 2015
author: Вадим Хайтов, Марина Варфоломеева

#Моделирование гетерогенности дисперсии
## Способствуют ли взрослые мидии притоку молоди?  (Khaitov, 2013)
myt <- read.table("myt.csv", sep=";", header =T)
head(myt, 12)

##В качестве зависимой перменной будем анализировать $\sqrt{N_{recruits}}$
myt$Sq_Recruits <- sqrt(myt$Recruits)
myt$fYear <- factor(myt$Year)

##Строим обычную регрессионную модель {.smaller}
mod_formula <- formula(Sq_Recruits ~ Large +  fYear + Bank  +  Large:fYear + Large:Bank )

M1_lm <- lm(mod_formula , data = myt)

anova(M1_lm)


##Проведем диагностику данной модели
library(ggplot2)
library(gridExtra)

diag_M1_lm <- fortify(M1_lm)

Res_plot1 <- ggplot(diag_M1_lm, aes(x=.fitted, y = .stdresid)) + geom_point() + geom_hline(yintercept = 0) + geom_smooth(se = F)

Res_plot2 <- ggplot(diag_M1_lm, aes(x=Large, y = .stdresid)) + geom_point() + geom_hline(yintercept = 0)+ geom_smooth(se = F)

Res_plot3 <- ggplot(diag_M1_lm, aes(x=fYear, y = .stdresid)) + geom_boxplot() + geom_hline(yintercept = 0) + theme(axis.text.x = element_text(angle = 90))

Res_plot4 <- ggplot(diag_M1_lm, aes(x=Bank, y = .stdresid)) + geom_boxplot() + geom_hline(yintercept = 0)

grid.arrange(Res_plot1, Res_plot2, Res_plot3, Res_plot4, ncol = 2)




#Различные формы структуры дисперсии
##Для дальнейших вычислений необходимо использовать функцию `gls` из пакета `nlme`
# Эта функция дает результаты полностью идентичные результатам функции `lm()`
library(nlme)
M1_gls <- gls(mod_formula, data = myt )
anova(M1_gls)


##Фиксированная структура дисперсии
M2_gls <- gls(mod_formula, data = myt, weights = varFixed( ~ Large))
##Сравним две модели
AIC(M1_gls, M2_gls)


##Разные дисперсии для разных уровней категориальных предикторов
M3_gls <- gls(mod_formula, data = myt, weights = varIdent(form = ~1|fYear))

##Сравнение моделей
anova(M1_gls, M3_gls)

##У нас два категориальных предиктора
M3_gls2 <- gls(mod_formula, data = myt,
               weights = varIdent(form = ~1|Bank))
anova(M1_gls, M3_gls2)

#Что произошло в результате работы функции `varIdent()`?
summary(M3_gls)

###Часть вывода `summary(M3_gls)`
# Variance function:
#  Structure: Different standard deviations per stratum
#  Formula: ~1 | fYear
#  Parameter estimates:
#  1997  1999  2000  2001  2002  2003  2004  2005  2006  2007  2008
#  1.00  2.62  4.39  3.47  2.84  5.85  4.93  3.21  2.95  3.87  7.98
#  2009  2010  2011
#  9.26  5.97 13.59


##Степенная зависимость дисперсии от ковариаты
M4_gls <- gls(mod_formula, data = myt, weights = varPower(form = ~ Large))
# Оценка параметра $\delta$
M4_gls$modelStruct


##Задание
# Степенная зависимость дисперсии от ковариаты может учитывать и взаимодействие ковариаты дисперсии с категориальными предикторами
### Напишите код, с помощью которого в модели будет учтена степенная зависимость  дисперсии от переменной `Large`, но разная для каждого уровня фактора `fYear`.  Аналогичный код напишите для фактора `Bank`
# *Hint* Изучите справку по функции `varPower()`

##Решение




##Усложненная степенная зависимость дисперсии от ковариаты
M10_gls <- gls(mod_formula, data = myt,
               weights = varConstPower(form = ~ Large))
#M11_gls <-gls(mod_formula, data = myt,
#               weights = varConstPower(form = ~ Large|fYear))
M12_gls <- gls(mod_formula, data = myt,
               weights = varConstPower(form = ~ Large|Bank))

##Оцененные параметры
M10_gls$modelStruct
M12_gls$modelStruct



##Комбинированная структура дисперсии
M13_gls <- gls(mod_formula, data = myt,
               weights = varComb(varIdent(form = ~ fYear),
                                 varPower(form = ~ Large)))
M14_gls <- gls(mod_formula, data = myt,
               weights = varComb(varIdent(form = ~ Bank),
                                 varPower(form = ~ Large)))
M15_gls <- gls(mod_formula, data = myt,
               weights = varComb(varIdent(form = ~ fYear),
                                 varExp(form = ~ Large)))
M16_gls <- gls(mod_formula, data = myt,
               weights = varComb(varIdent(form = ~ Bank),
                                 varExp(form = ~ Large)))


##Задание
###Найдите модель с наилучшей структурой дисперсии

##Решение





##Диагностика модели с оптимальной структурой дисперсии
diag_gls <- data.frame(.pears_resid = residuals(M5_gls, type = "pearson"), .fitted = fitted(M5_gls), Large = myt$Large, fYear = myt$fYear, Bank = myt$Bank)


Diag_gls_plot1 <- ggplot(diag_gls, aes(x=.fitted, y = .pears_resid)) + geom_point() + geom_hline(yintercept = 0) + geom_smooth(se = F)

Diag_gls_plot2 <- ggplot(diag_gls, aes(x=Large, y = .pears_resid)) + geom_point() + geom_hline(yintercept = 0)+ geom_smooth(se = F)

Diag_gls_plot3 <- ggplot(diag_gls, aes(x=fYear, y = .pears_resid)) + geom_boxplot() + geom_hline(yintercept = 0)

Diag_gls_plot4 <- ggplot(diag_gls, aes(x=Bank, y = .pears_resid)) + geom_boxplot() + geom_hline(yintercept = 0)

Res_plot1 <- Res_plot1 + ggtitle("Было \nв начальной модели")
Diag_gls_plot1 <- Diag_gls_plot1 + ggtitle("Стало после моделирования \nструктуры дисперсии")

grid.arrange(Res_plot1, Diag_gls_plot1, ncol = 2)


##Диагностика модели с оптимальной структурой дисперсии
Res_plot2 <- Res_plot2 + ggtitle("Было \nв начальной модели")
Diag_gls_plot2 <- Diag_gls_plot2 + ggtitle("Стало после моделирования \nструктуры дисперсии")
grid.arrange(Res_plot2, Diag_gls_plot2, ncol = 2)


##Диагностика модели с оптимальной структурой дисперсии
Res_plot3 <- Res_plot3 + ggtitle("Было \nв начальной модели")
Diag_gls_plot3 <- Diag_gls_plot3 + ggtitle("Стало после моделирования \nструктуры дисперсии")
grid.arrange(Res_plot3, Diag_gls_plot3, ncol = 2)


##Диагностика модели с оптимальной структурой дисперсии
Res_plot4 <- Res_plot4 + ggtitle("Было \nв начальной модели")
Diag_gls_plot4 <- Diag_gls_plot4 + ggtitle("Стало после моделирования \nструктуры дисперсии")
grid.arrange(Res_plot4, Diag_gls_plot4, ncol = 2)



##Можно ли упростить модель?
M5_gls_ML <- update(M5_gls, method = "ML")
drop1(M5_gls_ML, test = "Chi")


##Структура дисперсии может иметь определенный биологический смысл
qplot(x=c(1997, 1999:2011), y=as.vector(unlist(M5_gls$modelStruct))) + xlab("Годы") + ylab("Delta")




#Моделирование структуры дисперсии при наличии группирующих (случайных) факторов
## Рост крыс при разной диете
# Пример взят из книги Pinheiro & Bates, 2000 (Hand and Crowder (1996))
# Три группы крыс, содержались при разных условиях кормления 64 дня. Каждую крысу взвешивали с определнной периодичностью.
data("BodyWeight")
bw <- as.data.frame(BodyWeight)
head(bw, 14)


###Задание:
# Постройте модель, которая дала бы ответ на вопрос: Изменяется ли характер роста крыс в зависимости от типа диеты?

## Решение

