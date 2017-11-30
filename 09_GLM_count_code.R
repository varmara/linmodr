<<<<<<< HEAD
# title: "Линейные модели для счетных данных"
# subtitle    : "Линейные модели..."
# author: Вадим Хайтов, Марина Варфоломеева



install.packages("AER")
install.packages("qcc")


# Пакеты ====
library(ggplot2)
library(car)
library(qcc)

#Настройка темы для графиков =============
theme_set(theme_bw(base_size = 16) +
            theme(legend.key = element_blank()))
update_geom_defaults("point", list(shape = 19, size = 3))


#Читаем данные =============

juv_ad <- read.table("mussel_juv_ad.csv", sep=";", header =T)


#Строим обычную линейную модель =============


M1 <- lm(Juv ~ Adult * factor(Year) , data = juv_ad)

summary(M1)

drop1(M1, test = "F")


# Сокращаем модель ==============
M2 <- lm(Juv ~ Adult  +  factor(Year), data = juv_ad)
Anova(M2)

summary(M2)


# Визуализируем предсказания модели ==================
MyData <- expand.grid(Year = factor(seq(min(juv_ad$Year), max(juv_ad$Year))), Adult = seq(min(juv_ad$Adult), max(juv_ad$Adult)))

MyData$Predicted <- predict(M2, newdata = MyData)

ggplot(MyData, aes(x=Adult, y = Predicted, group = Year)) + geom_line(color = "blue") + geom_hline(yintercept = 0) + ylab("Ожидаемое количество молоди")


# Диагностика модели ===============

M2_diag <- fortify(M2)
ggplot(M2_diag, aes(x=.fitted, y = .stdresid)) + geom_point() + geom_hline(yintercept = 0) + geom_smooth(se = F)


#Пуассоновская модель===============

M3 <- glm(Juv ~ Adult * factor(Year), data = juv_ad, family = "poisson")
Anova(M3)

# Диагностика модели ==============
M3_diag <- data.frame(.fitted = predict(M3),  .pears_resid = residuals(M3, type = "pearson"))

ggplot(M3_diag, aes(x=.fitted, y = .pears_resid)) + geom_point() + geom_hline(yintercept = 0) + geom_smooth(se = F)


##Избыточность дисперсии (Overdispersion)


Resid_M3 <- resid(M3, type = "pearson") # Пирсоновские остатки

N <- nrow(juv_ad) # Объем выборки

p <- length(coef(M3))   # Число параметров в модели

df <- (N - p) # число степенейсвободы

fi <- sum(Resid_M3^2) /df  #Величина fi показывает во сколько раз в среднем
#sigma > mu для данной модели

fi


# Квази-пуассоновская модель=============

M4 <- glm(Juv ~ Adult * factor(Year), data = juv_ad, family = "quasipoisson")
Anova(M4)

# Упрощенная модель без взаимодействия =============

M4a <- glm(Juv ~ Adult  + factor(Year), data = juv_ad, family = "quasipoisson")
Anova(M4a)


#Предсказания упрощенной модели

MyData <- expand.grid(Year = factor(seq(min(juv_ad$Year), max(juv_ad$Year))), Adult = seq(min(juv_ad$Adult), max(juv_ad$Adult)))

MyData$Predicted <- predict(M4a, newdata = MyData, type = "response")

ggplot(MyData, aes(x=Adult, y = Predicted, group = Year)) + geom_line(color = "blue") + geom_hline(yintercept = 0) + ylab("Ожидаемое количество молоди") +ylab("Ожидаемое количество молоди") + geom_point(data = juv_ad, aes(x = Adult, y = Juv, color = factor(Year) )) + facet_wrap(~Year) + guides(color=FALSE)

#Модель, основанная на отрицательном биномиальном распределении

library(MASS)

M5 <- glm.nb(Juv ~ Adult*factor(Year) , data = juv_ad, link = "log")
Anova(M5)

drop1(M5, test = "Chi")
# Здесь будет Ваш код для проверки избыточност дисперсии в модели M5 =====



Resid_M5 <- resid(M5, type = "pearson") # Пирсоновские остатки

summary(M5)

N <- nrow(juv_ad) # Объем выборки

p <- length(coef(M5)) + 1    # Число параметров в модели

df <- (N - p) # число степенейсвободы

fi <- sum(Resid_M5^2) /df  #Величина fi показывает во сколько раз в среднем
#sigma > mu для данной модели

fi



#Визуализируем предсказание модели ========












MyData <-unique(juv_ad)


MyData$Predicted <- predict(M5, newdata = MyData, type = "response")

MyData$SE <- predict(M5, newdata = MyData, type = "response", se = TRUE)$se.fit


ggplot(MyData, aes(x = Adult, y = Predicted, group = Year)) + geom_line(color = "blue") + geom_smooth(aes(ymin = x - 1.96*SE, ymax = x + 1.96*SE))+  geom_hline(yintercept = 0) + ylab("Ожидаемое количество молоди") + geom_point(data = juv_ad, aes(x = Adult, y = Juv, color = factor(Year) )) + facet_wrap(~Year, ncol = 5) + guides(color=FALSE)



# Строим оптимальную модель ==========


data(Affairs, package = "AER")
af <- Affairs









=======
# title: "Линейные модели для счетных данных"
# author: Марина Варфоломеева, Вадим Хайтов

## Пример: Гадючий лук, копеечник и визиты опылителей #####
#
# [Гадючий
# лук](https://ru.wikipedia.org/wiki/Гадючий_лук_хохлатый)
# (_Leopoldia comosa_, сем. спаржевые) ---
# представитель родной флоры острова Менорка
# (Балеарские острова, Средиземное море). В
# 18-19вв на остров завезли [копеечник
# венечный](https://ru.wikipedia.org/wiki/Копеечник_венечный)
# (_Hedysarum coronarium_, сем. бобовые), который
# быстро натурализовался. Оба вида цветут
# одновременно и нуждаются в опылении насекомыми.
#
# Как зависит опыление гадючьего лука от
# присутствия вида-вселенца и разнообразия флоры в
# ближайшей окрестности (Montero-Castaño, Vilà,
# 2015)?
#
# - `Visits` --- число визитов всех опылителей на
# цветок _Leopoldia_
# - `Treatment`:
# `Invaded` --- _Leopoldia_ в смеси с видом-вселенцем _Hedysarum_;
# `Removal` --- _Leopoldia_ в смеси с видом-вселенцем с удаленными цветками;
# `Control` --- _Leopoldia_ без вида вселенца
# - `DiversityD_1` --- Разнообразие флоры ---
# $exp(H’)$,  где $H'$ --- индекс Шеннона,
# расчитанный с использованием натурального
# логарифма
# - `Flowers` --- число цветков _Leopoldia_
# - `Hours` --- продолжительность наблюдений

library(readxl)
pol <- read_excel("data/Pollinators_Montero-Castano, Vila, 2015.xlsx", sheet = 1)
head(pol)

# Есть ли пропущенные значения?
sum(is.na(pol))

# Сколько площадок в каждом тритменте?
table(pol$Treatment)

# Как распределено число визитов насекомых?
library(ggplot2)
ggplot(pol, aes(x = Visits)) + geom_histogram()

# Нет ли выбросов?
gg_dot <- ggplot(pol, aes(y = 1:nrow(pol))) + geom_point()
library(gridExtra)
grid.arrange(gg_dot + aes(x = DiversityD_1),
             gg_dot + aes(x = Flowers),
             gg_dot + aes(x = Hours),
             nrow = 1)


## GLM с Гауссовым распределением отклика ###############

M0 <- glm(Visits ~ Treatment + DiversityD_1 + Flowers + Hours, data = pol)
## Результаты
summary(M0)

## Анализ девиансы для гауссовой модели {.smaller}
# К какому бы мы пришли выводу, глядя на этот анализ?
library(car)
Anova(M0)
# Можем ли мы доверять этим результатам?


## Визуализируем предсказания простой линейной модели ####

## Код для графика
library(plyr)
NewData <- ddply(
  .data = pol, .variables = .(Treatment), .fun = summarise,
  Flowers = seq(min(Flowers), max(Flowers), length = 100))
NewData$DiversityD_1 = mean(pol$DiversityD_1)
NewData$Hours = max(pol$Hours)
# модельная матрица
X <- model.matrix(~ Treatment + DiversityD_1 + Flowers + Hours, data = NewData)
# коэффициенты
betas <- coef(M0)
# предсказанные значения
NewData$mu <- X %*% betas  # в масштабе функции связи
NewData$fit <- NewData$mu  # в масштабе значений отклика
# стандартные ошибки
NewData$SE <- sqrt(diag(X %*% vcov(M0) %*% t(X)))
# доверительный интервал
NewData$upr <- NewData$mu + 1.96 * NewData$SE
NewData$lwr <- NewData$mu - 1.96 * NewData$SE
# график предсказаний
ggplot(NewData, aes(x = Flowers, y = fit, group = Treatment)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = Treatment), alpha = 0.3) +
  geom_line(aes(colour = Treatment)) +
  geom_hline(yintercept = 0)


## Задание 1 -------------------------------------
# Постройте график пирсоновских остатков этой модели.
# Какие нарушения условий применимости вы на нем увидите?
# Дополните код

# Данные для анализа остатков
M0_diag <- data.frame(.fitted = ,
                      .pears_resid = )
# График остатков
ggplot(M0_diag, aes(x=, y = )) +
  geom_ + geom_(yintercept = 0) +
  geom_(se = FALSE, method = "loess")


# GLM с Пуассоновским распределением отклика #############

M1 <- glm(Visits ~ Treatment + DiversityD_1 + Flowers + Hours, data = pol,  family = "poisson")
## Результаты
summary(M1)


## Анализ девиансы для Пуассоновской модели
# К какому бы мы пришли выводу, глядя на этот анализ?
Anova(M1)
# Можем ли мы доверять этим результатам?


## Задание 2 -------------------------------------
#
# Постройте график предсказаний модели
# Дополните код

NewData <- ddply(
  .data = , .variables = .(), .fun = summarise,
  DiversityD_1 = seq(min(), max(), length = 100))
NewData$Flowers = mean(pol$Flowers)
NewData$Hours = mean(pol$Hours)
# модельная матрица
X <- model.matrix(, data = NewData)
# коэффициенты
betas <-
# предсказанные значения
NewData$mu <-       # в масштабе функции связи
NewData$fit <-  # в масштабе значений отклика
# стандартные ошибки
NewData$SE <- sqrt(diag(X %*% vcov(M1) %*% t(X)))
# доверительный интервал в масштабе значений отклика
NewData$upr <- (NewData$mu + 1.96 * NewData$SE)
NewData$lwr <- (NewData$mu - 1.96 * NewData$SE)
# график предсказаний
ggplot(NewData, aes(x = , y = , group = Treatment)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = ), alpha = 0.3) +
  geom_line(aes(colour = )) +
  geom_hline(yintercept = 0)


## Диагностика модели ############################

### График остатков
M1_diag <- data.frame(.fitted = predict(M1, type = "response"),
                      .pears_resid = residuals(M1, type = "pearson"))
ggplot(M1_diag, aes(x=.fitted, y = .pears_resid)) + geom_point() +
  geom_hline(yintercept = 0) + geom_smooth(se = FALSE, method = "loess")



## Проверка на сверхдисперсию
pear_res <- resid(M1, type = "pearson") # Пирсоновские остатки
p <- length(coef(M1))                   # число параметров в модели
N <- nrow(model.frame(M1)) # объем выборки
df <- N - p       # число степеней свободы
Overdisp <- sum(pear_res^2) / df

Overdisp


## Квазипуассоновская модель #####################
M2 <- glm(Visits ~ Treatment + DiversityD_1 + Flowers + Hours, data = pol, family = "quasipoisson")
## Результаты
summary(M2)

## Анализ девиансы для квази-пуассоновской модели
# К какому бы мы пришли выводу, глядя на этот анализ?
Anova(M2, test = "F")
# Можем ли мы доверять этим результатам?


## GLM с отрицательным биномиальным распределением отклика ############

library(MASS)
M3 <- glm.nb(Visits ~ Treatment + DiversityD_1 + Flowers + Hours, data = pol, link = "log")
## Результаты
summary(M3)

## Анализ девиансы для модели с отрицательным биномиальным распределением отклика
# К какому бы мы пришли выводу, глядя на этот анализ?
Anova(M3)
# Можем ли мы доверять этим результатам?

## Диагностика модели
### График остатков
M3_diag <- data.frame(.fitted = predict(M3, type = "response"),
                      .pears_resid = residuals(M3, type = "pearson"))
ggplot(M3_diag, aes(x = .fitted, y = .pears_resid)) + geom_point() +
  geom_hline(yintercept = 0) + geom_smooth(se = FALSE, method = "loess")

## Проверка на сверхдисперсию
pear_res <- resid(M3, type = "pearson") # Пирсоновские остатки
p <- length(coef(M3))  + 1             # число параметров в модели
N <- nrow(model.frame(M3))             # объем выборки
df <- N - p                            # число степеней свободы
Overdisp <- sum(pear_res^2) / df

Overdisp


## Задание 3 --------------------------------------
#
# Визуализируйте предсказания модели, основанной на отрицательном биномиальном распределении

NewData <- ddply(
  .data = , .variables = .(), .fun = summarise,
  DiversityD_1 = )
NewData$Flowers = mean(pol$Flowers)
NewData$Hours = mean(pol$Hours)
# предсказанные значения
Predictions <- predict()
NewData$fit <- Predictions$fit
# стандартные ошибки
NewData$SE <- Predictions$se.fit
# доверительный интервал
NewData$upr <- NewData$fit + 1.96 * NewData$SE
NewData$lwr <- NewData$fit - 1.96 * NewData$SE

ggplot(NewData, aes(x = , y = , group = )) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3) +
  geom_line() +
  geom_hline(yintercept = 0)



# Пример: Неверность #############################
# Какие факторы определяют супружескую неверность?
data(Affairs, package = "AER")
af <- Affairs

# Fair, R.C. (1978). A Theory of Extramarital Affairs. Journal of Political Economy, 86, 45–61.

# `affairs` - Количество внебрачных свзяей за последний год
# `gender` - пол
# `age` - возраст
# `yearsmarried` - сколько ле в браке
# `children` - наличие детей
# `religiousness` - уровеь религиозности
# `education` - уровень образования
# `rating` - самооценка ощущений от брака

## Задание 4 -------------------------------------

# 1. Постройте оптимальную модель, описывающую зависимость количества внебрачных связей в зависимости от пола, времени, проведенного в браке, наличия детей, уровня религиозности и уровня образованности.
# 2. Проверьте валидность данной модели



## Визуализируем предсказание модели ##############

Mod_nb_final <- glm.nb(formula = affairs ~ yearsmarried + religiousness, data = af,
       init.theta = 0.1297897878, link = log)

NewData <- expand.grid(
  yearsmarried = seq(min(af$yearsmarried), max(af$yearsmarried)),
  religiousness = seq(min(af$religiousness), max(af$religiousness), length.out = 3))

# предсказанные значения
Predictions <- predict(Mod_nb_final, newdata = NewData, se.fit = TRUE, type = "response")
NewData$fit <- Predictions$fit
# стандартные ошибки
NewData$SE <- Predictions$se.fit
# доверительный интервал
NewData$upr <- NewData$fit + 1.96 * NewData$SE
NewData$lwr <- NewData$fit - 1.96 * NewData$SE
# график предсказаний модели
ggplot(NewData, aes(x = yearsmarried, y = fit, group = religiousness)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = religiousness), alpha = 0.2) +
  geom_line(aes(colour = religiousness), size = 1.5) +
  geom_hline(yintercept = 0) +
  scale_color_gradient(low = "yellow2", high = "red") +
  scale_fill_gradient(low = "yellow2", high = "red")
>>>>>>> e92a52d0fc00374c3f8488769bb250d53ec636e8
