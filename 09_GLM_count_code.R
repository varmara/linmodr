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
  DiversityD_1 = seq(min(DiversityD_1), max(DiversityD_1), length = 100))
NewData$Flowers = mean(pol$Flowers)
NewData$Hours = mean(pol$Hours)
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
ggplot(NewData, aes(x = DiversityD_1, y = fit, group = Treatment)) +
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
