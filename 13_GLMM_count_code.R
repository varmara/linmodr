# title       : "Смешаные линейные модели для счетных данных"
# subtitle    : "Линейные модели..."
# author: Марина Варфоломеева, Вадим Хайтов

## От чего зависит призывный крик совят? #########
#
# 27 семейств сов в западной Швейцарии наблюдали с
# июня по август 1997. В день наблюдений совятам
# либо давали дополнительную подкормку (сытые),
# либо забирали остатки пищи из гнезда (голодные).
# Оба варианта манипуляций использовали в каждом
# из гнезд в случайном порядке. С 21:30 до 5:30
# утра записывали звуки и видео.
#
# Данные из Roulin & Bersier 2007, пример из кн. Zuur et al., 2007
#
# -`SiblingNegotiation` - число звуков в течение 15 минут до прибытия родителя
# - `FoodTreatment` - тритмент (сытые или голодные)
# - `SexParent` - пол родителя
# - `FoodTreatment x SexParent`
# - `ArrivalTime` - время прибытия родителя
# - `ArrivalTime х SexParent`
# - `Nest` - гнездо
#

## Знакомство с данными ##########################

Owls <- read.delim("data/Roulin_Bersier_2007_Owls.csv")
str(Owls)

# SiblingNegotiation - число криков совят - заменим на более короткое название
Owls$NCalls <- Owls$SiblingNegotiation

# Число пропущенных значений
sum(!complete.cases(Owls))

## Есть ли выбросы?
library(ggplot2)
theme_set(theme_bw())
gg_dot <- ggplot(Owls, aes(y = 1:nrow(Owls))) +
  geom_point(colour = "steelblue")

library(gridExtra)
grid.arrange(gg_dot + aes(x = NCalls),
  gg_dot + aes(x = ArrivalTime), nrow = 1)


## Различаются ли гнезда?
ggplot(Owls, aes(x = Nest, y = NCalls)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Сколько наблюдений в каждом гнезде?
table(Owls$Nest)


## Отклик --- счетная переменная
ggplot(Owls, aes(x = NCalls)) +
  geom_histogram(binwidth = 1, fill = "steelblue", colour = "black")

# Какова доля нулей?
mean(Owls$NCalls == 0)


## Какого размера выводки в гнездах?
range(Owls$BroodSize)

ggplot(Owls, aes(x = Nest, y = BroodSize)) +
  stat_summary(geom = "bar", fun.y = mean, fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Может быть есть взаимодействие?
ggplot(Owls) +
  stat_summary(aes(x = FoodTreatment, y = NCalls, colour = SexParent),
               fun.data = "mean_cl_boot", position = position_dodge(width = 0.2))


## Задание 1 -------------------------------------

# Постройте график зависимости числа криков от времени прибытия.
# На графике на разных панелях должны быть гнезда (столбцы), пол и тритмент (строки)


ggplot(Owls, aes(x = , y = ,
                 colour = , fill = )) +

  facet_grid(FoodTreatment +  ~ Nest) +
  theme(legend.position = ,
        axis.text.x = element_text(angle = , hjust = 1))



## Колинеарность
M0 <- lm(NCalls ~ SexParent + FoodTreatment + ArrivalTime, data = Owls)
library(car)
vif(M0)


## Линейная модель с пуассоновским распределением остатков #############

library(lme4)
M1 <- glmer(NCalls ~ SexParent * FoodTreatment +
              SexParent * ArrivalTime +
              offset(logBroodSize) + (1 | Nest),
            family = "poisson", data = Owls)


## Стандартизируем непрерывные предикторы
Owls$ArrivalTime_std <-

M1 <- glmer(NCalls ~ SexParent * FoodTreatment +
              SexParent * ArrivalTime_std +
              offset(logBroodSize) + (1 | Nest),
            family = "poisson", data = Owls)


## Задание 2 --------------------------------------

# Проверьте модель M1 на избыточность дисперсии

R_M1 <- # Пирсоновские остатки
N <-    # Объем выборки
p <-    # Число параметров в модели
df <-   # число степенейсвободы
overdisp <-    # Показывает во сколько раз в среднем sigma > mu
overdisp

## График остатков
M1_diag <- data.frame(Owls,
                      .fitted = predict(M1, type = "response"),
                      .pears_resid = residuals(M1, type = "pearson"))

gg_resid <- ggplot(M1_diag, aes(x = .fitted, y = .pears_resid,
                       colour = FoodTreatment)) +
  geom_point() +
  facet_grid(SexParent ~ FoodTreatment)
gg_resid


## Есть ли еще какие-то паттерны в остатках?
gg_resid %+% aes(x = ArrivalTime) + geom_smooth(method = "loess")


## Проверяем, есть ли нелинейный паттерн в остатках #######################

library(mgcv)
nonlin1 <- gam(.pears_resid ~ s(ArrivalTime),
               data = M1_diag)
summary(nonlin1)
plot(nonlin1)
abline(h = 0, lty = 2)


## Модель с отрицательным биномиальным распределением ###################

M2 <- glmer.nb(NCalls ~ SexParent * FoodTreatment +
                 SexParent * ArrivalTime_std +
                 offset(logBroodSize) + (1 | Nest),
               data = Owls)
## Если эта модель вдруг не сходится, есть обходной маневр. Можно попробовать заранее определить k  при помощи внутренней функции. В lme4 параметр k называется theta
# th <- lme4:::est_theta(M1)
# M2 <- update(M1, family = negative.binomial(theta=th))


## Задание 3 --------------------------------------

# Проверьте модель с отрицательным биномиальным распределением отклика
#
# - на избыточность дисперсии
# - наличие паттернов в остатках
# - нелинейность паттернов в остатках
#





## Подбор оптимальной модели ####################
#
# Все ли достоверно?
summary(M2)

## Задание 4 --------------------------------------
#
# Попробуйте упростить модель M2




## Модель изменилась. Нужно повторить диагностику ###########

# Избыточность дисперсии (Overdispersion)
R_M5 <- resid(M5, type = "pearson") # Пирсоновские остатки
N <- nrow(Owls) # Объем выборки
p <- length(fixef(M5)) + 1 + 1  # Число параметров в модели
df <- (N - p) # число степенейсвободы
overdisp <- sum(R_M5^2) /df  # Показывает во сколько раз в среднем sigma > mu
overdisp

M5_diag <- data.frame(Owls,
                      .fitted <- predict(M5, type = "response"),
                      .pears_resid <- residuals(M5, type = "pearson"))
gg_resid <- ggplot(M5_diag, aes(x = .fitted, y = .pears_resid,
                                colour = FoodTreatment)) +
  geom_point() +
  facet_grid(SexParent ~ FoodTreatment)
gg_resid

## Есть ли еще какие-то паттерны в остатках?
gg_resid %+% aes(x = ArrivalTime) + geom_smooth(method = "loess")


## Проверяем, есть ли нелинейные паттерны
nonlin5 <- gam(.pears_resid ~ s(ArrivalTime), data = M5_diag)
summary(nonlin5)
plot(nonlin5)
abline(h = 0)


## График предсказаний модели ####################

## Готовим данные для графика модели
library(plyr)
NewData <- ddply(Owls, .variables = .(FoodTreatment), summarise,
                 ArrivalTime_std = seq(min(ArrivalTime_std),
                                       max(ArrivalTime_std),
                                       length = 100))
NewData$ArrivalTime <- NewData$ArrivalTime_std * sd(Owls$ArrivalTime) +
  mean(Owls$ArrivalTime)

## Предсказания и ошибки
# Модельная матрица
X <- model.matrix(~ FoodTreatment + ArrivalTime_std, data = NewData)
# К предсказанным значениям нужно прибавить оффсет.
# Мы будем делать предсказания для среднего размера выводка.
NewData$Pred <- X %*% fixef(M5) + log(mean(Owls$BroodSize))
# Стандартные ошибки предсказаний
NewData$SE <- sqrt(diag(X %*% vcov(M5) %*% t(X)))
NewData$lwr <- exp(NewData$Pred - 1.96 * NewData$SE)
NewData$upr <- exp(NewData$Pred + 1.96 * NewData$SE)

## График предсказанных значений
ggplot() +
  geom_point(data = Owls, aes(x = ArrivalTime, y = NCalls), colour = "steelblue") +
  geom_ribbon(data = NewData, aes(x = ArrivalTime,  ymax = upr,  ymin = lwr), alpha = 0.3) +
  geom_line(data = NewData,  aes(x = ArrivalTime, y = exp(Pred), group = FoodTreatment)) +
  facet_wrap(~ FoodTreatment)

