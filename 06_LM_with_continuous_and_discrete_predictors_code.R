# title: "Линейные модели с непрерывными и дискретными предикторами"
# subtitle    : "Линейные модели..."
# author: Марина Варфоломеева, Вадим Хайтов

# Если не поставлены пакеты
# install.packages("car")
#
# install.packages("dplyr")
#




## Читаем данные
BD <- read.csv("data/loyn.csv")
str(BD)
BD$fGRAZE <- factor(BD$GRAZE)

## Проверяем сбаллансированность комплекса
table(BD$fGRAZE)

# "пустой график"
dot <- ggplot(data = BD, aes(y = 1:nrow(BD))) + geom_point()
library(ggplot2)
library(gridExtra)

## Добавляем эстетики
dot1 <- dot + aes(x = ABUND)
dot2 <- dot + aes(x = AREA)
dot3 <- dot + aes(x = YRISOL)
dot4 <- dot + aes(x = DIST)
dot5 <- dot + aes(x = LDIST)
dot6 <- dot + aes(x = ALT)

# Собираем все графики на одну панель
grid.arrange(dot1, dot2, dot3, dot4, dot5, dot6, ncol = 3)

## Логарифмируем предикторы
BD$LOGAREA  <- log10(BD$AREA)
BD$LOGDIST  <- log10(BD$DIST)
BD$LOGLDIST <- log10(BD$LDIST)


## Проверяем условия применимости
library(car)
M0 <- lm(ABUND ~ LOGAREA + LOGDIST + LOGLDIST + YRISOL + ALT + fGRAZE, data = BD)

vif(M0)

M0.1 <- lm(ABUND ~ LOGAREA + LOGDIST + LOGLDIST + ALT + fGRAZE, data = BD)

vif(M0.1)

M0.2 <- lm(ABUND ~   LOGAREA + LOGDIST +  ALT + fGRAZE, data = BD)

vif(M0.2)

## Подбираем оптимальную модель
M1 <- lm(ABUND ~   LOGAREA + LOGDIST +  ALT + fGRAZE, data = BD)
drop1(M1, test = "F")

M2 <- update(M1, ~.- ALT)
drop1(M2, test = "F")

M3 <- update(M2, ~.- LOGDIST)
drop1(M3, test = "F")

summary(M3)


## Модельная матрица для M3
X <- model.matrix(M3)
X

## Применям матричную алгебру для поиска вектора коэффициентов

Matr <- (t(X) %*% X)
solve(Matr) %*% (t(X) %*% BD$ABUND)


## Меняем базовый уровень
  levels(BD$fGRAZE)

  BD$fGRAZE <- relevel(BD$fGRAZE, ref = "5")

  levels(BD$fGRAZE) # Теперь первым идет "5"

## Пересчитываем модель
M3_ref_5 <- lm(formula = formula(M3), data = BD)
summary(M3_ref_5)



## Визуализируем модель

#По привычной схеме
my_df <- expand.grid(
  fGRAZE = unique(BD$fGRAZE),
  LOGAREA = seq(min(BD$LOGAREA), max(BD$LOGAREA), 0.1 )
  )

my_df <- unique(BD[, c("fGRAZE", "LOGAREA")]) # еще один метод

my_df$Predicted <- predict(M3, newdata = my_df)
my_df$SE <- predict(M3, newdata = my_df, se.fit = TRUE)$se.fit


my_df$Predicted <- predict(M3, newdata = my_df)
my_df$SE <- predict(M3, newdata = my_df, se.fit = TRUE)$se.fit

head(my_df)

# С применением конвейерного метода обработки данных
library(dplyr)
##
## # Конвейерный принцип обработки данных
##
minmax <- BD %>% group_by(fGRAZE) %>% #Передаем датафрейм функции group_by()
           summarise(minAREA = min(LOGAREA),
            maxAREA = max(LOGAREA)) #Передаем функции summarise()

## # Pазбиваем minmax по переменной GRAZE создаем датафрейм,
## # где LOGAREA - это последовательность от минимума до максимума для данного fGRAZE,
## # снимаем группировку,
## # добавляем предсказанные значения и стандартные ошибки
##
my_df2 <- minmax %>% group_by(fGRAZE) %>%
           do(data.frame(LOGAREA = seq(.$minAREA, .$maxAREA, length = 10))) %>%
             ungroup %>%
               mutate(Predicted = predict(M3, newdata = .),
                 SE = predict(M3, newdata = ., se.fit = TRUE)$'se.fit')

## Строим рафики

gg_parallel1 <- ggplot(data = my_df, aes(y = Predicted, x = LOGAREA, colour = fGRAZE, fill = fGRAZE)) +
  geom_path() +
  geom_ribbon(aes(ymax = Predicted + 1.98 * SE,
                  ymin = Predicted - 1.98 * SE),
                  alpha = 0.3, colour = NA) +
  geom_point(data = BD, aes(x = LOGAREA, y = ABUND)) +
  labs(colour = "Уровень выпаса", fill = "Уровень выпаса", x = "Логарифм площади", y = "Обилие птиц") +
  ggtitle("Наша модель") +
  theme(text = element_text(size = 10) )


gg_parallel2 <- ggplot(data = my_df2, aes(y = Predicted, x = LOGAREA, colour = fGRAZE, fill = fGRAZE)) +
  geom_line() +
  geom_ribbon(aes(ymax = Predicted + 1.98 * SE,
                  ymin = Predicted - 1.98 * SE),
                  alpha = 0.3, colour = NA) +
    geom_point(data = BD, aes(x = LOGAREA, y = ABUND)) +
  labs(colour = "Уровень выпаса", fill = "Уровень выпаса", x = "Логарифм площади",
       y = "Обилие птиц") + ggtitle("Наша модель") +
  theme(text = element_text(size = 10) )


gg_initial <- ggplot(data = BD, aes(x = LOGAREA, y = ABUND, color = fGRAZE)) + geom_point() + geom_smooth(method = "lm") + ggtitle("По сырым данным")  +
  theme(text = element_text(size = 10) )


grid.arrange(gg_parallel1, gg_parallel2, ncol = 2)

grid.arrange(gg_parallel2, gg_initial, ncol = 2)

## Модель с взаимодействиям предикторов
M4 <- lm(ABUND ~ (LOGAREA * LOGDIST *  ALT * fGRAZE), data = BD)

M4 <- lm(ABUND ~ (LOGAREA + LOGDIST +  ALT + fGRAZE)^2, data = BD)
drop1(M4, test = "F")

M5 <- update(M4, ~.-LOGAREA:LOGDIST )
drop1(M5, test = "F")

M6 <- update(M5, ~.-LOGDIST:fGRAZE )
drop1(M6, test = "F")

M7 <- update(M6, ~.-ALT:fGRAZE)
drop1(M7, test = "F")

M8 <- update(M7, ~.-LOGDIST:ALT)
drop1(M8, test = "F")

M9 <- update(M8, ~.-LOGDIST)
drop1(M9, test = "F")

M10 <- update(M9, ~.-LOGAREA:ALT)
drop1(M10, test = "F")

M11 <- update(M10, ~.-ALT)
drop1(M11, test = "F")

M12 <- update(M11, ~.-LOGAREA:fGRAZE )
drop1(M12, test = "F")

library(car)
op <- par(mfrow = c(1, 1)) # располагаем картинки в 3 колонки
par(mfrow = c(1, 3))
plot(M12, which = 4)          # Расстояние Кука
residualPlot(M12)             # График остатков
qqPlot(M12)                   # Квантильный график остатков
par(op)

## Сравнивем модели по AIC
formula(M12)

formula(M11)

AIC(M12, M11)




library(caret)
SEED = 12345

set.seed(SEED)

train_control <- trainControl(method = "boot")

f1 <- formula(M11)

MCV1 <- train(f1, data = BD, trControl = train_control, method = "lm")


MCV1$results # итоговая статистика

## Меняем структуру модели
f2 <- formula(M12)


set.seed(SEED)

MCV2 <- train(f2, data = BD, trControl = train_control, method = "lm")

c(
  MCV1$results$RMSE,
  MCV2$results$RMSE
  )





## Визуализируем модель с взаимодействиями
my_df <- unique(BD[, c("fGRAZE", "LOGAREA")]) # еще один метод

my_df$Predicted <- predict(M11, newdata = my_df)
my_df$SE <- predict(M11, newdata = my_df, se.fit = TRUE)$se.fit


gg_not_parallel <- ggplot(data = my_df, aes(y = Predicted, x = LOGAREA, colour = fGRAZE, fill = fGRAZE)) +
  geom_line() +
  geom_ribbon(aes(ymax = Predicted + 1.98 * SE,
                  ymin = Predicted - 1.98 * SE),
                  alpha = 0.3, colour = NA) +
  geom_point(data = BD, aes(x = LOGAREA, y = ABUND)) +
  labs(colour = "Уровень выпаса", fill = "Уровень выпаса", x = "Логарифм площади",  y = "Обилие птиц") + ggtitle("Модель с взаимодействиями") +
  theme(text = element_text(size = 10) )

grid.arrange(gg_parallel2, gg_not_parallel, ncol = 2)

## Пример с козами и паразитами

goat <- read.delim("data/goats.csv")
str(goat)

# переименуем переменные для краткости

colnames(goat) <- c("treat", "wt", "init")

levels(goat$treat)
Mod <- lm(wt ~ treat*init, data = goat )

Mod_vif <- lm(wt ~ treat + init, data = goat )
vif(Mod_vif)


## Меняем уровни местами
goat$treat <- relevel(goat$treat, ref = "standard")

#Решение

library(car)
op <- par(mfrow = c(1, 1)) # располагаем картинки в 3 колонки
par(mfrow = c(1, 3))
plot(Mod, which = 4)          # Расстояние Кука
residualPlot(Mod)             # График остатков
qqPlot(Mod)                   # Квантильный график остатков
par(op)

drop1(Mod, test = "F" )

anova(Mod)

summary(Mod)

Mod2 <- update(Mod, ~ . - treat:init )

summary(Mod2)

my_df <- unique(goat[, c("treat", "init")]) # еще один метод

my_df$Predicted <- predict(Mod2, newdata = my_df)
my_df$SE <- predict(Mod2, newdata = my_df, se.fit = TRUE)$se.fit


ggplot(my_df, aes(x = init, y = Predicted, color = treat)) + geom_path() + geom_point(data = goat, aes(x = init, y = wt, color = treat), size = 4) + geom_ribbon(aes(ymax = Predicted + 1.98 * SE, ymin = Predicted - 1.98 * SE, color = treat), alpha = 0.4) 

op <- par(mfrow = c(1, 1)) # располагаем картинки в 3 колонки
par(mfrow = c(1, 3))
plot(Mod2, which = 4)          # Расстояние Кука
residualPlot(Mod)             # График остатков
qqPlot(Mod)                   # Квантильный график остатков
par(op)

