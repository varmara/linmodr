
# title: "Сравнение линейных моделей"
# subtitle    : "Линейные модели..."
# author: Вадим Хайтов, Марина Варфоломеева

# install.packages("caret")

## Читаем данные
birds <- read.csv("data/loyn.csv")

M <- lm(ABUND ~ ., data = birds)

library(car)

vif(M) # есть колинеарные предикторы
# GRAZE - избыточный предиктор, удаляем

M1 <- update(M, .~. - GRAZE)

vif(M1)

summary(M1)

## Сравниваем две вложенные модели

M2 <- update(M1, . ~ . - AREA)
anova(M1, M2)

## Более простой способ

drop1(M1, test = "F")
# Нужно убрать AREA
# Убрали AREA
M2 <- update(M1, . ~ . - AREA)
drop1(M2, test = "F")
# Нужно убрать LDIST

# Убрали LDIST
M3 <- update(M2, . ~ . - LDIST)
drop1(M3, test = "F")
# Больше ничего убрать не получается

summary(M3)


## Подбираем параметры модели методом максимального правдоподобия

# Создаем симулированные данные
xy <- data.frame(X = rep(1:10, 3))
xy$Y <- 10*xy$X + rnorm(30, 0, 10)

# Подбираем модель
Mod <- lm(Y ~ X, data = xy)
Mod_glm <- glm(Y ~ X, data = xy)

coefficients(Mod)

##
coefficients(Mod_glm)

## Значение logLik
logLik(Mod_glm)


# Вычисляем руками значение функции максимального правдоподобия для побобранной модели
##
xy$predicted <- predict(Mod) # Предсказанные моделью значения

SD <- summary(Mod)$sigma # Оценка дисперсии

xy$Prob <- dnorm(xy$Y, mean = xy$predicted, sd = SD) # Вероятности для каждой точки

xy$LogProb <- log(xy$Prob) # Логарифм вероятностей

sum(xy$LogProb) # Логарифм произведения, равный сумме логарифмов


## Подбираем параметры модели методом максимальноо правдоподобия
GLM1 <- glm(ABUND ~ . - GRAZE, data = birds)


drop1(GLM1, test = "Chisq")
# Нужно убрать AREA

# Убираем AREA
GLM2 <- update(GLM1, . ~ . - AREA)
drop1(GLM2, test = "Chisq")
# Нужно убрать LDIST

# Убираем LDIST
GLM3 <- update(GLM2, . ~ . - LDIST)
drop1(GLM3, test = "Chisq")
# Больше ничего убрать не получается

summary(GLM3)

## AIC для моделей
AIC(GLM1, GLM2, GLM3, k=2)
# По AIC лучшая модель GLM3

#Кросс-валидация

library(caret)

SEED <- 233

set.seed(SEED)

# Кросс-валидация, 5-кратная
train_control <- trainControl(method = "cv", number = 5)

f1 <- ABUND ~ AREA + YRISOL + DIST + LDIST + ALT

MCV1 <- train(f1, data = birds, trControl = train_control, method = "lm")

MCV1$resample # результаты на разных фолдах

MCV1$results # итоговая статистика

## Меняем структуру модели
f2 <- ABUND ~ YRISOL + DIST + LDIST + ALT

f3 <- ABUND ~ YRISOL + DIST + ALT

set.seed(SEED)

MCV2 <- train(f2, data = birds, trControl = train_control, method = "lm")

f3 <- ABUND ~ YRISOL + DIST + ALT

set.seed(SEED)

MCV3 <- train(f3, data = birds, trControl = train_control, method = "lm")

# Сравниваем три модели
c(
MCV1$results$RMSE,
MCV2$results$RMSE,
MCV3$results$RMSE
)
# самая лучшая модель та, у котоой RMSE минимальна



# Бутстреп
train_control <- trainControl(method = "boot")

f1 <- ABUND ~ AREA + YRISOL + DIST + LDIST + ALT

MCV1 <- train(f1, data = birds, trControl = train_control, method = "lm")


MCV1$results # итоговая статистика

## Меняем структуру модели
f2 <- ABUND ~ YRISOL + DIST + LDIST + ALT

f3 <- ABUND ~ YRISOL + DIST + ALT

set.seed(SEED)

MCV2 <- train(f2, data = birds, trControl = train_control, method = "lm")

f3 <- ABUND ~ YRISOL + DIST + ALT

set.seed(SEED)

MCV3 <- train(f3, data = birds, trControl = train_control, method = "lm")

# Сравниваем три модели
c(
  MCV1$results$RMSE,
  MCV2$results$RMSE,
  MCV3$results$RMSE
)
# самая лучшая модель та, у котоой RMSE минимальна
