# title: "Множественная регрессия"
# author: Марина Варфоломеева, Вадим Хайтов

#### Пример: птицы в лесах Австралии ##############
# Фрагментация лесных местообитаний - одна из важнейших проблем Австралии.
# От каких характеристик лесного участка зависит обилие птиц во фрагментированных лесных массивах? (Loyn, 1987)
#
# 56 лесных участков:
# - ABUND - обилие птиц
# - AREA - площадь участка
# - YRISOL - год изоляции участка
# - DIST - расстояние до ближайшего леса
# - LDIST - расстояние до ближайшего большого леса
# - GRAZE - пастбищная нагрузка (1-5)
# - ALT - высота над уровнем моря
#
# Пример из кн. Quinn, Keugh, 2002, данные из Loyn, 1987)


#### Читаем данные
bird <- read.csv("data/loyn.csv")

# Все ли правильно открылось?
str(bird)

# Есть ли пропущенные значения?
colSums(is.na(bird))

#### Можно ли ответить на вопрос таким методом?
cor(bird)

#### Знакомство с данными
library(car)
scatterplotMatrix(bird)





scatterplotMatrix(bird[, c("ABUND", "logAREA", "YRISOL", "logDIST", "logLDIST", "GRAZE", "ALT")])



#### Мультиколлинеарность ########################

#### Задание 1 -------------------------------------
#
# - Постройте множественную линейную регрессию для зависимости обилия птиц (`ABUND`) от других переменных (`logAREA`, `YRISOL`, `logDIST`, `logLDIST`, `GRAZE`, `ALT`)
# - Используйте функцию vif, чтобы проверить, коллинеарны ли предикторы.
#
# Дополните этот код:
mod1 <- lm(data = bird)
vif()








coef(summary(mod2))



#### Задание 2 ------------------------------------
#
# Проверьте, выполняются ли условия применимости для модели `mod2`
#
# Дополните этот код

library()
mod2_diag <- data.frame(fortify(), $GRAZE)
# 1) График расстояния Кука
ggplot(data = , aes(x = 1:, y = .cooksd)) + geom_bar(stat = "")
# 2) График остатков от предсказанных значений
gg_resid <- ggplot(data = , aes(x = , y = )) + geom_point() + geom_hline()
gg_resid
# 3) Графики остатков от предикторов в модели и нет
res_1 <- gg_resid + aes(x = logAREA)
res_2 <- gg_resid
res_3 <- gg_resid
res_4 <- gg_resid
res_5 <- gg_resid
res_6 <- gg_resid
# все графики вместе
library(gridExtra)
grid.arrange(res_1, res_2, nrow = 2)
# 4) Квантильный график остатков
library(car)
qq

#### Описание множественной линейной регрессии ##############
summary(mod2)


#### Сравнение силы влияния разных предикторов ###############

mod2_scaled <- lm(ABUND ~ scale(logAREA) + scale(YRISOL) + scale(logDIST) + scale(logLDIST) + scale(ALT), data = bird)

#### Какой из предиктов оказывает наиболее сильное влияние на обилие птиц?
coef(summary(mod2_scaled))


#### График предсказаний модели ###################

# Искуственный датафрейм для предсказаний
MyData <- data.frame(
  logAREA = seq(min(bird$logAREA), max(bird$logAREA), length.out = 100),
  YRISOL = mean(bird$YRISOL),
  logDIST = mean(bird$logDIST),
  logLDIST = mean(bird$logLDIST),
  ALT = mean(bird$ALT))

# Предсказанные значения
Predictions <- predict(mod2, newdata = MyData, se.fit = TRUE)
MyData$fit <- Predictions$fit

# Стандартные ошибки
MyData$SE <- Predictions$se.fit

# Доверительный интервал
MyData$upr <- MyData$fit + 1.96 * MyData$SE
MyData$lwr <- MyData$fit - 1.96 * MyData$SE

# Обратная трансформация предикторов
MyData$AREA <- exp(MyData$logAREA)
MyData$DIST <- exp(MyData$logDIST)
MyData$LDIST <- exp(MyData$logLDIST)

# График предсказаний модели
Pl_predict <- ggplot(MyData, aes(x = AREA, y = fit)) +
  geom_ribbon(alpha = 0.2, aes(ymin = lwr, ymax = upr)) +
  geom_line() + xlab("Площадь леса") + ylab("Обилие птиц")
Pl_predict


#### Вопрос -------------------------------------
#
# Имеет ли смысл на таком графике изображать исходные наблюдения?
#
# Обычно это делают, чтобы (1) посмотреть на величину остатков, (2) посмотреть на диапазон исходных наблюдений.
#
# Будет ли это иметь смысл здесь?



#### Задание 3 -------------------------------------
#
# - Постройте модель описывающую связь между усилием мышц, осуществляющих выдох (`pemax`) и следующими переменными:
#
# - `age` --- Возраст
# - `height` --- Рост (см)
# - `weight` --- Вес (кг)
# - `bmp` --- Отклонения в весе от нормы (% от нормы)
# - `fev1` --- Объем наполненных легких
# - `rv` --- Остаточный объем легких
# - `frc` --- Функциональная остаточная емкость легких
# - `tlc` --- Общая емкость легких
#
# - Не учитывайте влияние пола `sex` (мы пока не умеем работать с дискретными переменными, о них позже)
# - Исключите из модели коллинеарные предикторы.
#
# Для получения данных выполните следующий код:
library(ISwR)
data(cystfibr)

