# ---
# title: "Множественная регрессия"
# author: Марина Варфоломеева, Вадим Хайтов


# ## Пример: птицы в лесах Австралии ###############################
#
# Фрагментация лесных местообитаний - одна из важнейших проблем Австралии.
# От каких характеристик лесного участка зависит обилие птиц во фрагментированных лесных массивах? (Loyn, 1987)

# 56 лесных участков:
#
# - ABUND - обилие птиц
# - AREA - площадь участка
# - YRISOL - год изоляции участка
# - DIST - расстояние до ближайшего леса
# - LDIST - расстояние до ближайшего большого леса
# - GRAZE - пастбищная нагрузка (1-5)
# - ALT - высота над уровнем моря
#
# Пример из кн. Quinn, Keugh, 2002, данные из Loyn, 1987)


# ## Читаем данные
bird <- read.csv("data/loyn.csv")

# Все ли правильно открылось?
str(bird)

# Есть ли пропущенные значения?
colSums(is.na(bird))

# ## Можно ли ответить на вопрос таким методом?
round(cor(bird), 3)


# ## Знакомство с данными #################################
library(car)
pairs(bird)
# - обратите внимание на форму связи между переменными
# - шкалы переменных







# Трансформируем переменные
bird$logAREA <- log(bird$AREA)
bird$logDIST <- log(bird$DIST)
bird$logLDIST <- log(bird$LDIST)



pairs(bird[, c("ABUND", "logAREA", "YRISOL", "logDIST", "logLDIST", "GRAZE", "ALT")])

library(ggplot2)

bird2 <- bird

bird2[,nrow(bird2)+1] <- bird2[, nrow(bird2)]+10


bird2$ABUND[4] <- 150


ggplot(bird2, aes(y = 1:nrow(bird), x = ABUND  )) + geom_point() +
  labs(y = 'Порядковый номер \nв датасете', x = 'Значения переменной')


gg_dot <- ggplot(bird, aes(y = 1:nrow(bird))) + geom_point() + ylab('index')
Pl1 <- gg_dot + aes(x = ABUND)
Pl2 <- gg_dot + aes(x = YRISOL)
Pl3 <- gg_dot + aes(x = logAREA)
Pl4 <- gg_dot + aes(x = logDIST)
Pl5 <- gg_dot + aes(x = logLDIST)
Pl6 <- gg_dot + aes(x = ALT)
Pl7 <- gg_dot + aes(x = GRAZE)

library(cowplot) # пакет для группировки графиков
theme_set(theme_bw())
plot_grid(Pl1, Pl2, Pl3, Pl4, Pl5, Pl6,
          Pl7, ncol = 3, nrow = 3)


# Условия применимости линейной регрессии #######################

Mod <-



# ### Проверка на мультиколлинеарность ###########################
# ## Задание 1 ------------------------------------------------
#
# - Постройте множественную линейную регрессию для
# зависимости обилия птиц (`ABUND`) от других
# переменных (`logAREA`, `YRISOL`, `logDIST`,
# `logLDIST`, `GRAZE`, `ALT`)
#
# ABUND_i = b_0 + b_1 logAREA_i + b_2 YRISOL_i   +
#               + b_3 logDIST_i + b_4 logLDIST_i +
#               + b_5 GRAZE_i   + b_6 ALT_i      + e_i
#
# - Используйте функцию `vif()`, чтобы проверить,
# коллинеарны ли предикторы.
#
# Дополните код:
names(bird)

mod1 <- lm(formula = ABUND ~ logAREA + YRISOL + logDIST + logLDIST + GRAZE + ALT, data = bird)



vif(mod1)

mod2 <- update(mod1, .~. - GRAZE)

vif(mod2)





# ## Уравнение модели ###################################################
# Модель, с которой мы теперь работаем
# ABUND_i = b_0 + b_1 logAREA_i + b_2 YRISOL_i   +
#               + b_3 logDIST_i + b_4 logLDIST_i +
#               + b_5 ALT_i     + e_i



# ## Задание 2 ------------------------------------------------------------
# Проверьте, выполняются ли условия применимости
# для модели `mod2`. Дополните код:
library()
mod2_diag <- data.frame(fortify(mod2), bird$GRAZE)
# 1) График расстояния Кука


ggplot(data = mod2_diag, aes(x = 1:nrow(mod2_diag), y = .cooksd)) + geom_bar(stat = "identity")

ggplot(data = mod2_diag, aes(x = 1:nrow(mod2_diag), y = .cooksd)) + geom_col()



# 2) График остатков от предсказанных значений
gg_resid <- ggplot(data = mod2_diag, aes(x = .fitted, y = .stdresid)) + geom_point() + geom_hline(yintercept = 0) + geom_smooth(method = "loess")
gg_resid

names(bird)

# 3) Графики остатков от предикторов в модели и нет
res_1 <- gg_resid + aes(x = logAREA)
res_1
res_2 <- gg_resid + aes(x = YRISOL)
res_3 <- gg_resid + aes(x = logDIST)
res_4 <- gg_resid + aes(x = logLDIST)
res_5 <- gg_resid + aes(x = ALT)
res_6 <- ggplot(data = mod2_diag, aes(x = bird.GRAZE, y = .stdresid)) + geom_point() + geom_hline(yintercept = 0) + geom_smooth(method = "loess")

# все графики вместе
library(gridExtra)
grid.arrange(res_1, res_2, res_3, res_4, res_5, res_6, nrow = 2)


# 4) Квантильный график остатков
library(car)
qqPlot(mod2)



# # Сравнение силы влияния разных предикторов #################################
summary(mod2)


# ## Какой из предикторов оказывает наиболее сильное влияние?
coef(summary(mod2))

# ## Какой из предикторов оказывает наиболее сильное влияние?
mod2_scaled <- lm(ABUND ~ scale(logAREA) + scale(YRISOL) + scale(logDIST) +
                          scale(logLDIST) + scale(ALT), data = bird)

summary(mod2_scaled)

coef(summary(mod2_scaled))

X <- model.matrix(mod2)

betas <- coef(mod2)


fit <- X %*% betas

resid <- bird$ABUND - fit



# # График предсказаний модели множественной линейной регрессии #############

# ## Выбираем предикторы для графика модели
coef(mod2_scaled)

# ## График предсказаний модели "как есть"
# Искуственный датафрейм для предсказаний
MyData <- data.frame(
  logAREA = seq(min(bird$logAREA), max(bird$logAREA), length.out = 100),
  YRISOL = mean(bird$YRISOL),
  logDIST = mean(bird$logDIST),
  logLDIST = mean(bird$logLDIST),
  ALT = mean(bird$ALT))
# Предсказанные значения
Predictions <- predict(mod2, newdata = MyData,  interval = 'confidence')
MyData <- data.frame(MyData, Predictions)
# График предсказаний модели
Pl_predict <- ggplot(MyData, aes(x = logAREA, y = fit)) +
  geom_ribbon(alpha = 0.2, aes(ymin = lwr, ymax = upr)) +
  geom_line()
Pl_predict


## Задание
# Получите предсказания модели, стандартные ошибки
# и границы доверительной зоны при помощи операций
# с матрицами



# ## График предсказаний модели после обратной трансформации предикторов
# Обратная трансформация предиктора, который будем изображать
MyData$AREA <- exp(MyData$logAREA)
# График предсказаний модели
Pl_predict_tr <- ggplot(MyData, aes(x = AREA, y = fit)) +
  geom_ribbon(alpha = 0.2, aes(ymin = lwr, ymax = upr)) +
  geom_line() + xlab("Площадь леса") + ylab("Обилие птиц")
Pl_predict_tr


# ## Задание 3 ----------------------------------------------
# Добавьте к графику предсказаний точки исходных наблюдений.
# Рассмотрите график и подумайте о природе данных.
# Имеет ли смысл на нем изображать исходные наблюдения?
# Аргументируйте вашу точку зрения.


## Задание 4 ------------------------------------------------
# В датасете cystfibr из пакета ISwR находится
# информация характеристиках дыхательной системы
# больных муковисцидозом (в возрасте от 7 до 23
# лет).
# 1) Постройте модель описывающую связь между усилием мышц,
# осуществляющих выдох (`pemax`) и следующими переменными:
# - `age`    --- Возраст
# - `height` --- Рост (см)
# - `weight` --- Вес (кг)
# - `bmp`    --- Отклонения в весе от нормы (% от нормы)
# - `fev1`   --- Объем наполненных легких
# - `rv`     --- Остаточный объем легких
# - `frc`    --- Функциональная остаточная емкость легких
# - `tlc`    --- Общая емкость легких
# Не учитывайте влияние пола `sex` (мы пока не
# умеем работать с дискретными переменными, о них
# позже)
# 2) Исключите из модели коллинеарные предикторы.
# 3) Проверьте валидность полученной модели
# 4) Какой из предикторов оказыват наиболее сильное влияние на переменную отклика?

library(ISwR)
data(cystfibr)

