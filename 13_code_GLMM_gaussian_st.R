# ---
# title       : смешаные линейные модели
# subtitle    : Линейные модели, осень 2015
# author: Марина Варфоломеева, Вадим Хайтов

## Пример: Как время реакции людей зависит от бессонницы?
# Данные из Belenky et al., 2003.
# В нулевой день эксперимента всем испытуемым давали поспать нормальное время. Начиная со следующей ночи давали спать по 3 часа.
# - `Reaction` - среднее время реакции в серии тестов в день наблюдения, мс
# - `Days` - число дней депривации сна
# - `Subject` - номер субъекта
library(lme4)
data(sleepstudy)
sl <- sleepstudy
head(sl, 3)



## Знакомство с данными
str(sl)
# пропущенные значения
sum(!complete.cases(sl))
# число субъектов
length(unique(sl$Subject))
# сбалансирован ли объем выборки?
table(sl$Subject)
with(sl, table(Subject, Days))
# Есть ли выбросы?
library(ggplot2)
theme_set(theme_bw(base_size = 16) + theme(legend.key = element_blank()))
update_geom_defaults("point", list(shape = 19))
# Есть ли наблюдения-выбросы? строим dot-plot
ggplot(sl, aes(x = Reaction, y = 1:nrow(sl), colour = Subject)) +
  geom_point() + guides(colour = guide_legend(ncol = 2))

## Что делать с разными субъектами?
## Неправильный вариант 1. Не учитываем группирующий фактор.
wrong1 <- lm(Reaction ~ Days, data = sl)

ggplot(sl, aes(x = Days, y = Reaction)) +
  geom_point() +
  geom_smooth(se = F, method = "lm", size = 1)

## Неправильный вариант 2. Группирующий фактор как фиксированный.
wrong2 <- lm(Reaction ~ Days + Subject, data = sl)

wrong2_diag <- fortify(wrong2)
ggplot(wrong2_diag, aes(x = Days, colour = Subject)) +
  geom_line(aes(y = .fitted, group = Subject)) +
  geom_point(data = sl, aes(y = Reaction)) +
  guides(colour = guide_legend(ncol = 2))


## Cмешаные линейные модели

## Модель со случайным отрезком с помощью `lme()` из пакета `nlme`
detach("package:lme4") # выгружаем lme4, чтобы не было конфликтов
library(nlme)
M1 <- lme(Reaction ~ Days, random = ~ 1 | Subject, data = sl)

## 1. Анализ остатков.  Все ли в порядке с моделью?
## 1) График остатков от предсказанных значений
# plot(M1)
sl$.stdresid <- residuals(M1, type = "pearson")
sl$.fitted <- fitted(M1)
ggplot(sl) + geom_point(aes(x = .fitted, y = .stdresid, colour = Subject))

## 2) График остатков от ковариат в модели
p <- ggplot(data = sl, aes(y = .stdresid))
library(gridExtra)
grid.arrange(
  p + geom_point(aes(x = Days, colour = Subject)),
  p + geom_boxplot(aes(x = Subject)),
  ncol = 2)

## 2. Проверяем, какие из фиксированных факторов влияют одним из трех вариантов:
## 2(а) По значениям t-(или -z) статистики (по REML оценке)
summary(M1)

## 2(б) F-критерий - приблизительный результат (REML оценка)
anova(M1)

## 2(в1) Попарное сравнение вложенных моделей при помощи likelihood ratio test
M1.ml <- lme(Reaction ~ Days, random = ~1|Subject, data = sl, method = "ML")
M2.ml <- update(M1.ml, . ~ . - Days)
anova(M1.ml, M2.ml)

## 2(в2) Сравнение моделей по AIC
AIC(M1.ml, M2.ml)

## 3. Представление результатов
# Переподбираем модель. REML оценка параметров более точна
MFinal1 <- lme(Reaction ~ Days, random = ~1|Subject, method = "REML", data = sl)
# Для проверки финальной модели необходимо провести анализ остатков (те же графики, что и в п.1). Поскольку модель не изменилась, не делаем

## Вычисляем внутриклассовую корреляцию
MFinal1
#     В результатах
#     Random effects:
#      Formula: ~1 | Subject
#             (Intercept) Residual
#     StdDev:    37.12383 30.99123
# Внутриклассовая корреляция
37.12383^2 / (37.12383^2 + 30.99123^2)

## График предсказанных значений для результатов
# 1) Новый датафрейм, для которого будем предсказывать
library(dplyr)
minmax <- sl %>% group_by(Subject) %>%
  summarise(mDays = min(Days), MDays = max(Days))
new_data <- minmax %>% group_by(Subject) %>%
  do(data.frame(Days = seq(.$mDays, .$MDays, length = 10)))

# 2) Матрица линейной модели
X <- model.matrix(~ Days, data = new_data)

# 3) Вычисляем предсказанные значения одним из двух способов
# level = 0 - для фиксированных эффектов (т.е. без учета субъекта)
new_data$.fitted <- predict(MFinal1, new_data, level = 0)
# или то же самое при помощи матриц
betas = fixef(MFinal1)
new_data$.fitted <- X %*% betas

# 4) Вычисляем стандартные ошибки предсказанных значений
# это квадратный корень из диагональных элементов
# матрицы ковариаций предсказанных значений X * cov(BETA) * t(X)
new_data$.se <- sqrt( diag(X %*% vcov(MFinal1) %*% t(X)) )

## 1-й вариант. График с предсказаниями по фиксированной части модели
ggplot(new_data) +
  geom_ribbon(alpha = 0.2, aes(x = Days, y = .fitted, ymin = .fitted - 1.98 * .se, ymax = .fitted + 1.98 * .se)) +
  geom_point(data = sl, aes(x = Days, y = Reaction))

## 2-й вариант. График с предсказаниями для индивидуальных уровней случайного фактора
# beta_0 + beta * Days + случайный эффект субъекта
new_data$.fitted1 <- predict(MFinal1, new_data, level = 1)
ggplot(new_data, aes(x = Days, y = .fitted1, group = Subject)) +
  geom_ribbon(aes(fill = Subject, ymin = .fitted1 - 1.98 * .se, ymax = .fitted1 + 1.98 * .se), alpha = 0.5) + geom_line() +
  geom_point(data = sl, aes(x = Days, y = Reaction))
  # попробуйте добавить facet_wrap(~Subject)

## Cмешаная модель со случайным интерсептом и углом наклона
MS1 <- lme(Reaction ~ Days, random = ~ 1 + Days|Subject, data = sl)

## Задание:
# Проверьте получившуюся модель MS1. Для этого проведите самостоятельно:
# - Анализ остатков
# - Проверку влияния факторов + подбор оптимальной модели
# - Визуализацию предсказаний



