# title: "Смешанные линейные модели" ##############
# subtitle: "Линейные модели..."
# author: "Марина Варфоломеева, Вадим Хайтов"

## Пример: Как время реакции людей зависит от бессонницы? #############
#
# В датасете `sleepstudy` из пакета `lme4`
# описание немного отличается от того, что в
# статье: В ночь перед нулевым днем всем
# испытуемым давали поспать нормальное время, а в
# следующие 9 ночей --- давали спать по 3 часа.
# Каждый день измеряли время реакции в серии
# тестов.
#
# Данные: Belenky et al. (2003) Patterns of
# performance degradation and restoration during
# sleep restriction and subsequent recovery: a
# sleep dose-response study. Journal of Sleep
# Research 12, 1–12.
#
# - `Reaction` --- среднее время реакции в серии тестов в день наблюдения, мс
# - `Days` --- число дней депривации сна
# - `Subject` --- номер испытуемого

library(lme4)
data(sleepstudy)
sl <- sleepstudy
head(sl, 3)

#
## Знакомство с данными
str(sl)
# пропущенные значения
colSums(is.na(sl))

# число субъектов
length(unique(sl$Subject))
# сбалансирован ли объем выборки?
table(sl$Subject)
table(sl$Subject, sl$Days)

## Есть ли выбросы?
library(ggplot2)
theme_set(theme_bw())
# построим дот-плот
ggplot(sl, aes(x = Reaction, y = 1:nrow(sl))) +
  geom_point()


## Как меняется время реакции разных субъектов?
ggplot(sl, aes(x = Reaction, y = Subject, colour = Days)) +
  geom_point()



## Не учитываем группирующий фактор
Wrong1 <- lm(Reaction ~ Days, data = sl)

summary(Wrong1)

ggplot(sl, aes(x = Days, y = Reaction)) +
  geom_point() +
  geom_smooth(se = TRUE, method = "lm", size = 1)


## Группирующий фактор как фиксированный.

Wrong2 <- lm(Reaction ~ Days + Subject, data = sl)

summary(Wrong2)

Wrong2_diag <- fortify(Wrong2)
ggplot(Wrong2_diag, aes(x = Days, colour = Subject)) +
  geom_line(aes(y = .fitted, group = Subject)) +
  geom_point(data = sl, aes(y = Reaction)) +
  guides(colour = guide_legend(ncol = 2))



## Фиксированные и случайные факторы #############

## Задание 1 -------------------------------------
#
# Какого типа эти факторы? Поясните ваш выбор.
#
# - Несколько произвольно выбранных градаций
# плотности моллюсков в полевом эксперименте, где
# плотностью манипулировали.
# - Фактор размер червяка (маленький, средний,
# большой) в выборке червей.
# - Деление губы Чупа на зоны с разной степенью
# распреснения.

## Cмешанные линейные модели #####################

## Подбор смешанных моделей в R #################


## Смешанные модели со случайным отрезком в R ############

# выгружаем lme4, чтобы не было конфликтов с nlme
detach(name = "package:lme4")
library(nlme)
M1 <- lme(Reaction ~ Days, random = ~ 1 | Subject, data = sl)


## Анализ остатков ##################################

## График остатков от предсказанных значений
M1_diag <- data.frame(sl,
                      .resid = resid(M1, type = "pearson"),
                      .fitted <- fitted(M1))
gg_resid <- ggplot(M1_diag, aes(y = .resid)) +
  guides(colour = guide_legend(ncol = 2))
gg_resid + geom_point(aes(x = .fitted, colour = Subject))

## Графики остатков от ковариат в модели и не в модели
library(gridExtra)
grid.arrange(gg_resid + geom_boxplot(aes(x = factor(Days))),
             gg_resid + geom_boxplot(aes(x = Subject)),
             ncol = 2, widths = c(0.4, 0.6))


## Тестирование гипотез в смешанных моделях #########


## (а) t-(или -z) тесты (REML) ###################

summary(M1)

## (б) F-тест (REML) #############################

library(car)
anova(M1, test = "F")


## (в) Попарное сравнение вложенных моделей при помощи тестов отношения правдоподобий (ML) ##########

M1.ml <- lme(Reaction ~ Days, random = ~1|Subject, data = sl, method = "ML")
M2.ml <- lme(Reaction ~ 1, random = ~1 | Subject, data = sl, method = "ML")

anova(M1.ml, M2.ml)

## (г) Сравнение моделей по AIC (ML) ###############

AIC(M1.ml, M2.ml)


## Подбор оптимальной модели и проверка условий применимости ############


## Представление результатов #####################

M1_fin <- lme(Reaction ~ Days, random = ~1|Subject, data = sl,
              method = "REML")


## Уравнение модели

fixef(M1_fin)    # Фиксированные эффекты
VarCorr(M1_fin)  # Случайные эффекты


## Внутриклассовая корреляция
# $\sigma_{effect}^2 / (\sigma_{effect}^2 + \sigma^2)$



## Данные для графика предсказаний фиксированной части модели #############

# Исходные данные
library(plyr)
NewData_M1 <- ddply(
  sl, .(Subject), summarise,
  Days = seq(min(Days), max(Days), length = 10)
  )

# Предсказанные значения при помощи predict()
# level = 0 - для фиксированных эффектов (т.е. без учета субъекта)
NewData_M1$fitted <- predict(M1_fin, NewData_M1, level = 0)

# Предсказанные значения при помощи матриц
X <- model.matrix(~ Days, data = NewData_M1)
betas <- fixef(M1_fin)
NewData_M1$fitted <- X %*% betas

# Cтандартные ошибки и дов. интервалы
NewData_M1$se <- sqrt( diag(X %*% vcov(M1_fin) %*% t(X)) )
NewData_M1$lwr <- NewData_M1$fitted - 1.98 * NewData_M1$se
NewData_M1$upr <- NewData_M1$fitted + 1.98 * NewData_M1$se


## График предсказаний фиксированной части модели ###############

ggplot(data = NewData_M1, aes(x = Days, y = fitted)) +
  geom_ribbon(alpha = 0.35, aes(ymin = lwr, ymax = upr)) +
  geom_line() +
  geom_point(data = sl, aes(x = Days, y = Reaction))


## Данные для графика предсказаний для индивидуальных уровней случайного фактора ###########

# Можно получить предсказания для каждого субъекта
# $\beta_0 + \beta_1 \cdot Days_{ij} + b_i$

NewData_M1$fit_subj <- predict(M1_fin, NewData_M1, level = 1)
# или то же самое при помощи матриц
# случайные эффекты для каждого субъекта
# это датафрейм с одним столбцом
rand <- ranef(M1_fin)
# "разворачиваем" для каждой строки данных
all_rand <- rand[as.numeric(NewData_M1$Subject), 1]
# прибавляем случайные эффекты к предсказаниям фикс. части
NewData_M1$fit_subj <- X %*% betas + all_rand


## График предсказаний для индивидуальных уровней случайного фактора ########

ggplot(NewData_M1, aes(x = Days, y = fit_subj, group = Subject)) +
  geom_ribbon(alpha = 0.5, aes(fill = Subject, ymin = fit_subj - 1.98*se,
                  ymax = fit_subj + 1.98*se)) +
  geom_line() +
  geom_point(data = sl, aes(x = Days, y = Reaction))  +
  guides(fill = guide_legend(ncol = 2))


## Смешанная модель со случайным отрезком и углом наклона #################

MS1 <- lme(Reaction ~ Days, random = ~ 1 + Days|Subject, data = sl)

## Задание 2 ------------------------------------
#
# Проверьте получившуюся модель MS1
#
# Сделайте самостоятельно:
#
# - Анализ остатков
# - Проверку влияния факторов + подбор оптимальной модели
# - Визуализацию предсказаний


