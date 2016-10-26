#' title: "Смешанные линейные модели"
#' subtitle: "Линейные модели..."
#' author: "Марина Варфоломеева, Вадим Хайтов"


#' ## Пример: Как время реакции людей зависит от бессонницы?
#' Данные из Belenky et al., 2003.
#' В нулевой день эксперимента всем испытуемым давали поспать нормальное время. Начиная со следующей ночи давали спать по 3 часа.
#' - `Reaction` --- среднее время реакции в серии тестов в день наблюдения, мс
#' - `Days` --- число дней депривации сна
#' - `Subject` --- номер субъекта
library(lme4)
data(sleepstudy)
sl <- sleepstudy
head(sl, 3)

#' ## Знакомство с данными
str(sl)
sapply(sl, function(x) sum(is.na(x)))
length(unique(sl$Subject))

# сбалансирован ли объем выборки?
table(sl$Subject)
with(sl, table(Subject, Days))

#' ## Есть ли выбросы?
library(ggplot2)
theme_set(theme_bw() + theme(legend.key = element_blank()))
update_geom_defaults("point", list(shape = 19))

ggplot(sl, aes(x = Reaction, y = 1:nrow(sl), colour = Subject)) +
  geom_point() + guides(colour = guide_legend(ncol = 2))


#' ## Задание
#' Какого типа эти факторы? Поясните ваш выбор.
#'
#' - Несколько произвольно выбранных градаций плотности моллюсков в полевом эксперименте, где плотностью манипулировали.
#' - Фактор размер червяка (маленький, средний, большой) в выборке червей.
#' - Деление губы Чупа на зоны с разной степенью распреснения.




#' # Смешанные модели со случайным отрезком в R

detach("package:lme4") # выгружаем lme4, из которого мы взяли данные, чтобы не было конфликтов с nlme
library(nlme)
M1 <- lme(Reaction ~ Days, random = ~ 1 | Subject, data = sl)



#' 1) График остатков от предсказанных значений
sl$res_M1 <- resid(M1, type = "pearson")
sl$fit_M1 <- fitted(M1)
ggplot(sl) + geom_point(aes(x = fit_M1, y = res_M1, colour = Subject))

#' 2) График остатков от ковариат в модели
p <- ggplot(data = sl, aes(y = res_M1))
library(gridExtra)
grid.arrange(p + geom_point(aes(x = Days, colour = Subject)),
             p + geom_boxplot(aes(x = Subject)),
             ncol = 2)


#' ## 2. Проверка влияния факторов

#' ## 2(а) По значениям t-(или -z) статистики (по REML оценке)
summary(M1)

#' ## 2(б) F-критерий - приблизительный результат (REML оценка)
anova(M1)

#' ## 2(в1) Попарное сравнение вложенных моделей при помощи likelihood ratio test
M1.ml <- lme(Reaction ~ Days, random = ~1|Subject, data = sl, method = "ML")
M2.ml <- update(M1.ml, . ~ . - Days)
anova(M1.ml, M2.ml)


#' ## 2(в2) Сравнение моделей по AIC
AIC(M1.ml, M2.ml)

#' ## 3. Представление результатов
M1_fin <- lme(Reaction ~ Days, random = ~1|Subject, method = "REML", data = sl)
# Внутриклассовая корреляция
37.12383^2 / (37.12383^2 + 30.99123^2)



#' ## График предсказанных значений для результатов

#' 1-й вариант --- предсказания по фиксированной части модели

library(plyr)
MyData_M1 <- ddply(
  sl, .(Subject), summarise,
  Days = seq(min(Days), max(Days), length = 10)
  )
# level = 0 - для фиксированных эффектов (т.е. без учета субъекта)
MyData_M1$fitted <- predict(M1_fin, MyData_M1, level = 0)

# или то же самое при помощи матриц
X <- model.matrix(~ Days, data = MyData_M1)
betas <- fixef(M1_fin)
MyData_M1$fitted <- X %*% betas

# стандартные ошибки и дов. интервалы
MyData_M1$se <- sqrt( diag(X %*% vcov(M1_fin) %*% t(X)) )
MyData_M1$lwr <- MyData_M1$fitted - 1.98 * MyData_M1$se
MyData_M1$upr <- MyData_M1$fitted + 1.98 * MyData_M1$se

ggplot(data = MyData_M1, aes(x = Days, y = fitted)) +
  geom_ribbon(alpha = 0.35, aes(ymin = lwr, ymax = upr)) +
  geom_line() +
  geom_point(data = sl, aes(x = Days, y = Reaction))


#' 2-й вариант --- предсказания для каждого субъекта

MyData_M1$fit1 <- predict(M1_fin, MyData_M1, level = 1)
# или то же самое при помощи матриц
# случайные эффекты для каждого субъекта
# это датафрейм с одним столбцом
rand <- ranef(M1_fin)
# "разворачиваем" для каждой строки данных
all_rand <- rand[as.numeric(MyData_M1$Subject), 1]
# прибавляем случайные эффекты к предсказаниям фикс. части
MyData_M1$fit1 <- X %*% betas + all_rand

ggplot(MyData_M1, aes(x = Days, y = fit1, group = Subject)) +
  geom_ribbon(alpha = 0.5, aes(fill = Subject, ymin = fit1 - 1.98*se,
                  ymax = fit1 + 1.98*se)) +
  geom_line() +
  geom_point(data = sl, aes(x = Days, y = Reaction))
  # попробуйте добавить facet_wrap(~Subject)

#' ## Смешанная модель со случайным отрезком и углом наклона

MS1 <- lme(Reaction ~ Days, random = ~ 1 + Days|Subject, data = sl)

#' ## Задание
#' Проверьте получившуюся модель MS1
#' Сделайте самостоятельно:
#' - Анализ остатков
#' - Проверку влияния факторов + подбор оптимальной модели
#' - Визуализацию предсказаний


