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




## Смешанные модели со вложенными случайными факторами #############


## Пример: Высота растений и выпас скота ###############
#
# Как в разные годы высота растительного покрова
# зависит от выпаса скота, экспозиции склона и
# проективного покрытия местных растений?

# Переменные:
# - **height** - высота растительного покрова
# - **graze** - выпас коров (0, 1)
# - **AspectCat** - экспозиция (S, N)
# - **nativecov** - покрытие местной флоры %
# - **slope** - наклон
# - **year** - год наблюдений
# - **Park** - парк
# - **plotID** - уникальный идентификатор участка
#
# Данные:
# Gennet, S., Spotswood, E., Hammond, M. and
# Bartolome, J.W., 2017. Livestock grazing
# supports native plants and songbirds in a
# California annual grassland. PloS one, 12(6),
# p.e0176367.
#
## Открываем данные
library(readxl)
library(tidyr)
gr <- read_excel("data/Grazing_native_plants_Gennet_et_al._2017_S1.xlsx")
graz <- gr %>% spread(Species, presence)

## Знакомство с данными ####

# Есть ли пропущенные значения?
sum(is.na(graz))

# Сколько участков было в каждом парке в каждый год?
with(graz, table(Park, year))

# Сделаем факторами переменные, которые понадобятся для модели
graz$graze_f <- factor(graz$graze)
graz$AspectCat <- factor(graz$AspectCat)
graz$year_f <- factor(graz$year)

# Извлечем корень из обилия местных видов
graz$nativecov_sq <- sqrt(graz$nativecov)


# Модель
MN1 <- lme(height ~ graze_f*AspectCat + year_f + nativecov_sq + slope,
           random = ~ 1|Park/plotID,
           data = graz, method = "ML")

## Анализ остатков #####

# Данные для анализа остатков
MN1_diag <- data.frame(
  graz,
  pear_res = residuals(MN1, type = "pearson"),
  fitted = fitted(MN1, type = "response"))

## График остатков
gg_res <- ggplot(data = MN1_diag, aes(y = pear_res))
gg_res + geom_point(aes(x = fitted)) +
  geom_smooth(aes(x = fitted))

#
## Графики остатков от переменных в модели
library(gridExtra)
grid.arrange(gg_res + geom_boxplot(aes(x = graze_f)),
             gg_res + geom_boxplot(aes(x = AspectCat)),
             gg_res + geom_boxplot(aes(x = year_f)),
             gg_res + geom_point(aes(x = nativecov_sq)),
             gg_res + geom_point(aes(x = slope)),
             ncol = 3)


## Графики остатков от переменных не в модели
grid.arrange(
  gg_res + geom_point(aes(x = heatloadrel)),
  gg_res + geom_point(aes(x = sqrt(litt))),
  gg_res + geom_point(aes(x = sqrt(bare))),
  ncol = 3)


## Результаты полной модели ##############
summary(MN1)

## Тесты отношения правдоподобий для полной модели
library(car)
Anova(MN1)


## Задание 3 -------------------------------------
#
# Рассчитайте внутриклассовую корреляцию
#
# - Для наблюдений на одном и том же участке
# - Для наблюдений в одном и том же парке







## Визуализация результатов ####################

## Данные для графика предсказаний фиксированной части модели

# Исходные данные
NewData_MN1 <- expand.grid(graze_f = levels(graz$graze_f),
                           AspectCat = levels(graz$AspectCat),
                           year_f = levels(graz$year_f))
NewData_MN1$nativecov_sq <- mean(graz$nativecov_sq)
NewData_MN1$slope <- mean(graz$slope)

# Предсказанные значения при помощи матриц
X <- model.matrix(~ graze_f * AspectCat + year_f + nativecov_sq + slope, data = NewData_MN1)
betas = fixef(MN1)
NewData_MN1$fitted <- X %*% betas

# Cтандартные ошибки и дов. интервалы
NewData_MN1$se <- sqrt( diag(X %*% vcov(MN1) %*% t(X)) )
NewData_MN1$lwr <- NewData_MN1$fit - 1.98 * NewData_MN1$se
NewData_MN1$upr <- NewData_MN1$fit + 1.98 * NewData_MN1$se


## График предсказаний фиксированной части модели
ggplot(data = NewData_MN1, aes(x = year_f, y = fitted, colour = graze_f)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr)) +
  facet_wrap(~ AspectCat) +
  geom_jitter(data = graz, aes(y = height), alpha = 0.35, size = 1) +
  theme(axis.text.x = element_text(angle = 90))


## Задание 4 ------------------------------------

# Приведите решение с подбором оптимальной модели

## Подбор оптимальной модели
## Анализ остатков
## Результаты после оптимизации


