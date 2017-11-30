# title: "Смешанные линейные модели (вложенные случайные факторы)"
# subtitle: "Линейные модели..."
# author: "Марина Варфоломеева, Вадим Хайтов"

## Пример: Высота растений и выпас скота
#
# Вообще-то, статья Gennet et al. 2017 о птицах, но чтобы про них что-то лучше понять, нужно разобраться с их местообитанием.
#
# Как в разные годы высота растительного покрова зависит от выпаса скота, экспозиции склона и проективного покрытия местных растений?
#
# Зависимая переменная:
#
# - **height** - высота растительного покрова
#
# Предикторы:
#
# - **graze** - выпас коров (0, 1)
# - **AspectCat** - экспозиция (S, N)
# - **nativecov** - покрытие местной флоры %
# - **slope** - наклон
# - **year** - год наблюдений
# - **Park** - парк
# - **plotID** - уникальный идентификатор участка
#

# Данные:
# Gennet, S., Spotswood, E., Hammond, M. and Bartolome, J.W., 2017. Livestock grazing supports native plants and songbirds in a California annual grassland. PloS one, 12(6), p.e0176367.


# Откроем и переформатируем данные так, чтобы не было дублирования и каждому участку соответствовала одна строчка.
library(readxl)
library(tidyr)
gr <- read_excel("data/Grazing_native_plants_Gennet_et_al._2017_S1.xlsx")
graz <- gr %>% spread(Species, presence)


## Знакомство с данными
#
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

## Модель

library(nlme)
MN1 <- lme(height ~ graze_f*AspectCat + year_f + nativecov_sq + slope,
          random = ~ 1|Park/plotID,
          data = graz, method = "ML")


## Анализ остатков

# Данные для анализа остатков
MN1_diag <- data.frame(
  graz,
  pear_res = residuals(MN1, type = "pearson"),
  fitted = fitted(MN1, type = "response"))


## График остатков
library(ggplot2)
gg_res <- ggplot(data = MN1_diag, aes(y = pear_res))
gg_res + geom_point(aes(x = fitted)) +
  geom_smooth(aes(x = fitted))


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



## Тесты отношения правдоподобий для полной модели

library(car)
Anova(MN1)


## Задание 1 -------------------------------------
#
# Рассчитайте внутриклассовую корреляцию
#
# - Для наблюдений на одном и том же участке
# - Для наблюдений в одном и том же парке
#


## Результаты полной модели
summary(MN1_fin)

## Данные для графика предсказаний фиксированной части модели

# Исходные данные
NewData_MN1_fin <- expand.grid(graze_f = levels(graz$graze_f),
            AspectCat = levels(graz$AspectCat),
            year_f = levels(graz$year_f))
NewData_MN1_fin$nativecov_sq <- mean(graz$nativecov_sq)
NewData_MN1_fin$slope <- mean(graz$slope)

# Предсказанные значения при помощи матриц
X <- model.matrix(~ graze_f * AspectCat + year_f + nativecov_sq + slope,
                  data = NewData_MN1_fin)
betas = fixef(MN1_fin)
NewData_MN1_fin$fitted <- X %*% betas

# Cтандартные ошибки и дов. интервалы
NewData_MN1_fin$se <- sqrt( diag(X %*% vcov(MN1_fin) %*% t(X)) )
NewData_MN1_fin$lwr <- NewData_MN1_fin$fit - 1.96 * NewData_MN1_fin$se
NewData_MN1_fin$upr <- NewData_MN1_fin$fit + 1.96 * NewData_MN1_fin$se

## График предсказаний фиксированной части модели
ggplot(data = NewData_MN1_fin, aes(x = year_f, y = fitted, colour = graze_f)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr)) +
  facet_wrap(~ AspectCat) +
  geom_jitter(data = graz, aes(y = height), alpha = 0.35, size = 1) +
  theme(axis.text.x = element_text(angle = 90))


## Задание 4 -------------------------------------
#
# Оптимизируйте модель с предыдущего шага
# Сделайте анализ остатков
# Опишите и визуализируйте финальную модель


