# ---
# title: "Смешанные линейные модели (вложенные случайные факторы)"
# subtitle: "Линейные модели..."
# author: "Марина Варфоломеева, Вадим Хайтов"

# ## Вложенные факторы (Nested effects)

# ## Пример: Высота растений и выпас скота
#
# Вообще-то, статья Gennet et al. 2017 о птицах,
# но чтобы про них что-то лучше понять, нужно
# разобраться с их местообитанием.
# Как в разные годы высота растительного покрова
# зависит от выпаса скота, экспозиции склона и
# проективного покрытия местных растений?
#
# Зависимая переменная:
# - **height** - высота растительного покрова
#
# Предикторы:
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
#
# ## Открываем данные
#
# Исходные данные не опрятны (this data is not
# tidy)! Каждый участок в каждом году фигурирует
# два раза (т.к. информация о
# присутствии-отсутствии каких-то видов на участке
# записана в разных строчках).
# Откроем и переформатируем данные так, чтобы не
# было дублирования и каждому участку
# соответствовала одна строчка.
library(readxl)
library(tidyr)
gr <- read_excel("data/Grazing_native_plants_Gennet_et_al._2017_S1.xlsx")
graz <- gr %>% spread(Species, presence)


# Есть ли пропущенные значения?
sum(is.na(graz))

# Сколько участков было в каждом парке в каждый год?
with(graz, table(Park, year))

# ## Как закодированы переменные?
str(graz)

# Сделаем факторами переменные, которые понадобятся для модели
graz$graze_f <- factor(graz$graze)
graz$AspectCat <- factor(graz$AspectCat)
graz$year_f <- factor(graz$year)

# Извлечем корень из обилия местных видов
graz$nativecov_sq <- sqrt(graz$nativecov)


# ## Модель

library(lme4)
ML1 <- lmer(height ~ graze_f*AspectCat + year_f + nativecov_sq + slope + (1|Park/plotID),
          data = graz, REML = FALSE)

# ## Анализ остатков
ML1_diag <- data.frame(
  graz,
  .pear_res = residuals(ML1, type = "pearson"),
  .fitted = fitted(ML1, type = "response"))

# ## График остатков
library(ggplot2); library(cowplot); theme_set(theme_bw())
gg_res <- ggplot(data = ML1_diag, aes(y = .pear_res))
gg_res + geom_point(aes(x = .fitted)) +
  geom_smooth(aes(x = .fitted))

# ## Графики остатков от переменных в модели
plot_grid(gg_res + geom_boxplot(aes(x = graze_f)),
gg_res + geom_boxplot(aes(x = AspectCat)),
gg_res + geom_boxplot(aes(x = year_f)),
gg_res + geom_point(aes(x = nativecov_sq)),
gg_res + geom_point(aes(x = slope)),
ncol = 3)


# ## Графики остатков от переменных не в модели
plot_grid(
  gg_res + geom_point(aes(x = heatloadrel)),
  gg_res + geom_point(aes(x = sqrt(litt))),
  gg_res + geom_point(aes(x = sqrt(bare))),
  ncol = 3)

# ## Тесты отношения правдоподобий для полной модели (ML!!!)
drop1(ML1, test = 'Chi')



# ## Задание 3 -------------------------------------------
#
# Рассчитайте внутриклассовую корреляцию (Нужна модель, подобранная REML!!!)
REML1 <- lmer()
# - Для наблюдений на одном и том же участке
# - Для наблюдений в одном и том же парке




# ## Данные для графика предсказаний фиксированной части модели
# Исходные данные
NewData_REML1 <- expand.grid(graze_f = levels(graz$graze_f),
            AspectCat = levels(graz$AspectCat),
            year_f = levels(graz$year_f))
NewData_REML1$nativecov_sq <- mean(graz$nativecov_sq)
NewData_REML1$slope <- mean(graz$slope)

# Предсказанные значения при помощи матриц
X <- model.matrix(~ graze_f * AspectCat + year_f + nativecov_sq + slope,
                  data = NewData_REML1)
betas = fixef(REML1)
NewData_REML1$fit <- X %*% betas

# Cтандартные ошибки и дов. интервалы
NewData_REML1$se <- sqrt( diag(X %*% vcov(REML1) %*% t(X)) )
NewData_REML1$lwr <- NewData_REML1$fit - 2 * NewData_REML1$se
NewData_REML1$upr <- NewData_REML1$fit + 2 * NewData_REML1$se

# ## График предсказаний фиксированной части модели
ggplot(data = NewData_REML1, aes(x = year_f, y = fit, colour = graze_f)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr)) +
  facet_wrap(~ AspectCat) +
  geom_jitter(data = graz, aes(y = height), alpha = 0.35, size = 1) +
  theme(axis.text.x = element_text(angle = 90))


# ## Задание 4 -----------------------------------------
#
# Оптимизируйте модель с предыдущего шага
# Сделайте анализ остатков
# Опишите и визуализируйте финальную модель
