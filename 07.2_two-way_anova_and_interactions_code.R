# title: "Дисперсионный анализ, часть 2"
# subtitle: "Линейные модели..."
# author: "Марина Варфоломеева, Вадим Хайтов"

#### Пример: Удобрение и беспозвоночные ###########

# Влияет ли добавление азотных и фосфорных
# удобрений на беспозвоночных?
#
# Небольшие искуственные субстраты экспонировали в
# течение разного времени в верхней части
# сублиторали (Hall et al., 2000, данные из Quinn, Keough, 2002).
#
# Зависимая переменная:
# - `richness` --- Число видов
#
# Факторы:
# - `time` --- срок экспозиции (2, 4 и 6 месяцев)
# - `treat` --- удобрения (добавляли или нет)
#
# Планировали сделать 5 повторностей для каждого сочетания факторов


# ## Знакомимся с данными ########################

fert <- read.csv(file="data/hall.csv")
str(fert)
# Для удобства названия переменных маленькими буквами
colnames(fert) <- tolower(colnames(fert))
# Время делаем фактором
fert$time <- factor(fert$time)
levels(fert$time)

# ## Пропущенные значения
colSums(is.na(x))

# ## Объемы выборок в группах
table(fert$time, fert$treat)

library(ggplot2)
theme_set(theme_bw())
gg_rich <- ggplot(data = fert, aes(x = time, y = richness, colour = treat)) +
  stat_summary(geom = "pointrange", fun.data = mean_se)
gg_rich

# Преобразовываем данные
fert$log_rich <- log10(fert$richness + 1)


#### Способы кодирования дискретных факторов #######

### В параметризации фиктивных переменных

# Уровни фактора "удобрения"
contr.treatment(levels(fert$treat))
# Уровни фактора "время экспозиции"
contr.treatment(levels(fert$time))
# Модельная матрица целиком
X_trt <- model.matrix(~ treat*time, data = fert)
X_trt

# ## В параметризации эффектов (contr.sum)

# Уровни фактора "удобрения"
contr.sum(levels(fert$treat))
# Уровни фактора "время экспозиции"
contr.sum(levels(fert$time))
# Модельная матрица целиком
X_sum <- model.matrix(~ treat*time, data = fert,
  contrasts = list(treat = "contr.sum", time = "contr.sum"))
X_sum




#### Многофакторный дисперсионный анализ в R ###########

## Дисперсионный анализ со II типом сумм квадратов
fmod2 <- lm(log_rich ~ treat * time, data = fert)
library(car)
Anova(fmod2, type = "II")

# ## Дисперсионный анализ c III типом сумм квадратов
fmod3 <- lm(log_rich ~ treat * time, data = fert,
            contrasts = list(treat = contr.sum, time = contr.sum))
Anova(fmod3, type = 3)


#### Пост хок тест для взаимодействия факторов ##########


#### Задание 1 -----------------------------------
# Дополните этот код, чтобы посчитать пост хок
# тест Тьюки по взаимодействию факторов

# Создаем переменную-взаимодействие
fert$treat_time <- interaction(fert$treat, fert$time)
# Подбираем линейную модель от этой переменной без
# свободного члена
fit_inter <- lm()
# Делаем пост хок тест для этой модели
library(multcomp)
dat_tukey <- glht(, linfct = mcp( = "Tukey"))
summary()


# ## Данные для графика при помощи `predict()` ##########
#
# У нас два дискретных фактора, поэтому вначале
# используем `expand.grid()`
MyData <- expand.grid(treat = levels(fert$treat),
                      time = levels(fert$time))
MyData <- data.frame(
  MyData,
  predict(fmod2, newdata = MyData, interval = "confidence")
  )
# Обратная трансформация (не забываем про
# единичку, которую прибавляли)
MyData$richness <- 10^MyData$fit - 1
MyData$LWR <- 10^MyData$lwr - 1
MyData$UPR <- 10^MyData$upr - 1
MyData


# ## Задание 2 -----------------------------------
#
# Создайте MyData вручную для модели в обычной параметризации:
#
# - предсказанные значения
# - стандартные ошибки
# - верхнюю и нижнюю границы доверительных интервалов

MyData <- expand.grid(treat = levels(fert$treat),
                     time = levels())
X <- model.matrix(~ , data = )
betas <- coef()
MyData$fit <-
MyData$se <-   (X %*% vcov(fmod2) %*% t(X))
MyData$lwr <- MyData$ - 1.96 *
MyData$upr <- MyData$ + 1.96 *

# Обратная трансформация
MyData$richness <-
MyData$LWR <-
MyData$UPR <-
MyData


# ## Задание 3 -----------------------------------

# Постройте график результатов, на котором будут
# изображены предсказанные средние значения
# видового богатства в зависимости от тритмента и
# времени экспозиции.

pos <- position_dodge(width = 0.2)
gg_linep <- ggplot(data = , aes()) +
  geom_  (position = pos) +
  geom_  (aes(group = ), position = pos) +
  geom_  (position = pos, width = 0.1)
gg_linep


# ## Приводим график в приличный вид
gg_final <- gg_linep + labs(x = "Экспозиция",  y = "Число видов") +
  scale_colour_brewer(name = "", palette = "Dark2",
    labels = c("Контроль", "Эксперимент"))
gg_final

