# ---
# title: "Дисперсионный анализ, часть 2"
# subtitle: "Линейные модели..."
# author: "Марина Варфоломеева, Вадим Хайтов"
# institute: "Кафедра Зоологии беспозвоночных, Биологический факультет, СПбГУ"

# ## Пример: Удобрение и беспозвоночные ########################
#
# Влияет ли добавление азотных и фосфорных
# удобрений на беспозвоночных?
# Небольшие искуственные субстраты экспонировали в
# течение разного времени в верхней части
# сублиторали (Hall et al., 2000).
# Зависимая переменная:
# - `richness` --- Число видов
# Факторы:
# - `time` --- срок экспозиции (2, 4 и 6 месяцев)
# - `treat` --- удобрения (добавляли или нет)
# Планировали сделать 5 повторностей для каждого сочетания факторов
# Данные из Quinn, Keough, 2002
#
# ## Знакомимся с данными
fert <- read.csv(file="data/hall.csv")
str(fert)
# Для удобства названия переменных маленькими буквами
colnames(fert) <- tolower(colnames(fert))
# Время делаем фактором
fert$time <- factor(fert$time)
levels(fert$time)

# ## Пропущенные значения
colSums(is.na(fert))

# ## Объемы выборок в группах
table(fert$time, fert$treat)

# ##   Посмотрим на график
library(ggplot2)
theme_set(theme_bw())
gg_rich <- ggplot(data = fert, aes(x = time, y = richness, colour = treat)) +
  stat_summary(geom = "pointrange", fun.data = mean_cl_normal)
gg_rich

# - Вполне возможно, здесь есть гетерогенность дисперсий.
# ## Преобразовываем данные
fert$log_rich <- log10(fert$richness + 1)

# # Двухфакторный дисперсионный анализ в параметризации индикаторов
# ## Подбираем линейную модель в параметризации индикаторов (contr.treatment)
mod_treatment <- lm(log_rich ~ treat * time, data = fert)
mod_treatment

# # Двухфакторный дисперсионный анализ в параметризации эффектов
# ## Подбираем линейную модель в параметризации эффектов (contr.sum)
mod_sum <- lm(log_rich ~ treat * time, data = fert,
              contrasts = list(treat = 'contr.sum', time = 'contr.sum'))
mod_sum

# # Диагностика линейной модели ###########################################

# Данные для анализа остатков
mod_treatment_diag <- fortify(mod_treatment) # функция из пакета ggplot2
head(mod_treatment_diag, 2)

# ## График расстояния Кука
ggplot(mod_treatment_diag, aes(x = 1:nrow(mod_treatment_diag), y = .cooksd)) +
  geom_bar(stat = 'identity')

# ## График остатков от предсказанных значений
gg_resid <- ggplot(data = mod_treatment_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0)
gg_resid

# ## График зависимости остатков от предикторов в модели
ggplot(data = mod_treatment_diag, aes(x = treat, y = .stdresid, colour = time)) +
  geom_boxplot() + geom_hline(yintercept = 0)

# ## Квантильный график остатков
library(car)
qqPlot(mod_treatment, id = FALSE) # функция из пакета car



# # Несбалансированные данные, типы сумм квадратов #####################################
#
# ## Несбалансированные данные - когда численности в группах по факторам различаются


# # Многофакторный дисперсионный анализ в R ###########################################

# ## Дисперсионный анализ со II типом сумм квадратов
# При таком способе, сначала тестируется взаимодействие, затем отдельные факторы в модели без взаимодействия.
mod_treatment <- lm(log_rich ~ treat * time, data = fert)
library(car)
Anova(mod_treatment, type = "II")

# ## Дисперсионный анализ c III типом сумм квадратов
# При этом способе вначале тестируют взаимодействие, когда все другие факторы есть в модели. Затем тестируют факторы, когда все другие факторы и взаимодействие есть в модели.
mod_sum <- lm(log_rich ~ treat * time, data = fert,
            contrasts = list(treat = contr.sum, time = contr.sum))
Anova(mod_sum, type = 3)

# # Пост хок тест для взаимодействия факторов ###########################################

# 1. Создаем переменную-взаимодействие
# 2. Подбираем модель без свободного члена
# 3. Делаем пост хок тест для этой модели


# ## Задание 1 ---------------------------------------------------------
# Дополните этот код, чтобы посчитать пост хок тест Тьюки по взаимодействию факторов

# Создаем переменную-взаимодействие
fert$treat_time <- interaction(fert$treat, fert$time)
# Подбираем линейную модель от этой переменной без свободного члена
fit_inter <- lm()
# Делаем пост хок тест для этой модели
library(multcomp)
dat_tukey <- glht(, linfct = mcp( = "Tukey"))
summary()



# График предсказаний модели ###########################################

# ## Данные для графика при помощи `predict()`

# У нас два дискретных фактора, поэтому вначале используем `expand.grid()`
MyData <- expand.grid(treat = levels(fert$treat),
                      time = levels(fert$time))
MyData <- data.frame(
  MyData,
  predict(mod_treatment, newdata = MyData, interval = "confidence")
  )
# Обратная трансформация (не забываем про единичку, которую прибавляли)
MyData$richness <- 10^MyData$fit - 1
MyData$LWR <- 10^MyData$lwr - 1
MyData$UPR <- 10^MyData$upr - 1
MyData


# ## Задание 2 ---------------------------------------------------
# Создайте MyData вручную для модели в обычной параметризации:
# - предсказанные значения
# - стандартные ошибки
# - верхнюю и нижнюю границы доверительных интервалов

MyData <- expand.grid(treat = levels(fert$treat),
                     time = levels())
X <- model.matrix(~ , data = )
betas <- coef()
MyData$fit <-
MyData$se <-   (X %*% vcov(mod_treatment) %*% t(X))
MyData$lwr <- MyData$ - 1.96 *
MyData$upr <- MyData$ + 1.96 *

# Обратная трансформация
MyData$richness <-
MyData$LWR <-
MyData$UPR <-
MyData


# ## Задание 3 ----------------------------------------------------
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

