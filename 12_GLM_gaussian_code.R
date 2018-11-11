# title: "Обобщенные линейные модели с нормальным распределением остатков"
# subtitle: "Линейные модели..."
# author: "Марина Варфоломеева, Вадим Хайтов"
# institute: "Кафедра Зоологии беспозвоночных, Биологический факультет, СПбГУ"

# ## Пример -- энергетическая ценность икры
#
# Один из показателей, связанных с
# жизнеспособностью икры -- доля сухого вещества.
# Она пропорциональна количеству запасенной
# энергии.
# Отличается ли энергетическая ценность икры
# большой озерной форели в сентябре и ноябре?
# Данные: Lantry et al., 2008
library(Stat2Data)
data(FishEggs)

str(FishEggs)

colSums(is.na(FishEggs))


# ## Меняем порядок уровней факторов
levels(FishEggs$Month)

# Делаем базовым уровнем сентябрь.
FishEggs$Month <- relevel(FishEggs$Month, ref = 'Sep')

# Теперь уровни в хронологическом порядке:
levels(FishEggs$Month)

# ## Объемы выборок
table(FishEggs$Month)

# ## Ищем выбросы
library(cowplot)
library(ggplot2)
theme_set(theme_bw())
gg_dot <- ggplot(FishEggs, aes(y = 1:nrow(FishEggs))) + geom_point()
plot_grid(gg_dot + aes(x = Age),
          gg_dot + aes(x = PctDM), nrow = 1)

# ## Подбираем модель
mod <- glm(PctDM ~ Age * Month, data = FishEggs)
mod
# Чтобы записать модель, нужна еще сигма.
sigma(mod)


# # Диагностика модели

# ## Разновидности остатков в GLM
# ### Остатки в масштабе отклика (response residuals)
resid(mod, type = 'response')[1:5]

# ### Пирсоновские остатки (Pearson's residuals)
resid(mod, type = 'pearson')[1:5]

# Для GLM с нормальным распределением отклика оба типа остатков одинаковы.


# ## Проверка на коллинеарность
vif(update(mod, . ~ . - Age:Month))

# ## Данные для анализа остатков
mod_diag <- fortify(mod) # функция из пакета ggplot2
head(mod_diag)

# ## График расстояния Кука
ggplot(mod_diag, aes(x = 1:nrow(mod_diag), y = .cooksd)) +
  geom_bar(stat = 'identity') +
  geom_hline(yintercept = 1, linetype = 2)

# ## График остатков от предсказанных значений
gg_resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0)
gg_resid


# ## График зависимости остатков от предикторов в модели
ggplot(data = mod_diag, aes(x = Age, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0)
ggplot(data = mod_diag, aes(x = Month, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)


# # Тестирование значимости коэффициентов
summary(mod)

# ## Анализ девиансы

# ## Тестируем значимость модели целиком при помощи LRT
null_model <- glm(PctDM ~ 1, data = FishEggs)
anova(null_model, mod, test = 'Chi')


# ## Тестируем значимость предикторов при помощи LRT

drop1(mod, test = 'Chi')
mod_no_int <- update(mod, . ~ . -Age:Month)
drop1(mod_no_int, test = 'Chi')


# ## Доля объясненной девиансы

(mod$null.deviance - mod$deviance) / mod$null.deviance


# ## Данные для графика предсказаний
library(dplyr)
New_Data <- FishEggs %>% group_by(Month) %>%
  do(data.frame(Age = seq(min(.$Age), max(.$Age), length.out = 100)))
head(New_Data)

# ## либо предсказания при помощи `predict()`
Predictions <- predict(mod, newdata = New_Data, se.fit = TRUE)
New_Data$fit <- Predictions$fit  # Предсказанные значения
New_Data$se <- Predictions$se.fit # Стандартные ошибки
t_crit <- qt(0.975, df = nrow(FishEggs) - length(coef(mod))) # t для дов. инт.
New_Data$lwr <- New_Data$fit - t_crit * New_Data$se
New_Data$upr <- New_Data$fit + t_crit * New_Data$se
head(New_Data)


# ## либо предсказания, полученные вручную
X <- model.matrix(~ Age * Month, data = New_Data) # Модельная матрица
betas <- coef(mod) # Коэффициенты
New_Data$fit <- X %*% betas  # Предсказанные значения
New_Data$se <- sqrt(diag(X %*% vcov(mod) %*% t(X))) # Стандартные ошибки
t_crit <- qt(0.975, df = nrow(FishEggs) - length(coef(mod))) # t для дов. инт.
New_Data$lwr <- New_Data$fit - t_crit * New_Data$se
New_Data$upr <- New_Data$fit + t_crit * New_Data$se
head(New_Data)

# ## График предсказаний модели
Plot_egg <- ggplot(New_Data, aes(x = Age, y = fit)) +
  geom_ribbon(alpha = 0.2, aes(ymin = lwr, ymax = upr, group = Month)) +
  geom_line(aes(colour = Month), size = 1) +
  geom_point(data = FishEggs, aes(x = Age, y = PctDM, colour = Month))
Plot_egg


