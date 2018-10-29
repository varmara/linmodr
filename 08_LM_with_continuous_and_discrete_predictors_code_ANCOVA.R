#"Дискретные предикторы в линейных моделях. Взаимодействие предикторов"


# Analysis of covariance (ANCOVA) --- частный случай общих линейных моделей  {.segue}

## Емкость легких у разных возрастных групп
# Различается ли объем легких  разных возрастных групп пациентов, которых готовят к операции?
#
#   Загрузите с сайта данные `tlc.csv` и поместите файл в папку `data` в рабочей директории.
#

tlc <- read.table('data/tlc.csv', sep = ';', header = TRUE)
str(tlc)

# Переменные:
#
#   - `age` -- возраст.
# - `sex` -- пол (1 - женский; 2 - мужской)
# - `height` -- рост (см)
# - `tlc`-- объем легких (л)

# Создаем переменную, кодирующую возрастную группу
tlc$age_group[tlc$age < 20] <- 'teenagers'
tlc$age_group[tlc$age < 30 & tlc$age >= 20] <- 'young'
tlc$age_group[tlc$age >= 30] <- 'adult'

# Создаем фактор с удобным порядком градаций
tlc$age_group <- factor(tlc$age_group, levels = c('teenagers', 'young', 'adult'))

table(tlc$age_group, tlc$sex)

Mod <- lm(tlc ~ age_group, data = tlc)
summary(Mod)


## Данные для графика предсказаний модели

# Создаем искусственный датасет со всеми возможными значениями предиктора
new_data <- data.frame(age_group = factor(levels(tlc$age_group),
                                          levels = levels(tlc$age_group)))
# Предсказанные моделью средние значения зависимой перменной
new_data$fit <- predict(Mod, newdata = new_data, se.fit = TRUE)$fit

# Стандартные ошибки
new_data$se <- predict(Mod, newdata = new_data, se.fit = TRUE)$se.fit

# Критические значения t для расчета доверительных интервалов
t_crit <- qt(0.975, df = nrow(tlc) - length(coef(Mod)))

# Границы доверительных интервалов
new_data$upr <- new_data$fit + t_crit * new_data$se
new_data$lwr <- new_data$fit - t_crit * new_data$se



## График предсказаний модели

library(ggplot2)
ggplot(new_data, aes(x = age_group, y = fit)) +
  geom_bar(stat = 'identity', aes(fill = age_group)) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
  scale_x_discrete(labels = c('Подростки', 'Молодые', 'Взрослые')) +
  guides(fill = 'none') +
  labs(x = 'Возрастная группа', y = 'Объем легких')



## Можно ли доверять полученным результатам?

Mod_diag <- fortify(Mod) # Создаем датафрейм с диагностическими данными
Mod_diag$height <- tlc$height # Переменная, не вошедшая в модель

ggplot(Mod_diag, aes(x = height, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0) +
  geom_smooth(method = 'lm')



## Полная модель с учетом ковариаты

Mod_cov <- lm(tlc ~ age_group * height, data = tlc)



## Присутствует ли взаимодействие?

  (  , test =  )


## ANCOVA: модель с учетом ковариаты
#
# Модель в ANCOVA ничем не отличается от любой другой регрессионной модели, включающей непрерывные и дискретные предикторы без их взаимодействия.

Mod_cov_2 <- lm(tlc ~ age_group + height, data = tlc)




## Диагностика модели в ANCOVA  {.segue}




library(car)
vif(Mod_cov_2)

## График остатков
# Датафрейм с диагностическими данными
Mod_cov_2_diag <- fortify(Mod_cov_2)

ggplot(Mod_cov_2_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0) +
  geom_smooth(method = 'lm')


  ## Остатки в зависимости от непрерывного предиктора
ggplot(Mod_cov_2_diag, aes(x = height, y = .stdresid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(method = 'lm')

## Остатки в зависимости от дискретного предиктора
ggplot(Mod_cov_2_diag, aes(x = age_group, y = .stdresid)) +
  geom_boxplot() +
  geom_hline(yintercept = 0)

## Результаты ANCOVA
summary(Mod_cov_2)



## Cкорректированные средние {.smaller}


# Создаем искусственный датасет со всеми возможными значениями дискретного предиктора
# и средним значением ковариаты
new_data <- data.frame(age_group = factor(levels(tlc$age_group),levels = levels(tlc$age_group)), height = mean(tlc$height))
# Предсказанные моделью скорретированные средние значения зависимой переменной
new_data$fit <- predict(Mod_cov_2, newdata = new_data, se.fit = TRUE)$fit

# Стандартные ошибки
new_data$se <- predict(Mod_cov_2, newdata = new_data, se.fit = TRUE)$se.fit
# t для расчета доверительного интервала
t_crit <- qt(0.975, df = nrow(tlc) - length(coef(Mod_cov_2)))
# Границы доверительных интервалов
new_data$upr <- new_data$fit + t_crit * new_data$se
new_data$lwr <- new_data$fit - t_crit * new_data$se



## Визуализация модели {.smaller}
ggplot(new_data, aes(x = age_group, y = fit)) +
  geom_bar(stat = 'identity', aes(fill = age_group)) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
  scale_x_discrete(labels = c('Подростки', 'Молодые', 'Взрослые')) +
  guides(fill = 'none') +
  labs(x = 'Возрастная группа', y = 'Объем легких')




