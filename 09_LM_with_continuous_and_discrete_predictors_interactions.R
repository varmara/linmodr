## "Взаимодействие дискретных и непрерывных предикторов"


# Модель со взаимодействием на примере данных о весе новорожденных

## Вес новорожденных и курение

# Как вес новорожденных зависит от возраста матери и того, курит ли она
#
# - `age` --- возраст матери
# - `lwt` --- вес матери до беременности
# - `race` --- раса (1-белые, 2-черные, 3-другие)
# - `smoke` --- курение во время беременности (1-да,2-нет)
# - `ptl` --- число предыдущих преждевременных родов
# - `ht` --- гипертензия
# - `ui` --- гипертонус матки
# - `ftv` --- число визитов к врачу в последний триместр
# - `bwt` --- вес новорожденного, г

wt <- read.table("data/birthwt.csv", header = TRUE, sep = ";")

wt$smoke[wt$smoke == 1] <- 'Smoker'
wt$smoke[wt$smoke == 0] <- 'Non smoker'

wt$smoke <- factor(wt$smoke)


wt$race <- factor(wt$race)
wt$smoke <- factor(wt$smoke)
wt$ptl <- factor(wt$ptl)
wt$ht <- factor(wt$ht)
wt$ui <- factor(wt$ui)
wt$ftv <- factor(wt$ftv)



## Задание
#
# - Исследуйте данные о весе новорожденных
# - Постройте модель зависимости веса новорожденных от возраста матери, того курит она или нет и взаимодействия предикторов
# - Проверьте условия применимости этой модели
# - Упростите модель, если это возможно
# - Напишите общее уравнение и отдельные уравнения модели
# - Постройте график предсказаний модели
#

gg_dot <- ggplot(wt, aes(y = 1:) + geom_point()
gg_dot + aes( = age)
gg_dot + aes(x = )


wt_mod_1 <- lm(bwt ~ , data = wt)

library(car)
(wt_mod_1)


## Проверка на гомогенность углов наклона

wt_mod_2 <- lm(bwt ~ , data = wt)
drop1(wt_mod_2, test = "F")

# Данные для графиков остатков
wt_mod_2_diag <- (wt_mod_2)
wt_mod_2_diag <- data.frame(
  wt_mod_2_diag,
  wt[, c("lwt", "race", "smoke", "ptl", "ht", "ui", "ftv")])

### График расстояния Кука

ggplot(wt_mod_2_diag, aes(x = 1:, y = )) +
  geom_bar(stat = "identity")

### График остатков от предсказанных значений

gg_resid <- ggplot(data = wt_mod_2_diag, aes(x = , y = )) +
  geom_point() + geom_hline(yintercept = ) + geom_smooth()
gg_resid

### Графики остатков от предикторов в модели и не в модели

library(gridExtra)
grid.arrange(gg_resid + aes(x = age),
             gg_resid + aes(x = lwt),
             nrow = 1)

gg_box <- ggplot(data = wt_mod_2_diag, aes(x = smoke, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)

grid.arrange(gg_box + aes(x = ),
             gg_box + aes(x = ),
             gg_box + aes(x = ),
             gg_box + aes(x = ),
             gg_box + aes(x = ),
             nrow = 2)

### Квантильный график остатков

qqPlot()

summary(wt_mod_2)


## Таблица дисперсионного анализа {.smaller}
Anova(, type = 3)



  ## Модельная матрица при наличии взаимодействий дискретного и непрерывного предиктора

X <- model.matrix()


## Вектор коэффициентов
b <- coefficients(wt_mod_2)

as.numeric(    (wt_mod_2)[1])



Матричное умножение
(    %*%   ) [1]


## Предсказанное значение для объекта, относящегося к базовому уровню

X[1,] # Первая строка в модельной матрице

b

# Что происходит при матричном умножении




## Предсказанное значение для объекта, не относящегося к базовому уровню

as.numeric(fitted(wt_mod_2)[3])

X[3,] # Третья строка в модельной матрице
b # Коэффциенты

# Что происходит при матричном умножении




  # Визуализация модели

  ## Сложные модели лучше по возможности изображать в виде графика {.smaller}

  ### Данные для графика

library(dplyr)
new_data <- wt %>% group_by(smoke)%>%
  do(data.frame(age = seq(min(.$age), max(.$age), length.out = 100)))

# Предсказанные значения
Predictions <- predict(wt_mod_2, newdata = new_data, se.fit = TRUE)
new_data$fit <- Predictions$fit

# Стандартные ошибки
new_data$se <- Predictions$se.fit




# Критические значения t
t_crit <- qt(0.975, df = nrow(wt) - length(coef(wt_mod_2)))

# Доверительный интервал
new_data$upr <- new_data$fit + t_crit * new_data$se
new_data$lwr <- new_data$fit - t_crit * new_data$se


## Рисуем график предсказаний

Pl_smoke <- ggplot(new_data, aes(x = age, y = fit)) +
  geom_ribbon(alpha = 0.2, aes(ymin = lwr, ymax = upr, group = smoke)) +
  geom_line(aes(colour = smoke))

Pl_smoke

## На графике предсказаний можно показать исходные значения

Pl_final <- Pl_smoke +
  geom_point(data = wt, aes(x = age, y = bwt, colour = smoke)) +
  scale_colour_discrete('Курила ли мать', labels = c('Не курила', 'Курила')) +
  labs(x = 'Возраст матери', y = 'Вес новорожденного')

Pl_final
