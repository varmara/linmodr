# ---
# title: "Обобщенные линейные модели с нормальным распределением остатков"
# subtitle: "Линейные модели..."
# author: "Марина Варфоломеева, Вадим Хайтов"


# ## Гадючий лук, копеечник и визиты опылителей
# Гадючий лук (мускари, _Leopoldia comosa_) ---
# представитель родной флоры острова Менорка. В
# 18-19вв на остров завезли копеечник венечный
# (_Hedysarum coronarium_), который быстро
# натурализовался. Оба вида цветут одновременно и
# нуждаются в опылении насекомыми.
#
# Как зависит число визитов опылителей на цветки
# мускари от присутствия вселенца и разнообразия
# флоры в ближайшей окрестности? (Данные
# Montero-Castaño, Vilà, 2015)

# ## Переменные
#
# - `Visits` --- число визитов всех опылителей на цветок _Leopoldia_
# - `Treatment` --- тип площадки, тритмент (фактор с 3 уровнями):
#     - `Invaded` --- _Leopoldia_ в смеси с видом-вселенцем;
#     - `Removal` --- _Leopoldia_ в смеси с видом-вселенцем с удаленными цветками;
#     - `Control` --- _Leopoldia_ без вида вселенца.
# - `DiversityD_1` --- Разнообразие флоры на площадке ($exp(H’)$,
# где $H'$ --- индекс Шеннона-Уивера)  (на луг с более разнообразной
# растительностью прилетит больше опылителей).
# - `Flowers` --- число цветков _Leopoldia_ на площадке (чем больше, тем больше опылителей).
# - `Hours` --- продолжительность наблюдений (чем дольше, тем больше насчитали).

# Другие переменные:
# - `Total_1` --- общая плотность цветков
# - `Visits_NO_Apis` --- посещения опылителей без учета пчел
# - `Fruit` --- число цветов с плодами через месяц
# - `No_Fruit` --- число цветов без плодов через месяц


# ## Открываем и знакомимся с данными
library(readxl)
pol <- read_excel("data/Pollinators_Montero-Castano, Vila, 2015.xlsx", sheet = 1)
head(pol)

# Сколько пропущенных значений?
colSums(is.na(pol))

# ## Есть ли выбросы?
library(cowplot)
library(ggplot2)
theme_set(theme_bw())

dot_plot <- ggplot(pol, aes(y = 1:nrow(pol))) + geom_point()
plot_grid(dot_plot + aes(x = DiversityD_1), dot_plot + aes(x = Flowers),
          dot_plot + aes(x = Hours), nrow = 1)

# ## Каков объем выборки?



# Как распределены короткие периоды наблюдений по тритментам?



# ## Коллинеарны ли непрерывные и дискретные предикторы?
box_plot <- ggplot(pol, aes(x = Treatment)) + geom_boxplot()

plot_grid(box_plot + aes(y = DiversityD_1),
          box_plot + aes(y = Flowers), nrow = 1)


# ## Как распределена переменная-отклик?



# Какова пропорция нулей?



# ## Линейна ли связь между предикторами и откликом?
gg_shape <- ggplot(pol, aes(y = Visits/Hours, colour = Treatment)) +
  theme(legend.position = 'bottom')
plot_grid(
  gg_shape + geom_point(aes(x = Flowers)),
  gg_shape + geom_point(aes(x = DiversityD_1)),
nrow = 1)


# ## Если мы подберем GLM с нормальным распределением отклика

M_norm <- glm(Visits ~ Treatment + DiversityD_1 + Flowers + Hours, data = pol)
coef(M_norm)
sigma(M_norm)


# ## Данные для графика предсказаний простой линейной модели
NewData <- pol %>% group_by(Treatment)%>%
  do(data.frame(Flowers = seq(min(.$Flowers), max(.$Flowers), length.out=50))) %>%
  mutate(DiversityD_1 = mean(pol$DiversityD_1),
         Hours = mean(pol$Hours))

head(NewData)

# Модельная матрица и коэффициенты
X <- model.matrix(~ Treatment + DiversityD_1 + Flowers + Hours, data = NewData)
b <- coef(M_norm)
# Предсказания в масштабе функции связи (eta) совпадают с масштабом отклика (mu)
NewData$mu <- X %*% b
NewData$SE_mu <- sqrt(diag(X %*% vcov(M_norm) %*% t(X)))  # SE

head(NewData, 3)

# ## График предсказаний
ggplot(NewData, aes(x = Flowers, y = mu, fill = Treatment)) +
  geom_ribbon(aes(ymin = mu - 2 * SE_mu, ymax = mu + 2 * SE_mu), alpha=0.3)+
  geom_line(aes(colour = Treatment)) +
  geom_hline(yintercept = 0)


# ## Смотрим на результаты подбора модели
summary(M_norm)

# ## Анализ девиансы для модели с нормальным распределением отклика
drop1(M_norm, test = 'Chi')

# ## Нет ли коллинеарности предикторов
library(car)
vif(M_norm)


# ## Задание 1 -------------------------------------------------------
#
# Постройте график пирсоновских остатков от предсказанных значений для модели `M_norm`.
# Какие нарушения условий применимости вы на нем видите?

# Дополните код:

M_norm_diag <- data.frame(.fitted = fitted(),
                          .resid_p = residuals())

ggplot(data = , aes()) + geom_hline( = 0) +
  geom_point()




# ## GLM с Пуассоновским распределением отклика #############################

M_pois <- glm(Visits ~ Treatment + DiversityD_1 + Flowers + Hours, data = pol,
                   family = "poisson")
coef(M_pois)


# ## Смотрим на результаты подбора модели
summary(M_pois)

# ## Анализ девиансы для модели с Пуассоновским распределением отклика
drop1(M_pois, test = 'Chi')


# ## Данные для предсказаний
NewData <- pol %>% group_by(Treatment)%>%
  do(data.frame(Flowers = seq(min(.$Flowers), max(.$Flowers), length.out=50))) %>%
  mutate(DiversityD_1 = mean(pol$DiversityD_1),
         Hours = mean(pol$Hours))


# ## Предсказания модели при помощи операций с матрицами

# Модельная матрица и коэффициенты
X <- model.matrix(~ Treatment + DiversityD_1 + Flowers + Hours, data = NewData)
b <- coef(M_pois)

# Предсказанные значения и стандартные ошибки...
# ...в масштабе функции связи (логарифм)
NewData$fit_eta <- X %*% b
NewData$SE_eta <- sqrt(diag(X %*% vcov(M_pois) %*% t(X)))

# ...в масштабе отклика (применяем функцию, обратную функции связи)
NewData$fit_mu <- exp(NewData$fit_eta)
NewData$SE_mu <- exp(NewData$SE_eta)

head(NewData, 2)

# ## График предсказаний в масштабе функции связи
ggplot(NewData, aes(x = Flowers, y = eta, fill = Treatment)) +
  geom_ribbon(aes(ymin = eta - 2 * SE_eta, ymax = eta + 2 * SE_eta), alpha=0.3)+
  geom_line(aes(colour = Treatment)) + geom_hline(yintercept = 0)

# ## График предсказаний в масштабе переменной-отклика
ggplot(NewData, aes(x = Flowers, y = mu, fill = Treatment)) +
  geom_ribbon(aes(ymin = mu - 2 * SE_mu, ymax = mu + 2 * SE_mu), alpha=0.3)+
  geom_line(aes(colour = Treatment)) + geom_hline(yintercept = 0)


# Предсказания при помощи функции predict()
# ...в масштабе функции связи
predict_eta <- predict(M_pois, newdata = NewData, se.fit = TRUE)
NewData$eta <- predict_eta$fit
NewData$SE_eta <- predict_eta$se.fit
# ...в масштабе отклика
predict_mu <- predict(M_pois, newdata = NewData,
                      se.fit = TRUE, type = 'response')
NewData$mu <- predict_mu$fit
NewData$SE_mu <- predict_mu$se.fit

head(NewData, 2)


# ## Условия применимости GLM с Пуассоновским распределением отклика

# - Случайность и независимость наблюдений внутри групп.
# - Отсутствие сверхдисперсии. (Дисперсия остатков равна мат.ожиданию при каждом уровне значений предикторов).
# - Отсутствие коллинеарности предикторов.


# ## График остатков
M_pois_diag <- data.frame(.fitted = predict(M_pois, type = "response"),
                            .resid_p = residuals(M_pois, type = "pearson"))
ggplot(M_pois_diag, aes(x = .fitted, y = .resid_p)) +
  geom_point() +
  geom_hline(yintercept = 0)


# ## Проверка на сверхдисперсию
# Функция для проверки наличия сверхдисперсии в модели (автор Ben Bolker)
# http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html
overdisp_fun <- function(model) {
    rdf <- df.residual(model) # Число степеней свободы N - p
    rp <- residuals(model,type="pearson") # Пирсоновские остатки
    Pearson.chisq <- sum(rp^2) # Сумма квадратов остатков
    prat <- Pearson.chisq/rdf  # Степень избыточности дисперсии
    pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE) # Уровень значимости
    c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)        # Вывод результатов
}

overdisp_fun(M_pois)



# ## Квази-пуассоновские модели

M_quasi <- glm(Visits ~ Treatment + DiversityD_1 + Flowers + Hours, data = pol,
                 family = "quasipoisson")

coef(M_quasi)
summary(M_quasi)$dispersion

# ## Смотрим на результаты подбора модели
summary(M_quasi)

# ## Анализ девиансы для квази-пуассоновской модели
drop1(M_quasi, test = "F")

# ## GLM с отрицательным биномиальным распределением отклика
library(MASS)
M_nb <- glm.nb(Visits ~ Treatment + DiversityD_1 + Flowers + Hours, data = pol,
                 link = "log")
coef(M_nb)
summary(M_nb)$theta

# ## Смотрим на результаты подбора модели
summary(M_nb)

# ## Анализ девиансы модели с отрицательным биномиальным распределением отклика
drop1(M_nb, test = 'Chi')


# ## Задание 2 ----------------------------------------------------------

# Проведите диагностику модели `M_nb`.
# Видите ли вы какие-нибудь нарушения условий применимости?



# ## Данные для предсказаний
#
## ------------------------------------------------------------------------
NewData <- pol %>% group_by(Treatment)%>%
  do(data.frame(Flowers = seq(min(.$Flowers), max(.$Flowers), length.out=50))) %>%
  mutate(DiversityD_1 = mean(pol$DiversityD_1),
         Hours = mean(pol$Hours))


# Задание 3 -----------------------------------------------------------

# Получите предсказания при помощи операций с матрицами,

# Модельная матрица и коэффициенты
X <- model.matrix(~, data = NewData)
b <-

# Предсказанные значения и стандартные ошибки...
# ...в масштабе функции связи (логарифм)
NewData$fit_eta <-
NewData$SE_eta <- sqrt(diag(X %*% vcov(M_nb) %*% t(X)))

# ...в масштабе отклика (применяем функцию, обратную функции связи)
NewData$fit_mu <-
NewData$SE_mu <-

head(NewData, 2)



# ## График предсказаний в масштабе функции связи
ggplot(NewData, aes(x = Flowers, y = eta, fill = Treatment)) +
  geom_ribbon(aes(ymin = eta - 2 * SE_eta, ymax = eta + 2 * SE_eta), alpha = 0.3) +
  geom_line(aes(colour = Treatment)) + geom_hline(yintercept = 0)

# ## График предсказаний в масштабе переменной-отклика
ggplot(NewData, aes(x = Flowers, y = mu, fill = Treatment)) +
  geom_ribbon(aes(ymin = mu - 2 * SE_mu, ymax = mu + 2 * SE_mu), alpha = 0.3) +
  geom_line(aes(colour = Treatment)) + geom_hline(yintercept = 0)



# Предсказания модели при помощи predict()
# ...в масштабе функции связи
predict_eta <- predict(M_nb, newdata = NewData, se.fit = TRUE)
NewData$eta <- predict_eta$fit
NewData$SE_eta <- predict_eta$se.fit
# ...в масштабе отклика
predict_mu <- predict(M_nb, newdata = NewData, se.fit = TRUE, type = 'response')
NewData$mu <- predict_mu$fit
NewData$SE_mu <- predict_mu$se.fit



