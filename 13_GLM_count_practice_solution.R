# Fair's Affairs #############################
# Какие факторы определяют супружескую неверность?

# Fair, R.C. (1978). A Theory of Extramarital
# Affairs. Journal of Political Economy, 86,
# 45–61.

# Переменные:
# `affairs` - Количество внебрачных свзяей за последний год
# `gender` - пол
# `age` - возраст
# `yearsmarried` - сколько ле в браке
# `children` - наличие детей
# `religiousness` - уровень религиозности
# `education` - уровень образования
# `rating` - субъективная оценка ощущений от брака

## Задание -------------------------------------

# Постройте модель, описывающую зависимость
# количества внебрачных связей в зависимости от
# пола, времени, проведенного в браке, наличия
# детей, уровня религиозности и уровня
# образованности.
# Проверьте валидность данной модели.
# Нарисуйте график предсказаний модели.

# Нужные пакеты и функции ############################
library(ggplot2)
library(dplyr)
library(tidyr)
library(car)
library(MASS)

# Функция для проверки наличия сверхдисперсии в модели (автор Ben Bolker)
# http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html
# Код модифицирован, чтобы учесть дополнительный параметр в NegBin GLMM, подобранных MASS::glm.nb()
overdisp_fun <- function(model) {
  rdf <- df.residual(model)  # Число степеней свободы N - p
  if (any(class(model) == 'negbin')) rdf <- rdf - 1 ## учитываем k в NegBin GLMM
  rp <- residuals(model,type='pearson') # Пирсоновские остатки
  Pearson.chisq <- sum(rp^2) # Сумма квадратов остатков, подчиняется Хи-квадрат распределению
  prat <- Pearson.chisq/rdf  # Отношение суммы квадратов остатков к числу степеней свободы
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE) # Уровень значимости
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)        # Вывод результатов
}


# Открываем данные ##################################
data(Affairs, package = "AER")

# Разведочный анализ ###############################
# Пропущенные значения?
colSums(is.na(Affairs))
# Объем выборки?
nrow(Affairs)

# Зависимая переменная здесь не просто счетная. Респондентам, скорее всего,
# давали выбрать из готовых вариантов. Иначе сложно объяснить, почему так много
# людей выбрало вариант 12, но никто не выбрал вариант 10 или какой-то еще
# близкий.
table(Affairs$affairs)
# Но вполне можно попытаться проанализировать эту переменную как счетную величину.

# Ищем выбросы
gg_dot <- ggplot(Affairs, aes(y = 1:nrow(Affairs)))
gg_dot + geom_point(aes(x = affairs))
gg_dot + geom_point(aes(x = age))
gg_dot + geom_point(aes(x = yearsmarried))
gg_dot + geom_point(aes(x = religiousness))
gg_dot + geom_point(aes(x = education))
gg_dot + geom_point(aes(x = rating))

# Как связаны предикторы друг с другом? (Коллинеарность)
# Способ 1.
# Поскольку величины вроде возраста, уровня образования и т.п.
# измерены в дискретной шкале, можем использовать table()
with(Affairs, table(age, gender, children)) # мало бездетных людей старшего возраста (что логично), намек на коллинеарность
with(Affairs, table(religiousness, gender)) # Уровень религиозности не связан с полом
with(Affairs, table(education, gender)) # уровень образования связан с полом, видно, что среди женщин ниже уровень образования, чем среди мужчин. Вероятно, это связано с различной доступностью образования в эти годы.
with(Affairs, table(children, yearsmarried)) # Наличие детей связано с продолжительностью брака

# Способ 2.
# Коллинеарность между дискретными и непрерывными предикторами можно проверить графически
gg_box <- ggplot(data = Affairs, aes(x = gender, fill = children)) + geom_boxplot()
gg_box + aes(y = age) # дети есть у людей более старшего возраста - намек на коллинеарность
gg_box + aes(y = yearsmarried) # дети есть в парах, которые дольше состоят в браке - намек на коллинеарность
gg_box + aes(y = religiousness)
gg_box + aes(y = education) # среди мужчин выше уровень образования
gg_box + aes(y = rating)


# Формальная проверка на коллинеарность при помощи vif
# Поскольку мы хотим построить модель зависимости от
# пола, времени, проведенного в браке, наличия
# детей, уровня религиозности и уровня образованности,
# строим вспомогательную модель с этими предикторами и вычисляем vif
mod <- glm(affairs ~ gender + children + yearsmarried + religiousness + education, data = Affairs)
vif(mod)
# Ничего криминального

# Пуассоновская модель ###################

# Начнем с модели в которой есть взаимодействие обоих дискретных предикторов
# и взаимодействия дискретных предикторов с каждым из непрерывных.
# Формула модели сохранена в переменной, чтобы ее потом не переписывать
frml <- affairs ~ gender * children +
  gender   * yearsmarried + gender   * religiousness + gender   * education +
  children * yearsmarried + children * religiousness + children * education

mod_pois <- glm(frml, data = Affairs, family = 'poisson')

# Проверка условий применимости

# Начинаем с проверки на наличие сверхдисперсии - это самое важное условие
overdisp_fun(mod_pois)
# Сверхдисперсия

# Дальше продолжаем диагностику, чтобы понять, с чем может быть связана сверхдисперсия.

mod_pois_diag <- data.frame(
  .fitted <- predict(mod_pois, type = 'response'),
  .resid_p <- resid(mod_pois, type = 'pearson'),
  Affairs
)
gg_resid <- ggplot(mod_pois_diag, aes(y = .resid_p))

# Остатки от предикторов в модели
# Боксплоты остатков по уровням дискретных предикторов
gg_resid + geom_boxplot(aes(x = gender))
# ок. остатки для женщин и для мужчин находятся на одном уровне
gg_resid + geom_boxplot(aes(x = children))
# ок
# Точечные графики остатков от непрерывных предикторов + сглаживатель
ggplot(mod_pois_diag, aes(x = yearsmarried, y = .resid_p)) + geom_point() + geom_smooth()
# ок
ggplot(mod_pois_diag, aes(x = religiousness, y = .resid_p)) + geom_point() + geom_smooth()
# ок
ggplot(mod_pois_diag, aes(x = education, y = .resid_p)) + geom_point() + geom_smooth()
# Какой-то скачок величины остатков при высоком уровне образования.
# Вряд ли он мог быть причиной гетерогенности дисперсий, но зато стоило бы проверить,
# вдруг есть предикторы, добавление которых в модель исправит ситуацию.

# Может быть нужно добавить другие предикторы в модель?
ggplot(mod_pois_diag, aes(x = age, y = .resid_p)) + geom_point() + geom_smooth()
# ок
ggplot(mod_pois_diag, aes(x = rating, y = .resid_p)) + geom_point() + geom_smooth()
# Остатки зависят от отношения к браку.
# В принципе, можно добавить этот предиктор в модель.
# но пока пойдем дальше. Может быть есть более серьезная причина для сверхдисперсии?

# Много ли нулей в данных?
mean(Affairs$affairs == 0)
# 75% всех значений отклика - нули. Zero-inflation?
# М.б. распределение Пуассона не может справиться с описанием такого количества нулей?

# Резюме по итогам диагностики модели:
# Мы искали причины сверхдисперсии среди наиболее вероятных.
# Возможно, в модели есть "потерянный" предиктор rating, но более вероятно, что
# сверхдисперсию в данных связана с большим количеством нулей.
# Давайте проверим, удастся ли побороть сверхдисперсию
# при помощи модели с отрицательным биномиальным распределением отклика...
# А потом при диагностике модели проверим, будет ли нам по-прежнему нужен rating.

# Модель с отрицательным биномиальным распределением отклика ##################

mod_nb <- glm.nb(frml, data = Affairs)

# Проверка условий применимости

overdisp_fun(mod_nb)
# Недодисперсия! Это не так страшно.
# Сверхдисперсии нет, можно работать с этой моделью дальше.

mod_nb_diag <- data.frame(
  .fitted <- predict(mod_nb, type = 'response'),
  .resid_p <- resid(mod_nb, type = 'pearson'),
  Affairs
)
gg_resid <- ggplot(mod_nb_diag, aes(y = .resid_p))

# Остатки от предикторов в модели
# Боксплоты остатков по уровням дискретных предикторов
gg_resid + geom_boxplot(aes(x = gender))
gg_resid + geom_boxplot(aes(x = children))
# Точечные графики остатков от непрерывных предикторов + сглаживатель
ggplot(mod_nb_diag, aes(x = yearsmarried, y = .resid_p)) + geom_point() + geom_smooth()
ggplot(mod_nb_diag, aes(x = religiousness, y = .resid_p)) + geom_point() + geom_smooth()
ggplot(mod_nb_diag, aes(x = education, y = .resid_p)) + geom_point() + geom_smooth()
# Как и раньше какой-то скачок величины остатков при высоком уровне образования.
# Теперь, когда гетерогенности дисперсий побеждена, точно стоило бы проверить,
# вдруг есть предикторы, добавление которых в модель исправит ситуацию.

# Может быть нужно добавить другие предикторы в модель?
ggplot(mod_nb_diag, aes(x = age, y = .resid_p)) + geom_point() + geom_smooth()
# ок
ggplot(mod_nb_diag, aes(x = rating, y = .resid_p)) + geom_point() + geom_smooth()
# Остатки зависят от отношения к браку.
# В принципе, можно добавить этот предиктор в модель.
# Причем добавить как фактор,
# т.к. изменение остатков при переходе от rating = 0 к 1 не такое же,
# как, например, при переходе от rating = 4 к 5.

# Резюме по итогам диагностики модели:
# Сверхдисперсия побеждена. Поэтому можно дальше работать с моделью
# с отрицательным биномиальным распределением отклика.
# Но можно попробовать добавить в модель "потерянный" предиктор.

# Добавляем в модель "потерянный" предиктор rating ############################
Affairs$rating <- factor(Affairs$rating)
mod_nb_1 <- update(mod_nb, .~. + rating)

# Проверка условий применимости

overdisp_fun(mod_nb_1)
# Недодисперсия стала меньше. Это хорошо.

mod_nb_1_diag <- data.frame(
  .fitted = predict(mod_nb_1, type = 'response'),
  .resid_p = resid(mod_nb_1, type = 'pearson'),
  Affairs
)
gg_resid_1 <- ggplot(mod_nb_1_diag, aes(y = .resid_p))

# Остатки от предикторов в модели
# Боксплоты остатков по уровням дискретных предикторов
gg_resid_1 + geom_boxplot(aes(x = gender))
gg_resid_1 + geom_boxplot(aes(x = children))
# Точечные графики остатков от непрерывных предикторов + сглаживатель
ggplot(mod_nb_1_diag, aes(x = yearsmarried, y = .resid_p)) + geom_point() + geom_smooth()
ggplot(mod_nb_1_diag, aes(x = religiousness, y = .resid_p)) + geom_point() + geom_smooth()
ggplot(mod_nb_1_diag, aes(x = education, y = .resid_p)) + geom_point() + geom_smooth()
ggplot(mod_nb_1_diag, aes(x = rating, y = .resid_p)) + geom_point() + geom_smooth()
# Остался скачок величины остатков при высоком уровне образования.
# Что с ним делать --- не ясно. В остальном --- ок.

# Может быть нужно добавить другие предикторы в модель?
ggplot(mod_nb_1_diag, aes(x = age, y = .resid_p)) + geom_point() + geom_smooth()

# Резюме по итогам диагностики модели:
# Добавление rating улучшило модель, но осталось неясным, что делать со скачками в education.
# М.б. лишние взаимодействия?

# Дальше у нас есть выбор:
# А. Описать модель "как есть"
# Б. Попытаться упростить модель
# Здесь в целях демонстрации мы пойдем по пути Б.

# Можем ли мы сократить модель? ###############################################
drop1(mod_nb_1, test = 'Chi')

m1 <- update(mod_nb_1, . ~ . - gender:education)
drop1(m1, test = 'Chi')

m2 <- update(m1, . ~. - gender:religiousness)
drop1(m2, test = 'Chi')

m3 <- update(m2, . ~. - children:religiousness)
drop1(m3, test = 'Chi')

m4 <- update(m3, . ~. - gender:children)
drop1(m4, test = 'Chi')

m5 <- update(m4, . ~. - gender:yearsmarried)
drop1(m5, test = 'Chi')

m6 <- update(m5, . ~. - gender)
drop1(m6, test = 'Chi')

# Финальная модель
coef(m6)

formula(m6)
# В результате упрощения в модели остались только два двухфакторных взаимодействия.
# Это значительно упростит интерпретацию, хотя все равно это будет проще сделать с использованием графика.
# Но для начала нам нужно опять проверить условия применимости.
# (Модель изменилась после упрощения. Вдруг в ней что-то "испортилось").

# Диагностика финальной модели #################################

overdisp_fun(m6)
# Сверхдисперсии нет, можно работать дальше.

mod_m6_diag <- data.frame(
  .fitted <- predict(m6, type = 'response'),
  .resid_p <- resid(m6, type = 'pearson'),
  Affairs
)
gg_resid_m6 <- ggplot(mod_m6_diag, aes(y = .resid_p))

# Остатки от предикторов в модели
# Боксплоты остатков по уровням дискретных предикторов
gg_resid_m6 + geom_boxplot(aes(x = gender))
gg_resid_m6 + geom_boxplot(aes(x = children))
# Точечные графики остатков от непрерывных предикторов + сглаживатель
ggplot(mod_m6_diag, aes(x = yearsmarried, y = .resid_p)) + geom_point() + geom_smooth()
ggplot(mod_m6_diag, aes(x = religiousness, y = .resid_p)) + geom_point() + geom_smooth()
ggplot(mod_m6_diag, aes(x = education, y = .resid_p)) + geom_point() + geom_smooth()
ggplot(mod_m6_diag, aes(x = rating, y = .resid_p)) + geom_point() + geom_smooth()
# Все как на предыдущем этапе.
# Потеряны ли предикторы?
ggplot(mod_m6_diag, aes(x = age, y = .resid_p)) + geom_point() + geom_smooth()
# Нет, все ок.

# Резюме. Хуже не стало.

# Анализ девиансы финальной модели ##########################################
# Не будем здесь конструировать полную таблицу анализа девиансы.
# Чтобы разобраться в результатах нам будет достаточно посмотреть на drop1().
drop1(m6, test = 'Chi')
# Религиозность и субхективный рейтинг брака статистически
# значимо влияют на число измен (делаем такой вывод,
# т.к. есть значимое влияние соответствующих факторов).
# В зависимости от числа детей на число измен по-разному влияет
# продолжительность брака, а так же уровень образования
# (об этом нам говорят значимые двухфакторные взаимодействия).
# Для интерпретации взаимодействия нужен график

# График предсказаний финальной модели ######################################

# Для одной и той же модели можно построить множество вариантов графиков.
# Цель - построить такой график, который наиболее полно проиллюстрирует смысл модели.
# Здесь в качестве примера построены три разных графика.
# Ни один из них не идеален. И ни один из них не показывает всё сразу.

# График (1) -----
# Измены и религиозность в зависимости от наличия детей для всех 5 уровней
# рейтинга  при средней для всего набора данных продолжительности брака и
# среднем уровне образования

# На этом графике не будет видно взаимодействия children:yearsmarried и
# children:education, т.к. для предсказаний использованы средняя
# продолжительность брака и средний уровень образования.

# ## Данные для предсказаний
NewData_1 <- Affairs %>%
  group_by(children, rating) %>%
  do(data.frame(religiousness = seq(min(.$religiousness), max(.$religiousness), length.out=50))) %>%
  mutate(yearsmarried = mean(Affairs$yearsmarried),
         education = mean(Affairs$education))
NewData_1

# ## Предсказания модели при помощи операций с матрицами

# Подсмотрим, как выглядит формула нашей финальной модели
formula(m6)
# Правую часть этой формулы будем использовать для создания модельной матрицы

# Модельная матрица и коэффициенты
X <- model.matrix(~ children + yearsmarried + religiousness + education +
                    rating + children:yearsmarried + children:education,
                  data = NewData_1)
b <- coef(m6)

# Предсказанные значения и стандартные ошибки...
# ...в масштабе функции связи (логарифм)
NewData_1$fit_eta <- X %*% b
NewData_1$SE_eta <- sqrt(diag(X %*% vcov(m6) %*% t(X)))

# ...в масштабе отклика (применяем функцию, обратную функции связи)
NewData_1$fit_mu <- exp(NewData_1$fit_eta)
# +- 2 SE
NewData_1$lwr <- exp(NewData_1$fit_eta - 2 * NewData_1$SE_eta)
NewData_1$upr <- exp(NewData_1$fit_eta + 2 * NewData_1$SE_eta)
head(NewData_1, 2)

# ## График предсказаний в масштабе переменной-отклика
gg_m6_1 <- ggplot(NewData_1, aes(x = religiousness, y = fit_mu, fill = rating)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, group = rating), alpha = 0.25) +
  geom_line(aes(colour = rating)) +
  geom_hline(yintercept = 0) +
  scale_colour_viridis_d(aesthetics = c("colour", "fill")) +
  facet_wrap(~ children, scales = "free")
gg_m6_1

# В более религиозных семьях меньше измен.
# В семьях с детьми меньше измен
# Чем хуже рейтинг качества брака, тем больше измен
# Недостатки:
# - На этом графике не видно взаимодействия, т.к. для предсказаний использованы
# средняя продолжительность брака и средний уровень образования.
# - Для лучшей читаемости лучше было бы отказаться от идеи изображать множество
# уровней рейтинга качества брака.

# График (2) -----
# Измены (y) и продолжительность брака (x)
# в зависимости от наличия детей (фасетки) и уровня образования (цвет)
# для рейтинга брака 3 и при среднем уровне религиозности

# Диапазон значений уровня образования
range(Affairs$education)
# Давайте построим отдельные графики для  уровней образования 12 и 20 лет

NewData_2 <- Affairs %>%
  group_by(children)%>%
  do(data.frame(yearsmarried = seq(min(.$yearsmarried), max(.$yearsmarried), length.out=50))) %>%
  crossing(education = c(12, 20)) %>%
  mutate(rating = factor(3, levels = levels(Affairs$rating)),
         religiousness = mean(Affairs$religiousness))
NewData_2

# ## Предсказания модели при помощи операций с матрицами

# Подсмотрим, как выглядит формула нашей финальной модели
formula(m6)
# Правую часть этой формулы будем использовать для создания модельной матрицы

# Модельная матрица и коэффициенты
X <- model.matrix(~ children + yearsmarried + religiousness + education +
                    rating + children:yearsmarried + children:education,
                  data = NewData_2)
b <- coef(m6)

# Предсказанные значения и стандартные ошибки...
# ...в масштабе функции связи (логарифм)
NewData_2$fit_eta <- X %*% b
NewData_2$SE_eta <- sqrt(diag(X %*% vcov(m6) %*% t(X)))

# ...в масштабе отклика (применяем функцию, обратную функции связи)
NewData_2$fit_mu <- exp(NewData_2$fit_eta)
# +- 2 SE
NewData_2$lwr <- exp(NewData_2$fit_eta - 2 * NewData_2$SE_eta)
NewData_2$upr <- exp(NewData_2$fit_eta + 2 * NewData_2$SE_eta)
head(NewData_2, 2)

# ## График предсказаний в масштабе переменной-отклика
# Измены (y) и продолжительность брака (x)
# в зависимости от наличия детей (фасетки) и уровня образования (цвет)
# для рейтинга брака 3 и при среднем уровне религиозности
gg_m6_2 <- ggplot(NewData_2, aes(x = yearsmarried, y = fit_mu)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = factor(education)), alpha = 0.5) +
  geom_line(aes(colour = factor(education))) +
  geom_hline(yintercept = 0)  +
  facet_wrap(~ children, scales = "free")
gg_m6_2
# Число измен растет с продолжительностью брака быстрее, если нет детей, чем
# когда есть дети. На этом графике мы показали одно из взаимодействий.


# График (3) -----
# Измены (y) и уровень образования (x)
# в зависимости от наличия детей (фасетки) и продолжительности брака (цвет)
# для рейтинга брака 3 и при среднем уровне религиозности

# Диапазон значений продолжительности брака
range(Affairs$yearsmarried)
unique(Affairs$yearsmarried)
# Давайте построим отдельные графики для продолжительности брака 4 годa и 15 лет

NewData_3 <- Affairs %>%
  group_by(children)%>%
  do(data.frame(education = seq(min(.$education), max(.$education), length.out=50))) %>%
  crossing(yearsmarried = c(4, 15)) %>%
  mutate(rating = factor(3, levels = levels(Affairs$rating)),
         religiousness = mean(Affairs$religiousness))
NewData_3

# ## Предсказания модели при помощи операций с матрицами

# Подсмотрим, как выглядит формула нашей финальной модели
formula(m6)
# Правую часть этой формулы будем использовать для создания модельной матрицы

# Модельная матрица и коэффициенты
X <- model.matrix(~ children + yearsmarried + religiousness + education +
                    rating + children:yearsmarried + children:education,
                  data = NewData_3)
b <- coef(m6)

# Предсказанные значения и стандартные ошибки...
# ...в масштабе функции связи (логарифм)
NewData_3$fit_eta <- X %*% b
NewData_3$SE_eta <- sqrt(diag(X %*% vcov(m6) %*% t(X)))

# ...в масштабе отклика (применяем функцию, обратную функции связи)
NewData_3$fit_mu <- exp(NewData_3$fit_eta)
# +- 2 SE
NewData_3$lwr <- exp(NewData_3$fit_eta - 2 * NewData_3$SE_eta)
NewData_3$upr <- exp(NewData_3$fit_eta + 2 * NewData_3$SE_eta)
head(NewData_3, 2)

# ## График предсказаний в масштабе переменной-отклика
# Измены (y) и уровень образования (x)
# в зависимости от наличия детей (фасетки) и продолжительности брака (цвет)
# для рейтинга брака 3 и при среднем уровне религиозности
gg_m6_3 <- ggplot(NewData_3, aes(x = education, y = fit_mu)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = factor(yearsmarried)), alpha = 0.5) +
  geom_line(aes(colour = factor(yearsmarried))) +
  geom_hline(yintercept = 0)  +
  facet_wrap(~ children, scales = "free")
gg_m6_3
# И т.д...

# Возможное продолжение анализа:
# - Можно построить графики скорректированных средних. М.б. они будут информативнее для изображения каких-то аспектов.
# - Можно сделать пост хок тест, чтобы сравнить скорректированные средние.






