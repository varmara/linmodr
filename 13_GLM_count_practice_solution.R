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
overdisp_fun <- function(model) {
  rdf <- df.residual(model) # Число степеней свободы N - p
  rp <- residuals(model,type="pearson") # Пирсоновские остатки
  Pearson.chisq <- sum(rp^2) # Сумма квадратов остатков
  prat <- Pearson.chisq/rdf  # Степень избыточности дисперсии
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE) # Уровень значимости
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)        # Вывод результатов
}


# Открываем данные ##################################
data(Affairs, package = "AER")
af <- Affairs

# Разведочный анализ ###############################
# Пропущенные значения?
colSums(is.na(af))
# Объем выборки?
nrow(af)

# Зависимая переменная здесь не просто счетная. Респондентам, скорее всего,
# давали выбрать из готовых вариантов. Иначе сложно объяснить, почему так много
# людей выбрало вариант 12, но никто не выбрал вариант 10 или какой-то еще
# близкий.
table(af$affairs)
# Но вполне можно попытаться проанализировать эту переменную как счетную величину.

# Ищем выбросы
gg_dot <- ggplot(af, aes(y = 1:nrow(af)))
gg_dot + geom_point(aes(x = affairs))
gg_dot + geom_point(aes(x = age))
gg_dot + geom_point(aes(x = yearsmarried))
gg_dot + geom_point(aes(x = religiousness))
gg_dot + geom_point(aes(x = education))
gg_dot + geom_point(aes(x = rating))

# Проверяем объемы групп, заданных дискретными предикторами
# (Заодно это проверка на коллинеарность дискретных предикторов)
with(af, table(age, gender, children)) # мало бездетных людей старшего возраста (что логично), намек на коллинеарность
with(af, table(religiousness, gender)) # Уровень религиозности не связан с полом
with(af, table(education, gender)) # уровень образования связан с полом
with(af, table(children, yearsmarried)) # Наличие детей связано с продолжительностью брака

# Коллинеарность между дискретными и непрерывными предикторами можно проверить графически
gg_box <- ggplot(data = af, aes(x = gender, fill = children)) + geom_boxplot()
gg_box + aes(y = age) # дети есть у людей более старшего возраста - намек на коллинеарность
gg_box + aes(y = yearsmarried) # дети есть в парах, которые дольше состоят в браке - намек на коллинеарность
gg_box + aes(y = religiousness)
gg_box + aes(y = education)
gg_box + aes(y = rating)


# Формальная проверка на коллинеарность при помощи vif
# Поскольку мы хотим построить модель зависимости от
# пола, времени, проведенного в браке, наличия
# детей, уровня религиозности и уровня образованности,
# строим вспомогательную модель с этими предикторами и вычисляем vif
mod <- glm(affairs ~ gender + children + yearsmarried + religiousness + education, data = af)
vif(mod)
# Ничего криминального

# Пуассоновская модель ###################

# Начнем с модели в которой есть взаимодействие обоих дискретных предикторов
# и взаимодействия дискретных предикторов с каждым из непрерывных.
# Формула модели сохранена в переменной, чтобы ее потом не переписывать
frml <- affairs ~ gender * children +
  gender   * yearsmarried + gender   * religiousness + gender   * education +
  children * yearsmarried + children * religiousness + children * education

mod_pois <- glm(frml, data = af, family = 'poisson')

# Проверка условий применимости

# Начинаем с проверки на наличие сверхдисперсии - это самое важное условие
overdisp_fun(mod_pois)
# Сверхдисперсия

# Дальше продолжаем диагностику, чтобы понять, с чем может быть связана сверхдисперсия.

mod_pois_diag <- data.frame(
  .fitted <- predict(mod_pois, type = 'response'),
  .resid_p <- resid(mod_pois, type = 'pearson'),
  af
)
gg_resid <- ggplot(mod_pois_diag, aes(y = .resid_p))

# Остатки от предикторов в модели
gg_resid + geom_boxplot(aes(x = gender))
gg_resid + geom_boxplot(aes(x = children))
gg_resid + geom_boxplot(aes(x = factor(yearsmarried)))
gg_resid + geom_boxplot(aes(x = factor(religiousness)))
gg_resid + geom_boxplot(aes(x = factor(education)))
# Ничего криминального...

# Может быть нужно добавить другие предикторы в модель?
gg_resid + geom_boxplot(aes(x = factor(age)))
gg_resid + geom_boxplot(aes(x = factor(rating)))
# Не похоже, что это необходимо...

# Много ли нулей в данных?
mean(af$affairs == 0)
# 75% всех значений отклика - нули.
# М.б. распределение Пуассона не может справиться с описанием такого количества нулей?

# Резюме по итогам диагностики модели:
# Мы искали причины сверхдисперсии среди наиболее вероятных, но
# не нашли признаков того, что есть "потерянные" предикторы.
# Может быть сверхдисперсию в данных (и заодно большое количество нулей) удастся описать адекватно
# при помощи модели с отрицательным биномиальным распределением отклика...

# Модель с отрицательным биномиальным распределением отклика ##################

mod_nb <- glm.nb(frml, data = af)

# Проверка условий применимости

overdisp_fun(mod_nb)
# Недодисперсия! Это не так страшно.
# Сверхдисперсии нет, можно работать с этой моделью дальше.

mod_nb_diag <- data.frame(
  .fitted <- predict(mod_nb, type = 'response'),
  .resid_p <- resid(mod_nb, type = 'pearson'),
  af
)
gg_resid <- ggplot(mod_nb_diag, aes(y = .resid_p))

# Остатки от предикторов в модели
gg_resid + geom_boxplot(aes(x = gender))
gg_resid + geom_boxplot(aes(x = children))
gg_resid + geom_boxplot(aes(x = factor(yearsmarried)))
gg_resid + geom_boxplot(aes(x = factor(religiousness)))
gg_resid + geom_boxplot(aes(x = factor(education)))
# ничего криминального...

# Может быть нужно добавить другие предикторы в модель?
gg_resid + geom_boxplot(aes(x = factor(age)))
gg_resid + geom_boxplot(aes(x = factor(rating)))
# Не похоже, что это необходимо...

# Резюме по итогам диагностики модели:
# Можно дальше работать с моделью с отрицательным биномиальным распределением отклика
# Дальше у нас есть выбор:
# А. Описать модель "как есть"
# Б. Попытаться упростить модель
# Здесь в целях демонстрации мы пойдем по пути Б.

# Можем ли мы сократить модель? ###############################################
drop1(mod_nb, test = 'Chi')

m1 <- update(mod_nb, . ~ . - gender:yearsmarried)
drop1(m1, test = 'Chi')

m2 <- update(m1, . ~. - gender:children)
drop1(m2, test = 'Chi')

m3 <- update(m2, . ~. - gender:religiousness)
drop1(m3, test = 'Chi')

m4 <- update(m3, . ~. - children:religiousness)
drop1(m4, test = 'Chi')

m5 <- update(m4, . ~. - gender:education)
drop1(m5, test = 'Chi')

m6 <- update(m5, . ~. - gender)
drop1(m6, test = 'Chi')

# Финальная модель
summary(m6)

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
  af
)
gg_resid <- ggplot(mod_m6_diag, aes(y = .resid_p))

# Остатки от предикторов в модели
gg_resid + geom_boxplot(aes(x = gender))
gg_resid + geom_boxplot(aes(x = children))
gg_resid + geom_boxplot(aes(x = factor(yearsmarried)))
gg_resid + geom_boxplot(aes(x = factor(religiousness)))
gg_resid + geom_boxplot(aes(x = factor(education)))

# Потеряны ли предикторы?
gg_resid + geom_boxplot(aes(x = factor(age)))
gg_resid + geom_boxplot(aes(x = factor(rating)))


# График предсказаний модели ######################################

# Для одной и той же модели можно построить множество вариантов графиков.
# Цель - построить такой график, который наиболее полно проиллюстрирует смысл модели.
# Здесь в качестве примера построены три постепенно усложняющихся графика.
# Ни один из них не идеален.

# График (1): измены и религиозность в зависимости от наличия детей ---------

# ## Данные для предсказаний
NewData <- af %>%
  group_by(children)%>%
  do(data.frame(religiousness = seq(min(.$religiousness), max(.$religiousness), length.out=50))) %>%
  mutate(yearsmarried = mean(af$yearsmarried),
         education = mean(af$education))
NewData

# ## Предсказания модели при помощи операций с матрицами

# Подсмотрим, как выглядит формула нашей финальной модели
formula(m6)
# Правую часть этой формулы будем использовать для создания модельной матрицы

# Модельная матрица и коэффициенты
X <- model.matrix(~ children + yearsmarried + religiousness + education +
                    children:yearsmarried + children:education,
                  data = NewData)
b <- coef(m6)

# Предсказанные значения и стандартные ошибки...
# ...в масштабе функции связи (логарифм)
NewData$fit_eta <- X %*% b
NewData$SE_eta <- sqrt(diag(X %*% vcov(m6) %*% t(X)))

# ...в масштабе отклика (применяем функцию, обратную функции связи)
NewData$fit_mu <- exp(NewData$fit_eta)
# +- 2 SE
NewData$lwr <- exp(NewData$fit_eta - 2 * NewData$SE_eta)
NewData$upr <- exp(NewData$fit_eta + 2 * NewData$SE_eta)
head(NewData, 2)

# ## График предсказаний в масштабе переменной-отклика
gg_m6_relig <- ggplot(NewData, aes(x = religiousness, y = fit_mu, fill = children)) +
  geom_ribbon(aes(ymin = lwr,
                  ymax = upr),
              alpha = 0.5) +
  geom_line(aes(colour = children)) +
  geom_hline(yintercept = 0)
gg_m6_relig
# В более религиозных семьях меньше измен.


# График (2): измены и религиозность в зависимости от наличия детей и числа лет в браке -------

# Диапазон значений продолжительности брака
range(af$yearsmarried)
# Давайте построим отдельные линии для браков продолжительностью 5 и 10 лет

NewData_1 <- af %>%
  group_by(children)%>%
  do(data.frame(religiousness = seq(min(.$religiousness), max(.$religiousness), length.out=50))) %>%
  crossing(yearsmarried = c(5, 10)) %>%
  mutate(education = mean(af$education))
NewData_1

# ## Предсказания модели при помощи операций с матрицами

# Подсмотрим, как выглядит формула нашей финальной модели
formula(m6)
# Правую часть этой формулы будем использовать для создания модельной матрицы

# Модельная матрица и коэффициенты
X <- model.matrix(~ children + yearsmarried + religiousness + education +
                    children:yearsmarried + children:education,
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
gg_m6_relig_yearsmarried <- ggplot(NewData_1, aes(x = religiousness, y = fit_mu)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = children), alpha = 0.5) +
  geom_line(aes(colour = children)) +
  geom_hline(yintercept = 0)
gg_m6_relig_yearsmarried + facet_wrap(~ yearsmarried)
# В более религиозных семьях меньше измен.
# При этом если брак был не продолжительным, то число измен не зависит от наличия детей.
# Если брак был долгим, то измен больше в бездетных браках.


# График (3): измены и уровень образования в зависимости от наличия детей и числа лет в браке -------

NewData_2 <- af %>%
  group_by(children)%>%
  do(data.frame(education = seq(min(.$education), max(.$education), length.out=50))) %>%
  crossing(yearsmarried = c(5, 10)) %>%
  mutate(religiousness = mean(af$religiousness))
NewData_2

# ## Предсказания модели при помощи операций с матрицами

# Подсмотрим, как выглядит формула нашей финальной модели
formula(m6)
# Правую часть этой формулы будем использовать для создания модельной матрицы

# Модельная матрица и коэффициенты
X <- model.matrix(~ children + yearsmarried + religiousness + education +
                    children:yearsmarried + children:education,
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
gg_m6_educ_yearsmarried <- ggplot(NewData_2, aes(x = education, y = fit_mu)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = children), alpha = 0.5) +
  geom_line(aes(colour = children)) +
  geom_hline(yintercept = 0)
gg_m6_educ_yearsmarried + facet_wrap(~ yearsmarried)
# Если есть дети, то от уровня образования ничего не зависит.
# Если нет детей, то меньше измен среди высокообразованных.
# Больше измен в долгих бездетных браках.



# И т.д...












