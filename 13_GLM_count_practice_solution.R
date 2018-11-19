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

with(af, table(age, gender, children))
with(af, table(religiousness, gender))
with(af, table(education, gender))

# Ищем выбросы
gg_dot <- ggplot(af, aes(x = 1:nrow(af)))
gg_dot + geom_point(aes(y = affairs))
gg_dot + geom_point(aes(y = age))
gg_dot + geom_point(aes(y = yearsmarried))
gg_dot + geom_point(aes(y = religiousness))
gg_dot + geom_point(aes(y = education))
gg_dot + geom_point(aes(y = rating))

# Проверка на коллинеарность
mod <- glm(affairs ~ gender + children + yearsmarried + religiousness + education, data = af)
vif(mod)

# Пуассоновская модель ###################

frml <- affairs ~ gender * children + gender * yearsmarried + gender * religiousness + gender * education + children * yearsmarried + children * religiousness + children * education

mod_pois <- glm(frml, data = af, family = 'poisson')

# Проверка условий применимости

overdisp_fun(mod_pois)

# Дальше продолжаем, чтобы понять, с чем может быть связана сверхдисперсия.

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

# Потеряны ли предикторы?
gg_resid + geom_boxplot(aes(x = factor(age)))
gg_resid + geom_boxplot(aes(x = factor(rating)))

# Что дальше?

# Модель с отрицательным биномиальным распределением отклика ##################

mod_nb <- glm.nb(frml, data = af)

# Проверка условий применимости

overdisp_fun(mod_nb)
# Сверхдисперсии нет, можно работать дальше.

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

# Потеряны ли предикторы?
gg_resid + geom_boxplot(aes(x = factor(age)))
gg_resid + geom_boxplot(aes(x = factor(rating)))

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


# TODO:График предсказаний модели






























