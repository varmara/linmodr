#"Дискретные предикторы в линейных моделях"


#' ## Глистогонные и рост коз
#'
#' Как связан прирост массы коз с начальным весом животного и интенсивностью профилактики паразитарных заболеваний?
#'
#'
#' - `Treatment` - обработка от глистов (стандартная, интенсивная)
#' - `Weightgain` - привес, кг
#' - `Initial.wt` - начальный вес, кг
#'Пример из библиотеки данных
#' http://www.statlab.uni-heidelberg.de/data/ancova/goats.story.html</div>
#'
#' ## Читаем данные и знакомимся с ними

library(readxl)
goat <- read_excel("data/goats.xlsx", sheet = 1)
head(goat)
str(goat)

colSums(is.na(goat))

# переименуем переменные для краткости
colnames(goat) <- c("Treatment", "Wt", "Init")

# объемы выборок
table(goat$Treatment)

goat$Treatment <- factor(goat$Treatment)

#' ## Есть ли выбросы?
#Строим дотплоты

library(ggplot2)


gg_dot <- ggplot(goat, aes(y = 1:nrow(goat))) + geom_point()
gg_dot + aes(x = Wt)
gg_dot + aes(x = Init)

##Строим модель#####

MG <- lm(Wt ~ Init + Treatment, data = goat)

#'
#' В этой модели мы молчаливо считаем,  что характер связи прироста коз с начальным весом будет одинаковым (нет взаимодействия предикторов). Но! Это надо специально проверять (об этом далее)


#'
##Проверяем условия применимости #####

#' ## Нет ли колинеарности между начальным весом и тритментом
library(car)
vif(MG)

ggplot(goat, aes(x = Treatment, y = Init)) + geom_boxplot()


# Создаем диагностические графики (дополниет недописанные части кода)

MG_diag <-


library(gridExtra)

Diag1 <-  ggplot(MG_diag, aes(x = , y = .cooksd)) + geom_bar(stat = )
Diag2 <-  ggplot(data = MG_diag, aes(x = .fitted, y = )) + geom_point() + geom_hline( )
Diag3 <-  ggplot(data = MG_diag, aes(x = , y = .stdresid)) + geom_point() + geom_hline()
Diag4 <-  ggplot(data = MG_diag, aes(x = Treatment, y = .stdresid)) + geom_()

grid.arrange(Diag1, Diag2, Diag3, Diag4, =2)


#' ## Нормальнсть распределения остатков

library(car)




#' ## График модели

gg_g <- ggplot(data = goat, aes(y = Wt, x = Init, colour = Treatment)) +
  geom_point()  +
  labs(x = "Начальный вес, кг",
       y = "Привес, кг") +
  scale_colour_discrete("Способ обработки",
                        breaks = c("intensive", "standard"),
                        labels = c("Интенсивный", "Стандартный"))


MyData <- unique(goat[ , c("Init", "Treatment")])
MyData$Predict <- predict(MG, newdata = MyData)
gg_g + geom_line(data = MyData, aes(x = Init, y = Predict, color = Treatment))





#' ##Результаты #####
#'

summary(MG)

#'
#' ##Меняем базовый уровень
#'
#' Это чисто формальная процедура от которой ничего не измеяется по сути, но это иногда необходимо для более удобной визуализации


goat$Treatment <- relevel(goat$Treatment, ref = "standard")

levels(goat$Treatment)

MG1 <- lm(Wt ~ Init + Treatment, data = goat)

summary(MG1)

#'

# Обобщенная характеристика влияния предикторов

library(car)
Anova(MG, type = 3)



#'
#' ## Влияет ли стаж работы на предприятиях, вырабатывающих кадмий, на жизненнй объем легких?
#'
#' Пример взят из книги:
#' P. Armitage and G. Berry (1987), Statistical Methods in Medical Research, 2nd ed., Blackwell, p.286.
#'
#' Данные представлены в пакете `ISwR`
#'
#' Переменные:
#'
#' `group` - Группа 1: Более 10 лет в отрасли; Группа 2 - менее 10 лет; Группа 3 -  не подвергались воздействию.
#'
#' `age` - возраст
#'
#' `vital.capacity` - объем легких (л).
#'

## Загружаем данные #####

vit <- read.table("data/vitcap2.csv", header = TRUE, sep = ";")

#' ##Немного преобразуем исходный датасет

vit$Group [vit$group == 1] <- "Long exposed"
vit$Group [vit$group == 2] <- "Short exposed"
vit$Group [vit$group == 3] <- "Not exposed"

#' ## Меняем порядок уровней

vit$Group <- factor(vit$Group, levels = c("Not exposed", "Short exposed", "Long exposed"))

levels(vit$Group)



M1 <- lm(vital.capacity ~ Group, data = vit)


#' ## Геометрическая интерпретация модели с дискретным предиктором
#'
#' Это будет график, отражающий средние значения зависимой переменной, вычисленные для каждой градации дискретного фактора
#'

MyData <- data.frame(Group = levels(vit$Group))

MyData$Group <- factor(MyData$Group, levels = c("Not exposed", "Short exposed", "Long exposed"))

MyData$Predicted <- predict(M1, newdata = MyData, se.fit = TRUE)$fit

MyData$SE <- predict(M1, newdata = MyData, se.fit = TRUE)$se.fit


library(ggplot2)
ggplot(MyData, aes(x = Group, y = Predicted)) +  geom_bar(stat = "identity", aes(fill = Group)) + geom_errorbar(aes(ymin = Predicted - SE, ymax = Predicted + SE), width = 0.2)

summary(M1)

#'
#' Куда делась одна градация фактора?
#'

#'
#' ## Задание
#' 1. Измените базовый уровень переменной `Group` на "Long exposed"
#' 2. Постройте модель, аналогичную `M1`
#' 3. Вычислите предсказанные моделью значения для каждой градации фактора `Group`
#'
#'






#'
#' Можно ли доверять полученным результатам?
#'
M1_diag <- fortify(M1)

ggplot(M1_diag, aes(x=.fitted, y=.stdresid)) + geom_point()

qplot(vit$age, M1_diag$.stdresid) + geom_smooth(method = "lm")

#'
#' Очевидный паттерн в остатках!
#'
#' Необходимо включать еще одну переменную - **`ковариату`**
#'
#' ## Analysis of covariance (ANCOVA)
#'
#' ###Меняем модель

M3 <- lm(vital.capacity ~ Group + age , data = vit)

#'
#' ##Диагностика модели
#'

M3_diag <- fortify(M3)
qplot(vit$age, M3_diag$.stdresid) + geom_smooth(method = "lm")


#'
#' Паттерн исчез!
#'

summary(M3)

anova(M3)

#'
#' Противоречие с результатами `summary()`!
#'
#' ## Поменяем порядок предикторов

M2 <- lm(formula = vital.capacity ~ age + Group, data = vit)

anova(M2)

#' `
#' Результат тестирования зависит от порядка предикторов.
#' Почему?
#'


#'
#' ## Вариант 1. Последовательное тестирование (SS type I)
#'
#' Факторы тестируются в порядке включения в модель. Результат тестирования зависит от порядка включения.

anova(lm(formula = vital.capacity ~ Group + age, data = vit))

anova(lm(formula = vital.capacity ~  age + Group, data = vit))

#'
#' ## Вариант 2. Иерархическое тестирование (SS type III)
#'
#' Каждый из факторов по отношению к модели только без него, но со всеми остальными.
#' Нужно, если много факторов и выборки разного размера. Тогда результат не будет зависеть от порядка включения факторов в модель.

library(car)
Anova(M3, type = 3)

# Задание
# Используя матричную алгебру, найдите коэффициенты линейной модели для зависимой переменой vital.capacity на основе модельной матрицы X, включающей дискретный фактор Group и непрерывный предиктор age.






##### ВЗАИМОДЕЙСТВИЯ факторов и непрерывных предикторов ####################
# ## Пример: Пуромицин
#
# Пуромицин - антибиотик пуринового ряда, ингибитор синтеза белков.
# Эти данные --- о том, как меняется активность фермента галактозил трансферазы под воздействием пуромицина (Treloar 1974). Измеряли скорость реакции в зависимости от концентрации субстрата на мембранах аппарата Гольджи из богатых мембранами фракций из печени крыс.
#
# - `conc` --- концентрация пуромицина
# - `rate` --- скорость химической реакции
# - `state` --- индикатор того, обработаны ли клетки пуромицином

library(readxl)
Puromycin <- read_excel("data/Puromycin.xlsx")
head(Puromycin)

# ## Знакомимся с данными
sapply(Puromycin, class)
colSums(is.na(Puromycin))
nrow(Puromycin)

table(Puromycin$state, Puromycin$conc)

library(ggplot2)
ggplot(Puromycin, aes(x = state, y = conc)) + geom_boxplot()


# ## Задание #####################################
#
# Постройте график зависимости скорости химической реакции от концентрации






# ## Запись формулы линейной модели со взаимодействием в R
#
# `Response ~ Continuous + Categorical + Continuous : Categorical` (полная запись)
#
# `Response ~ Continuous * Categorical`(сокращенная запись)
#
# `Continuous : Categorical` --- Кодовое обозначение взаимодействия. Добавление взаимодействия непрерывного и дискретного факторов в модель означает, что возможен разный угол наклона прямых для разных групп.

# ## Подберем модель со взаимодействием
Puromycin$lc <- log(Puromycin$conc)

M1 <- lm(rate ~ lc + state + lc:state, data = Puromycin)

# M1 <- lm(rate ~ lc * state, data = Puromycin) # То же самое

summary(M1)





# ## Проверяем выполнение условий применимости


# ## Рисуем график предсказаний
# Что не так с этим графиком? Это наша модель?
ggplot(Puromycin, aes(x = lc, y = rate, colour = state)) +
  geom_point() +
  geom_smooth(method = "lm")



# ## Задание --------------------------------------
# Постройте график предсказаний модели по этим данным
#
# ### Данные для графика

library(plyr)
NewData <- ddply(
  Puromycin, .variables = .(state), summarise,
  lc = seq(min(lc), max(lc), length = 100))
# предсказанные значения
Predictions <- predict(M1, newdata = NewData, se.fit = TRUE)
NewData$fit <- Predictions$fit
# стандартные ошибки
NewData$SE <- Predictions$se.fit
# доверительный интервал
NewData$upr <- NewData$fit + 1.96 * NewData$SE
NewData$lwr <- NewData$fit - 1.96 * NewData$SE
# Обратная трансформация предиктора
NewData$conc <- exp(NewData$lc)



# ##  Задание ------------------------------------
# Запишите уравнение этой модели и уравнения для каждой группы
coef(M1)



# ## Тестируем гомогенность углов наклона
M2 <- lm(rate ~ lc + state, data = Puromycin)
# anova(M1, M2, test="F")
drop1(M1, test = "F")


# ## Представляем результаты в виде таблицы

# ### Вариант 1. Последовательное тестирование (SS type I)
M3 <- lm(rate ~ state + lc + state:lc, data = Puromycin)
# сравните
anova(M1)
anova(M3)

# ### Вариант 2. Иерархическое тестирование (SS type III)
library(car)
Anova(M1, type = 3)
# Anova(M3, type = 3) # сравните


# # Модель со взаимодействием на примере данных о весе новорожденных
# Как вес новорожденных зависит от возраста матери и того, курит ли она
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

# ## Задание -------------------------------------
# - Исследуйте данные о весе новорожденных
# - Постройте модель зависимости веса новорожденных от возраста матери и взаимодействия
# - Проверьте условия применимости этой модели
# - Упростите модель, если это возможно
# - Напишите общее уравнение и отдельные уравнения модели для групп по дискретному фактору
# - Постройте график предсказаний модели

# Проверка на наличие выбросов


#Проверка на наличие коллиенарности между smoke и age

wt_mod_1 <- lm()

library(car)




# Запись модели
wt_mod_2 <- lm()


# Проверка гомогенности углов наклона

drop1( , test = "F")



# Проверка валидности модели

wt_mod_2_diag <- (wt_mod_2)
wt_mod_2_diag <- data.frame(wt_mod_2_diag, wt[, c("lwt", "race", "smoke", "ptl", "ht", "ui", "ftv")])

# График расстояния Кука

ggplot(wt_mod_2_diag, aes(x = , y = )) + geom_bar(stat = )



# Residual plot от предсказанных значений

gg_resid <- ggplot(data = , aes(x = , y = .stdresid)) +  geom_point() + geom_hline(yintercept = 0)

gg_resid


# Графики остатков от предикторов в модели и не в модели

library(gridExtra)
grid.arrange(gg_resid + aes(x = ),
             gg_resid + aes(x = ),
             nrow = 1)

gg_box <- ggplot(data = wt_mod_2_diag, aes(x = smoke, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)

grid.arrange(gg_box + aes(x = ),
             gg_box + aes(x = ),
             gg_box + aes(x = ),
             gg_box + aes(x = ),
             gg_box + aes(x = ),
             nrow = 2)


# Квантильный график остатков

qqPlot()


# Смотрим на результаты

summary(wt_mod_2)


# Записываем уравнение модели




# Таблица дисперсионного анализа


# График предсказаий модели

library(plyr)
# Диапазон возрастов разный для курящих и некурящих, поэтому
NewData <- ddply(
  .data = wt, .variables = .(smoke), .fun = summarise,
  age = seq(min(age), max(age), length = 100))




# предсказанные значения
Predictions <- predict(wt_mod_2, newdata = NewData, se.fit = TRUE)
NewData$fit <- Predictions$fit
# стандартные ошибки
NewData$SE <- Predictions$se.fit
# доверительный интервал
NewData$upr <- NewData$fit + 1.96 * NewData$SE
NewData$lwr <- NewData$fit - 1.96 * NewData$SE


ggplot(NewData, aes(x = age, y = fit)) +
  geom_ribbon(alpha = 0.2, aes(ymin = lwr, ymax = upr, group = smoke)) +
  geom_line(aes(colour = smoke))


