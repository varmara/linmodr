#' title       : "Коррекция гетерогенности дисперсий"
#' subtitle: "Линейные модели..."
#' author: "Марина Варфоломеева, Вадим Хайтов"

#' ## Способы моделирования "поведения" дисперсии

#' ## Пример: Способствуют ли взрослые мидии притоку молоди?
#' Даные взяты из работы Khaitov, 2013
myt <- read.table("data/myt.csv", sep=";", header = TRUE)
head(myt, 12)
myt$Sq_Recruits <- sqrt(myt$Recruits)
myt$fYear <- factor(myt$Year)

#' ## Строим обычную регрессионную модель
mod_formula <- Sq_Recruits ~ Large + fYear + Bank + Large:fYear + Large:Bank
M1_lm <- lm(mod_formula, data = myt)

library(car)
Anova(M1_lm)

#' ## Диагностика модели

library(ggplot2)
library(gridExtra)
diag_M1_lm <- fortify(M1_lm)
Res_plot1 <- ggplot(diag_M1_lm, aes(x=.fitted, y = .stdresid)) + geom_point() + geom_hline(yintercept = 0) + geom_smooth(se = F)
Res_plot2 <- ggplot(diag_M1_lm, aes(x=Large, y = .stdresid)) + geom_point() + geom_hline(yintercept = 0)+ geom_smooth(se = F)
Res_plot3 <- ggplot(diag_M1_lm, aes(x=fYear, y = .stdresid)) + geom_boxplot() + geom_hline(yintercept = 0) + theme(axis.text.x = element_text(angle = 90))
Res_plot4 <- ggplot(diag_M1_lm, aes(x=Bank, y = .stdresid)) + geom_boxplot() + geom_hline(yintercept = 0)
grid.arrange(Res_plot1, Res_plot2, Res_plot3, Res_plot4, ncol = 2)


#' # Различные формы структуры дисперсии

library(nlme)
M1_gls <- gls(mod_formula, data = myt)

#' ## 1. Фиксированная структура дисперсии
M2_gls <- gls(mod_formula, data = myt, weights = varFixed( ~ Large))

AIC(M1_gls, M2_gls)


#' ## 2. Разные дисперсии для разных уровней категориальных предикторов
M3_gls <- gls(mod_formula, data = myt, weights = varIdent(form = ~1|fYear))

anova(M1_gls, M3_gls)

M3_gls2 <- gls(mod_formula, data = myt, weights = varIdent(form = ~1|Bank))

anova(M1_gls, M3_gls2)



#' ## 3. Степенная зависимость дисперсии от ковариаты
M4_gls <- gls(mod_formula, data = myt, weights = varPower(form = ~ Large))

summary(M4_gls)

M4_gls$modelStruct

#' ## Задание
#' Степенная зависимость дисперсии от ковариаты может учитывать и взаимодействие ковариаты дисперсии с категориальными предикторами
#' Напишите код, с помощью которого в модели будет учтена степенная зависимость  дисперсии от переменной `Large`, но разная для каждого уровня фактора `fYear`.  Аналогичный код напишите для фактора `Bank`
#' __Подсказка:__ Изучите справку по функции `varPower()`







#' ## 4. Экспоненциальная зависимость дисперсии от ковариаты
M7_gls <- gls(mod_formula, data = myt, weights = varExp(form = ~ Large))
M8_gls <- gls(mod_formula, data = myt, weights = varExp(form = ~ Large|fYear))
M9_gls <- gls(mod_formula, data = myt, weights = varExp(form = ~ Large|Bank))

M7_gls$modelStruct
M8_gls$modelStruct
M9_gls$modelStruct

#' ## 5. Усложненная степенная зависимость дисперсии от ковариаты

M10_gls <- gls(mod_formula, data = myt,
               weights = varConstPower(form = ~ Large))
#M11_gls <-gls(mod_formula, data = myt,
#               weights = varConstPower(form = ~ Large|fYear))
M12_gls <- gls(mod_formula, data = myt,
               weights = varConstPower(form = ~ Large|Bank))

M10_gls$modelStruct
M12_gls$modelStruct

#' ## 6. Комбинированная структура дисперсии
M13_gls <- gls(mod_formula, data = myt,
               weights = varComb(varIdent(form = ~ fYear),
                                 varPower(form = ~ Large)))
M14_gls <- gls(mod_formula, data = myt,
               weights = varComb(varIdent(form = ~ Bank),
                                 varPower(form = ~ Large)))
M15_gls <- gls(mod_formula, data = myt,
               weights = varComb(varIdent(form = ~ fYear),
                                 varExp(form = ~ Large)))
M16_gls <- gls(mod_formula, data = myt,
               weights = varComb(varIdent(form = ~ Bank),
                                 varExp(form = ~ Large)))

#' ## Задание
#' Найдите модель с наилучшей структурой дисперсии





#' ## Диагностика модели с оптимальной структурой дисперсии





#' ## Можно ли упростить модель?

M5_gls_ML <- update(M5_gls, method = "ML")
drop1(M5_gls_ML, test = "Chi")






#' ## Структура дисперсии может иметь определенный биологический смысл
dat <- data.frame(x = c(1997, 1999:2011),
                  y = as.vector(unlist(M5_gls$modelStruct)))
ggplot(dat, aes(x = x, y = y)) + geom_point() + xlab("Годы") + ylab("Delta")






#' # Моделирование структуры дисперсии при наличии группирующих (случайных) факторов
#' ## Пример: Рост крыс при разной диете
#' Пример взят из книги Pinheiro & Bates, 2000 (Hand and Crowder (1996))
#' Три группы крыс, содержались при разных условиях кормления 64 дня. Каждую крысу взвешивали с определнной периодичностью.
data("BodyWeight")
bw <- as.data.frame(BodyWeight)
head(bw, 14)

#' ## Задание
#' Постройте модель, которая дала бы ответ на вопрос, изменяется ли характер роста крыс в зависимости от типа диеты?





############## Самостоятельная работа################

# Вариант1
# Пользуясь данными из встроенного датасета Spruce (пакет nlme) постройте модель роста деревьев взависимости от времени


data("Spruce")

head(Spruce)






# Вариант2
# Пользуясь данными из встроенного датасета BodyWeight (пакет nlme) постройте модель изменения веса крыс в зависимости от времени и типа диеты

data(BodyWeight)

head(BodyWeight)





# Вариант3
# Пользуясь данными из встроенного датасета RatPupWeight (пакет nlme) выясните зависит ли вес новорожденных крысят от пола и количества детенышей в помете. Для простоты возьмите только контрольную группу крыс.

data(RatPupWeight)

head(RatPupWeight)








