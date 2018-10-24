#"Дискретные предикторы в линейных моделях. Взаимодействие предикторов"


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


