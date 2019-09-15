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
colnames(goat) <- c("Treatment", "Wt", "Stw")

# объемы выборок
table(goat$Treatment)

goat$Treatment <- factor(goat$Treatment)

#' ## Есть ли выбросы?
#Строим дотплоты

library(ggplot2)


gg_dot <- ggplot(goat, aes(y = 1:nrow(goat))) + geom_point()

gg_dot + aes(x = Wt)
gg_dot + aes(x = Stw)

##Строим модель#####

Mod_goat_full <- lm(Wt ~ Stw + Treatment + Stw:Treatment, data = goat)

Mod_goat_full <- lm(Wt ~ Stw * Treatment, data = goat)


drop1(Mod_goat_full, test = "F")

Mod_goat_reduced <- update(Mod_goat_full, . ~ . - Stw:Treatment )


#'
##Проверяем условия применимости #####

#' ## Нет ли колинеарности между начальным весом и тритментом
library(car)
vif(Mod_goat_reduced)

ggplot(goat, aes(x = Treatment, y = Stw)) + geom_boxplot()


# Создаем диагностические графики (дополниет недописанные части кода)

MG_diag <- fortify(Mod_goat_reduced)


head(MG_diag)



library(gridExtra)

Diag1 <-  ggplot(MG_diag, aes(x = , y = .cooksd)) + geom_bar(stat = "identity")



Diag2 <-  ggplot(data = MG_diag, aes(x = , y = .stdresid)) + geom_point() + geom_hline(yintercept = 0 )


Diag3 <-  ggplot(data = MG_diag, aes(x =  , y = .stdresid)) + geom_point() + geom_hline()


Diag4 <-  ggplot(data = MG_diag, aes(x = Treatment, y = )) + geom_boxplot()

grid.arrange(Diag1, Diag2, Diag3, Diag4, =2)


#' ## Нормальнсть распределения остатков

library(car)



#' ## График модели

gg_g <- ggplot(data = goat, aes(y = Wt, x = Stw, colour = Treatment)) +
  geom_point(size = 4) +
  labs(x = "Начальный вес, кг",
       y = "Привес, кг") +
  scale_colour_discrete("Способ обработки",
                        breaks = c("intensive", "standard"),
                        labels = c("Интенсивный", "Стандартный"))


MyData <- unique(goat[ , c("Stw", "Treatment")])

MyData$Predict <- predict(Mod_goat_reduced, newdata = MyData)

gg_g + geom_line(data = MyData, aes(x = Stw, y = Predict, color = Treatment))





#' ##Результаты #####
#'

summary(Mod_goat_reduced)

#'
#' ##Меняем базовый уровень
#'
#' Это чисто формальная процедура от которой ничего не измеяется по сути, но это иногда необходимо для более удобной визуализации


goat$Treatment <- relevel(goat$Treatment, ref = "standard")

levels(goat$Treatment)

Mod_goat_reduced_2 <- lm(Wt ~ Stw + Treatment, data = goat)

summary(Mod_goat_reduced_2)


# Обобщенная характеристика влияния предикторов

library(car)
Anova(Mod_goat_reduced, type = 3)


