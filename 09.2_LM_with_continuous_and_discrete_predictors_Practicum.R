# Практикум по построению моделей, включающих дискретные и непрерывные предикторы и их взаимодействия

# Данные

# От каких характеристик лесного участка зависит обилие птиц в лесах юго-западной Виктории, Австралия (Loyn, 1987)
#
# Переменных много, мы хотим из них выбрать __оптимальный небольшой__ набор.
#
# 56 лесных участков:
#
# - ABUND - обилие птиц
# - AREA - площадь участка
# - YRISOL - год изоляции участка
# - DIST - расстояние до ближайшего леса
# - LDIST - расстояние до ближайшего большого леса
# - GRAZE - пастбищная нагрузка (1-5)
# - ALT - высота над уровнем моря
#


bird <- read.csv("data/loyn.csv")


# Представим, что в фокусе нашего анализа интенсивность пастбищной нагрузки, которая является дискретным фактором.
# Наша задача построить оптимальную модель, обоснованно включив в нее только необходимые педикторы


bird$GRAZE <- factor(bird$GRAZE)




# В качестве ковариат мы будем использовать

bird$logAREA <- log(bird$AREA)
bird$logDIST <- log(bird$DIST)
bird$logLDIST <- log(bird$LDIST)

bird$YRISOL

# Первичный анализ данных мы провели на предыдущих лекциях


Mod_1 <- lm(ABUND ~ logAREA + YRISOL + logDIST + logLDIST + GRAZE + logAREA:GRAZE + YRISOL:GRAZE + logDIST:GRAZE + logLDIST:GRAZE, data = bird)

drop1(Mod_1, test = "F")

Mod_2 <- update(Mod_1, .~.-YRISOL:GRAZE)
drop1(Mod_2, test = "F")

Mod_3 <- update(Mod_2, .~.-logLDIST:GRAZE)
drop1(Mod_3, test = "F")

Mod_4 <- update(Mod_3, .~.-logDIST:GRAZE)
drop1(Mod_4, test = "F")


Mod_5 <- update(Mod_4, .~.-logAREA:GRAZE)
drop1(Mod_5, test = "F")


Mod_6 <- update(Mod_5, .~.-logDIST)
drop1(Mod_6, test = "F")


Mod_7 <- update(Mod_6, .~.-YRISOL)
drop1(Mod_7, test = "F")

Mod_8 <- update(Mod_7, .~.-logLDIST)
drop1(Mod_8, test = "F")



# Проверка валидности модели

Mod_8_diag <- fortify(Mod_8)

ggplot(Mod_8_diag, aes(x = 1:nrow(Mod_8_diag), y = .cooksd)) + geom_bar(stat = "identity")


# График остатков от предсказанных значений


gg_resid <- ggplot(data = Mod_8_diag, aes(x = .fitted, y = .stdresid)) +  geom_point() + geom_hline(yintercept = 0)

library(gridExtra)
grid.arrange(gg_resid, gg_resid + geom_smooth(),
             gg_resid + geom_smooth(method = "lm"), nrow = 1)



### 3) Графики остатков от предикторов в модели и не в модели

gg_resid + aes(x = bird$logDIST)

gg_resid + aes(x = bird$logLDIST)

gg_resid + aes(x = bird$YRISOL)


ggplot(Mod_8_diag, aes(x = GRAZE, y = .stdresid)) + geom_boxplot()

library(car)
qqPlot(Mod_8)



summary(Mod_8)

Anova(Mod_8)



# Визуализация финальной модели

MyData <- unique(bird[,c("logAREA", "GRAZE")])

MyData$Predict <- predict(Mod_8, newdata = MyData, se.fit = TRUE )$fit
MyData$SE <- predict(Mod_8, newdata = MyData, se.fit = TRUE )$se.fit

library(ggplot2)

ggplot(MyData, aes(x = logAREA, y = Predict, color = GRAZE)) + geom_line() + geom_ribbon(aes(ymin = Predict - 1.96*SE, ymax = Predict + 1.96*SE), alpha = 0.2) + geom_point(data = bird, aes(x = logAREA, y = ABUND, color = GRAZE))





