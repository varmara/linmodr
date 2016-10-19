# title: "Линейные модели для счетных данных"
# subtitle    : "Линейные модели..."
# author: Вадим Хайтов, Марина Варфоломеева



install.packages("AER")
install.packages("qcc")


# Пакеты ====
library(ggplot2)
library(car)
library(qcc)

#Настройка темы для графиков =============
theme_set(theme_bw(base_size = 16) +
            theme(legend.key = element_blank()))
update_geom_defaults("point", list(shape = 19, size = 3))


#Читаем данные =============

juv_ad <- read.table("mussel_juv_ad.csv", sep=";", header =T)


#Строим обычную линейную модель =============


M1 <- lm(Juv ~ Adult * factor(Year) , data = juv_ad)

summary(M1)

drop1(M1, test = "F")


# Сокращаем модель ==============
M2 <- lm(Juv ~ Adult  +  factor(Year), data = juv_ad)
Anova(M2)

# Визуализируем предсказания модели ==================
MyData <- expand.grid(Year = factor(seq(min(juv_ad$Year), max(juv_ad$Year))), Adult = seq(min(juv_ad$Adult), max(juv_ad$Adult)))

MyData$Predicted <- predict(M2, newdata = MyData)

ggplot(MyData, aes(x=Adult, y = Predicted, group = Year)) + geom_line(color = "blue") + geom_hline(yintercept = 0) + ylab("Ожидаемое количество молоди")


# Диагностика модели ===============

M2_diag <- fortify(M2)
ggplot(M2_diag, aes(x=.fitted, y = .stdresid)) + geom_point() + geom_hline(yintercept = 0) + geom_smooth(se = F)


#Пуассоновская модель===============

M3 <- glm(Juv ~ Adult * factor(Year), data = juv_ad, family = "poisson")
Anova(M3)

# Диагностика модели ==============
M3_diag <- data.frame(.fitted = predict(M3),  .pears_resid = residuals(M3, type = "pearson"))

ggplot(M3_diag, aes(x=.fitted, y = .pears_resid)) + geom_point() + geom_hline(yintercept = 0) + geom_smooth(se = F)


##Избыточность дисперсии (Overdispersion)


Resid_M3 <- resid(M3, type = "pearson") # Пирсоновские остатки

N <- nrow(juv_ad) # Объем выборки

p <- length(coef(M3))   # Число параметров в модели

df <- (N - p) # число степенейсвободы

fi <- sum(Resid_M3^2) /df  #Величина fi показывает во сколько раз в среднем 
#sigma > mu для данной модели

fi


# Квази-пуассоновская модель=============

M4 <- glm(Juv ~ Adult * factor(Year), data = juv_ad, family = "quasipoisson")
Anova(M4)

# Упрощенная модель без взаимодействия =============   

M4a <- glm(Juv ~ Adult  + factor(Year), data = juv_ad, family = "quasipoisson")
Anova(M4a)


#Предсказания упрощенной модели 

MyData <- expand.grid(Year = factor(seq(min(juv_ad$Year), max(juv_ad$Year))), Adult = seq(min(juv_ad$Adult), max(juv_ad$Adult)))

MyData$Predicted <- predict(M4a, newdata = MyData, type = "response")

ggplot(MyData, aes(x=Adult, y = Predicted, group = Year)) + geom_line(color = "blue") + geom_hline(yintercept = 0) + ylab("Ожидаемое количество молоди") +ylab("Ожидаемое количество молоди") + geom_point(data = juv_ad, aes(x = Adult, y = Juv, color = factor(Year) )) + facet_wrap(~Year) + guides(color=FALSE)

#Модель, основанная на отрицательном биномиальном распределении  

library(MASS)

M5 <- glm.nb(Juv ~ Adult*factor(Year) , data = juv_ad, link = "log")
Anova(M5)

# Здесь будет Ваш код для проверки избыточност дисперсии в модели M5 =====







#Визуализируем предсказание модели ========






# Строим оптимальную модель ==========


data(Affairs, package = "AER")
af <- Affairs









