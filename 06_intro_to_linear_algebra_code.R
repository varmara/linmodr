# ---
# title: "Краткое введение в мир матричной алгебры"
# subtitle: "Линейные модели..."
# author: "Вадим Хайтов, Марина Варфоломеева"

#Создаем матрицу из вектора
# Задание: постройте марицу 4х3 из чисел от 1 до 12

matrix(  , )


# Единичная матрица




# Создаем искусственный датасет для демонстрации сложения матриц
Large <- data.frame(Sp1 = round(rnorm(5, 10, 2)), Sp2 = round(rnorm(5, 10, 3)), Sp3 = round(rnorm(5, 10, 2)))

rownames(Large) <- c("Sample1", "Sample2", "Sample3", "Sample4", "Sample5" )

Small <- data.frame(Sp1 = round(rnorm(5, 50, 5)), Sp2 = round(rnorm(5, 50, 5)), Sp3 = round(rnorm(5, 50, 5)))

rownames(Small) <- c("Sample1", "Sample2", "Sample3", "Sample4", "Sample5" )

Large
Small


Large + Small


##Простое произведеине

Rpocessed_portion <- c(1, 1, 1/2, 1/3, 1/4)
Processed_Factor <- 1/Rpocessed_portion

Small * Processed_Factor






#Матричное произведение двух вектров

N <- c(20, 40, 32, 45, 80, 50, 10)
Fert <- c( 0,  0,   1,   2,   2,   0,   0)

t(N) %*% (Fert)

###################################
#
# Задание:
# В доме есть следующие электроприборы.
#
# Чайник  2 шт по  1200 Вт
# Обогреватели  3 шт. по 1300 Вт
# Осушитель  1 шт.  1100 Вт
# Стиральная машина  1 шт. 1500 Вт
# Фен  2 шт. по  800 Вт


# Какова будет суммарная мощность всех электроприборов, если их включить одновременно?

###################################





#Матричные произведения




## Демографическая модель с использованием матриц Лесли

T1 <- c(20, 40, 32, 45, 80, 50, 10)
Age <- c("0", "1-10", "11-20", "21-35", "36-45", "46-55", "56-65")
Pop <- data.frame(Age, T1)

Lesl <- matrix(
c( 0,  0,   1,   2,   2,   0,   0,
  0.6, 0,   0,   0,   0,   0,   0,
   0,  0.7, 0,   0,   0,   0,   0,
   0,  0,   0.8, 0,   0,   0,   0,
   0,  0,   0,   0.7, 0,   0,   0,
   0,  0,   0,   0,   0.6, 0,   0,
   0, 0,    0,   0,   0,  0.2, 0  ),
byrow = T,
ncol = 7)

Lesl

Pop$T2 <- as.vector( Lesl %*% (Pop$T1 ))
Pop$T3 <- as.vector( Lesl %*% (Pop$T2 ))
Pop$T4 <- as.vector( Lesl %*% (Pop$T3 ))
Pop$T5 <- as.vector( Lesl %*% (Pop$T4 ))
Pop$T6 <- as.vector( Lesl %*% (Pop$T5 ))
Pop$T7 <- as.vector( Lesl %*% (Pop$T6 ))
Pop$T8 <- as.vector( Lesl %*% (Pop$T7 ))
Pop$T9 <- as.vector( Lesl %*% (Pop$T8 ))
Pop$T10 <- as.vector( Lesl %*% (Pop$T9 ))

library(ggplot2)
library(reshape2)
Pop2 <- melt(Pop)
ggplot(Pop2, aes(x=Age, y = value)) + geom_bar(stat = "identity") + facet_wrap(~variable, ncol = 2)


##########################
# Задание
# Найдите выгодноо поставщика






###########################




#Вычисляем матрицу корреляций с помощью матричной алгебры


brain <- read.csv("data/IQ_brain.csv", header = TRUE)
br <- brain[complete.cases(brain), -1]
br <- as.matrix(br)
br_scaled <- scale(br) #Стандартизация данных

cor_matrix <- t(br_scaled) %*% br_scaled / (nrow(br_scaled) - 1)

cor_matrix


###########################
# Задание
# Используя датасет `iris`, вычислите корреляцию между `Sepal.Length`  и `Sepal.Width`





###########################


###########################

# Решение систем линейных уравнений с помощью матричной алгебры







###########################



##Строим линию регрессии  с помощью средств ggplot2

data(cars)
Mod <- lm(dist ~ speed, data = cars)



#####################################
# Постройте график линейной регрессии, опсанной данной моделью, используя приемы матричной алгебры



##Строим линию регрессии вручную

X <- model.matrix()
Y <-
betas <- solve(t() %*% ) %*% (t() %*% )

betas


predict_values <-  %*%

resid_values <- cars$dist -

s2 <- sum(resid_values^2)/(length(resid_values) - length(betas))

covbetas <-

covbetas

MyData <- data.frame(speed = seq()))
head(MyData)

X <- model.matrix( ~ , data = )
head(X)


MyData$predicted <-  %*%

##График модели

ggplot(MyData, aes()) + geom_abline(slope = , intercept = )


MyData$se <- sqrt(diag( %*%  %*%   ))

MyData$CiUp  <- MyData$predicted + 1.96 *MyData$se

MyData$CiLow  <- MyData$predicted - 1.96 *MyData$se

ggplot(MyData, aes(x = speed, y = predicted)) +


