# ---
# title: "Краткое введение в мир матричной алгебры"
# subtitle: "Линейные модели..."
# author: "Вадим Хайтов, Марина Варфоломеева"

#Создаем матрицу из вектора
# Задание: постройте марицу 4х3 из чисел от 1 до 12

matrix(data = 1:12,  )
matrix(1:12, nrow = 3, ncol = 4)

matrix(1:12, nrow = 3, ncol = 4, byrow = TRUE )


# Единичная матрица

diag(1, nrow=4)



A <- matrix(1:12, ncol = 3)
A

t(A)


A + 4

A + (A+4)

A + t(A)


B <- t(A)

A + B

# Создаем искусственный датасет для демонстрации сложения матриц
Large <- data.frame(Sp1 = round(rnorm(5, 10, 2)), Sp2 = round(rnorm(5, 10, 3)), Sp3 = round(rnorm(5, 10, 2)))

rownames(Large) <- c("Sample1", "Sample2", "Sample3", "Sample4", "Sample5" )

Small <- data.frame(Sp1 = round(rnorm(5, 50, 5)), Sp2 = round(rnorm(5, 50, 5)), Sp3 = round(rnorm(5, 50, 5)))

rownames(Small) <- c("Sample1", "Sample2", "Sample3", "Sample4", "Sample5" )

Large
Small


Large + Small


A * 4


d <- c(10, 11, 12)

A * d




##Простое произведеине

Rpocessed_portion <- c(1, 1, 1/2, 1/3, 1/4)
Processed_Factor <- 1/Rpocessed_portion

Small * Processed_Factor






#Матричное произведение двух вектров

N <- c(20, 40, 32, 45, 80, 50, 10)
Fert <- c( 0,  0,   1,   2,   2,   0,   0)

N %*% Fert

(N) %*% t(Fert)

t(Fert) %*% t(N)


A

B

A %*% B

B %*% A


C <- B[-3, ]

A %*% C

A %*% A

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

pr <- c(10, 20, 30, 40)

Nakr <- matrix(c(0.1, 0.15, 0.2, 0.15, 0.15, 0.05, 0.05, 0.09, 0.1, 0.05, 0.01, 0.1), ncol = 4)

Nakr %*% pr



###########################




#Вычисляем матрицу корреляций с помощью матричной алгебры


brain <- read.csv("data/IQ_brain.csv", header = TRUE)

br <- brain[complete.cases(brain), -1]

cor(br)

br <- as.matrix(br)
str(br)

br_scaled <- scale(br) #Стандартизация данных

cor_matrix <- t(br_scaled) %*% br_scaled / (nrow(br_scaled) - 1)

cor_matrix


cor(br)

###########################
# Задание
# Используя датасет `iris`, вычислите корреляцию между `Sepal.Length`  и `Sepal.Width`

iris
str(iris)
cor(iris$Sepal.Length, iris$Sepal.Width)

iris_subs <- iris[, c(1,2)]

iris_subs_scaled <- scale(iris_subs)

(t(iris_subs_scaled) %*% iris_subs_scaled / (nrow(iris_subs_scaled) - 1))[2, 1]



###########################

A
B
C

A %*% C

C %*% A


C %*% t(C)

t(C) %*% (C)


t(C %*% A)

t(A) %*% t(C)



det(A %*% t(A))

det(A[-4, ])

B[3,3] <- B[3,3] + 1

X <- (B[, -4])

det(X)

round(solve(X) %*% X)



###########################

# Решение систем линейных уравнений с помощью матричной алгебры


Coef <- matrix(c(1,2,3,4,5,6,7,8,10), nrow = 3, byrow = TRUE)

det(Coef)

Answer <- matrix(c(2,4,10), ncol = 1)

XYZ <- solve(Coef) %*% Answer


1*XYZ[1] + 2*XYZ[2] + 3*XYZ[3]


###########################


brain
M <- lm(PIQ ~ MRINACount, data = brain)


coef(M)

model.matrix(M)

X <-  data.frame(Inntercept = 1, MRINACount = brain$MRINACount)

X <- as.matrix(X)

det(t(X) %*% X)

y <- brain$PIQ

betas <- solve(t(X) %*% X) %*% (t(X) %*% y)

coef(M)


library(ggplot2)
theme_set(theme_bw())
ggplot(brain, aes(x = MRINACount, y = PIQ)) + geom_point() + geom_smooth(method = "lm")




##Строим линию регрессии  с помощью средств ggplot2

data(cars)
Mod <- lm(dist ~ speed, data = cars)



#####################################
# Постройте график линейной регрессии, опсанной данной моделью, используя приемы матричной алгебры



##Строим линию регрессии вручную

data.frame(Inntercept = 1, MRINACount = brain$MRINACount)

X <- as.matrix(X)

Y <- brain$PIQ

betas <- solve(t(X) %*% X) %*% (t(X) %*% Y)

betas

predict(M)

predict_values <-  X %*% betas

head(X, 2)

betas

qplot(x = brain$MRINACount, y = predict_values) + geom_line(color = "blue") + geom_point(data = brain, aes(y = PIQ))



My_data <- data.frame(MRINACount = seq(min(brain$MRINACount), max(brain$MRINACount), 100))

X2 <- data.frame(Intercept = 1, MRINACount = My_data$MRINACount)

X2 <- as.matrix(X2)

My_data$Predict <- X2 %*% betas

Pl <- ggplot(My_data, aes(x = MRINACount, y =  Predict)) +
   geom_line(color = "blue", size = 1) + geom_point(data = brain, aes(y = PIQ))



vcov(M)


RES <- brain$PIQ - X %*% betas

S2 <- sum(RES^2)/(nrow(brain) - length(betas))


VarCov <- S2 * solve(t(X) %*% X)






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


