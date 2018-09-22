# "Регресионный анализ для бинарных данных"
# Вадим Хайтов, Марина Варфоломеева
# Каф. Зоологии беспозвоночных, СПбГУ
# Линейные модели на R, осень 2015


liz <- read.csv("data/polis.csv")



Mod_1 <- lm(PA ~ PARATIO, data = liz)
summary(Mod_1)




liz_model <- glm(PA ~ PARATIO , family="binomial", data = liz)
summary(liz_model)


-2*logLik(liz_model)
nul_model <-update(liz_model, .~.-PARATIO)

-2*logLik(nul_model)

## Задание. Вычислите вручную значение критерия G2 для модели, описывающей встречаемость ящериц (`liz_model`) и оцените уровень значимости для него


#Остаточная девианса
Dev_resid <- -2*logLik(liz_model)


#Нулевая девианса
Dev_nul <- -2*logLik(nul_model)

# Значение критерия G2


G2 <-Dev_nul - Dev_resid


p_value <- 1 - pchisq(G2, df = 1)

anova(liz_model, nul_model, test = "Chi")

library(car)
Anova(liz_model)

exp(coef(liz_model)[2])

## Построение логистической кривой средствами ggplot




ggplot(liz, aes(x=PARATIO, y=PA)) + geom_point() + geom_smooth(method="glm", method.args = list( family="binomial"), se=TRUE, size = 2) + ylab("Вероятность встречи ящериц")



##Задание: Постройте график логистической ререссии для модели `liz_model`  без использования `geom_smooth()`

MyData <- data.frame(PARATIO = seq(min(liz$PARATIO),
                                   max(liz$PARATIO),
                                   length.out = 100))




MyData$Predicted <- predict(liz_model,
                            newdata = MyData, type = "response" )
predict.glm()

ggplot(MyData, aes(x = PARATIO, y = Predicted)) + geom_line()





# Формируем модельную матрицу для искуственно созданных данных
X <- model.matrix( ~ PARATIO, data = MyData)


# Вычисляем параметры подобранной модели и ее матрицу ковариаций
betas    <- coef(liz_model) # Векор коэффицентов
Covbetas <- vcov(liz_model) # Ковариационная матрица

# Вычисляем предсказанные значения, перемножая модельную матрицу на вектор

# коэффициентов
MyData$eta <- X %*% betas

qplot( MyData$eta, MyData$Predicted)

## Получаем предсказанные значения

# Переводим предсказанные значения из логитов в вероятности
MyData$Pi  <- exp(MyData$eta) / (1 + exp(MyData$eta))

## Вычисляем границы доверительного интервала

MyData$se <- sqrt(diag(X %*% Covbetas %*% t(X)))

# Вычисляем доверительные интервалы
MyData$CiUp  <- exp(MyData$eta + 1.96 *MyData$se ) /
  (1 + exp(MyData$eta  + 1.96 * MyData$se))

MyData$CiLow  <- exp(MyData$eta - 1.96 *MyData$se ) /
  (1 + exp(MyData$eta  - 1.96 * MyData$se ))


## Строим график

ggplot(MyData, aes(x = PARATIO, y = Pi)) +
  geom_line(aes(x = PARATIO, y = CiUp),
            linetype = 2, size = 1) +
  geom_line(aes(x = PARATIO, y = CiLow),
            linetype = 2, size = 1) +
  geom_line(color = "blue", size=2) +
  ylab("Вероятность встречи")






# Проверка на избыточность дисперсии

E <- resid(liz_model, type = "pearson") #Пирсоновские остатки
p <- length(coef(liz_model)) #число параметров в модели
df <-   nrow(model.frame(liz_model)) - p #число степеней свободы
Overdisp <- sum(E^2) / df

Overdisp

summary(liz_model)


# Множественная логистическая регрессия

surviv <- read.table("data/ICU.csv", header=TRUE, sep=";")


##Сделаем факторами те дискретные предикторы, которые обозначенны цифрами
surviv$PO2 <- factor(surviv$PO2)
surviv$PH <- factor(surviv$PH)
surviv$PCO <- factor(surviv$PCO)
surviv$BIC <- factor(surviv$BIC)
surviv$CRE <- factor(surviv$CRE)
surviv$LOC <- factor(surviv$LOC)


M1 <- glm(STA ~ ., family = "binomial", data = surviv)
summary(M1)


##Задание: Проведите анализ девиансы для данной модели
Anova(M1)

##Задание: Подберите оптмальную модель и проведите ее диагностику
drop1(M1, test = "Chi")
M2 <- update(M1, .~.-CRE)

step(M1, direction = "backward")




M16 <- glm(formula = STA ~ AGE + CAN + SYS + TYP + PH + PCO + LOC, family = "binomial",   data = surviv)

Anova(M16)

AIC(M15, M16)

anova(M15, M1, test = "Chi")

summary(M15)


M15_diagn <- fortify(M15)

ggplot(M15_diagn, aes(x = .fitted, y =.stdresid)) + geom_point() + geom_smooth()







library(dplyr)

M15_diagn$group <- ntile(M15_diagn$.fitted, 10)

resi_and_fit <- M15_diagn %>%  group_by(group) %>%  summarise(mean_fit = mean(.fitted), mean_res = mean(.stdresid))

qplot(resi_and_fit$mean_fit, resi_and_fit$mean_res ) + geom_smooth()

lm(mean_res ~ mean_fit, data = resi_and_fit )

exp(coef(M15)[3])


#Визуализируем предсказания модели, взяв пр этом TYPE == mergency

MyData = expand.grid(AGE = ,
                     CAN = ,
                     SYS = ,
                     LOC = ,
                     TYP = "Emergency")

MyData$Predicted <- predict(M15, newdata = MyData, type = )



ggplot(MyData, aes(x=, y = Predicted, color = , group = )) + geom_line() + facet_grid(LOC~ CAN, labeller = label_both) + scale_color_gradient(low = "green",  high = "red") + labs(label = list(x = "Давление в момент реанимации (SYS)", y = "Вероятность гибели", color = "Возраст", title = "Предсказания модели"))



##########################################
#Самостоятельная работа

##########################################

# Загрузите датасет malaria из пакета ISwR и выясните есть ли связь между вероятностью заболевания малярией и возрастом


library(ISwR)
data("malaria")



