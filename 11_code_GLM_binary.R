# "Регресионный анализ для бинарных данных"
# Вадим Хайтов, Марина Варфоломеева
# Каф. Зоологии беспозвоночных, СПбГУ
# Линейные модели на R, осень 2015


liz <- read.csv("polis.csv")

liz_model <- glm(PA ~ PARATIO , family="binomial", data = liz)
summary(liz_model)

## Задание. Вычислите вручную значение критерия G2 для модели, описывающей встречаемость ящериц (`liz_model`) и оцените уровень значимости для него




## Построение логистической кривой средствами ggplot

ggplot(liz, aes(x=PARATIO, y=PA)) + geom_point() + geom_smooth(method="glm", family="binomial", se=TRUE, size = 2) + ylab("Предсказанная вероятность встречи") + annotate("text", x=40, y=0.75, parse=TRUE, label = "pi == frac(e ^ {beta[0]+beta[1] %.% x}, 1 + e ^ {beta[0]+beta[1] %.% x})", size = 10)


##Задание: Постройте график логистической ререссии для модели `liz_model`  без использования `geom_smooth()`












########################
# Строим график вручную 
########################

MyData <- data.frame(PARATIO = seq(min(liz$PARATIO), max(liz$PARATIO)))

X <- model.matrix( ~ PARATIO, data = MyData)

betas    <- coef(liz_model) #Векор коэффицентов
Covbetas <- vcov(liz_model) # Ковариационная матрица

MyData$eta <- X %*% betas

MyData$Pi  <- exp(MyData$eta) / (1 + exp(MyData$eta))


#Вычисляем стандартные отшибки путем перемножения матриц
MyData$se <- sqrt(diag(X %*% Covbetas %*% t(X)))

MyData$CiUp  <- exp(MyData$eta + 1.96 * MyData$se) / (1 + exp(MyData$eta  + 1.96  * MyData$se))

MyData$CiLow  <- exp(MyData$eta - 1.96 *MyData$se) / (1 + exp(MyData$eta  - 1.96 * MyData$se))

## Строим график  

ggplot(MyData, aes(x = PARATIO, y = Pi)) +  geom_line(aes(x = PARATIO, y = CiUp), linetype = 2, size = 1) + geom_line(aes(x = PARATIO, y = CiLow), linetype = 2, size = 1) +  geom_line(color = "blue", size=2) + ylab("Вероятность встречи")




# Множественная логистическая регрессия

surviv <- read.table("ICU.csv", header=TRUE, sep=";")




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





##Упростим модель с помощью функции step()

step(M1, direction = "backward")

M2 <- glm(formula = STA ~ AGE + CAN + SYS + TYP + PH + PCO + LOC, family = "binomial",   data = surviv)

# M2 вложена в M1 следовательно их можно сравнить тестом отношения правдоподобий
anova(M1, M2, test = "Chi")


## Вопрос. Во сколько раз изменяется отношение шансов на выживание при условии, что пациент онкологический больной (при прочих равных условиях)?



#Визуализируем предсказания модели

MyData = expand.grid(AGE = seq(min(surviv$AGE), max(surviv$AGE), 1), CAN = levels(surviv$CAN),  SYS = seq(min(surviv$SYS), max(surviv$SYS), 10),  TYP =  "Emergency", PH = "1", PCO = "1", LOC ="1") 

MyData$Predicted <- predict(M2, newdata = MyData, type = "response")


ggplot(MyData, aes(x=SYS, y = Predicted, color = AGE, group = AGE)) + geom_line() + facet_grid(~ CAN, labeller = label_both) + scale_color_gradient(low = "green",  high = "red") + labs(label = list(x = "Давление в момент реанимации (SYS)", y = "Вероятность гибели", color = "Возраст", title = "Предсказания модели"))

