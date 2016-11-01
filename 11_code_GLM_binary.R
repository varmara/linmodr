# "Регресионный анализ для бинарных данных"
# Вадим Хайтов, Марина Варфоломеева
# Каф. Зоологии беспозвоночных, СПбГУ
# Линейные модели на R, осень 2015


liz <- read.csv("data/polis.csv")

Mod_1 <- lm(PA ~ PARATIO, data = liz)
summary(Mod_1)




liz_model <- glm(PA ~ PARATIO , family="binomial", data = liz)
summary(liz_model)

## Задание. Вычислите вручную значение критерия G2 для модели, описывающей встречаемость ящериц (`liz_model`) и оцените уровень значимости для него












#Остаточная девианса
Dev_resid <- -2*as.numeric(logLik(liz_model))

#Нулевая девианса
Dev_nul <- -2*as.numeric(logLik(update(liz_model, ~-PARATIO)))

# Значение критерия
(G2 <- Dev_nul - Dev_resid)

anova(liz_model, update(liz_model, ~-PARATIO), test = "Chi")

drop1(liz_model, test = "Chi")


(p_value <- 1 - pchisq(G2, df = 1))



exp(coef(liz_model)[2])


## Построение логистической кривой средствами ggplot








ggplot(liz, aes(x=PARATIO, y=PA)) + geom_point() + geom_smooth(method="glm", method.args = list( family="binomial"), se=TRUE, size = 2) + ylab("Вероятность встречи ящериц") + annotate("text", x=40, y=0.75, parse=TRUE, label = "pi == frac(e ^ {beta[0]+beta[1]%.%x}, 1 + e ^ {beta[0]+beta[1]%.%x})", size = 10) = 10


##Задание: Постройте график логистической ререссии для модели `liz_model`  без использования `geom_smooth()`

MyData <- data.frame(PARATIO =
                       seq(min(liz$PARATIO), max(liz$PARATIO)))




MyData$Predicted <- predict(liz_model,
                            newdata = MyData,
                            type = "response"
                            )









MyData <- data.frame(PARATIO = seq(min(liz$PARATIO), max(liz$PARATIO)))




# Формируем модельную матрицу для искуственно созданных данных
X <- model.matrix( ~ PARATIO, data = MyData)

```

## Извлекаем характеристики подобранной модели и получаем предсказанные значения










```{r}
# Вычисляем параметры подобранной модели и ее матрицу ковариаций
betas    <- coef(liz_model) # Векор коэффицентов
Covbetas <- vcov(liz_model) # Ковариационная матрица

# Вычисляем предсказанные значения, перемножая модельную матрицу на вектор

# коэффициентов
MyData$eta <- X %*% betas


## Получаем предсказанные значения

# Переводим предсказанные значения из логитов в вероятности
MyData$Pi  <- exp(MyData$eta) / (1 + exp(MyData$eta))

## Вычисляем границы доверительного интервала

```{r}
# Вычисляем стандартные отшибки путем перемножения матриц
MyData$se <- sqrt(diag(X %*% Covbetas %*% t(X)))

# Вычисляем доверительные интервалы
MyData$CiUp  <- exp(MyData$eta + 1.96 *MyData$se) /
  (1 + exp(MyData$eta  + 1.96 *MyData$se))

MyData$CiLow  <- exp(MyData$eta - 1.96 *MyData$se) /
  (1 + exp(MyData$eta  - 1.96 *MyData$se))


```

## Строим график

ggplot(MyData, aes(x = PARATIO, y = Pi)) +
  geom_line(aes(x = PARATIO, y = CiUp),
            linetype = 2, size = 1) +
  geom_line(aes(x = PARATIO, y = CiLow),
            linetype = 2, size = 1) +
  geom_line(color = "blue", size=2) +
  ylab("Вероятность встречи")
```








ggplot(MyData, aes(x = PARATIO, y = Predicted)) +
  geom_line(size=2, color = "blue") +
  xlab("Отношение периметра к площади") +
  ylab ("Вероятность") +
  ggtitle("Вероятность встречи ящериц")











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
anova(M1, update(M1, ~-.), test = "Chi")

##Упростим модель с помощью функции step()

step(M1, direction = "backward")

M2 <- glm(formula = STA ~ AGE + CAN + SYS + TYP + PH + PCO + LOC, family = "binomial",   data = surviv)

# M2 вложена в M1 следовательно их можно сравнить тестом отношения правдоподобий
anova(M1, M2, test = "Chi")


anova(M2, update(M2, ~-.), test = "Chi")


anova(M2, test = "Chi")

coef(M2)


## Вопрос. Во сколько раз изменяется отношение шансов на выживание при условии, что пациент онкологический больной (при прочих равных условиях)?



#Визуализируем предсказания модели

MyData = expand.grid(AGE = seq(min(surviv$AGE), max(surviv$AGE), 1), CAN = levels(surviv$CAN),  SYS = seq(min(surviv$SYS), max(surviv$SYS), 10),  TYP =  "Emergency", PH = "1", PCO = "1", LOC ="1")

MyData$Predicted <- predict(M2, newdata = MyData, type = "response")


ggplot(MyData, aes(x=SYS, y = Predicted, color = AGE, group = AGE)) + geom_line() + facet_grid(~ CAN, labeller = label_both) + scale_color_gradient(low = "green",  high = "red") + labs(label = list(x = "Давление в момент реанимации (SYS)", y = "Вероятность гибели", color = "Возраст", title = "Предсказания модели"))

