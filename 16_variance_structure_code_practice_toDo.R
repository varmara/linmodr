#' title       : "Коррекция гетерогенности дисперсий"
#' subtitle: "Линейные модели..."
#' author: "Вадим Хайтов, Марина Варфоломеева"



############## Самостоятельная работа################

## Пользуясь данными из датасета mussel_juv_ad.csv постройте модель, отражающую связь между обилием молодых мидий и обилием старых особей.

myt <- read.table("data/mussel_juv_ad.csv", sep = ";", header = TRUE)

str(myt)

unique(myt$Bank)

ggplot(myt, aes(x = Adult, y = Juv)) + geom_point() + facet_wrap(~Year)

myt$Year <- factor(myt$Year)


M1 <- gls(Juv ~ Adult*Year, data = myt)

plot(M1)


M2 <- gls(Juv ~ Adult*Year, data = myt, weights = varFixed(~Adult))

M3 <- gls(Juv ~ Adult*Year, data = myt, weights = varPower(~Adult))



M4 <- gls(Juv ~ Adult*Year, data = myt, weights = varIdent(form = ~1|Year))


AIC(M1, M4)

plot(M4)

Anova(M4)


# Пользуясь данными из встроенного датасета Spruce (пакет nlme) постройте модель роста деревьев в зависимости от времени


data("Spruce")

head(Spruce)






# Пользуясь данными из встроенного датасета RatPupWeight (пакет nlme) выясните зависит ли вес новорожденных крысят от экспериментального воздействия, пола и количества детенышей в помете.

data(RatPupWeight)

head(RatPupWeight)

RatPupWeight$Treatment

RatPupWeight$Litter

M1 <- gls(weight ~ sex*Lsize*Treatment, data = RatPupWeight)
plot(M1)

M2 <- lme(weight ~ sex*Lsize*Treatment, random = ~1|Litter, data = RatPupWeight)


M3 <- lme(weight ~ sex*Lsize*Treatment, random = ~1 + Lsize|Litter, data = RatPupWeight)


AIC(M1, M2, M3)


M2_1 <- lme(weight ~ sex*Lsize*Treatment, random = ~1|Litter, data = RatPupWeight, weights = varFixed(~Lsize))

M2_2 <- lme(weight ~ sex*Lsize*Treatment, random = ~1|Litter, data = RatPupWeight, weights = varPower(form = ~Lsize))

M2_3 <- lme(weight ~ sex*Lsize*Treatment, random = ~1 |Litter, data = RatPupWeight, weights = varPower(form =~Lsize|Treatment))

M2_3 <- lme(weight ~ sex*Lsize*Treatment, random = ~1|Litter, data = RatPupWeight, weights = varPower(form = ~Lsize|sex))

M2_4 <- lme(weight ~ sex*Lsize*Treatment, random = ~1 |Litter, data = RatPupWeight, weights = varIdent(form = ~Treatment))

M2_5 <- lme(weight ~ sex*Lsize*Treatment, random = ~1|Litter, data = RatPupWeight, weights = varExp(form = ~Lsize|Treatment))

M2_6 <- lme(weight ~ sex*Lsize*Treatment, random = ~1|Litter, data = RatPupWeight, weights = varExp(form = ~Lsize|sex))

M2_10 <- lme(weight ~ sex*Lsize*Treatment, random = ~1|Litter, data = RatPupWeight, weights = varConstPower(form = ~Lsize))

M2_7 <- lme(weight ~ sex*Lsize*Treatment, random = ~1|Litter, data = RatPupWeight, weights = varConstPower(form = ~Lsize|Treatment))

M2_8 <- lme(weight ~ sex*Lsize*Treatment, random = ~1|Litter, data = RatPupWeight, weights = varConstPower(form = ~Lsize|sex))

M2_9 <- lme(weight ~ sex*Lsize*Treatment, random = ~1|Litter, data = RatPupWeight, weights = varIdent(form = ~1|sex))


AIC(M2, M2_1, M2_2, M2_3, M2_4, M2_5, M2_6, M2_7, M2_8, M2_9, M2_10)

plot(M2_5)

plot(M2)



M2_5 <- lme(weight ~ sex * Lsize * Treatment, random = ~1|Litter, data = RatPupWeight, weights = varExp(form = ~Lsize|Treatment))


MyData <-  RatPupWeight %>% group_by(Treatment, sex, Litter) %>% do(data.frame(Lsize = seq(min(.$Lsize), max(.$Lsize))))

Predicted <- predict(M2_5, newdata = MyData, level = 0)




ggplot(RatPupWeight, aes(x = Lsize, y = weight, color = Litter)) + geom_point()



