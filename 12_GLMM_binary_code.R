# title       : "Смешанные модели для бинарных зависимых величин"
# subtitle    : "Линейные модели..."
# author: Вадим Хайтов, Марина Варфоломеева

#Читаем данные

bal <- read.table("data/Yakovis2.csv", header = TRUE, sep = ";")

#Some housekeeping
bal$Site <- factor(bal$Site)
bal$Sample <- factor(bal$Sample)
bal$Substrate_ID <- factor(bal$Substrate_ID)


#Доля живых со следами сверления


# Доля мертвых со следами сверления



#Формулы для фиксированных и случайных эффектов





##Подбираем модель с помощью функции `glmmPQL`
library(MASS)

M1_PQL <- glmmPQL(Fix_effect, random = ~1|Sample/Substrate_ID, data = bal2, family = "binomial")

summary(M1_PQL)

length(unique(bal2$Substrate_ID))

##Подбираем модель с помощью функции `glmmML()`

library(glmmML)

M1_ML <- glmmML(Fix_effect, cluster = Substrate_ID, data = bal2)

M2_ML <- glmmML(Fix_effect, cluster = Sample, data = bal2)

summary(M1_ML)

summary(M2_ML)


##Подбираем модель с помощью функции `glmer()`

library(lme4)
M1_glmer <- glmer(Drill ~ BorN + ALength + Age + Position + Site +  (1|Sample/Substrate_ID), data = bal2, family = "binomial")

summary(M1_glmer)

##Сравним коэффициенты, подобранные разными функциями


GLMMPQL <- round(as.numeric(fixed.effects(M1_PQL)), 3)
GLMMML_1 = round(as.numeric(coefficients(M1_ML)), 3)
GLMMML_2 = round(as.numeric(coefficients(M2_ML)), 3)
GLMER <- round(as.numeric(fixed.effects(M1_glmer)), 3)

Coef <- data.frame(Parameter = names(fixed.effects(M1_glmer)), glmmPQL = GLMMPQL, glmmML_1 = GLMMML_1, glmmML_2 = GLMMML_2, glmer = GLMER )

Coef



##Выбор оптимальной модели

drop1(M1_glmer, test = "Chi")

##Выбор оптимальной модели
M2_glmer <- update(M1_glmer, .~.- Site)
anova(M1_glmer, M2_glmer)


##Выбор оптимальной модели

drop1(M2_glmer, test = "Chi")

##Выбор оптимальной модели

M3_glmer <- update(M2_glmer, .~.-Age)
anova(M2_glmer, M3_glmer)


##Выбор оптимальной модели

drop1(M3_glmer, test = "Chi")

##Результаты
summary(M3_glmer)

##Проверка валидности модели
plot(M3_glmer)


##Проверка валидности модели
library(ggplot2)
library(gridExtra)
diagnost <- data.frame(.fitted = fitted(M3_glmer), .resid = residuals(M3_glmer, type ="pearson"))

diagnost<-cbind(diagnost, bal2)

Pl1 <- ggplot(diagnost, aes(x = .fitted, y = .resid)) + geom_point() + geom_smooth(se=F)

Pl2 <- ggplot(diagnost, aes(x = BorN, y = .resid)) + geom_point() + geom_smooth(method = "loess", se=F)

Pl3 <- ggplot(diagnost, aes(x = ALength, y = .resid)) + geom_point() + geom_smooth( se=F)

Pl4 <- ggplot(diagnost, aes(x = Position, y = .resid)) + geom_boxplot()




png("diagnost.png",   width = 600, height = 600)

grid.arrange(Pl1, Pl2, Pl3, Pl4, ncol = 2)

dev.off()



#Визуализация предсказаний модели
MyData <- expand.grid(BorN = seq(min(bal2$BorN), max(bal2$BorN)), ALength = seq(min(bal2$ALength), max(bal2$ALength)),Position = levels(bal2$Position))


MyData$Predicted <- predict(M3_glmer,newdata = MyData, re.form = NA)


ggplot(MyData, aes(x = ALength, y = Predicted, color = BorN)) + geom_line(aes(group = BorN), size = 1.5) + facet_grid(~Position, labeller = label_both) + scale_color_gradient(low = "green", high = "red")



#Строим модель для идентификации криптических видов
# Данные взяты из работы M.Katolikova, V.Khaitov, R.Väinölä, M.Gantsevich, P.Strelkov "Genetic, Ecological and Morphological Distinctness of the Blue Mussels Mytilus trossulus Gould and M. edulis L. in the White Sea" PLOS ONE DOI:10.1371/journal.pone.0152963

myt <- read.table("data/myt_gen_morph.csv", header = TRUE, sep = ";")
head(myt)





##Вводим бинарную переменную

myt$Sp



# Строим модели









# Визуализация модели

MyData <- unique(myt[, c("Z", "population")])

MyData$L <- mean(myt$L)


MyData$Predict_random <- predict(    , newdata = MyData, type =     )



MyData2 <- data.frame (Z = seq(0, max(myt$Z), length.out = 100), L = mean(myt$L))

MyData2$Predict_fix <- predict(    , newdata =     , re.form =    , type =     )

ggplot(MyData, aes(x = Z)) + geom_line(aes(y = Predict_random, group = population), color = "gray") + geom_line(data = MyData2, aes(x = Z, y = Predict_fix), color = "red", size = 2) + theme_bw() + labs(x = "Z-индекс", y = "Вероятость быть M.trossulus")


#Самостоятельная работа


# ▶ graze - выпас коров (0, 1)
# ▶ AspectCat - экспозиция (S, N)
# ▶ nativecov - покрытие местной флоры %
# ▶ slope - наклон
# ▶ year - год наблюдений
# ▶ Park - парк
# ▶ plotID - уникальный идентификатор участка

# Моделируем встречаемость разных видов птиц

# Western Meadowlark (Sturnella neglecta)
# Horned Lark (Eremophila alpestris)
# Grasshopper Sparrow (Ammodramus savannarum)








