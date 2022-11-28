# title       : "Смешанные модели для бинарных зависимых величин"
# subtitle    : "Линейные модели..."
# author: Вадим Хайтов, Марина Варфоломеева




## Читаем данные
astr2 <- read.csv('data/aster_mussel_full.csv', header = TRUE)
head(astr2)

str(astr2)


## Наводим порядок в кодировке переменных

astr2$Year <- factor(astr2$Year)
astr2$Box <- factor(astr2$Box)
astr2$Sp <- factor(astr2$Sp)
astr2$Experiment <- factor(astr2$Experiment)
astr2$Out <- ifelse(test = astr2$Outcome == 'eaten', yes = 1,  no = 0)

## Знакомимся с данными

# Нет ли пропущенных значений?
colSums(is.na(astr2))

## Каковы объемы выборок?
table(astr2$Box)

## Нет ли коллинеарности

library(cowplot); library(ggplot2); theme_set(theme_bw())

Pl_Sp <- ggplot(astr2, aes(x = Sp, y = L)) + geom_boxplot()
Pl_exp <- ggplot(astr2, aes(x = Experiment, y = L)) + geom_boxplot()
Pl_year <- ggplot(astr2, aes(x = Year, y = L)) + geom_boxplot()
plot_grid(Pl_Sp, Pl_exp, Pl_year, ncol = 3)

## Есть ли выбросы?

ggplot(astr2, aes(y = 1:nrow(astr2))) + geom_point(aes(x = L) )

library(lme4)

model1_ri <- glmer(Out ~ L*Sp*Year + (1|Experiment/Box) , data = astr2, family = binomial(link = "logit"))



astr2$L_scaled <- scale(astr2$L)

qplot(astr2$L, astr2$L_scaled)

model1_ri <- glmer(Out ~ L_scaled*Sp*Year +
                     (1|Experiment/Box) , data = astr2,
                   family = binomial(link = "logit"))


# model1_rsi_1<- glmer(Out ~ L_scaled * Sp * Year + (1 + Sp|Experiment/Box), data = astr2, family = binomial(link = "logit"))

model1_rsi_2 <- glmer(Out ~ L_scaled * Sp * Year + (1 + L_scaled |Experiment/Box) ,
                      data = astr2, family = binomial(link = "logit"))








model1_rsi_1<- glmer(Out ~ L_scaled * Sp * Year + (1 + L_scaled |Experiment/Box) ,
                      data = astr2, family = binomial(link = "logit"),
                      control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))


## Сравниваем три модели

AIC(model1_ri,model1_rsi_1)




## Диагностика модели: линейность связи
library(ggplot2)

model1_diagn <- fortify.merMod(model1_ri)


ggplot(model1_diagn, aes(x = .fitted, y = .scresid)) + geom_point() + geom_smooth()









## Диагностика модели: избыточность дисперсии

library(performance)
check_overdispersion(model1_ri)

summary(model1_ri)
## Задание: Проведите упрощение модели в соответствии с протоколом backward selection

model1_ri_6 <- glmer(Out ~ L_scaled + Sp +
                     (1|Experiment/Box) , data = astr2,
                   family = binomial(link = "logit"))








## Диагностика финальной модели: линейность связи
model6_diagn <- fortify.merMod(model1_ri_6)
ggplot(model6_diagn, aes(x = .fitted, y = .scresid)) + geom_point() + geom_smooth()



## Диагностика финальной модели: избыточность дисперсии

check_overdispersion(model1_ri_6)


#Случайные эффекты

icc(model1_ri_6)

library(partR2)
partR2(model1_ri_6)



summary(model1_ri_6)

exp(fixef(model1_ri_6)[3])


## Подготовка к визуализации в виде логистических кривых

logit_back <- function(x) exp(x)/(1 + exp(x)) # обратная логит-трансформация

library(dplyr)
new_data <- astr2 %>% group_by(Sp) %>% do(data.frame(L_scaled = seq(min(.$L_scaled),
                                                                    max(.$L_scaled),
                                                                    length.out = 100)))

X <- model.matrix(~  L_scaled + Sp, data = new_data)
b <- fixef(model1_ri_6)

new_data$fit_eta <- as.numeric(X %*% b)
new_data$se_eta <- sqrt(diag(X %*% vcov(model1_ri_6) %*% t(X)))

new_data$fit_pi <- logit_back(new_data$fit_eta)
new_data$lwr <- logit_back(new_data$fit_eta - 2 * new_data$se_eta)
new_data$upr <- logit_back(new_data$fit_eta + 2 * new_data$se_eta)

Pl_log <- ggplot(new_data, aes(x = L_scaled, y = fit_pi)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = Sp), alpha = 0.2) +
  geom_line(aes(color = Sp)) + labs(x = "Стандартизированная длина", y = "Вероятность \n быть съеденной" )
Pl_log



# Задание: Визуализируйте модель в виде столбчатой диаграммы

new_data <- data.frame(Sp = levels(astr2$Sp), L_scaled = 0)


X <- model.matrix(~  L_scaled + Sp, data = new_data)
b <- fixef(model1_ri_6)

new_data$fit_eta <- as.numeric(X %*% b)
new_data$se_eta <- sqrt(diag(X %*% vcov(model1_ri_6) %*% t(X)))

new_data$fit_pi <- logit_back(new_data$fit_eta)
new_data$lwr <- logit_back(new_data$fit_eta - 2 * new_data$se_eta)
new_data$upr <- logit_back(new_data$fit_eta + 2 * new_data$se_eta)


ggplot(new_data, aes(x = Sp, y = fit_pi)) +
  geom_col(aes(fill = Sp)) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
  guides(fill = "none") +
  labs(x = "Species", y = "Probability of being eaten") +
  scale_fill_manual(values = c("red", "blue"))



## Как можно показать первичные данные

# Разбиваем на размерные классы приблизительно рвного объема
astr2$Size_class <- ntile(astr2$L_scaled, 10)

table(astr2$Size_class, astr2$Sp)


# Средние показатели в каждом из размерных классов

Mean_Out <- astr2 %>% group_by(Size_class, Sp) %>%
  do(data.frame(Out = mean(.$Out), L_scaled = mean(.$L_scaled)))

Pl_log +
  geom_point(data = Mean_Out, aes(x = L_scaled, y = Out, color = Sp)) +
  geom_point(data = astr2, aes(y = Out, color = Sp), position = position_jitter(height = 0.1))




# Тестовая выборка

astr_test <- read.csv('data/aster_mussel.csv', header = TRUE)


model6_unscaled <- glmer(Out ~ L + Sp +
                           (1|Experiment/Box) , data = astr2,
                         family = binomial(link = "logit"))

# Задание: Сделайте предсказания для новых данных

astr_test$Predicted <- predict(model6_unscaled, newdata = astr_test, re.form = NA)

astr_test$Predicted_pi <- logit_back(astr_test$Predicted)




# Задание: Предложите способ визуализировать соотношение предсказанных и наблюдаемых значений.

ggplot(astr_test, aes(x = Outcome, y = Predicted_pi, fill = Sp)) +
  geom_boxplot(notch = T)





##################################################################

# Пример построения модели для данных, где события даны в виде частот

library(partR2)
data("Grasshoppers")

# BIO18 = precipitation of warmest quarter


Mod_gh <- glmer(cbind(nGreen, nBrown) ~ Sex + Bio18 + (1|SiteID), data = Grasshoppers, family = binomial(link = "logit") )


Mod_gh <- glmer(cbind(nGreen, nBrown) ~  Sex + scale(Bio18) + (1|SiteID) , data = Grasshoppers, family = binomial(link = "logit") )

library(car)
vif(Mod_gh)

library(performance)
check_overdispersion(Mod_gh)



Mod_gh_diagn <- fortify.merMod(Mod_gh)

ggplot(Mod_gh_diagn, aes(x = .fitted, y = .scresid)) + geom_point() + geom_smooth() + geom_hline(yintercept = 0)

ggplot(Mod_gh_diagn, aes(x = Sex, y = .scresid)) + geom_point() + geom_boxplot()

ggplot(Mod_gh_diagn, aes(x = Bio18, y = .scresid)) + geom_point() + geom_smooth() + geom_hline(yintercept = 0)


# summary(Mod_gh)



library(mgcv)
Grasshoppers$SiteID = factor(Grasshoppers$SiteID)
Grasshoppers$Sex = factor(Grasshoppers$Sex)


Mod_gh_gam <- gam(cbind(nGreen, nBrown) ~ s(Bio18)  + Sex + s(SiteID, bs = "re", k = 42), data = Grasshoppers, family = binomial(link = "logit"), method = "REML" )

plot(Mod_gh_gam)


qplot(x = fitted(Mod_gh_gam), y = residuals(Mod_gh_gam, type = "pearson")) +
  geom_smooth() +
  geom_hline(yintercept = 0)


qplot(x = (Grasshoppers$Bio18), y = residuals(Mod_gh_gam, type = "pearson")) + geom_smooth()

qplot(x = fitted(Mod_gh_gam), y = residuals(Mod_gh_gam, type = "pearson")) + geom_smooth() + geom_hline(yintercept = 0)

summary(Mod_gh_gam)




########### Самостоятельная работа  ###########################

# Задание 1. От чего зависит судьба морских желудей?
# Данные взяты из работы Yakovis, E., & Artemieva, A. (2015). Bored to death: community-wide effect of predation on a foundation Species in a low-Disturbance arctic subtidal system. PloS one, 10(7), e0132973.

#Читаем данные

bal <- read.table("data/Yakovis2.csv", header = TRUE, sep = ";")



#
# Site -точка сбора материала
# Sample - квдарат 1х1 м, на котором производился сбор друз
# BorN - количество Boreotrophon clathratus на квадрате
# Substrate_ID - Номер друзы
# ALength - Диаметр апертуры
# Age - Возраст балянуса
# Position - Расположение балянуса (первичный субстрат/вторичный субстрат)
# Status - живой/мертвый
#
# Drill - Зависимая переменная (0 - нет следов сверления; 1 - есть следы сверления)
#
# Для ответа на поставленный вопрос целесообразно работать с мертвыми особями




# Вопрос: от каких факторов зависит будет ли атакован балянус хищником?
# Задание: Как связана вероятность гибели балнуса от
# BorN ALength Age Position  Site


#Some housekeeping
bal$Site <- factor(bal$Site)
bal$Sample <- factor(bal$Sample)
bal$Position <- factor(bal$Position)
bal$Substrate_ID <- factor(bal$Substrate_ID)


#Доля живых со следами сверления
sum(bal[bal$Status == "live_barnacle", ]$Drill)/length(bal[bal$Status == "live_barnacle", ]$Drill)


mean(bal[bal$Status == "live_barnacle", ]$Drill)

mean(bal[bal$Status == "live_barnacle", ]$Drill == 1)

# Доля мертвых со следами сверления

mean(bal[bal$Status == "empty_test", ]$Drill)




bal2 <- bal[bal$Status == "empty_test", ]

pl <- ggplot(bal2, aes(y = 1:nrow(bal2))) +geom_point()
pl + aes(x = BorN)
pl +aes(x = ALength)
pl + aes(x= Age)
table(bal2$Position)
table(bal2$Site)

library(lme4)
M1_glm <- glm(Drill ~ Position + ALength + Age +  BorN + Site, data = bal2, family = "binomial")

vif(M1_glm)

M2_glm <- glm(Drill ~ Position + ALength +  BorN + Site, data = bal2, family = "binomial")
vif(M2_glm)

M_glmer <- glmer(Drill ~ Position * scale(ALength) * scale(BorN) + Site + (1|Sample/Substrate_ID), data = bal2, family = "binomial", control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

M_glmer_diag <- fortify.merMod(M_glmer)
ggplot(M_glmer_diag, aes (x = .fitted, y = .scresid)) + geom_point() + geom_smooth()

ggplot(M_glmer_diag, aes (x = ALength, y = .scresid)) + geom_point() + geom_smooth()

ggplot(M_glmer_diag, aes (x = BorN, y = .scresid)) + geom_point() + geom_smooth(method = "loess")

ggplot(M_glmer_diag, aes (x = Position, y = .scresid)) + geom_boxplot()

ggplot(M_glmer_diag, aes (x = Position, y = .scresid)) + geom_boxplot()

summary(M_glmer)
model.matrix(M_glmer)


###################################################
#Задание 2. Можно ли использовать морфологический маркер для идентификации криптических видов мидий
# Данные взяты из работы M.Katolikova, V.Khaitov, R.Väinölä, M.Gantsevich, P.Strelkov "Genetic, Ecological and Morphological Distinctness of the Blue Mussels Mytilus trossulus Gould and M. edulis L. in the White Sea" PLOS ONE DOI:10.1371/journal.pone.0152963

myt <- read.table("data/myt_gen_morph.csv", header = TRUE, sep = ";")
head(myt)





##Вводим бинарную переменную

myt$Sp[myt$structure >= 0.5] <- 1

myt$Sp[myt$structure < 0.5] <- 0





