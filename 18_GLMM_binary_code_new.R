# title       : "Смешанные модели для бинарных зависимых величин"
# subtitle    : "Линейные модели..."
# author: Вадим Хайтов, Марина Варфоломеева




## Читаем данные
astr2 <- read.csv('data/aster_mussel_full.csv', header = TRUE)
head(astr2)

## Наводим порядок в кодировке переменных

astr2$Year <- factor(astr2$Year)
astr2$Box <- factor(astr2$Box)
astr2$Sp <- factor(astr2$Sp)
astr2$Out <- ifelse(test = astr2$Outcome == 'eaten', yes = 1,  no = 0)

## Знакомимся с данными

# Нет ли пропущенных значений?
colSums(is.na(astr2))

## Каковы объемы выборок?
table(astr2$Box)

## Нет ли коллинеарности

library(cowplot);
library(ggplot2);
theme_set(theme_bw())

Pl_Sp <- ggplot(astr2, aes(x = Sp, y = L)) + geom_boxplot()
Pl_exp <- ggplot(astr2, aes(x = Experiment, y = L)) + geom_boxplot()
Pl_year <- ggplot(astr2, aes(x = Year, y = L)) + geom_boxplot()
plot_grid(Pl_Sp, Pl_exp, Pl_year, ncol = 3)

## Есть ли выбросы?

ggplot(astr2, aes(y = 1:nrow(astr2))) + geom_point(aes(x = L) )

library(lme4)

model1_ri <- glmer(Out ~ L*Sp*Year + (1|Experiment/Box) , data = astr2,
                   family = binomial(link = "logit"))



astr2$L_scaled <- as.numeric(scale(astr2$L))

model1_ri <- glmer(Out ~ L_scaled*Sp*Year +
                     (1|Experiment/Box) , data = astr2,
                   family = binomial(link = "logit"))






model1_rsi_1<- glmer(Out ~ L_scaled * Sp * Year + (1 + Sp|Experiment/Box) ,
                     data = astr2, family = binomial(link = "logit"))



model1_rsi_2 <- glmer(Out ~ L_scaled * Sp * Year + (1 + L_scaled |Experiment/Box), data = astr2, family = binomial(link = "logit"))








model1_rsi_1<- glmer(Out ~ L_scaled * Sp * Year + (1 + Sp|Experiment/Box) ,
                     data = astr2, family = binomial(link = "logit"),
                     control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

model1_rsi_2 <- glmer(Out ~ L_scaled * Sp * Year + (1 + L_scaled |Experiment/Box) ,
                      data = astr2, family = binomial(link = "logit"),
                      control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))


## Сравниваем две модели

AIC(model1_rsi_2, model1_ri)



## Диагностика модели: линейность связи
library(ggplot2)

model1_diagn <- fortify(model1_ri)

ggplot(model1_diagn, aes(x = .fitted, y = .scresid)) + geom_point() + geom_smooth()









## Диагностика модели: избыточность дисперсии

library(sjstats)
overdisp(model1_ri)

## Задание: Проведите упрощение модели в соответствии с протоколом backward selection
drop1(model1_ri)

model2 <- update(model1_ri, . ~ . -L_scaled:Sp:Year )

drop1(model2)

model3 <- update(model2, . ~ . -L_scaled:Year )

drop1(model3)

model4 <- update(model3, . ~ . -L_scaled:Sp )

drop1(model4)

model5 <- update(model4, . ~ . -Sp:Year )

drop1(model5)

model6 <- update(model5, . ~ . -Year )

drop1(model6)


## Диагностика финальной модели: линейность связи
model6_diagn <- fortify(model6)
ggplot(model6_diagn, aes(x = .fitted, y = .scresid)) + geom_point() + geom_smooth()



## Диагностика финальной модели: избыточность дисперсии

overdisp(model6)


summary(model6)


#Случайные эффекты


library(sjstats)
icc(model6)


## Подготовка к визуализации в виде логистических кривых

logit_back <- function(x) exp(x)/(1 + exp(x)) # обратная логит-трансформация

library(dplyr)
new_data <- astr2 %>% group_by(Sp) %>% do(data.frame(L_scaled = seq(min(.$L_scaled),max(.$L_scaled),
                                                                    length.out = 100)))

X <- model.matrix(~  L_scaled + Sp, data = new_data)
b <- fixef(model6)

new_data$fit_eta <- X %*% b
new_data$se_eta <- sqrt(diag(X %*% vcov(model6) %*% t(X)))

new_data$fit_pi <- logit_back(new_data$fit_eta)
new_data$lwr <- logit_back(new_data$fit_eta - 2 * new_data$se_eta)
new_data$upr <- logit_back(new_data$fit_eta + 2 * new_data$se_eta)

Pl_log <- ggplot(new_data, aes(x = L_scaled, y = fit_pi)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = Sp), alpha = 0.2) +
  geom_line(aes(color = Sp)) + labs(x = "Стандартизированная длина", y = "Вероятность \n быть съеденной" )
Pl_log



# Задание: Визуализируйте модель в виде столбчатой диаграммы

new_data <- astr2 %>% group_by(Sp) %>% do(data.frame(L_scaled = 0))

X <- model.matrix(~  L_scaled + Sp, data = new_data)
b <- fixef(model6)

new_data$fit_eta <- X %*% b
new_data$se_eta <- sqrt(diag(X %*% vcov(model6) %*% t(X)))

new_data$fit_pi <- logit_back(new_data$fit_eta)
new_data$lwr <- logit_back(new_data$fit_eta - 2 * new_data$se_eta)
new_data$upr <- logit_back(new_data$fit_eta + 2 * new_data$se_eta)

ggplot(new_data, aes(x = Sp, y = fit_pi)) + geom_col(fill = "gray") + geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2)


  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = Sp), alpha = 0.2) +
  geom_line(aes(color = Sp)) + labs(x = "Стандартизированная длина", y = "Вероятность \n быть съеденной" )
Pl_log






## Как можно показать перввичные данные

# Разбиваем на размерные классы приблизительно рвного объема
astr2$Size_class <- ntile(astr2$L_scaled, 10)

table(astr2$Size_class, astr2$Sp)


# Средние показатели в каждом из размерных классов

Mean_Out <- astr2 %>% group_by(Size_class, Sp) %>%
  do(data.frame(Out = mean(.$Out), L_scaled = mean(.$L_scaled)))
Pl_log + geom_point(data = Mean_Out, aes(x = L_scaled, y = Out, color = Sp))




# Тестовая выборка

astr_test <- read.csv('data/aster_mussel.csv', header = TRUE)


model6_unscaled <- glmer(Out ~ L + Sp +
                           (1|Experiment/Box) , data = astr2,
                         family = binomial(link = "logit"))

# Задание: Сделайте предсказания для новых данных





# Задание: Предложите способ визуализировать соотношение предсказанных и наблюдаемых значений.





########### Самостоятельная работа  ###########################
#Читаем данные

bal <- read.table("data/Yakovis2.csv", header = TRUE, sep = ";")
head(bal)

# Задание: Как связана вероятность гибели балнуса от
BorN ALength  Position  Site

#Some housekeeping
bal$Site <- factor(bal$Site)
bal$Sample <- factor(bal$Sample)
bal$Substrate_ID <- factor(bal$Substrate_ID)


#Доля живых со следами сверления
sum(bal[bal$Status == "live_barnacle", ]$Drill)/length(bal[bal$Status == "live_barnacle", ]$Drill)

mean(bal[bal$Status == "live_barnacle", ]$Drill)

mean(bal[bal$Status == "live_barnacle", ]$Drill == 1)

# Доля мертвых со следами сверления

mean(bal[bal$Status == "empty_test", ]$Drill)


bal2 <- bal[bal$Status == "empty_test", ]




##Подбираем модель с помощью функции `glmer()`

library(lme4)

summary(M1_glmer)








#Строим модель для идентификации криптических видов
# Данные взяты из работы M.Katolikova, V.Khaitov, R.Väinölä, M.Gantsevich, P.Strelkov "Genetic, Ecological and Morphological Distinctness of the Blue Mussels Mytilus trossulus Gould and M. edulis L. in the White Sea" PLOS ONE DOI:10.1371/journal.pone.0152963

myt <- read.table("data/myt_gen_morph.csv", header = TRUE, sep = ";")
head(myt)

##Вводим бинарную переменную

myt$Sp[myt$structure >= 0.5] <- 1

myt$Sp[myt$structure < 0.5] <- 0



head(myt)


# Строим модели

Myt_M1 <- glmer(Sp ~ Z + (1|population), data = myt, family = "binomial")


summary(Myt_M1)




diagnost <- fortify(Myt_M1)


diagnost<-data.frame(diagnost, myt)
head(diagnost)

Pl1 <- ggplot(diagnost, aes(x = .fitted, y = .scresid)) + geom_point() + geom_smooth(se=F)

Pl2 <- ggplot(diagnost, aes(x = Z, y = .scresid)) + geom_point() + geom_smooth(method = "loess", se=F)

Pl3 <- ggplot(diagnost, aes(x = L, y = .scresid)) + geom_point() + geom_smooth( se=F)


plot_grid(Pl1, Pl2, Pl3, ncol = 2)



library(sjstats)

overdisp(Myt_M1)


# Как решить эту проблему?

head(myt)


Myt_M2 <- glmer(Sp ~ Z*L + (1|population), data = myt, family = "binomial")






diagnost2<-data.frame(fortify(Myt_M2), myt)
head(diagnost2)

Pl1 <- ggplot(diagnost2, aes(x = .fitted, y = .scresid)) + geom_point() + geom_smooth(se=F)

Pl2 <- ggplot(diagnost2, aes(x = Z, y = .scresid)) + geom_point() + geom_smooth(method = "loess", se=F)

Pl3 <- ggplot(diagnost2, aes(x = L, y = .scresid)) + geom_point() + geom_smooth( se=F)


plot_grid(Pl1, Pl2, Pl3, ncol = 2)



ggplot(diagnost2, aes(x = population, y = .scresid)) + geom_boxplot()





overdisp(Myt_M2)

length(unique(myt$population))

Myt_M3 <- glmer(Sp ~ Z*scale(L) + (1|population), data = myt, family = "binomial")

overdisp(Myt_M3)



Myt_M4 <- glmer(Sp ~ Z*L + (1 + L|population), data = myt, family = "binomial")

Myt_M5 <- glmer(Sp ~ Z*L + (1 + Z|population), data = myt, family = "binomial")


overdisp(Myt_M4)


overdisp(Myt_M5)


AIC(Myt_M4, Myt_M5)


drop1(Myt_M5)


Myt_M6 <- update(Myt_M5, . ~ . - Z:L)

drop1(Myt_M6)


diagnost3<-data.frame(fortify(Myt_M6), myt)
head(diagnost2)

Pl1 <- ggplot(diagnost3, aes(x = .fitted, y = .scresid)) + geom_point() + geom_smooth(se=F)

Pl2 <- ggplot(diagnost3, aes(x = Z, y = .scresid)) + geom_point() + geom_smooth(method = "loess", se=F)

Pl3 <- ggplot(diagnost3, aes(x = L, y = .scresid)) + geom_point() + geom_smooth( se=F)


plot_grid(Pl1, Pl2, Pl3, ncol = 2)



ggplot(diagnost3, aes(x = population, y = .scresid)) + geom_boxplot()




summary(Myt_M6)

library(dplyr)

new_data <- myt %>% group_by(population) %>% do(data.frame(Z = seq(min(.$Z), max(.$Z), length.out = 100)))

new_data$L <- mean(myt$L)





new_data$fit <- predict(Myt_M6, newdata = new_data)



logit_back <- function(x) exp(x)/(1 + exp(x)) # обратная логит-трансформация

new_data$fit_pi <- logit_back(new_data$fit)





new_data2 <- myt %>% do(data.frame(Z = seq(min(.$Z), max(.$Z), length.out = 100)))

new_data2$L <- mean(myt$L)
new_data2$fit <- predict(Myt_M6, newdata = new_data2, re.form = NA)

new_data2$fit_pi <- logit_back(new_data2$fit)



ggplot(new_data, aes(x = Z, y = fit_pi, group = population)) + geom_line()  + geom_line(data = new_data2, aes(x = Z, y = fit_pi, group =1), color = "blue", size =2)


