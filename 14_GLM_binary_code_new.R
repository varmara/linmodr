# "Регресионный анализ для бинарных данных"
# Вадим Хайтов, Марина Варфоломеева
# Каф. Зоологии беспозвоночных, СПбГУ
# Линейные модели на R, осень 2015



## Читаем данные

astr <- read.csv('data/aster_mussel.csv', header = TRUE)
head(astr)


astr$Box <- factor(astr$Box)


## Знакомимся с данными

# Нет ли пропущенных значений?

colSums(is.na(astr))

# Каковы объемы выборок?

table(astr$Box)

## Нет ли коллинеарности

library(ggplot2)
library(cowplot)

Pl_Sp <- ggplot(astr, aes(x = Sp, y = L)) + geom_boxplot()
Pl_Box <- ggplot(astr, aes(x = Box, y = L)) + geom_boxplot()
plot_grid(Pl_Sp, Pl_Box, ncol = 2)


## Есть ли выбросы?

ggplot(astr, aes(y = 1:nrow(astr))) + geom_point(aes(x = L) )


## Кодирование бинарной переменной

astr$Out <- ifelse(test = astr$Outcome == 'eaten', yes = 1,  no = 0)


mod_norm <- glm(Out ~ Sp * L * Box, data = astr)

library(dplyr)
new_data <- astr %>% group_by(Sp, Box)%>%
  do(data.frame(L = seq(min(.$L), max(.$L), length.out = 100)))

new_data$fit <- predict(mod_norm, newdata = new_data) # Предсказанные значения

ggplot(new_data, aes(x = L, y = fit)) +
  geom_line(aes(group = Box)) + facet_wrap(~ Sp, ncol = 2) +
  geom_point(data = astr, aes(x = L, y = Out), size = 0.5, color = 'blue')

# Что смущает?


mod_norm_diag <- fortify(mod_norm)
ggplot(mod_norm_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() + geom_vline(xintercept = 0)



# Строим полную модель

mod <- glm(Out ~ Sp*L*Box, family = binomial(link = 'logit'), data = astr)


## Анализ девиансы для полной модели

library(car)
Anova(mod)

#
# Эту модель можно упростить!
# Упростите ее







## Сравните AIC для финальной и полной модели







# Диагностика финальной модели

mod6_diag <- data.frame(.fitted = fitted(mod6, type = 'response'),
                        .resid_p = resid(mod6, type = 'pearson'))

ggplot(mod6_diag, aes(y = .resid_p, x = .fitted)) + geom_point() +
  geom_hline(yintercept = 0) +  geom_smooth(method = 'loess')



# Проверка на сверхдисперсию

# Функция для проверки наличия сверхдисперсии в модели (автор Ben Bolker)
# http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html
# Код модифицирован, чтобы учесть дополнительный параметр в NegBin GLMM, подобранных MASS::glm.nb()
overdisp_fun <- function(model) {
  rdf <- df.residual(model)  # Число степеней свободы N - p
  if (any(class(model) == 'negbin')) rdf <- rdf - 1 ## учитываем k в NegBin GLMM
  rp <- residuals(model,type='pearson') # Пирсоновские остатки
  Pearson.chisq <- sum(rp^2) # Сумма квадратов остатков, подчиняется Хи-квадрат распределению
  prat <- Pearson.chisq/rdf  # Отношение суммы квадратов остатков к числу степеней свободы
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE) # Уровень значимости
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)        # Вывод результатов
}


overdisp_fun(mod6)

summary(mod6)

# Визуализация модели

library(dplyr)
new_data <- astr %>% group_by(Sp) %>%
  do(data.frame(L = seq(min(.$L), max(.$L), length.out = 100)))

## Предсказания модели при помощи операций с матрицами

# Модельная матрица и коэффициенты
X <- model.matrix(~ Sp + L, data =  new_data)
b <- coef(mod6)

# Предсказанные значения и стандартные ошибки...
# ...в масштабе функции связи (логит)

new_data$fit_eta <- X %*% b
new_data$se_eta <- sqrt(diag(X %*% vcov(mod6) %*% t(X)))

# ...в масштабе отклика (применяем функцию, обратную функции связи)

logit_back <- function(x) exp(x)/(1 + exp(x)) # обратная логит-трансформация


new_data$fit_pi <- logit_back(new_data$fit_eta)

new_data$lwr_pi <- logit_back(new_data$fit_eta - 2 * new_data$se_eta)
new_data$upr_pi <- logit_back(new_data$fit_eta + 2 * new_data$se_eta)

head(new_data, 2)


ggplot(new_data, aes(x = L, y = fit_eta, fill = Sp)) +
  geom_ribbon(aes(ymin = fit_eta - 2 * se_eta, ymax = fit_eta + 2 * se_eta), alpha = 0.5) +   geom_line(aes(color = Sp))




## Визуализация в шкале вероятностей интуитивно понятнее

ggplot(new_data, aes(x = L, y = fit_pi, fill = Sp)) +
  geom_ribbon(aes(ymin = lwr_pi, ymax = upr_pi), alpha = 0.5) +
  geom_line(aes(color = Sp)) +
  labs(y='Вероятность', title = 'Вероятность быть съеденной')








##########################################
#Самостоятельная работа

##########################################





#####################################3

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

