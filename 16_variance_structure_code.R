#' title       : "Коррекция гетерогенности дисперсий"
#' subtitle: "Линейные модели..."
## Способы моделирования "поведения" дисперсии

## Читаем данные

library(faraway)
data(fruitfly)
fly <- fruitfly # Переименуем датасет для краткости
str(fly)


# Есть ли пропущенные значения?

colSums(is.na(fly))

# Сколько измерений по каждой из градаций?
table(fly$activity)


## Нет ли выбросов: пишем код

library(ggplot2)
theme_set(theme_bw())

gg_dot <- ggplot(fly, aes(y = 1:nrow(fly))) +
  geom_point()
Pl1 <- gg_dot + aes(x = longevity)
Pl2 <- gg_dot + aes(x = thorax)

library(cowplot)
plot_grid(Pl1, Pl2)


## Нет ли коллинеарности

ggplot(fly, aes(x = activity, y = thorax)) + geom_boxplot()



  ## Код для подгонки модели

mod_formula <- longevity ~ thorax*activity
M1 <- lm(mod_formula, data = fruitfly)

library(car)
Anova(M1)


M1_diagn <- fortify( )
ggplot( , aes(x = , y = )) + geom_point() + geom_hline(yintercept = 0)


##########################################################################
# Почему Нельзя доверять результатам если выявлена гетероскедастичность? #
##########################################################################


set.seed(568)  # this makes the example exactly reproducible

N_trial <- 1000 # RКоличество повторных оценок значений параметров модели

b1_val <- rep(NA, N_trial) #Создаем вектор "заготовок" для значений оценок коэффициента b1 в модели y ~ b0 +b1*x

p_val <- rep(NA, N_trial) #Создаем вектор "заготовок" для значений p_values для оценок коэффициента b1 в модели y ~ b0 +b1*x

pb = txtProgressBar(min = 0, max = N_trial, initial = 0) # а programming tricks - создает прогресс-бар

for(i in 1:N_trial){
  x      = rep(1:100,3) #Значения предиктора
  b0      = 10 #Intercept
  b1      = 0 #Slope
  sigma2 = x^4 #Дисперсия связана с предиктором
  eps    = rnorm(x,mean=0,sd=sqrt(sigma2)) #Задаем случайную часть модели
  y      = b0+b1*x + eps #Задаем выборочные значения переменной отклика
  mod    = lm(y ~ x) #Модель
  b1_val[i] <- summary(mod)$coefficients[2,1] #Извлекаем оценки b1 из результатов
  p_val[i] <- summary(mod)$coefficients[2,4] #Извлекаем p_values из результатов

  setTxtProgressBar(pb,i) #Рисует прогресс-бар
}


mean(p_val < 0.05) #Доля статистически значимых результатов

hist(b1_val) #Распределение оценок коэффициента b1


############################################################







library(nlme)
M1_gls <- gls(mod_formula, data = fruitfly)


#Дагностика модели

Pl_resid_M1_gls <- qplot(x = fitted(M1_gls), y = residuals(M1_gls, type = "pearson")) + geom_hline(yintercept = 0)

Pl_resid_M1_gls


# Моделирование структуры дисперсии


##  Фиксированная структура дисперсии: varFixed()

M2_gls <- gls(mod_formula, data = fly, weights = varFixed( ~ thorax))

# Сравните две модели



## Степенная зависимость дисперсии от ковариаты: varPower()

M3_gls <- gls(mod_formula, data = fly, weights = varPower(form = ~ thorax))


summary(M3_gls)

M3_gls$modelStruct


## Степенная зависимость дисперсии от ковариаты для разных уровней дискретного фактора

M4_gls <- gls(mod_formula, data = fly,
              weights = varPower(form = ~ thorax|activity))

M4_gls$modelStruct



## Экспоненциальная зависимость дисперсии от ковариаты: varExp()
M5_gls <- gls(mod_formula, data = fly,
              weights = varExp(form = ~ thorax))
M6_gls <- gls(mod_formula, data = fly,
              weights = varExp(form = ~ thorax|activity))


M5_gls$modelStruct
M6_gls$modelStruct



## Усложненная степенная зависимость дисперсии от ковариаты

M7_gls <- gls(mod_formula, data = fly,
              weights = varConstPower(form = ~ thorax))
M8_gls <- gls(mod_formula, data = fly,
              weights = varConstPower(form = ~ thorax|activity))


M7_gls$modelStruct
M8_gls$modelStruct



## Разные дисперсии для разных уровней категориальных предикторов:  varIdent()

M9_gls <- gls(mod_formula, data = fly,
              weights = varIdent(form = ~1|activity))

summary(M9_gls)


## Комбинированная структура дисперсии: varComb()

M10_gls <- gls(mod_formula, data = fly,
               weights = varComb(varIdent(form = ~ 1|activity),
                                 varFixed(~ thorax)))
M11_gls <- gls(mod_formula, data = fly,
               weights = varComb(varIdent(form = ~ 1|activity),
                                 varPower(form = ~ thorax)))

M12_gls <- gls(mod_formula, data = fly,
               weights = varComb(varIdent(form = ~1| activity),
                                 varExp(form = ~ thorax)))

M13_gls <- gls(mod_formula, data = fly,
               weights = varComb(varIdent(form = ~ 1|activity),
                                 varConstPower(form = ~ thorax)))


AICs <- AIC(M1_gls, M2_gls, M3_gls,
            M4_gls, M5_gls, M6_gls,
            M7_gls, M8_gls, M9_gls,
            M10_gls, M12_gls,M13_gls)



# Кака модель наилучшая?





## Диагностика финальной модели

Pl_resid_M1_gls <- Pl_resid_M1_gls  + ggtitle("Было") +
  labs(x = ".fitted", y = "Pearson resid.")
Pl_resid_M10_gls <-  qplot(x = fitted(M10_gls),
                           y = residuals(M10_gls, type = "pearson")) +
  geom_hline(yintercept = 0) +
  ggtitle("Стало")+ labs(x = ".fitted", y = "Pearson resid.")

library(cowplot)
plot_grid(Pl_resid_M1_gls, Pl_resid_M10_gls)




# Задание: упростите модель







## Финальная модель и подготовка визуализации

M10_final <- update(   )

library(dplyr)
new_data <- fly %>% group_by(activity) %>%
  do(data.frame(thorax = seq(min(.$thorax), max(.$thorax), length.out = 100)))

X <- model.matrix(~ thorax + activity, data = new_data)
b <- coef(M10_final)

new_data$fitted <- X%*%b

new_data$SE <- sqrt(diag(X %*% vcov(M10_final) %*% t(X)))

ggplot(new_data, aes(x = thorax, y = fitted, color = activity)) +
  geom_line() +
  geom_ribbon(aes(ymin = fitted - 2 * SE,
                  ymax = fitted + 2 * SE,
                  fill = activity), alpha = 0.5) +
  geom_point(data = fly, aes(x = thorax, y = longevity))











#' # Моделирование структуры дисперсии при наличии группирующих (случайных) факторов
#' ## Пример: Рост крыс при разной диете
#' Пример взят из книги Pinheiro & Bates, 2000 (Hand and Crowder (1996))
#' Три группы крыс, содержались при разных условиях кормления 64 дня. Каждую крысу взвешивали с определнной периодичностью.


data("BodyWeight")
bw <- as.data.frame(BodyWeight)
head(bw, 14)


#' ## Задание
#' Постройте модель, которая дала бы ответ на вопрос, изменяется ли характер роста крыс в зависимости от типа диеты?




##Выбираем наилучшую модель


M3_1 <- update(M3, weights = varIdent(form = ~ 1|Diet))
M3_2 <- update(M3, weights = varPower(form = ~Time))
M3_3 <- update(M3, weights = varPower(form = ~Time|Diet))
# M3_4 <- update(M3, weights = varConstPower(form = ~Time))
M3_5 <- update(M3, weights = varExp(form = ~Time))
M3_6 <- update(M3, weights = varExp(form = ~Time|Diet))
M3_7 <- update(M3, weights = varComb(varExp(form = ~Time),
                                     varIdent(form = ~1|Diet)))
M3_8 <- update(M3, weights = varComb(varPower(form = ~Time),
                                     varIdent(form = ~1|Diet)))




## Диагностика модели



## Смотрим на предсказания модели

MyData <- expand.grid(Time = unique(bw$Time), Diet = factor(1:3))

MyData$Predicted <- predict(M3_6, newdata = MyData, level = 0)

ggplot(MyData, aes(x = Time, y = Predicted,  color = Diet)) +
  geom_line( size = 1.5) +
  geom_point(data = bw, aes(x = Time, y = weight),
             position = position_jitter())






