# title: Демонстрация бутстрепа
# subtitle: "Линейные модели..."
# author: "Марина Варфоломеева"
# institute: "Кафедра Зоологии беспозвоночных, Биологический факультет, СПбГУ"

# ## Пример -- недосып
# В ночь перед нулевым днем всем испытуемым давали поспать нормальное время, а в следующие 9 ночей --- только по 3 часа. Каждый день измеряли время реакции в серии тестов.
#
# Как время реакции людей зависит от бессонницы?
# Belenky et al., 2003
# - `Reaction` --- среднее время реакции в серии тестов в день наблюдения, мс
# - `Days` --- число дней депривации сна
# - `Subject` --- номер испытуемого
library(lme4)
library(pbkrtest) # для бутстрепа
library(ggplot2) # для графиков

data(sleepstudy)
sl <- sleepstudy

# ## Тесты отношения правдоподобий (LRT)
# Другой пример и подробности о методе см. Faraway 2017, near p 2015


# LRT для фиксированных эффектов #########################################

# Протестируем значимость фиксированного эффекта Days
# Для этого нам нужно сравнить правдоподобия двух вложенных моделей
# Модель, в которой есть Days
m1 <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sl, REML = FALSE)
# Модель, в которой нет Days (соответствует нашей H_0, про отсутствие влияния Days)
m0 <- lmer(Reaction ~ (1 + Days| Subject), data = sl, REML = FALSE)

# LRT с помощью anova(test = 'Chi')
anova(m0, m1, test = 'Chi')

# LRT вручную
my_LRT <- as.numeric(2 * (logLik(m1) - logLik(m0)))
my_LRT
# 23.53654

# p-value для LRT
1 - pchisq(my_LRT, df = 1)
# [1] 1.22564e-06
# получилось ну очень маленькое значение


# Алгоритм параметрического бутстрепа ################33

# Цель: Нужно сгенерировать из данных,
# соответствующих H0 распределение нужной нам
# статистики (LRT).

# Как это делается?
# 1) Симулируем данные из модели, соответствующей
# H0 об отсутствии влияния Days
y_simulated <- simulate(m0)
# 2) На этих симулированных данных нам нужно
# протестировать влияние Days при помощи LRT.
# Подбираем модель с Days и без Days на симулированных данных:
mod_H0 <- refit(m0, newresp = y_simulated)
mod_HA <- refit(m1, newresp = y_simulated)
# Вычисляем LRT:
LRT_simulated <- as.numeric(2 * (logLik(mod_HA) - logLik(mod_H0)))
LRT_simulated
# 3) Если мы повторим 1 и 2 многократно, то сможем
# построить распределение LRT для случая, когда
# верна H0 об отсутствии влияния Days.


# Теперь, вооружившись этим знанием, мы...
# Сделаем параметрический бутстреп вручную ################
set.seed(42)
nsim <- 500 # число итераций бутстрепа
# (Нужно 1000 или больше, но для тестирования нового кода можно меньше)
LRTboot <- numeric(nsim)  # резервируем место для результатов всех итераций
# Делаем симуляцию:
for(i in 1:nsim){
  y_simulated <- unlist(simulate(m0))
  mod_H0 <- refit(m0, newresp = y_simulated)
  mod_HA <- refit(m1, newresp = y_simulated)
  LRTboot[i] <- as.numeric(2 * (logLik(mod_HA) - logLik(mod_H0)))
}

# В векторе LRTboot записаны значения LRT для всех итераций бутстрепа
length(LRTboot) # длина вектора - количество итераций (nsim)

# Теперь мы можем построить распределение значений
# LRT для случая, когда верна H0 об
# отсутствии влияния Days
ggplot(data = data.frame(LRT_H0 = LRTboot)) +
  geom_histogram(aes(x = LRT_H0))

# Это распределение мы можем использовать для тестирования гипотезы

# Уровень значимости для нашего реального значения
# my_LRT - это доля итераций бутстрепа, где
# получилось значение LRTboot > my_LRT

# Наша оценка p-value для my_LRT
p_val <- mean(LRTboot > my_LRT)
p_val

# Это можно изобразить на графике
ggplot(data = data.frame(LRT_H0 = LRTboot)) +
  geom_histogram(aes(x = LRT_H0)) +
  geom_vline(xintercept = my_LRT, size = 2, colour = 'red')


# Более эффективный способ, без циклов ##############

# Можно написать функцию, которая получает LRT в
# одной единственной симуляции
one_bootstrap_iteration <- function(m0, m1){
  y_simulated <- unlist(simulate(m0))
  mod_H0 <- refit(m0, newresp = y_simulated)
  mod_HA <- refit(m1, newresp = y_simulated)
  LRTboot <- as.numeric(2 * (logLik(mod_HA) - logLik(mod_H0)))
  return(LRTboot)
}
# Вот одна симуляция
one_bootstrap_iteration(m0, m1)

# Как и раньше мы можем многократно повторить эту
# симуляцию, но только теперь используем функцию
# replicate вместо цикла for
nsim_new <- 500  # число итераций бутстрепа (Нужно 1000 или больше)
set.seed(42)
LRTboot_new <- replicate(n = nsim_new, expr = one_bootstrap_iteration(m0, m1))

# В векторе LRTboot_new записаны значения LRT для
# всех итераций бутстрепа
length(LRTboot_new) # длина вектора - количество итераций (nsim_new)

# И т.д.
# Наша оценка p-value для my_LRT
p_val_new <- mean(LRTboot_new > my_LRT)
p_val_new

# Это можно изобразить на графике
ggplot(data = data.frame(LRT_H0 = LRTboot_new)) +
  geom_histogram(aes(x = LRT_H0)) +
  geom_vline(xintercept = my_LRT, size = 2, colour = 'red')

# Еще более эффективный способ #########################

# Параметрический бутстреп при помощи пакета pbkrtest
pmod <- PBmodcomp(m1, m0, nsim = 1000, seed = 42)
summary(pmod)

# Осторожно! Обращайте внимание на предупреждения об ошибках.
# Результаты могут быть смещенными, если модели не сошлись

