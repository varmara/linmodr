# title: "Проверка валидности линейных моделей"
# author: Марина Варфоломеева, Вадим Хайтов

# ## Пример: IQ и размеры мозга ##################
# Зависит ли уровень интеллекта от размера головного мозга? (Willerman et al. 1991)
# Было исследовано 20 девушек и 20 молодых людей
# - вес
# - рост
# - размер головного мозга (количество пикселей на изображении ЯМР сканера)
# - Уровень интеллекта измеряли с помощью IQ тестов
# Пример: Willerman, L., Schultz, R., Rutledge, J. N., and Bigler, E. (1991), "In Vivo Brain Size and Intelligence", Intelligence, 15, p.223--228.

# ## Вспомним, на чем мы остановились ############

library(readxl)
brain <- read.csv("data/IQ_brain.csv", header = TRUE)

brain_model <- lm(PIQ ~ MRINACount, data = brain)
summary(brain_model)

library(ggplot2)
theme_set(theme_bw())
ggplot(brain, aes(x = MRINACount, y = PIQ)) +
  geom_point() +
  geom_smooth(method = "lm")

# ## Зависит ли IQ от размера головного мозга?

# ## Тестирование гипотез ####
summary(brain_model)

# ## Оценка качества подгонки модели ####
summary(brain_model)


#### Диагностика линейных моделей ################

dat <- read.table('data/orly_owl_Lin_4p_5_flat.txt')

fit <- lm(V1 ~ V2 + V3 + V4 + V5 - 1, data = dat)

coef(summary(fit))


# ## Задание 1 --------------------------------------

# Постройте график зависимости остатков от
# предсказанных значений при помощи этого кода

library(car)
residualPlot(fit, pch = ".")



#
# http://www4.stat.ncsu.edu/~stefanski/NSF\_Supported/Hidden\_Images/stat\_res\_plots.html
#



# ## Данные для анализа остатков
library(ggplot2)
brain_diag <- fortify(brain_model)
head(brain_diag, 2)

# График расстояния Кука
ggplot(brain_diag, aes(x = 1:nrow(brain_diag), y = .cooksd)) +
  geom_bar(stat = "identity")

# График остатков от предсказанных значений
gg_resid <- ggplot(data = brain_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() +
  geom_hline(yintercept = 0)
gg_resid

# Графики зависимости остатков от предикторов в модели
ggplot(data = brain_diag, aes(x = MRINACount, y = .stdresid)) +
  geom_point() +
  geom_hline(yintercept = 0)

## # То же самое с использованием ранее созданного gg_resid
## gg_resid + aes(x =  MRINACount)

# Графики зависимости остатков от предикторов не в модели
# В данном случае их нет

# Квантильный график остатков
qqPlot(brain_model) # из пакета car

# Проверка на гетероскедастичность
# Этот график у нас уже есть
gg_resid


# Тренинг по анализу остатков --------------------

# Задание 2 ----------------------------------------
#
# Выполните три блока кода
#
# Какие нарушения условий применимости линейных моделей здесь наблюдаются?
#
# Вам понадобятся
#
# 1. График расстояния Кука
# 2. График остатков от предсказанных значений
# 3. Графики остатков от предикторов в модели и не в модели
# 4. Квантильный график остатков


# ## Задание, блок 1 -----------------------------
set.seed(90829)
x1 <- seq(1, 100, 1)
y1 <-  diffinv(rnorm(99))  + rnorm(100, 0.2, 4)
dat1 = data.frame(x1, y1)
ggplot(dat1, aes(x = x1, y =  y1)) + geom_point()+
  geom_smooth(method="lm", alpha = 0.7)


# ## Задание, блок 2 -----------------------------
set.seed(7657674)
x2 <- runif(1000, 1, 100)
b_0 <- 100;  b_1 <- -20
h <- function(x) x^0.5
eps <- rnorm(1000, 0, h(x2))
y2 <- b_0 + b_1 * x2 + eps
dat2 <- data.frame(x2, y2)
ggplot(dat2, aes(x = x2, y = y2)) + geom_point() + geom_smooth(method = "lm")


# ## Задание, блок 3 -----------------------------
set.seed(9283)
x3 <- rnorm(25, 50, 10)
b_0 <- 20; b_1 <- 20; eps <- rnorm(50, 0, 100)
y3 <- b_0 + b_1*x3 + eps
y3[100] <- 1000; x3[100] <- 95; y3[99] <- 1300; x3[99] <- 90; y3[98] <- 1500; x3[98] <- 80
dat3 <- data.frame(x3, y3)
ggplot(dat3, aes(x=x3, y=y3)) + geom_point() + geom_smooth(method="lm")

