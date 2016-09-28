#' title: "Диагностика линейных моделей"
#' subtitle: "Линейные модели..."
#' author: "Марина Варфоломеева, Вадим Хайтов"
#' institute: "Кафедра Зоологии беспозвоночных, Биологический факультет, СПбГУ"


dat <- read.table('http://www4.stat.ncsu.edu/~stefanski/NSF_Supported/Hidden_Images/orly_owl_files/orly_owl_Lin_4p_5_flat.txt')

fit <- lm(V1 ~ V2 + V3 + V4 + V5 - 1, data = dat)

coef(summary(fit))

# Companion to Applied Regression
library(car)

residualPlot(fit, pch = ".")

#' ## С этим примером мы познакомились в прошлый раз: IQ и размеры мозга
#' (Willerman et al. 1991)


#' Не забудьте войти в вашу директорию для матметодов при помощи `setwd()`


# Данные можно загрузить с сайта
library(downloader)
# в рабочем каталоге создаем суб-директорию для данных
if(!dir.exists("data")) dir.create("data")
# скачиваем файл
download(
  url = "https://varmara.github.io/linmodr-course/data/IQ_brain.csv",
  destfile = "data/IQ_brain.csv")


#' ### Подберем модель
brain <- read.csv("data/IQ_brain.csv", header = TRUE)
brain_model <- lm(PIQ ~ MRINACount, data = brain)
coef(summary(brain_model))


#' # Проверка на наличие влиятельных наблюдений

#' График расстояния Кука.
plot(brain_model, which = 4)


#' Второй вариант --- построить обычный график остатков, и отметить на нем расстояния Кука.

#' Извлечем из результатов сведения для анализа остатков при помощи функции `fortify()` из пакета `{ggplot2}`
library(ggplot2)
brain_diag <- fortify(brain_model)
head(brain_diag, 2)

#' ## Задание
#'
#' Используя данные из датафрейма `brain_diag`,
#' постройте график зависимости стандартизированных остатков модели `brain_model` от предсказанных значений.
#'
#' Сделайте так, чтобы размер точек изменялся в зависимости от значения расстояния Кука.
#'
#' ## Решение



#' ## Квантильный график остатков

qqPlot(brain_model) # из пакета car

#' ## Аналогичный график при помощи `ggplot2`
mean_val <- mean(brain_diag$.stdresid)
sd_val <- sd(brain_diag$.stdresid)
ggplot(brain_diag, aes(sample = .stdresid)) + geom_point(stat = "qq") + geom_abline(intercept = mean_val, slope = sd_val)


#' Проверка на гетероскедастичность

#' Мы уже строили график остатков в `ggplot2`
ggplot(data = brain_diag,
       aes(x = .fitted, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0)

#' Можем построить аналогичный график остатков средствами пакета `car`
residualPlot(brain_model)

#' ## Задание
#'
#' Выполните три блока кода (см. код лекции).
#'
#' Какие нарушения условий применимости линейных моделей здесь наблюдаются?
#'
#' ## Задание, блок 1
#'
## ------------------------------------------------------------------------
set.seed(12345)
x1 <- seq(1, 100, 1)
y1 <-  diffinv(rnorm(99)) + rnorm(100, 0.2, 2)
dat1 = data.frame(x1, y1)
ggplot(dat1, aes(x = x1, y = y1)) + geom_point()+
  geom_smooth(method="lm", alpha = 0.7)








#' ## Задание, блок 2
#'
## ------------------------------------------------------------------------
set.seed(12345)
x2 <- runif(1000, 1, 100)
b_0 <- 100;  b_1 <- 20
h <- function(x) x^0.5
eps <- rnorm(1000, 0, h(x2))
y2 <- b_0 + b_1 * x2 + eps
dat2 <- data.frame(x2, y2)
ggplot(dat2, aes(x = x2, y = y2)) + geom_point() + geom_smooth(method = "lm")








#' ## Задание, блок 3
#'
## ------------------------------------------------------------------------
set.seed(2309587)
x3 <- rnorm(100, 50, 10)
b_0 <- 100; b_1 <- 20; eps <- rnorm(100, 0, 100)
y3 <- b_0 + b_1*x3 + eps
y3[100] <- 1000; x3[100] <- 95; y3[99] <- 1300; x3[99] <- 90; y3[98] <- 1500; x3[98] <- 80
dat3 <- data.frame(x3, y3)
ggplot(dat3, aes(x=x3, y=y3)) + geom_point() + geom_smooth(method="lm")








