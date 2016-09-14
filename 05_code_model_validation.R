# Введение в регрессионный анализ. Часть 2.
# Проверка валидности моделей
# Вадим Хайтов
# Осень 2015
---------------------------------

  # Вам понадобятся следующие пакеты: ggplot2, car, lmtest, gvlma
  # install.packages(c("car", "lmtest", "gvlma"))


  # Еще раз читаем данные про зависимость IQ от размера головного мозга

  brain <- read.csv("IQ_brain.csv", header=TRUE)

  # Еще раз строим модель

  brain_model <- lm(PIQ ~ MRINACount, data = brain)


#   library(car)
#   # Проводим data exploration
#   scatterplotMatrix(brain[, c("PIQ", "MRINACount")], spread=FALSE)
#   scatterplotMatrix(brain, spread=FALSE)

  # Экстрагируем результаты подгонки модели в отдельный датафрейм
  library(ggplot2)
  brain_diag <- fortify(brain_model)
  head(brain_diag, 2)

  # Здесь будет Ваш код для построения Residual plot для модели brain_model





  # Проверяем, нет ли в данных автокорреляции с помощью теста Дарбина-Уотсона

  durbinWatsonTest(brain_model)

  # Провряем brain_model на нормальность распределения остатков
  library(car)
  qqPlot(brain_model)

  # Все то же самое с использоваением возможностей `ggplot`

  mean_val <- mean(brain_diag$.stdresid)

  sd_val <- sd(brain_diag$.stdresid)

  ggplot(brain_diag, aes(sample = .stdresid)) +
    geom_point(stat = "qq") +
    geom_abline(intercept = mean_val, slope = sd_val)

  # Проверяем модель brain_model на гетроскедастичность

  library(lmtest)

  bptest(brain_model)

  # Проверка соответствия всем assumptions

  library(gvlma)
  gvlma(brain_model)
  plot(gvlma(brain_model))



  # Код задания №1
  library(ggplot2)
  set.seed(12345)
  x1 <- seq(1, 100, 1)
  y1 <-  diffinv(rnorm(99)) + rnorm(100, 0.2, 2)
  dat1 = data.frame(x1, y1)
  ggplot(dat1, aes(x=x1, y=y1)) + geom_point()+ geom_smooth(method="lm")


  # Код задания №2
  set.seed(12345)
  x2 <- runif(1000,1, 100)
  b_0 <- 100
  b_1 <- 20
  h <- function(x) 0.1* x
  eps <- rnorm(1000, 0, h(x2))
  y2 <- b_0 + b_1*x2 + eps
  dat2 <- data.frame(x2, y2)
  ggplot(dat2, aes(x=x2, y=y2)) + geom_point() + geom_smooth(method="lm")



  # Код задания №3
  x3 <- rnorm(100, 50, 10)
  b_0 <- 100
  b_1 <- 20
  eps <- rnorm(100, 0, 100)
  y3 <- b_0 + b_1*x3 + eps
  y3[100] <- 1000
  x3[100] <- 150
  y3[99] <- 1500
  x3[99] <- 120
  y3[98] <- 1800
  x3[98] <- 100
  dat3 <- data.frame(x3, y3)
  ggplot(dat3, aes(x=x3, y=y3)) + geom_point() + geom_smooth(method="lm")




# Бонуc
  # Скачайте датасетб выполнив следующий код

fun <- read.table('http://www4.stat.ncsu.edu/~stefanski/NSF_Supported/Hidden_Images/orly_owl_files/orly_owl_Lin_4p_5_flat.txt')

# Постройте линейную модель, в которой V1 будет зависмая переменная, а все остальные переменные будут независмыми предикторами (без взаимодействия и без свободного члена)

fit <- lm(V1 ~ . - 1, data = fun)

# Значимо ли влияние предикторов на зависимую переменную?
# Можно ди предсказать поведение V1, зная значения других переменных?

# А теперь проверьте валидность этой модели, построив график рассеяния остатков :)

  plot(predict(fit), resid(fit), pch = '.')

