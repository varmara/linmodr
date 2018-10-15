# title: "Описание, проверка значимости линейных моделей"
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

# ## Тестирование гипотез с помощью t-критерия  ####
summary(brain_model)


# ## Тестирование гипотез при помощи F-критерия ####
summary(brain_model)


# ## Оценка качества подгонки модели ####
# Коэффициент детерминации
summary(brain_model)
