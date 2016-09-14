# title: Анализ мощности
# subtitle: Математические методы в зоологии на R, осень 2015
# author: Марина Варфоломеева

# Инсталлируем пакеты, которыми будем сегодня пользоваться
install.packages(c("pwr", "effsize", "readxl"))

## Величина эффекта из общих соображений
# сильные, умеренные и слабые эффекты (Cohen, 1982)
library(pwr)
cohen.ES(test = "t", size = "large")

## Рассчитайте
# величину умеренных и слабых эффектов для t-критерия


## Величина эффекта из пилотных данных
alpha <- 0.05
power <- 0.80
sigma <- 27.7 # варьирование плотности халиотисов
diff <- 23.2 # ожидаемые различия плотности халиотисов
effect <- diff/sigma # величина эффекта
effect

## Считаем объем выборки
pwr.t.test(n = NULL, d = effect, power = power, sig.level = alpha, type = "two.sample", alternative = "two.sided")

## Рассчитайте
# сколько нужно обследовать мест, чтобы обнаружить слабый эффект с вероятностью 0.8, при уровне значимости 0.01



## Пример: Поддержание равновесия
# http://lib.stat.cmu.edu/DASL/Stories/MaintainingBalance.html

## Читаем данные из файла
# Не забудте войти в вашу директорию для матметодов
library(readxl)
bal <- read_excel(path = "balance.xlsx", sheet = 1)

str(bal) # Структура данных

## Вспомним, как обращаться с датафреймами
# Первые несколько строк файла


# Первые три значения переменной side_side


# 12-14 строки, 1, 2 и 4 столбцы


## Сделаем age_group фактором
bal$age_group <- factor(bal$age_group, levels = c("young", "elderly"), labels = c("молодые", "пожилые"))
# Посмотреть, что получилось, можно так
levels(bal$age_group)


## Бокс-плот
library(ggplot2)
ggplot(data = bal, aes(x = age_group, y = forward_backward)) +
  geom_boxplot()

# Задание: Установите до конца сеанса другую тему


## Раскрашиваем график

# эстетика `fill` - заливка
ggplot(data = bal, aes(x = age_group, y = forward_backward, fill = age_group)) + geom_boxplot()

# эстетика `colour` - контуры
ggplot(data = bal, aes(x = age_group, y = forward_backward, colour = age_group)) + geom_boxplot()

# ## Задание
#
# Добавьте
# - подписи осей
# - название графика
# - название легенды
#
# Сохраните график в переменной


# A priory анализ мощности
## Представим, что перед нами данные пилотного исследования
# Мы хотим сравнить возрастные группы

## Величина эффекта по исходным данным
library(effsize)
effect <- cohen.d(bal$forward_backward, bal$age_group)
effect

## Как добыть значение величины эффекта?

### 1. Как называется в структуре объекта элемент, где записана величина эффекта?
str(effect)
### 2. Обращаемся к этому элементу по имени через `$`
effect$estimate
### 3. Вычислим модуль, поскольку для pwr.t.test() эффект должен быть положительным
effect <- abs(effect$estimate)

# ## Рассчитайте
# объем выборки, чтобы показать различия между группами с вероятностью 0.8?
# Вам понадобится функция `pwr.t.test`




# Post hoc анализ мощности

## Пример: Влияние витамина C на рост зубов у морских свинок
# фрагмент данных из McNeil, D. R. (1977) Interactive Data Analysis. New York: Wiley

## Открываем данные
teeth <- read_excel("teeth.xlsx", sheet = 1)
str(teeth)


# ## Задание
#
# Проверьте при помощи t-критерия будет ли различаться размер зубов при разных способах употребления



# ## Post hoc анализ
effect_real <- cohen.d(teeth$len, teeth$supp)
effect_real <- abs(effect_real$estimate)
pwr.t.test(n = 10, d = effect_real,
           power = NULL, sig.level = 0.01,
           type = "two.sample",
           alternative = "two.sided")
