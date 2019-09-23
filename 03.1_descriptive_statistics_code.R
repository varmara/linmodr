# ---
# title: "Описательные статистики"
# author: "Вадим Хайтов, Юта Тамберг, Марина Варфоломеева"
# ---

# # ЧАСТЬ 1. Медиана и квантили.  #############################

# ## Медиана
apples <- c(7.3, 9.7, 10.1,  9.5,  8.0,  9.2,  7.9,  9.1)
apples

# Чтобы увидеть медиану, мы должны ранжировать,
# или отсортировать, наш вектор по возрастанию:
sort(apples)


# В ранжированном ряду медиана расположена так,
# что слева и справа от нее находится равное число
# измерений


# Проверим себя
median(apples)



# ## Медиана устойчива к выбросам
# Создадим вектор с новым значением
apples2 <- c(apples, 68)
# Найдите медиану
sort(apples2)

median(apples2)



# Для сравнения посмотрим на среднее.
mean(apples)
mean(apples2)


# ## Квантили

# Квартили
quantile(x = apples, probs = c(0.25, 0.5, 0.75))


# 5-number summary
quantile(apples)

# Персентили
quantile(apples, probs = c(0.1, 0.99))


# ## Боксплот: 5-number summary на графике
library(ggplot2)
theme_set(theme_bw())

apple_data <- data.frame(diameter = apples)
head(apple_data)

ggplot(data = apple_data) +
  geom_boxplot(aes(x = 'Медиана \nи квантили', y = diameter))



# ## Case study: диатомовые водоросли в желудках фильтраторов. ######
# Самостоятельная работа.

# В морских сообществах встречаются два вида
# фильтраторов, один из которых любит селиться
# прямо на поверхности тела другого.
# *Tegella armifera* это вид-хозяин. Он может жить
# как сам по себе, так и вместе с симбионтом.
# *Loxosomella nordgardi* --- вид-симбионт. Он
# практически никогда не встречается в
# одиночестве.
# Данные: Юта Тамберг

# В файле `diatome_count.csv` даны количества
# диатомовых водорослей в желудках этих животных.
# 2 переменные:
# - вид (sp)
# - число водорослей в желудке (count)
# В переменной sp есть три варианта значений:
# - "host_free" (хозяин без симбионта)
# - "host_symbiont" (хозяин с симбионтом)
# - "symbiont" (симбионт)

diatoms <- read.table("data/diatome_count.csv",
                      header = TRUE, sep = "\t")


# ## Все ли правильно открылось?
head(diatoms)
str(diatoms)

# ## Есть ли пропущенные значения?
sum(! complete.cases(diatoms))

# Что это за случаи?
diatoms[! complete.cases(diatoms), ]


# ## Задание 1 --------------------------------------------

# Ваша задача рассчитать 5-number summary для
# количества диатомовых в желудках хозяев и
# симбионтов (всех трех категорий).



# ## Боксплоты в ggplot2

ggplot(data = diatoms, aes(y = count, x = sp)) + geom_boxplot()



# # ЧАСТЬ 2. Нормальное распределение - первое знакомство #####



# # ЧАСТЬ 3. Среднее и стандартное отклонение #################

# ## Центральная тенденция
# ### Среднее арифметическое

sum(apples) / length(apples)

mean(apples)

# ## Как оценить разброс значений?

# ### Девиата (отклонение)
raw_deviates <- apples - mean(apples)
raw_deviates


# ### Девиаты не годятся как мера разброса
round(sum(raw_deviates))

# ### Сумма квадратов = SS, Sum of Squares

sum(raw_deviates^2)

# ### Как усреднить отклонения от среднего значения?

# ### Дисперсия = MS, Mean Square, Variance
sum(raw_deviates^2) / (length(apples) - 1)
var(apples)

# ### Среднеквадратичное/стандартное отклонение = Standard Deviation
sqrt(sum(raw_deviates^2) / (length(apples) - 1))
sd(apples)


# ## Среднее и стандартное отклонение при помощи `stat_summary()`
ggplot(data = apple_data) +
  stat_summary(geom = 'pointrange', fun.data = mean_sdl,
               fun.args = list(mult = 1),
               aes(x = 'Среднее \nи стандартное отклонение', y = diameter))



# ## Задание 2 ------------------------------------------------
#
# Из пяти положительных чисел создайте выборку со средним = 10 и медианой = 7




# ## Как соотносятся способы оценки центра и размаха в выборке? ######
ggplot(data = apple_data) +
  geom_boxplot(aes(x = 'Медиана \nи квантили', y = apples)) +
  stat_summary(geom = 'pointrange', fun.data = mean_sdl,
               fun.args = list(mult = 1),
               aes(x = 'Среднее \nи стандартное отклонение', y = diameter))
