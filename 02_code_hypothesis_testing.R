# https://varmara.github.io/linmodr-course/02_code_hypothesis_testing.R
#' title: "Тестирование статистических гипотез"
#' author: "Марина Варфоломеева, Вадим Хайтов"

#' ## Простейший пример тестирования гипотезы

# Зерно для генератора случайных чисел для сопоставимости результатов
set.seed(456)
# Создаем две выборки по 100 из нормального распределения с разными параметрами
male <- rnorm(n = 100, mean = 130, sd = 5)
female <- rnorm(n = 100, mean = 129, sd = 5)
gender <- c(rep("M", 100), rep("F", 100))
# Сохраняем выборки в датафрейме для удобства
df_height <- data.frame(
  gender = factor(gender),
  height = c(male, female))

library(ggplot2)
ggplot(data = df_height, aes(x = height)) +
  geom_histogram(binwidth = 5, colour = "grey80")


#' ## Изменим ширину интервалов гистограммы
ggplot(df_height, aes(x = height)) +
  geom_histogram(binwidth = 2, colour = "grey40")

#' ## Изменим оформление (тему) графика

# # На один раз
ggplot(df_height, aes(x = height)) +
  geom_histogram(binwidth = 2, colour = "grey40") +
  theme_bw()

# # Или до конца сессии
theme_set(theme_bw())
ggplot(df_height, aes(x = height)) +
  geom_histogram(binwidth = 2, colour = "grey40")


#' ## Разделим гистограммы по переменной gender при помощи цвета

ggplot(df_height, aes(x = height, fill = gender)) +
  geom_histogram(binwidth = 3, colour = "grey40", position = "dodge")


#' ## При помощи логических векторов посчитаем средние по переменной gender

f_male <- df_height$gender == "M"
f_female <- !f_male
mean_m <- mean(df_height$height[f_male])
mean_f <- mean(df_height$height[f_female])

#' ## Добавим линии, обозначающие средние значения
gg_height <- ggplot(df_height, aes(x = height, fill = gender)) +
  geom_histogram(binwidth = 3, colour = "grey40", position = "dodge") +
  geom_vline(xintercept = mean_f, colour = "red", size = 1) +
  geom_vline(xintercept = mean_m, colour = "blue", size = 1, linetype = "dashed")
gg_height


#' ## Добавим подписи осей и заголовок
gg_height +
  labs(x = "Height (cm)",
       y = "Count",
       title ="Height distribution",
       fill = "Gender")

#' ## Сравним две выборки с помощью t-критерия Стьюдента
t_height <- t.test(height ~ gender, data = df_height)
t_height

#' ### Вопрос: Вероятность какого события отражает уровень значимости p=`r t_height$p.value`?

sd_m <- sd(df_height$height[f_male])
sd_f <- sd(df_height$height[f_female])

#' Сравним при помощи пермутаций две выборки, описывающие рост мальчиков и девочек (`male` и `female`)
head(male)
head(female)

d_initial <- abs(mean(male) - mean(female))

Nperm <- 10000              # число пермутаций
dperm <- rep(NA, Nperm)     # пустой вектор для результатов
set.seed(76)             # зерно для генератора случайных чисел

for (i in 1:(Nperm - 1))    # Повторяем 9999 раз
{
  BOX <- c(male, female)    # смешиваем наши вектора
  ord <- sample(x = 1:200, size = 200)   # задаем новый порядок значений
  f <- BOX[ord[1:100]]      # первые 100 перемешанных значений
  m <- BOX [ord[101:200]]   # следующие 100 перемешанных значений
  dperm[i] <- abs(mean(m) - mean(f))  # считаем пермутационную статистику
}

head(dperm)

tail(dperm)

#' Последнее 10000-е значение не заполнено!


dperm[Nperm] <- d_initial


#' Для удобства, положим данные в датафрейм и отметим значения d < d_initial
df_perm <- data.frame(d_p = dperm)
df_perm$less <- df_perm$d_p < d_initial

#' ## Получаем распределение статистики $d_{perm}$
#' ## Задание: Постройте график самостоятельно

ggplot(data = df_perm, aes(x = d_p, fill = less)) +
  geom_histogram(binwidth = 0.08, colour = "black") +
  geom_vline(xintercept = d_initial, colour = "red")


names(df_perm)

#' ## Расчитаем величину уровня значимости
p_perm <- sum(dperm >= d_initial) / Nperm
mean(dperm >= d_initial)
