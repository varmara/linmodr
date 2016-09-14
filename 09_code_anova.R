# title: "Линейные модели с дискретными предикторами"
# subtitle: Линейные модели, дисперсионный и регрессионный анализ с использованием R, осень 2015
# author: Марина Варфоломеева, Вадим Хайтов

## Пример: яйца кукушек
library(DAAG)
data("cuckoos")
# Положим данные в переменную с коротким названием, чтобы меньше печатать
cu <- cuckoos
head(cu, 3)

## Исследуем данные
any(is.na(cu))
sum(is.na(cu))

dim(cu)
table(cu$species)
summary(cu)

str(cu)
is.factor(cu$species)
levels(cu$species)

length(unique(cu$species))

## Изменим названия уровней фактора, чтобы было легче понять о каких птицах речь
levels(cu$species)
levels(cu$species) <- c("лес_зав", "луг_кон", "бел_тряс", "малин", "лес_кон", "крапив")


# ## Задание:
# 1) Постройте график зависимости размера яиц кукушек от вида птиц-хозяев, в гнездах которых были обнаружены яйца. Какой геом лучше подойдет для изображения (`geom_point`, `geom_boxplot`)?
# 2) Раскрасьте график в зависимости от вида птиц-хозяев (используйте эстетики `fill` или `colour` - чем отличаются результаты?)






# ### 3) Дополнительное задание:
# Попробуйте сменить палитру раскраски, используя `scale_colour_brewer` (варианты можно посмотреть в справке в подразделе примеров или в интернете [Colors (ggplot2): раздел RColorBrewer palette chart](http://www.cookbook-r.com/Graphs/Colors_(ggplot2\)/#palettes-color-brewer ))






# ## Хорошо бы поменять порядок уровней
# (по убыванию средней длины яиц):
# 1) при помощи функции reorder() упорядочиваем по возрастанию средней длины яиц
cu$species <- reorder(cu$species, cu$length, FUN = mean)
# 2) меняем порядок уровней на противоположный.
cu$species <- factor(cu$species, levels = rev(levels(cu$species)))


library(ggplot2)
## График с новым порядком уровней
# придется полностью обновить график (т.к.`ggplot()` хранит данные внутри графика).

gg_box <- ggplot(data = cu, aes(x = species, y = length)) +
  geom_boxplot(aes(fill = species))
gg_box

## Понравившийся график, если понадобится, можно в любой момент довести до ума, а остальные удалить
gg_box + labs(x = "Вид хозяев", y = "Длина яиц кукушек, мм") +
  # Другая палитра заливки
  scale_fill_brewer(name = "Вид \nхозяев", palette = "Dark2") +
  # Названия видов
  scale_x_discrete(labels = c("Лесная\nзавирушка", "Лесной\nконек",
"Белая\nтрясогузка", "Малиновка", "Луговой\nконек", "Крапивник")) +
  # Положение легенды, формат подписей по оси х
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")


## Задание:
# - Сколько переменных-болванок нужно, чтобы записать модель зависимости длины яиц кукушек от вида птиц-хозяев?
# - Подберите линейную модель зависимости длины яиц кукушек в гнездах от вида птиц-хозяев
# - Проверьте условия применимости дисперсионного анализа




## Коэффициенты линейной модели
summary(mod)


## Делаем дисперсионный анализ в R
# `Anova()` из пакета `car`
cu_anova <- Anova(mod)
cu_anova

## Как понять, какие именно группы различаются?

## Пост-хок тест Тьюки в R
# - `glht()` - "general linear hypotheses testing"
# - `linfct` - аргумент, задающий гипотезу для тестирования
# - `mcp()` - функция, чтобы задавать множественные сравнения (обычные пост-хоки)
# - `species` = "Tukey" - тест Тьюки по фактору `species`
library(multcomp)
cu_ph <- glht(mod, linfct = mcp(species = "Tukey"))


## Результаты попарных сравнений (тест Тьюки)
summary(cu_ph)

## Таблица с описательной статистикой по группам Пакет `dplyr`:
# - `%>%` оператор, перенаправляет значение слева в функцию справа в качестве исходных данных
# - `group_by` дальнейшие вычисления идут по группам, заданным ее аргументами
# - `summarise` описывает группу строк значениями, вычисленным по формуле или нескольким
library(dplyr)
cu_summary <- cu %>%
  group_by(species) %>%
  summarise(.n = n(),
            .mean = mean(length),
            .sd = sd(length))
cu_summary

## Задание:
# Дополните код, чтобы одновременно рассчитывать доверительные интервалы к средним значениям (.mean +/- qnorm(0.025) * .se)





## Столбчатый график можно использовать для представления результатов
gg_means <- ggplot(cu_summary, aes(x = species, y = .mean)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.5) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
  labs(x = "Вид птиц-хозяев", y = "Длина яиц кукушек, мм")
gg_means



## Можно привести результаты пост-хок теста на столбчатом графике
# Достоверно различающиеся по пост-хок тесту группы обозначим разными буквами
gg_means_coded <- gg_means +
  geom_text(aes(y = 1.6,  label = c("A", "A", "AB", "AB", "B", "C")), colour = "white", size = 5)
gg_means_coded
```


## Если не нравится, как висят столбцы,<br />можно настроить развертку оси $y$
gg_means_coded + scale_y_continuous(expand = c(0, 0), limits = c(0, max(cu$length) + 1))


## И наконец, можно переименовать уровни фактора species прямо внутри графика
gg_means_coded +
  scale_y_continuous(expand = c(0,0), limits = c(0, max(cu$length) + 1)) +
  scale_x_discrete(labels = c("Лесная\nзавирушка", "Лесной\nконек",
"Белая\nтрясогузка", "Малиновка", "Луговой\nконек", "Крапивник"))



## Сохраняем таблицу дисперсионного анализа в файл одним из нескольких способов
# 1) в csv
write.table(file = "cuckoos_res.csv", x = cu_anova, sep = "\t")

# 2) в xls или xlsx с помощью XLConnect
# library(XLConnect)
# writeWorksheetToFile(data = cu_anova, file = "cuckoos_res.xls",
# sheet = "anova_table")

# 3) отправляем в буфер обмена (только Windows) для вставки в Word-Excel
write.table(file = "clipboard", x = cu_anova, sep = "\t")

# После этого, таблицу нужно отредактировать вручную
