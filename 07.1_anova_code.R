#' ---
#' title: "Линейные модели с дискретными предикторами"
#' subtitle: "Линейные модели..."
#' author: "Марина Варфоломеева, Вадим Хайтов"

#' ## Пример: яйца кукушек
#' Данные: Latter, 1902; источник: Tippett, 1931
#' - `species`  --- вид птиц-хозяев (фактор)
#' - `length` --- длина яиц кукушек в гнездах хозяев (зависимая переменная)
library(DAAG)
data("cuckoos")
# Положим данные в переменную с коротким названием, чтобы меньше печатать
cu <- cuckoos
head(cu, 3)

sum(is.na(cu))
table(cu$species)

#' ## Изменим названия уровней фактора
levels(cu$species)
levels(cu$species) <- c("лес_зав", "луг_кон", "бел_тряс",
                        "малин", "лес_кон", "крапив")

#' ## Задание: Постройте график
#' Постройте график зависимости размера яиц кукушек от вида птиц-хозяев, в гнездах которых были обнаружены яйца (используйте `geom_boxplot`).
#' Раскрасьте график в зависимости от вида птиц-хозяев (используйте эстетики `fill` или `colour`)
#'
#' ### Дополнительное задание:
#' Попробуйте сменить палитру раскраски, используя `scale_colour_brewer` (варианты можно посмотреть в справке в подразделе примеров или в интернете [Colors (ggplot2): раздел RColorBrewer palette chart](http://www.cookbook-r.com/Graphs/Colors_(ggplot2\)/#palettes-color-brewer ))



#' ## Меняем порядок уровней
cu$species <- reorder(cu$species, cu$length, FUN = mean)
cu$species <- factor(cu$species, levels = rev(levels(cu$species)))
ggplot(data = cu, aes(x = species, y = length)) +
  geom_boxplot(aes(fill = species))


#' ## Задание:
#' - Подберите линейную модель зависимости длины яиц кукушек в гнездах от вида птиц-хозяев
#' - Проверьте условия применимости





#' ## Коэффициенты линейной модели
#' Влияет ли вид птиц-хозяев на длину яиц кукушек?
coef(summary(cmod))


#' ## Делаем дисперсионный анализ в R
cu_anova <- Anova(cmod)
cu_anova




#' # Пост хок тесты
library(multcomp)
cu_ph <- glht(cmod, linfct = mcp(species = "Tukey"))
summary(cu_ph)


#' ## Данные для графика при помощи `predict()`
MyData <- data.frame(
  species = factor(levels(cu$species),
                   levels = levels(cu$species)))
MyData <- data.frame(MyData, predict(cmod, newdata = MyData,
                                     interval = "confidence"))
MyData


#' ## Задание:
#' Создайте MyData вручную:
#' - предсказанные значения
#' - стандартные ошибки
#' - верхнюю и нижнюю границы доверительных интервалов
#'
#' ## Решение:





#' ## Точечный график
gg_points <- ggplot(data = MyData, aes(x = species, y = fit)) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.1) +
  geom_point(aes(colour = species), size = 3) +
  labs(x = "Вид хозяев", y = "Длина яиц кукушек, мм") +
  scale_fill_brewer(name = "Вид \nхозяев", palette = "Dark2") +
  scale_x_discrete(labels = c("Лесная\nзавирушка", "Лесной\nконек",
                              "Белая\nтрясогузка", "Малиновка", "Луговой\nконек", "Крапивник")) +
  theme(legend.position = "none")
gg_points

#' ## Столбчатый график
gg_bars <- ggplot(data = MyData, aes(x = species, y = fit)) +
  geom_bar(stat = "identity", aes(fill = species), width = 0.5) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.1) +
  labs(x = "Вид хозяев", y = "Длина яиц кукушек, мм") +
  scale_fill_brewer(name = "Вид \nхозяев", palette = "Dark2") +
  scale_x_discrete(labels = c("Лесная\nзавирушка", "Лесной\nконек",
                              "Белая\nтрясогузка", "Малиновка", "Луговой\nконек", "Крапивник")) +
  theme(legend.position = "none")
gg_bars

#' ## Можно привести результаты пост хок теста на столбчатом графике
gg_bars_coded <- gg_bars +
  geom_text(aes(y = 1.6,  label = c("A", "A", "AB", "AB", "B", "C")),
            colour = "white", size = 7)
gg_bars_coded
