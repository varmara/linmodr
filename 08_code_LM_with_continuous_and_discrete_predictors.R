# title: "Линейные модели с непрерывными и дискретными предикторами"
# subtitle: Линейные модели, дисперсионный и регрессионный анализ с использованием R, осень 2015
# author: Марина Варфоломеева, Вадим Хайтов

# install.packages(c("plyr", "dplyr"))

## Пример: Клевер и тысячелистник
clover <- read.delim("clover.csv")
names(clover)
# Для удобства переименуем переменные
names(clover) <- c("Plot", "Clover", "Yarrow")

## Посмотрим внимательно на данные
nrow(clover)
# есть ли пропущенные значения?
any(is.na(clover))
# как закодирована переменная Plot?
class(clover$Plot) # это фактор
levels(clover$Plot) # уровни этого фактора
# # Если вы открывали xlsx файл при помощи read_excel,
# # то фактор вам придется сделать самостоятельно
# clover$Plot <- factor(clover$Plot)

## Сколько площадок собрали с каждого из участков?
table(clover$Plot)


## Построим график зависимости урожайности клевера от плотности тысячелистника
library(ggplot2)
theme_set(theme_bw(base_size = 14)+ theme(legend.key = element_blank()))
gg_cl <- ggplot(data = clover, aes(y = Clover, x = Yarrow)) +  geom_point() +
  labs(x = "Плотность тысячелистника, цветоносов/кв.м", y = "Урожайность клевера, г/кв.м")
gg_cl + geom_smooth(method = "lm")


## Не зная про несколько участков, мы бы просто подобрали обычную линейную регрессию
M1 <- lm(Clover ~ Yarrow, data = clover)
summary(M1)


## Запишем уравнение этой линейной регрессии
coef(M1) #Коэффициенты модели
# Clover = 57.49 - 0.11Yarrow


## Но мы знаем, что было несколько участков
gg_cl + aes(colour = Plot) + labs(colour = "Участок")

## Пробуем вручную создавать переменные-болванки
clover$Plot_B <- as.numeric(clover$Plot == "B")
clover$Plot_C <- as.numeric(clover$Plot == "C")
# Почему нам не нужна отдельная переменная-болванка для участка A?
clover[, c("Plot", "Plot_B", "Plot_C")]

## Подбираем параметры линейной модели
M2_dummy <- lm(Clover ~ Yarrow + Plot_B + Plot_C, data = clover)
### На самом деле, в ручном создании болванок нет необходимости.
M2 <- lm(Clover ~ Yarrow + Plot, data = clover)

# Значения коэффициентов получатся одинаковые
coef(M2)
coef(M2_dummy)

## Смотрим на результаты линейной модели {.smaller}
summary(M2)

## Попробуем записать уравнение модели, где появился дискретный предиктор {.smaller}
coef(M2)
# Общее уравнение модели с параллельными линиями:
# Clover = 57.58 - 0.11Yarrow + 0.32PlotB - 0.54PlotC
## Давайте запишем отдельные уравнения для каждого типа участков
# Для участка A:
# Clover = 57.58 - 0.11 Yarrow
# Для участка B:
# Clover = (57.58 + 0.32) - 0.11 Yarrow = 57.9 - 0.11 Yarrow
# Для участка C:
# Clover = (57.58 - 0.54) - 0.11 Yarrow = 57.04 - 0.11 Yarrow


## Что произойдет с моделью, если мы изменим базовый уровень?
clover$Plot <- relevel(clover$Plot, ref = "C")
levels(clover$Plot) # Теперь первым идет "C"
M2_C <- lm(Clover ~ Yarrow + Plot, data = clover)
summary(M2_C)

## Изменился ли смысл модели от смены уровней? Нет!
coef(M2_C)
# Общее уравнение модели с параллельными линиями:
# Clover = 57.03 - 0.11Yarrow + 0.54PlotA + 0.86PlotB
# Для участка A:
# Clover = (57.03 + 0.54) - 0.11 Yarrow =
# = 57.57 - 0.11 Yarrow
# Для участка B:
# Clover = (57.03 + 0.86) - 0.11 Yarrow =
# = 57.89 - 0.11 Yarrow
# Для участка C:
# Clover = 57.03 - 0.11 Yarrow


## График модели с одинаковыми углами наклона
# Решение средствами пакета `plyr`:
library(plyr)
# делим датафрейм на части по переменной Plot
# по каждой из частей считаем при помощи summarise
# последовательность от мин. до макс. значения Yarrow
my_df <- plyr::ddply(clover, .(Plot),
    summarise,
    Yarrow = seq(min(Yarrow),  max(Yarrow), length = 10))
# Считаем предсказанные значения и стандартные ошибки
M2_pred <- predict(M2, newdata = my_df, se.fit = TRUE)
# Добавляем их в датафрейм
my_df$Clover <- M2_pred$fit
my_df$SE <- M2_pred$se.fit


## Самостоятельно попробуйте разобрать, как сделать тот же самый исходный датафрейм в пакете dplyr
# Выгружаем plyr, потому что dplyr конфликтует с plyr
detach("package:plyr", unload=TRUE)
library(dplyr)
# Разбиваем clover на группы по переменной Plot
# и считаем минимум и максимум Yarrow
minmax <- clover %>% group_by(Plot) %>%
  summarise(mYarrow = min(Yarrow),
  MYarrow = max(Yarrow))
# Pазбиваем minmax по переменной Plot создаем датафрейм,
# где Yarrow - это последовательность от минимума до максимума для данного Plot,
# снимаем группировку,
# добавляем предсказанные значения и стандартные ошибки
my_df <- minmax %>% group_by(Plot) %>%
  do(data.frame(Yarrow = seq(.$mYarrow, .$MYarrow, length = 10))) %>%
  ungroup %>%
  mutate(Clover = predict(M2, newdata = .),
    SE = predict(M2, newdata = ., se.fit = TRUE)$'se.fit')


## Строим график с параллельными прямыми
gg_cl_parallel <- ggplot(data = my_df, aes(y = Clover, x = Yarrow, colour = Plot, fill = Plot)) +
  geom_line() +
  geom_ribbon(aes(ymax = Clover + 1.98 * SE,
                  ymin = Clover - 1.98 * SE),
              alpha = 0.3, colour = NA) +
    geom_point(data = clover) +
  labs(colour = "Участок", fill = "Участок",
       x = "Плотность тысячелистника, цветоносов/кв.м",
       y = "Урожайность клевера, г/кв.м")
gg_cl_parallel

## Добавим отдельные панели
gg_cl_parallel + facet_wrap(~ Plot)

## Но может быть линии на самом деле не параллельны?
### Мы должны были начать с этой, полной, модели!!!
# Для начала вернем уровни на место в прежнем порядке
clover$Plot <- factor(clover$Plot, levels = c("A", "B", "C"), labels = c("A", "B", "C"))
M3 <- lm(Clover ~ Yarrow * Plot, data = clover)
coef(M3)
# Общее уравнение модели с разными углами наклона:
  # Clover = 55.219 - 0.094Yarrow + 0.606PlotB + 8.081PlotC - 0.002Yarrow:PlotB - 0.054Yarrow:PlotC
# Для участка A:
# Clover = 55.219 - 0.094 Yarrow
# Для участка B:
# Clover = (55.219 + 0.606) + (- 0.094 - 0.002) Yarrow = 55.825 - 0.096 Yarrow
# Для участка C:
# Clover = (55.219 + 8.081) + (- 0.094 - 0.054) Yarrow = 63.3 - 0.148 Yarrow

## График с разными углами наклона
gg_cl + aes(colour = Plot, fill = Plot) +
  geom_smooth(method = "lm", alpha = 0.2) +
  labs(colour = "Участок", fill = "Участок")


## Проверяем условия применимости
library(car)
op <- par(mfrow = c(1, 3), cex = 1)
qqPlot(M3)
plot(M3, which = 3)
plot(M3, which = 4)
par(op)
library(lmtest)
bptest(M3)

## Как проверить, действительно ли различается урожайность между участками?









## После подбора финальной модели нужно еще раз проверить условия применимости












# Модель M1 - финальная
coef(M1)
# Clover = 57.49 - 0.11Yarrow


## Пример: Козы и глисты
## Читаем и знакомимся с данными
goat <- read.delim("goats.csv")
str(goat)
# переименуем переменные для краткости
colnames(goat) <- c("treat", "wt", "init")
any(is.na(goat))
table(goat$treat)

## Проверим порядок уровней фактора
levels(goat$treat)

# Хорошо бы поменять их местами для удобства интерпретации
goat$treat <- relevel(goat$treat, ref = "standard")

## Задание
# 1. Подберите модель, описывающую зависимость между увеличением веса коз и способом прфилактической обработки животных.
# 2. Проверьте условия применимости этой модели
# 3. Попробуйте сократить модель, чтобы она стала оптимальной.
# 4. Проверьте условия применимости финальной модели.

## Решение





