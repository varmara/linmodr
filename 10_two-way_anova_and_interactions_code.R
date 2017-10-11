#' ---
#' title: "Дисперсионный анализ, часть 2"
#' subtitle: "Линейные модели..."
#' author: "Марина Варфоломеева, Вадим Хайтов"
#'
#' #' ## Многофакторный дисперсионный анализ

#' ## Пример: Удобрение и беспозвоночные
#' Влияет ли добавление азотных и фосфорных удобрений на беспозвоночных?
# Данные из Quinn, Keough, 2002
#' Небольшие искуственные субстраты экспонировали в течение разного времени в верхней части сублиторали (Hall et al., 2000).
#' Зависимая переменная:
#' - `richness` --- Число видов
#' Факторы:
#' - `time` --- срок экспозиции (2, 4 и 6 месяцев)
#' - `treat` --- удобрения (добавляли или нет)

fert <- read.csv(file="data/hall.csv")
str(fert)

colnames(fert) <- tolower(colnames(fert))

fert$time <- factor(fert$time)
levels(fert$time)


sum(is.na(fert))
sapply(fert, function(x)sum(is.na(x)))

table(fert$time, fert$treat)

library(ggplot2)
theme_set(theme_bw(base_size = 18) + theme(legend.key = element_blank()))
gg_rich <- ggplot(data = fert, aes(x = time, y = richness, colour = treat)) +
  geom_boxplot()
gg_rich



fert$log_rich <- log10(fert$richness + 1)


#' # Многофакторный дисперсионный анализ в R

#' ## Дисперсионный анализ со II типом сумм квадратов
fmod2 <- lm(log_rich ~ treat * time, data = fert)
library(car)
Anova(fmod2, type = "II")

#' ## Задание
#' Проверьте условия применимости дисперсионного анализа


#' ## Результаты дисперсионного анализа
Anova(fmod2, type = 2)

#' ## Дисперсионный анализ c III типом сумм квадратов
fmod3 <- lm(log_rich ~ treat * time, data = fert, contrasts = list(treat = contr.sum, time = contr.sum))
Anova(fmod3, type = 3)

#' ## Пост хок тест для взаимодействия факторов
fert$treat_time <- interaction(fert$treat, fert$time)
fit_inter <- lm(log_rich ~ treat_time - 1, data = fert)
library(multcomp)
dat_tukey <- glht(fit_inter, linfct = mcp(treat_time = "Tukey"))
summary(dat_tukey)

#' ## Данные для графика при помощи `predict()`
MyData <- expand.grid(treat = levels(fert$treat),
                      time = levels(fert$time))
MyData <- data.frame(MyData, predict(fmod2, newdata = MyData,
                                     interval = "confidence"))
# Обратная трансформация
MyData$richness <- 10^MyData$fit
MyData$LWR <- 10^MyData$lwr
MyData$UPR <- 10^MyData$upr
MyData

#' ## Задание:
#' Создайте MyData вручную:
#' - предсказанные значения
#' - стандартные ошибки
#' - верхнюю и нижнюю границы доверительных интервалов





#' ## Графики для результатов: Столбчатый график
pos <- position_dodge(width = 0.9)
gg_barp <- ggplot(data = MyData, aes(x = time, y = richness,
            ymin = LWR,  ymax = UPR, fill = treat)) +
  geom_bar(stat = "identity", position = pos) +
  geom_errorbar(width = 0.1, position = pos)
gg_barp



#' ## Графики для результатов: Линии с точками
gg_linep <- ggplot(data = MyData, aes(x = time, y = richness,
              ymin = LWR,  ymax = UPR, colour = treat)) +
  geom_point(size = 3, position = pos) +
  geom_line(aes(group = treat), position = pos) +
  geom_errorbar(width = 0.1, position = pos)
gg_linep


#' ## Приводим понравившийся график в приличный вид
gg_final <- gg_linep + labs(x = "Экспозиция",  y = "Число видов") +
  scale_colour_brewer(name = "", palette = "Dark2",
    labels = c("Контроль", "Эксперимент"))
gg_final

