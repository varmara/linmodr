# title       : Дисперсионный анализ, часть 2
# subtitle    : Линейные модели, осень 2015
# author: Марина Варфоломеева, Вадим Хайтов

## Пример: Удобрение и беспозвоночные
# Влияет ли добавление азотных и фосфорных удобрений на беспозвоночных? (Hall et al., 2000).
# Факторы:
# - срок экспозиции (2, 4 и 6 месяцев)
# - удобрения (добавляли или нет)
# Планировали сделать 5 повторностей для каждого сочетания факторов
# Зависимая переменная:
# - Число видов


## Знакомимся с данными
dat <- read.csv(file="hall.csv")
str(dat)
# Для удобства названия переменных маленькими буквами
colnames(dat) <- tolower(colnames(dat))
# Время делаем фактором
dat$time <- factor(dat$time)
levels(dat$time)

## Пропущенные значения
sum(is.na(dat))
sapply(dat, function(x)sum(is.na(x)))

## Объемы выборок в группах
table(dat$time, dat$treat)

##   Задание:
# Постройте боксплот





## Преобразовываем данные
dat$log_rich <- log10(dat$richness + 1)

## Линейная модель
# Внимание: при использовании III типа сумм квадратов, нужно __обязательно указывать тип контрастов для факторов__ (`contrasts=list(фактор_1 = contr.sum, фактор_2=contr.sum)`).

fit <- lm(log_rich ~ treat * time, data = dat, contrasts = list(treat = contr.sum, time = contr.sum))


## Задание: Проверьте условия применимости дисперсионного анализа
# - Есть ли гомогенность дисперсий?
# - Не видно ли трендов в остатках?
# - Нормальное ли у остатков распределение?



## Результаты дисперсионного анализа
Anova(fit, type = 3)
```



## Пост хок тест

# 1. создаем переменную-взаимодействие
dat$treat_time <- interaction(dat$treat, dat$time)

# 2. подбираем модель без intercept
fit_inter <- lm(log_rich ~ treat_time - 1, data = dat)

# 3. делаем пост хок тест
library(multcomp)
dat_tukey <- glht(fit_inter, linfct = mcp(treat_time = "Tukey"))
summary(dat_tukey)



## Данные для графиков
library(dplyr)
dat_summary <- dat %>%
  group_by(treat, time) %>%
  summarise(
    .mean = mean(richness),
    .sd = sd(richness),
    .n = sum(!is.na(richness)),
    .se = .sd/sqrt(.n),
    lwr = .mean - qnorm(0.975) * .se,
    upr = .mean + qnorm(0.975) * .se)
dat_summary

## Графики для результатов: Столбчатый график

pos <- position_dodge(width = 0.9)
gg_barp <- ggplot(data = dat_summary, aes(x = treat, y = .mean, ymin = lwr,  ymax = upr, fill = time)) +
  geom_bar(stat = "identity", position = pos) +
  geom_errorbar(width = 0.1, position = pos)
gg_barp


## Графики для результатов: Линии с точками

gg_linep <- ggplot(data = dat_summary, aes(x = treat, y = .mean, ymin = lwr,  ymax = upr, colour = time)) +
  geom_point(size = 3, position = pos) +
  geom_line(aes(group = time), position = pos) +
  geom_errorbar(width = 0.1, position = pos)
gg_linep

# Задание:
# расположите графики рядом





gg_final <- gg_linep + labs(x = "Эксперимент",  y = "Число видов") +
  scale_x_discrete(labels = c("Контроль", "Удобрения")) +
  scale_colour_brewer(name = "Экспозиция", palette = "Dark2", labels = c("2 месяца", "4 месяца", "6 месяцев"))
gg_final
```

# Регрессия в матричном виде
# ## Задание:
# В примере с влиянием удобрений на макрофауну:
# - Какова будет размерность матрицы независимых переменных (сколько в ней строк и столбцов)?
# - Сколько значений будет в матрице коэффициентов? Что они будут означать?
# - Какова размерность матрицы ошибок?






# Вручную получаем предсказанные значения и строим доверительные интервалы

## Матрица независимых переменных
X <- model.matrix(fit)
X

## Матрица коэффициентов
betas <- coef(fit)
betas

## Предсказанные значения
Pred <- X %*% betas
# значения совпадают с вычисленными автоматически
cbind(Pred, fitted(fit))

## Стандартные ошибки предсказанных значений
# Стандартные ошибки предсказанных значений это корень из диагональных элементов матрицы X * cov(betas) * t(X)
SE <- sqrt(diag(X %*% vcov(fit) %*% t(X)))
# совпадают с вычисленными автоматически
cbind(SE, predict(fit, se.fit = TRUE)$se)

## 95% доверительный интервал
Upr <- Pred + qnorm(0.975) * SE
Lwr <- Pred - qnorm(0.975) * SE
cbind(Lwr, Upr, predict(fit, interval = "confidence"))
