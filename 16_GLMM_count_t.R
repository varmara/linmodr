# title       : Смешаные линейные модели для счетных данных
# subtitle    : Линейные модели, осень 2015
# author: Марина Варфоломеева, Вадим Хайтов

## Пример:
# Данные из Roulin & Bersier 2007, пример из кн. Zuur et al., 2007
# Будем моделировать зависимость числа криков совят (SiblingNegotiation) от многих факторов
# - `FoodTreatment` - тритмент (сытые или голодные)
# - `SexParent` - пол родителя
# - `FoodTreatment x SexParent`
# - `ArrivalTime` - время прибытия родителя
# - `ArrivalTime х SexParent`
# - `Nest` - гнездо

## Загружаем пакеты и данные
library(downloader) # для загрузки файлов из интернета
library(ggplot2)
theme_set(theme_bw(base_size = 14) + theme(legend.key = element_blank()))
update_geom_defaults("point", list(shape = 19))
library(gridExtra)

# Загружаем данные из интернета и сохраняем в файл, если его еще нет
url <- "https://raw.githubusercontent.com/varmara/linmodr/master/16-glmm-pois-negbin/Owls_Roulin_Bersier_2007.csv"
filename <- "Owls_Roulin_Bersier_2007.csv"
if (!file.exists(filename)) download(url, filename)

## Знакомство с данными
Owls <- read.delim("Owls_Roulin_Bersier_2007.csv")
str(Owls)
# SiblingNegotiation - число криков совят - заменим на более короткое название
Owls$NCalls <- Owls$SiblingNegotiation

##Есть ли наблюдения-выбросы? строим dot-plot
dotplot <- ggplot(Owls, aes(y = 1:nrow(Owls))) + geom_point(colour = "steelblue")

grid.arrange(dotplot + aes(x = NCalls),
  dotplot + aes(x = ArrivalTime), nrow = 1)

## Различаются ли гнезда?
ggplot(Owls, aes(x = Nest, y = NCalls)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Сколько наблюдений в каждом гнезде?
table(Owls$Nest)

## Как распределен отклик?
ggplot(Owls, aes(x = NCalls)) + geom_histogram(binwidth = 1, fill = "steelblue", colour = "black")

## Сколько нулей?
sum(Owls$NCalls == 0)/nrow(Owls)

## Какого размера выводки в гнездах?
range(Owls$BroodSize)
library(dplyr)
Owls %>% group_by(Nest) %>% summarise(BroodSize = mean(BroodSize)) %>%
  ggplot(., aes(y = BroodSize, x = Nest)) + geom_bar(stat = "identity", fill = "steelblue") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Боксплоты для дискретных факторов
ggplot(Owls, aes(y = NCalls, x = FoodTreatment, fill = SexParent)) + geom_boxplot()

ggplot(Owls) + stat_summary(aes(x = FoodTreatment, y = NCalls, colour = SexParent), fun.data = "mean_cl_boot", position = position_dodge(width = 0.5))

## Задание: Постройте график для будущей модели

ggplot(Owls) + geom_segment(aes(x = ArrivalTime, y = 0, xend = ArrivalTime, yend = NCalls, colour = FoodTreatment)) + facet_grid(FoodTreatment + SexParent ~ Nest) + ylab("NCall") + theme(legend.position = "bottom")

## Колинеарность
M0 <- lm(NCalls ~ SexParent + FoodTreatment + ArrivalTime, data = Owls)
library(car)
vif(M0)

## Подберем линейную модель
library(lme4)
M1 <- glmer(NCalls ~ SexParent * FoodTreatment + SexParent * ArrivalTime + offset(logBroodSize) + (1 | Nest), family = "poisson", data = Owls)

Owls$ArrivalTime_std <- (Owls$ArrivalTime - mean(Owls$ArrivalTime)) / sd(Owls$ArrivalTime)

M1 <- glmer(NCalls ~ SexParent * FoodTreatment + SexParent * ArrivalTime_std + offset(logBroodSize) + (1 | Nest), family = "poisson", data = Owls)


## Задание:
# Проверьте модель M1 на избыточность дисперсии

## Избыточность дисперсии (Overdispersion)
R_M1 <- resid(M1, type = "pearson") # Пирсоновские остатки
N <- nrow(Owls) # Объем выборки
p <- length(fixef(M1)) + 1   # Число параметров в модели (сигма для гнезда)
df <- (N - p) # число степенейсвободы
fi <- sum(R_M1^2) /df  #Величина fi показывает во сколько раз в среднем sigma > mu для данной модели
fi


## Почему здесь могла быть избыточность дисперсии?




## Диагностика модели
plot(M1)

## Более удобный график остатков
M1_diag <- fortify(M1)
M1_diag$.rfitted <- predict(M1, type = "response")

gg_resid <- ggplot(M1_diag, aes(x = .rfitted, y = .scresid, colour = FoodTreatment)) + geom_point() + facet_wrap(~ FoodTreatment + SexParent)
gg_resid

## Есть ли еще какие-то паттерны в остатках?

# gg_resid %+% aes(x = ArrivalTime)
gg_resid %+% aes(x = ArrivalTime) + geom_smooth()

## Проверяем, есть ли нелинейный паттерн в остатках
library(mgcv)
nonlin1 <- gam(.scresid ~ s(ArrivalTime), data = M1_diag)
summary(nonlin1)
plot(nonlin1)
abline(h = 0, lty = 2)

## У нас была сверхдисперсия. Нужно NB GAMM
M2 <- glmer.nb(NCalls ~ SexParent * FoodTreatment + SexParent * ArrivalTime_std + offset(logBroodSize) + (1 | Nest), data = Owls)
# # Если эта модель вдруг не сходится, есть обходной маневр. Можно попробовать заранее определить Theta при помощи внутренней функции
# th <- lme4:::est_theta(M1)
# M2 <- update(M1, family = negative.binomial(theta=th))


# ## Задание:
# Проверьте модель с отрицательным биномиальным распределением отклика
# - на избыточность дисперсии
# - наличие паттернов в остатках
# - нелинейность паттернов в остатках

## Избыточность дисперсии (Overdispersion)
R_M2 <- resid(M2, type = "pearson") # Пирсоновские остатки
N <- nrow(Owls) # Объем выборки
p <- length(fixef(M2)) + 1 + 1  # Число параметров в модели (тета и сигма для гнезда)
df <- (N - p) # число степенейсвободы
fi <- sum(R_M2^2) /df  #Величина fi показывает во сколько раз в среднем sigma > mu для данной модели
fi

## Диагностика отр. биномиальной модели
M2_diag <- fortify(M2)
M2_diag$.rfitted <- predict(M2, type = "response")
gg_resid <- ggplot(M2_diag, aes(x = .rfitted, y = .scresid, colour = FoodTreatment)) + geom_point() + facet_wrap(~FoodTreatment + SexParent)
gg_resid

## Есть ли еще какие-то паттерны в остатках?
# gg_resid %+% aes(x = ArrivalTime)
gg_resid %+% aes(x = ArrivalTime) + geom_smooth()

## Проверяем, есть ли нелинейные паттерны {.smaller .columns-2}
nonlin2 <- gam(.scresid ~ s(ArrivalTime), data = M2_diag)
summary(nonlin2)
plot(nonlin2)
abline(h = 0)


## Подбор оптимальной модели
summary(M2)

drop1(M2, test = "Chi")

M3 <- update(M2, .~.-SexParent:ArrivalTime_std)
drop1(M3, test = "Chisq")

M4 <- update(M3, .~.-SexParent:FoodTreatment)
drop1(M4, test = "Chisq")

M5 <- update(M4, .~.-SexParent)
drop1(M5, test = "Chisq")

## Второй способ подбора оптимальной модели - AIC
AIC(M2, M3, M4, M5)


## Модель изменилась. Нужно повторить диагностику
Избыточность дисперсии (Overdispersion)
R_M5 <- resid(M5, type = "pearson") # Пирсоновские остатки
N <- nrow(Owls) # Объем выборки
p <- length(fixef(M5)) + 1 + 1  # Число параметров в модели (тета и сигма для гнезда)
df <- (N - p) # число степенейсвободы
fi <- sum(R_M5^2) /df  #Величина fi показывает во сколько раз в среднем sigma > mu для данной модели
fi

## Диагностика отр. биномиальной модели
M5_diag <- fortify(M5)
M5_diag$.rfitted <- predict(M5, type = "response")
gg_resid <- ggplot(M5_diag, aes(x = .rfitted, y = .scresid, colour = FoodTreatment)) + geom_point() + facet_wrap(~FoodTreatment + SexParent)
gg_resid

## Есть ли еще какие-то паттерны в остатках?
# gg_resid %+% aes(x = ArrivalTime)
gg_resid %+% aes(x = ArrivalTime) + geom_smooth()


## Проверяем, есть ли нелинейные паттерны
nonlin5 <- gam(.scresid ~ s(ArrivalTime), data = M5_diag)
summary(nonlin5)
plot(nonlin5)
abline(h = 0)

## Готовим данные для графика модели
library(plyr)
NewData <- ddply(Owls, .variables = .(FoodTreatment), summarise, ArrivalTime_std = seq(min(ArrivalTime_std),  max(ArrivalTime_std), length = 100))
NewData$ArrivalTime <- NewData$ArrivalTime_std * sd(Owls$ArrivalTime) + mean(Owls$ArrivalTime)

## Предсказания и ошибки
# Модельная матрица
X <- model.matrix(~ FoodTreatment + ArrivalTime_std, data = NewData)
# К предсказанным значениям нужно прибавить оффсет. Мы будем делать предсказания для среднего размера выводка.
NewData$Pred <- X %*% fixef(M5) + log(mean(Owls$BroodSize))
# Стандартные ошибки предсказаний
NewData$SE <- sqrt(diag(X %*% vcov(M5) %*% t(X)))

## График предсказанных значений
ggplot() +
  geom_point(data = Owls, aes(x = ArrivalTime, y = NCalls), colour = "steelblue") +
  geom_ribbon(data = NewData, aes(x = ArrivalTime, ymax = exp(Pred + 1.96 * SE), ymin = exp(Pred - 1.96 * SE)), alpha = 0.3) +
  geom_line(data = NewData, aes(x = ArrivalTime, y = exp(Pred), group = FoodTreatment)) +
  facet_wrap(~FoodTreatment)

