## Муравьи и кофейные бурильщики #################

# Кофейный бурильщик [_Hypothenemus hampei_](https://ru.wikipedia.org/wiki/Hypothenemus_hampei) --- важный вредитель на кофейных плантациях по всему миру. Чтобы бороться с этим жуком в Центральной Америке используют муравья [_Azteca sericeasur_](https://www.antweb.org/description.do?rank=species&genus=Azteca&name=sericeasur&project=worldants). В статье Morris et al. (2015) описан эксперимент, который позволяет оценить насколько эффективен такой метод борьбы с вредителем.

# На ферме собрали всех жуков - потом их использовали, чтобы создать четыре разных уровня плотности кофейных жуков. На каждом участке выбрали несколько веток кофе. На каждой ветке кофе на один побег муравьи могли беспрепятственно проникать, а на соседнем побеге была установлена преграда, которая предотвращала доступ муравьев.
# Через сутки после начала эксперимента подсчитывали число сверлений кофейных жуков.

# Источник данных:
# Morris, J.R., Vandermeer, J. and Perfecto, I., 2015. A keystone ant species provides robust biological control of the coffee berry borer under varying pest densities. PloS one, 10(11), p.e0142850.

# https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0142850

# Файл:

# Morris_2015_Ants.xlsx

# Переменные:

# - Site -
# + Treatment - плотность жуков (4 градации)
# - StartDate -
# - StartTime -
# - StartBushAct -
# - StartBranchAct - начальная активность муравьев на ветке
# - Bored -
# - NonBored -
# + BranchBerries - число ягод на ветке
# = TotRemoved - общее число удаленных во время подготовки ягод
# = Cluster Berries - суммарное число кластеров ягод на ближайших ветвях
# - PlacementDate -
# - PlacementTime -
# - PlacementBushAct -
# - PlacementBranchAct -
# - FinalBushAct -
# - FinalBranchAct - разница числа кофейных жуков между побегами на ветке
# + Infestation - число кофейных жуков, просверливших плоды кофе на данном побеге через 24 часа
# + Branch - доступ муравьев (0, 1)

library(readxl)
library(ggplot2)
theme_set(theme_bw() + theme(legend.key = element_blank()))
update_geom_defaults("point", list(shape = 19))
library(car)
library(dplyr)
library(tidyr)
library(lme4)
library(sjstats)

stand <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}


## Подготовка данных ####
ants <- read_excel("data/Morris_2015_Ants.xlsx", sheet = "Data", na = "NA")
str(ants)

# Переименовываем переменные
colnames(ants)[c(10, 12)] <- c("BranchBerries", "ClusterBerries")

# поправляем типы данных
ants$Site <- factor(ants$Site)
ants$Branch <- factor(ants$Branch)
ants$Treatment <- factor(ants$Treatment)

# Пропущенные значения
colSums(is.na(ants))
# Удаляем
ants <- ants[!is.na(ants$TotRemoved), ]
colSums(is.na(ants))

# Численность групп по факторам
table(ants$Branch)
table(ants$Site, ants$Treatment)

# Есть ли наблюдения-выбросы? строим dot-plot
dotplot <- ggplot(ants, aes(y = 1:nrow(ants))) +
  geom_point()
dotplot + aes(x = StartBushAct)
dotplot + aes(x = StartDate)

# Сразу много дот-плотов
ants %>% mutate(Index = row_number()) %>%
  gather(key = Variable, value = Value,
         -c(Index, Site, Treatment, Branch, StartDate,
            StartTime, PlacementDate, PlacementTime))  %>%
  ggplot(., aes(x = Value, y = Index)) +
  geom_point() +
  facet_wrap(~ Variable, scales = "free_x")
# выбросов нет

## Сколько нулей?
sum(ants$Infestation == 0)/nrow(ants)
# нулей мало

## Трансформация данных
ants <- ants %>% mutate(
  StartBranchAct_std = stand(StartBranchAct),
  BranchBerries_std = stand(BranchBerries),
  ClusterBerries_std = stand(ClusterBerries),
  TotRemoved_std = stand(TotRemoved)
)

# Проверка на колинеарность
M <- lm(Infestation ~ Treatment + Branch + StartBranchAct_std + BranchBerries_std + ClusterBerries_std + TotRemoved_std, data = ants)
vif(M)

## Модель из статьи ##############################

# # Модель, составленная по описанию из статьи у
# меня не сходится. С ней можно повозиться, но мы
# этого делать не будем.

# Как бороться за сходимость модели?
# https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html

# M <- glmer(Infestation ~ Treatment * Branch +
#              StartBranchAct + BranchBerries +
#              ClusterBerries + TotRemoved + (1|Site),
#            data = ants, family = "poisson")

## Задание #######################################

# Постройте модель зависимости заражения кофейным
# бурильщиком от плотности жуков, доступа
# муравьев, начальной активности муравьев на
# ветке, общего числа ягод.

# Пуассоновская GLMM по сырым данным ##################

M1 <- glmer(Infestation ~ Treatment * Branch + StartBranchAct + BranchBerries + (1|Site), data = ants, family = "poisson")
# Warning message:
# In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
# Model is nearly unidentifiable: very large eigenvalue
# - Rescale variables?

# Как бороться за сходимость модели?
# https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html

# Пуассоновская GLMM после трансформации ####################

M2 <- glmer(Infestation ~ Treatment * Branch + StartBranchAct_std + BranchBerries_std + (1|Site), data = ants, family = "poisson")

overdisp(M2)
# Сверхдисперсия. Почему?

# Есть ли паттерны в остатках
M2_diag <- data.frame(ants,
                      .pearson_resid = residuals(M2, type = "pearson"),
                      .fitted = predict(M2, type = "response")
)
gg_resid <- ggplot(data = M2_diag, aes(y = .pearson_resid))
gg_resid + geom_point(aes(x = .fitted))
gg_resid + geom_boxplot(aes(x = Site)) # разные остатки в разных сайтах
gg_resid + geom_boxplot(aes(x = Treatment))
gg_resid + geom_point(aes(x = StartDate))  # нелинейная связь?
gg_resid + geom_point(aes(x = StartTime))
gg_resid + geom_point(aes(x = StartBushAct))
gg_resid + geom_point(aes(x = StartBranchAct))  #?
gg_resid + geom_point(aes(x = Bored))
gg_resid + geom_point(aes(x = NonBored))
gg_resid + geom_point(aes(x = BranchBerries))
gg_resid + geom_point(aes(x = TotRemoved))
gg_resid + geom_point(aes(x = ClusterBerries)) #?
gg_resid + geom_point(aes(x = PlacementDate))  # нелинейная связь?
gg_resid + geom_point(aes(x = PlacementTime)) # часть жуков выпустили в другое время
gg_resid + geom_point(aes(x = PlacementBushAct)) #
gg_resid + geom_point(aes(x = PlacementBranchAct)) ##?
gg_resid + geom_point(aes(x = FinalBushAct))
gg_resid + geom_point(aes(x = FinalBranchAct))

# Может быть, здесь нелинейный паттерн?
# Самые подходящие кандидаты - это переменные, которые показывают время.
# В других случаях сложно придумать осмысленное объяснение появления нелинейности.
library(mgcv)

# StartDate
nonlin1 <- gam(M2_diag$.pearson_resid ~ s(as.numeric(ants$StartDate)))
summary(nonlin1)
plot(nonlin1)
abline(h = 0)

# PlacementDate
nonlin2 <- gam(M2_diag$.pearson_resid ~ s(as.numeric(ants$PlacementDate)))
summary(nonlin2)
plot(nonlin2)
abline(h = 0)

# Стоило бы включить нелинейный эффект PlacementDate или StartDate
# Что то одно, т.к. они связаны)
plot(ants$PlacementDate, ants$StartDate)

summary(M2)

# РЕЗЮМЕ по M2:
# Здесь мы не можем доверять стандартным ошибкам
# из-за избыточности дисперсии.
# Здесь есть нелинейные паттерны в остатках,
# стоило бы включить нелинейный эффект PlacementDate.

# Варианты борьбы со сверхдисперсией
# (0) Неучтенные переменные (в основном нелинейный эффект,
# который скорее всего связан с датой начала, но можно рискнуть добавить)
# (1) Нелинейный паттерн в остатках - включить нелинейный эффект PlacementDate.
# (2) Отр. биномиальное распределение.
# (3) Случайный отрезок на уровне наблюдения (Observation-level random intercept)
# Но нужно помнить, что это очень грубый способ.

# Рассмотрим два варианта борьбы со сверхдисперсией (2) и (3)

# (2) GLM с отр. биномиальным распределением ######

NB1 <- glmer.nb(Infestation ~ Treatment * Branch + StartBranchAct_std + BranchBerries_std + (1 | Site), data = ants)
# Warning messages:
# 1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
# Model failed to converge with max|grad| = 0.00593457 (tol = 0.001, component 1)
# 2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
# Model failed to converge with max|grad| = 0.0608566 (tol = 0.001, component 1)
# 3: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
# Model failed to converge with max|grad| = 0.338333 (tol = 0.001, component 1)
# 4: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
# Model failed to converge with max|grad| = 0.0317026 (tol = 0.001, component 1)

# Опять пытаемся заставить модель сойтись:
# (a) Увеличиваем число итераций
# (б) Пытаемся подобрать более простую модель
# (в) Пробуем подсунуть в модель заранее оцененную theta (она же k)

# (a) Увеличиваем число итераций
ctrl.nb = glmerControl(optimizer = 'bobyqa',
                       optCtrl = list(maxfun = 200000))
NB1 <- glmer.nb(Infestation ~ Treatment * Branch + StartBranchAct_std + BranchBerries_std + (1 | Site), data = ants, control = ctrl.nb)
# singular fit

# (б) Пытаемся подобрать более простую модель
NB1 <- glmer.nb(Infestation ~ Treatment + Branch + StartBranchAct_std + BranchBerries_std + (1 | Site), data = ants, control = ctrl.nb)
# singular fit

# (в) Пробуем подсунуть в модель заранее оцененную тету
# Оцениваем theta из исходной пуассоновской модели, которая сошлась
th <- lme4:::est_theta(M2)
# Подбираем модель с отрицательным биномиальным распределением и этой theta
NB1 <- update(M2, family = negative.binomial(theta = th))
# Ура!

overdisp(NB1)
# Сверхдисперсия осталась!

# Есть ли паттерны в остатках?
NB1_diag <- data.frame(ants,
                      .pearson_resid = residuals(NB1, type = "pearson"),
                      .fitted = predict(NB1, type = "response")
)
gg_resid <- ggplot(data = NB1_diag, aes(y = .pearson_resid))
gg_resid + geom_point(aes(x = .fitted))
gg_resid + geom_boxplot(aes(x = Site)) # разные остатки в разных сайтах
gg_resid + geom_boxplot(aes(x = Treatment))
gg_resid + geom_point(aes(x = StartDate))  # нелинейная связь?
gg_resid + geom_point(aes(x = StartTime))
gg_resid + geom_point(aes(x = StartBushAct))
gg_resid + geom_point(aes(x = StartBranchAct))  #?
gg_resid + geom_point(aes(x = Bored))
gg_resid + geom_point(aes(x = NonBored))
gg_resid + geom_point(aes(x = BranchBerries))
gg_resid + geom_point(aes(x = TotRemoved))
gg_resid + geom_point(aes(x = ClusterBerries)) #?
gg_resid + geom_point(aes(x = PlacementDate))  # нелинейная связь?
gg_resid + geom_point(aes(x = PlacementTime)) # часть жуков выпустили в другое время
gg_resid + geom_point(aes(x = PlacementBushAct)) #
gg_resid + geom_point(aes(x = PlacementBranchAct)) ##?
gg_resid + geom_point(aes(x = FinalBushAct))
gg_resid + geom_point(aes(x = FinalBranchAct))

# Может быть, здесь нелинейный паттерн?
# StartDate
nonlin3 <- gam(NB1_diag$.pearson_resid ~ s(as.numeric(ants$StartDate)))
summary(nonlin3)
plot(nonlin3)
abline(h = 0)

# PlacementDate
nonlin4 <- gam(NB1_diag$.pearson_resid ~ s(as.numeric(ants$PlacementDate)))
summary(nonlin4)
plot(nonlin4)
abline(h = 0)


summary(NB1)
# РЕЗЮМЕ по NB1:
# Здесь мы не можем доверять стандартным ошибкам
# из-за избыточности дисперсии.
# Здесь есть нелинейные паттерны в остатках,
# стоило бы включить нелинейный эффект PlacementDate.



# (3) Случайный отрезок на уровне наблюдения (Observation-level random intercept) #####

# Чтобы побороть сверхдисперсию в Пуассоновской модели иногда используют трюк:
# подбирают свой интерсепт для каждого наблюдения. Т.е. теперь у нас будет в модели два случайных эффекта: сайт и наблюдение.
# Этот случайный эффект призван убрать сверхдисперсию. Но это очень грубый способ.
ants$Index <- 1:nrow(ants)
M_ori1 <- glmer(Infestation ~ Treatment * Branch + StartBranchAct_std + BranchBerries_std + (1 | Site) + (1|Index), data = ants, family = "poisson")
# Warning message:
# In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
# Model failed to converge with max|grad| = 0.0164161 (tol = 0.001, component 1)

# Увеличиваем число итераций
ctrl.pois = glmerControl(optimizer = 'bobyqa',
                         optCtrl = list(maxfun = 200000))
M_ori1 <- glmer(Infestation ~ Treatment * Branch + StartBranchAct_std + BranchBerries_std + (1 | Site) + (1|Index), data = ants, family = "poisson", control = ctrl.pois)
# Ура!

overdisp(M_ori1)
# Сверхдисперсия ушла. Почему и как?

# Давайте посмотрим в summary()
summary(M_ori1)
# Random effects:
#  Groups Name        Variance Std.Dev.
#  Index  (Intercept) 0.4632   0.681
#  Site   (Intercept) 0.0389   0.197
# Number of obs: 163, groups:  Index, 163; Site, 20
# Очень много дисперсии забрал наш новый жадный случайный эффект на уровне наблюдения.

# Есть ли паттерны в остатках?
M_ori1_diag <- data.frame(ants,
                       .pearson_resid = residuals(M_ori1, type = "pearson"),
                       .fitted = predict(M_ori1, type = "response")
)
gg_resid <- ggplot(data = M_ori1_diag, aes(y = .pearson_resid))
gg_resid + geom_point(aes(x = .fitted))
gg_resid + geom_boxplot(aes(x = Site)) # разные остатки в разных сайтах
gg_resid + geom_boxplot(aes(x = Treatment))
gg_resid + geom_point(aes(x = StartDate))  # нелинейная связь уже почти не читается
gg_resid + geom_point(aes(x = StartTime))
gg_resid + geom_point(aes(x = StartBushAct))
gg_resid + geom_point(aes(x = StartBranchAct))  #?
gg_resid + geom_point(aes(x = Bored))
gg_resid + geom_point(aes(x = NonBored))
gg_resid + geom_point(aes(x = BranchBerries))
gg_resid + geom_point(aes(x = TotRemoved))
gg_resid + geom_point(aes(x = ClusterBerries)) #?
gg_resid + geom_point(aes(x = PlacementDate))  # нелинейная связь уже почти не читается
gg_resid + geom_point(aes(x = PlacementTime)) # часть жуков выпустили в другое время
gg_resid + geom_point(aes(x = PlacementBushAct)) #
gg_resid + geom_point(aes(x = PlacementBranchAct)) ##?
gg_resid + geom_point(aes(x = FinalBushAct))
gg_resid + geom_point(aes(x = FinalBranchAct))

# Немного лучше
# Может быть, здесь нелинейный паттерн?
# StartDate
nonlin5 <- gam(M_ori1_diag$.pearson_resid ~ s(as.numeric(ants$StartDate)))
summary(nonlin5)
plot(nonlin5)
abline(h = 0)
# PlacementDate
nonlin6 <- gam(M_ori1_diag$.pearson_resid ~ s(as.numeric(ants$PlacementDate)))
summary(nonlin6)
plot(nonlin6)
abline(h = 0)
# Все равно паттерн остался

# РЕЗЮМЕ по M_ori1:
# Мы побороли избыточность дисперсии, но...
# Здесь есть нелинейные паттерны в остатках,
# стоило бы включить нелинейный эффект PlacementDate.


