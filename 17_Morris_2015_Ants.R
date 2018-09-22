## Муравьи и кофейные бурильщики #################

# Кофейный бурильщик [_Hypothenemus hampei_](https://ru.wikipedia.org/wiki/Hypothenemus_hampei) --- важный вредитель на кофейных плантациях по всему миру. Чтобы бороться с этим жуком в Центральной Америке используют муравья [_Azteca sericeasur_](https://www.antweb.org/description.do?rank=species&genus=Azteca&name=sericeasur&project=worldants). В статье Morris et al. (2015) описан эксперимент, который позволяет оценить насколько эффективен такой метод борьбы с вредителем.

# На ферме собрали всех жуков - потом их использовали, чтобы создать четыре разных уровня плотности кофейных жуков. На каждом участке выбрали несколько веток кофе. На каждой ветке кофе на один побег муравьи могли беспрепятственно проникать, а на соседнем побеге была установлена преграда, которая предотвращала доступ муравьев.
# Через сутки после начала эксперимента подсчитывали число сверлений кофейных жуков.

# Источник данных:
# Morris, J.R., Vandermeer, J. and Perfecto, I., 2015. A keystone ant species provides robust biological control of the coffee berry borer under varying pest densities. PloS one, 10(11), p.e0142850.

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

stand <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

overdisp_fun <- function(model) {
  ## number of variance parameters in
  ##   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
  rdf <- nrow(model.frame(model))-model.df
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

## Подготовка данных ####
ants <- read_excel("Morris_2015_Ants.xlsx", sheet = "Data", na = "NA")
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
