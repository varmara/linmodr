# title: "Смешанные линейные модели (случайный интерсепт и случайный угол наклона)"
# subtitle: "Линейные модели..."
# author: "Марина Варфоломеева"
# institute: "Кафедра Зоологии беспозвоночных, Биологический факультет, СПбГУ"



# ## Пример -- недосып
# В ночь перед нулевым днем всем испытуемым давали поспать нормальное время, а в следующие 9 ночей --- только по 3 часа. Каждый день измеряли время реакции в серии тестов.
#
# Как время реакции людей зависит от бессонницы?
# Belenky et al., 2003
# - `Reaction` --- среднее время реакции в серии тестов в день наблюдения, мс
# - `Days` --- число дней депривации сна
# - `Subject` --- номер испытуемого
library(lme4)
data(sleepstudy)

sl <- sleepstudy
str(sl)

# Есть ли пропущенные значения?
colSums(is.na(sl))
# Сколько субъектов?
length(unique(sl$Subject))
# Сколько наблюдений для каждого субъекта?
table(sl$Subject)

# ## Есть ли выбросы?
library(ggplot2)
theme_set(theme_bw())

ggplot(sl, aes(x = Reaction, y = 1:nrow(sl))) +
  geom_point()


# ## Как меняется время реакции разных людей?
ggplot(sl, aes(x = Reaction, y = Subject, colour = Days)) +
  geom_point()


# ## Плохое решение: не учитываем группирующий фактор
W1 <- glm(Reaction ~ Days, data = sl)
summary(W1)
ggplot(sl, aes(x = Days, y = Reaction)) +
  geom_point() +
  geom_smooth(se = TRUE, method = "lm", size = 1)


# ## Громоздкое решение: группирующий фактор как фиксированный
W2 <- glm(Reaction ~ Days + Subject, data = sl)
coef(W2)
ggplot(fortify(W2), aes(x = Days, colour = Subject)) +
  geom_line(aes(y = .fitted, group = Subject)) +
  geom_point(data = sl, aes(y = Reaction)) +
  guides(colour = guide_legend(ncol = 2))



# # GLMM со случайным отрезком
M1 <- lmer(Reaction ~ Days + (1 | Subject), data = sl)
summary(M1)

# Данные для графика предсказаний фиксированной части модели:
library(dplyr)
NewData <- sl %>% group_by(Subject) %>%
  do(data.frame(Days = seq(min(.$Days), max(.$Days), length = 10)))
head(NewData, 3)

# ## Предсказания фиксированной части модели при помощи predict()
NewData$fit <- predict(M1, NewData, type = 'response', re.form = NA)
head(NewData, 3)


# ## Предсказания фиксированной части модели в матричном виде
X <- model.matrix(~ Days, data = NewData)
betas <- fixef(M1)
NewData$fit <- X %*% betas
# Cтандартные ошибки
NewData$SE <- sqrt( diag(X %*% vcov(M1) %*% t(X)) )
NewData$lwr <- NewData$fit - 2 * NewData$SE
NewData$upr <- NewData$fit + 2 * NewData$SE

# ## График предсказаний фиксированной части модели
ggplot(data = NewData, aes(x = Days, y = fit)) +
  geom_ribbon(alpha = 0.35, aes(ymin = lwr, ymax = upr)) +
  geom_line() + geom_point(data = sl, aes(x = Days, y = Reaction))


# ## Предсказания для каждого уровня случайного фактора
NewData$fit_subj <- predict(M1, NewData, type = 'response')
ggplot(NewData, aes(x = Days, y = fit_subj)) +
  geom_ribbon(alpha = 0.3, aes(ymin = lwr, ymax = upr)) +
  geom_line(aes(colour = Subject)) +
  geom_point(data = sl, aes(x = Days, y = Reaction, colour = Subject))  +
  guides(colour = guide_legend(ncol = 2))



# ## Коэффициент внутриклассовой корреляции (intra-class correlation, ICC)
#
# $ICC = \sigma_b^2 / (\sigma^2 + \sigma_b^2)$

summary(M1)
VarCorr(M1)  # Случайные эффекты отдельно

# # Диагностика модели

# ## Данные для анализа остатков
M1_diag <- data.frame(
  sl,
  .fitted = predict(M1),
  .resid = resid(M1, type = 'pearson'),
  .scresid = resid(M1, type = 'pearson', scaled = TRUE))

head(M1_diag, 4)

# ## График остатков от предсказанных значений
gg_resid <- ggplot(M1_diag, aes(y = .scresid)) +
  geom_hline(yintercept = 0)
gg_resid + geom_point(aes(x = .fitted))

# ## Графики остатков от ковариат в модели и не в модели
gg_resid + geom_boxplot(aes(x = factor(Days)))
gg_resid + geom_boxplot(aes(x = Subject))


# # GLMM со случайным отрезком и углом наклона
MS1 <- lmer(Reaction ~ Days + ( 1 + Days|Subject), data = sl)

summary(MS1)

# ## Данные для графика предсказаний фиксированной части модели
library(dplyr)
NewData <- sl %>% group_by(Subject) %>%
  do(data.frame(Days = seq(min(.$Days), max(.$Days), length = 10)))

NewData$fit <- predict(MS1, NewData, type = 'response', re.form = NA)
head(NewData, 3)


# ## Предсказания фиксированной части модели в матричном виде
X <- model.matrix(~ Days, data = NewData)
betas <- fixef(MS1)
NewData$fit <- X %*% betas

# Cтандартные ошибки
NewData$SE <- sqrt( diag(X %*% vcov(MS1) %*% t(X)) )

NewData$lwr <- NewData$fit - 2 * NewData$SE
NewData$upr <- NewData$fit + 2 * NewData$SE


# ## График предсказаний фиксированной части модели
gg_MS1_normal <- ggplot(data = NewData, aes(x = Days, y = fit)) +
  geom_ribbon(alpha = 0.35, aes(ymin = lwr, ymax = upr)) +
  geom_line() + geom_point(data = sl, aes(x = Days, y = Reaction))
gg_MS1_normal

# ## Предсказания для каждого уровня случайного фактора
NewData$fit_subj <- predict(MS1, NewData, type = 'response')
ggplot(NewData, aes(x = Days, y = fit_subj)) +
  geom_ribbon(alpha = 0.3, aes(ymin = lwr, ymax = upr)) +
  geom_line(aes(colour = Subject)) +
  geom_point(data = sl, aes(x = Days, y = Reaction, colour = Subject))  +
  guides(colour = guide_legend(ncol = 2))


# # Диагностика модели
# ## Данные для анализа остатков
MS1_diag <- data.frame(
  sl,
  .fitted = predict(MS1),
  .resid = resid(MS1, type = 'pearson'),
  .scresid = resid(MS1, type = 'pearson', scaled = TRUE))

head(MS1_diag, 4)

# ## График остатков от предсказанных значений
gg_resid <- ggplot(MS1_diag, aes(y = .scresid)) +
  geom_hline(yintercept = 0)
gg_resid + geom_point(aes(x = .fitted))

# ## Графики остатков от ковариат в модели и не в модели
gg_resid + geom_boxplot(aes(x = factor(Days)))
gg_resid + geom_boxplot(aes(x = Subject))



# # Тестирование гипотез в смешанных моделях

# t-(или -z) тесты Вальда
coef(summary(MS1))

# ## Тесты отношения правдоподобий (LRT)
# ## LRT для случайных эффектов
MS1 <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sl, REML = TRUE)
MS0 <- lmer(Reaction ~ Days + (1 | Subject), data = sl, REML = TRUE)
anova(MS1, MS0, refit = FALSE)

# ## LRT для фиксированных эффектов
MS1.ml <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sl, REML = FALSE)
MS0.ml <- lmer(Reaction ~ 1 + (1 + Days | Subject), data = sl, REML = FALSE)
anova(MS1.ml, MS0.ml)


# ## Сравнение моделей по AIC
AIC(MS1.ml, MS0.ml)



# ## Бутстреп для тестирования значимости и для предсказаний

# ## Параметрический бутстреп для LRT фиксированных эффектов
library(pbkrtest)
pmod <- PBmodcomp(MS1.ml, MS0.ml, nsim = 100) # 1000 и больше для реальных данных
summary(pmod)


# ## Бутстреп-оценка доверительной зоны регрессии
NewData <- sl %>% group_by(Subject) %>%
  do(data.frame(Days = seq(min(.$Days), max(.$Days), length = 10)))
NewData$fit <- predict(MS1, NewData, type = 'response', re.form = NA)

# Многократно симулируем данные из модели и получаем для них предсказанные значения
bMS1 <- bootMer(x = MS1,
                FUN = function(x) predict(x, new_data = NewData, re.form = NA),
                nsim = 100)

# Рассчитываем квантили предсказанных значений для всех итераций бутстрепа
b_se <- apply(X = bMS1$t,
              MARGIN = 2,
              FUN = function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE))

# Доверительная зона для предсказанных значений
NewData$lwr <- b_se[1, ]
NewData$upr <- b_se[2, ]

# ## График предсказаний фиксированной части модели
gg_MS1_boot <- ggplot(data = NewData, aes(x = Days, y = fit)) +
  geom_ribbon(alpha = 0.35, aes(ymin = lwr, ymax = upr)) +
  geom_line() + geom_point(data = sl, aes(x = Days, y = Reaction))
gg_MS1_boot

library(cowplot)
plot_grid(gg_MS1_normal + labs(title = "normal"),
          gg_MS1_boot + labs(title = "bootstrap"),
          ncol = 2)
