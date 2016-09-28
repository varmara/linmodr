#' ---
#' title: "Множественная регрессия"
#' subtitle: "Линейные модели..."
#' author: "Марина Варфоломеева, Вадим Хайтов"
#' institute: "Кафедра Зоологии беспозвоночных, Биологический факультет, СПбГУ"


#' ## Пример: Птицы в лесах Австралии
#' - `ABUND` - Обилие птиц на стандартном маршруте
#' - `AREA` - площадь лесного массива (Га)
#' - `YRISOL` - год, в котором произошла изоляция лесного массива
#' - `DIST` - расстояние до ближайшего лесного массива (км)
#' - `LDIST` - расстояние до ближайшего более крупного массива (км)
#' - `GRAZE` - качественная оценка уровня выпаса скота (1 - низкий уровень, 5 - высокий уровень)
#' - `ALT` - высота над уровнем моря (м)
#' Пример из кн. Quinn, Keugh, 2002, данные из Loyn, 1987)


#' ## Скачиваем данные
#' Не забудьте войти в вашу директорию для матметодов при помощи `setwd()`

library(downloader)
# в рабочем каталоге создаем суб-директорию для данных
if(!dir.exists("data")) dir.create("data")
# скачиваем файл
download(
  url = "https://varmara.github.io/linmodr-course/data/loyn.csv",
  destfile = "data/loyn.csv")

#' ## Читаем данные
bird <- read.csv("data/loyn.csv")

#' ### Проверяем, все ли правильно открылось
str(bird)

#' ### Есть ли пропущенные значения?
sapply(bird, function(x)sum(is.na(x)))

#' ## Можно ли ответить на вопрос таким методом?
cor(bird)

#' ## Исследование данных (Data Exploration)
library(car)
scatterplotMatrix(bird)


#' ## Задание
#' - Постройте множественную линейную регрессию для зависимости обилия птиц (`ABUND`) от других переменных (`AREA`, `YRISOL`, `DIST`, `LDIST`, `GRAZE`, `ALT`)




#' ## Задание
#' - Проверьте условия применимости модели обилия птиц





#' # Мультиколинеарность
vif(mod1)

mod2 <- update(mod1, ~ . -GRAZE)
vif(mod2)


coef(summary(mod2))

#' ## Какой из предиктов оказывает наиболее сильное влияние?



mod2_scaled <- lm(ABUND ~ scale(AREA) + scale(YRISOL) + scale(DIST) + scale(LDIST) + scale(ALT), data = bird)
coef(summary(mod2_scaled))


#' ## Задание
#' Постройте модель описывающую связь между усилием мышц, осуществляющих выдох (`pemax`) и следующими переменными:
#' - `age` - Возраст
#' - `sex` - Пол (0: male, 1:female)
#' - `height` - Рост (cm)
#' - `weight` - Вес (kg)
#' - `bmp` - Отклонения в весе от нормы (% of normal)
#' - `fev1` - Объем наполенных легких
#' - `rv` - Остаточный объем легких
#' - `frc` - Функциональная остаточная емкость легких
#' - `tlc` - Общая емкость легких
#'
#' Исключите из модели колинеарные предикторы.
#'
#' Для получения данных выполните следующий код:
library(ISwR)
data(cystfibr)



#' # Взаимодействия предикторов

#' ## Вернемся к данным по обилию птиц и построим модель для двух предикторов
mod3 <- lm(ABUND ~ YRISOL * GRAZE, data = bird)
summary(mod3)


#' График отклика при нескольких значениях предиктора
MyData <- expand.grid(YRISOL = seq(1890, 1976, 1),
                      GRAZE = seq(1, 5, 1))
MyData$Predicted <- predict(mod3, newdata = MyData)
ggplot(MyData, aes(x = YRISOL,  y = Predicted, group = GRAZE)) +
  geom_line(aes(color = GRAZE), size = 2) +
  geom_point(data = bird, aes(x = YRISOL, y = ABUND, color = GRAZE), size = 4) +
  scale_colour_continuous(low = "yellow", high = "red") +
  xlab ("Год изоляции") + ylab("Обилие птиц")



