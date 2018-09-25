# Визуализция данных средствами ggplot2



# Данные взяты из работы
# Tager, I. B., Weiss, S. T., Rosner, B., and Speizer, F. E. (1979). Effect of parental cigarette smoking on pulmonary function in children. American Journal of Epidemiology, 110, 15-26.
# Rosner, B. (1990). Fundamentals of Biostatistics, 3rd Edition. PWS-Kent, Boston, Massachusetts.
# Источник данных: http://www.statsci.org/data/general/fev.html
#
# Структура данных
# Age 	 -  	Возраст
# FEV 	 -  	Объем легких при выдохе (литры) (forced expiratory volume)
# Height 	 -  	Рост (дюймы)
# Sex 	 -  	 пол (Male or Female)
# Smoker 	 -  	некурящие (Non), курящие (Current)



#############################################################################
#Загрузка пакетов

library(readxl)
library(ggplot2)

#############################################################################

#############################################################################
# Читаем данные

fev <- read_excel("data/fev.xls", sheet = "tidy_data", col_names = TRUE, na = "NA", skip = 1 )




# Структура данных

# имена переменных, заголовки столбцов

names(fev)

# [1] "Age"    "FEV"    "Height" "Sex"    "Smoker"


str(fev) #дает информацию о структуре датафрейма

# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	654 obs. of  5 variables:
#   $ Age   : num  9 8 7 9 9 8 6 6 8 9 ...
# $ FEV   : num  1.71 1.72 1.72 1.56 1.9 ...
# $ Height: num  57 67.5 54.5 53 57 61 58 56 58.5 60 ...
# $ Sex   : chr  "Female" "Female" "Female" "Male" ...
# $ Smoker: chr  "Non" "Non" "Non" "Non" ...


#Необходимо заменить формат в переменных fev$Sex и fev$Smoker

fev$Sex <- factor(fev$Sex)
fev$Smoker <- factor(fev$Smoker)

#функция factor() превращает числовые или текстовые данные в дискретные факторы

# Если необходимо убрать объекты, у которых что-то не измерено (NA), то надо произвести "очистку данных". НО! не увлекайтесь

#############################################################################
#Визуализация данных (первый заход)

#Задача: построить точечную диаграмму, где по оси OX отложен Age, а по оси OY отложен FEV

ggplot(data = fev, aes(x = Age, y = FEV)) + geom_point()

# Убираем серый фон

ggplot(data = fev, aes(x = Age, y = FEV)) + geom_point() + theme_bw()

ggplot(data = fev, aes(x = Age, y = FEV)) + geom_point() + theme_classic()

ggplot(data = fev, aes(x = Age, y = FEV)) + geom_point() + theme_minimal()

#Устанавливаем понравившуюся тему, как основную.
theme_set(theme_bw()) # далее все графики, производимые в данной сессии, будут использовать именно эту тему

ggplot(data = fev, aes(x = Age, y = FEV)) + geom_point()

# Изменяем подписи осей

ggplot(data = fev, aes(x = Age, y = FEV)) + geom_point() + labs(x = "Возраст", y = "Объем легких")

#Создаем верхний заголовок рисунка

ggplot(data = fev, aes(x = Age, y = FEV)) + geom_point() + labs(x = "Возраст", y = "Объем легких", title = "Зависимость между \n возрастом и объемом легких")

# Делаем заголовок центральным
ggplot(data = fev, aes(x = Age, y = FEV)) + geom_point() + labs(x = "Возраст", y = "Объем легких", title = "Зависимость между \n возрастом и объемом легких") + theme(plot.title = element_text(hjust = 0.5))


# Меняем размер точек

#Крупнее
ggplot(data = fev, aes(x = Age, y = FEV)) + geom_point(size = 3) + labs(x = "Возраст", y = "Объем легких", title = "Зависимость между \n возрастом и объемом легких") + theme(plot.title = element_text(hjust = 0.5))



#Мельче
ggplot(data = fev, aes(x = Age, y = FEV)) + geom_point(size = 0.1) + labs(x = "Возраст", y = "Объем легких", title = "Зависимость между \n возрастом и объемом легких") + theme(plot.title = element_text(hjust = 0.5))



# Меняем цвет и форму точек

ggplot(data = fev, aes(x = Age, y = FEV)) + geom_point(color = "blue") + labs(x = "Возраст", y = "Объем легких", title = "Зависимость между \n возрастом и объемом легких") + theme(plot.title = element_text(hjust = 0.5))


ggplot(data = fev, aes(x = Age, y = FEV)) + geom_point(shape = 22, color = "red", fill = "yellow", size = 2) + labs(x = "Возраст", y = "Объем легких", title = "Зависимость между \n возрастом и объемом легких") + theme(plot.title = element_text(hjust = 0.5))



#Сохраняем рисунок в файл

ggplot(data = fev, aes(x = Age, y = FEV)) + geom_point(shape = 22, color = "red", fill = "yellow", size = 2) + labs(x = "Возраст", y = "Объем легких", title = "Зависимость между \n возрастом и объемом легких") + theme(plot.title = element_text(hjust = 0.5))


ggsave("MyPicture.wmf", plot = last_plot())

#Рисунок можно, и это правильно, поместить в специальную переменную
Plot_1 <- ggplot(data = fev, aes(x = Age, y = FEV)) + geom_point(shape = 22, color = "red", fill = "yellow", size = 2)

Plot_1

# Далее эту переменную можно модифицировать

Plot_1 + labs(x = "Возраст", y = "Объем легких", title = "Зависимость между \n возрастом и объемом легких") + theme(plot.title = element_text(hjust = 0.5))


Plot_2 <- Plot_1 + labs(x = "Возраст", y = "Объем легких", title = "Зависимость между \n возрастом и объемом легких") + theme(plot.title = element_text(hjust = 0.5))


ggsave("MyPicture_2.wmf", plot = Plot_2)


#############################################################################
#Визуализация данных (Aesthetics)
# В философии ggplot эстетики - это та информация (данные), которую можно выразить графиком.
# Минимальные эстетики - Положение на OX и положение на OY
# Однако наши данные содержат еще и информацию о поле (переменная fev$Sex). Если эти данные для нас важны, то мы должны эту информацию выразить на графике


#Отражаем данные о поле с помощью цвета
Plot_1 <- ggplot(data = fev, aes(x = Age, y = FEV, color = Sex )) + geom_point(size = 2) + labs(x = "Возраст", y = "Объем легких", title = "Зависимость между \n возрастом и объемом легких") + theme(plot.title = element_text(hjust = 0.5))

Plot_1

# Меняем цвет на тот, который нам нравится
Plot_1 + scale_color_manual(values = c("pink","blue"))


# Меняем положение легенды
Plot_1 + scale_color_manual(values = c("pink","blue")) + theme(legend.position =  "bottom")

Plot_1 + scale_color_manual(values = c("pink","blue")) + theme(legend.position =  "left")

Plot_1 + scale_color_manual(values = c("pink","blue")) + theme(legend.position =  c(0.1, 0.9)) # c(0.1, 0.9) координаты указываются в долях от сторон рисунка



#Отражаем данные о поле с помощью формы точек
Plot_1 <- ggplot(data = fev, aes(x = Age, y = FEV, shape = Sex, color = Sex )) + geom_point(size = 2) + labs(x = "Возраст", y = "Объем легких", title = "Зависимость между \n возрастом и объемом легких") + theme(plot.title = element_text(hjust = 0.5))

Plot_1


# В нашем датафрейме есть еще и данные о курении.
# Если мы хотим выразить графиком одновременно данные по полу и по курению, то мы должны задать две разные эстетики


Plot_1 <- ggplot(data = fev, aes(x = Age, y = FEV, shape = Sex, color = Smoker )) + geom_point(size = 2) + labs(x = "Возраст", y = "Объем легких", title = "Зависимость между \n возрастом и объемом легких") + theme(plot.title = element_text(hjust = 0.5))

Plot_1 #в этом трудно разобраться


#Используем фасетирование

Plot_1 + facet_wrap( ~ Smoker) #уже лучше

Plot_1 + facet_grid(Sex ~ Smoker)


#В нашем датафрейме есть еще и данные по росту.

Plot_1 <- ggplot(data = fev, aes(x = Age, y = FEV, shape = Sex, color = Smoker, size = Height)) + geom_point() + labs(x = "Возраст", y = "Объем легких", title = "Зависимость между \n возрастом и объемом легких") + theme(plot.title = element_text(hjust = 0.5))

Plot_1 + facet_grid(Sex ~ Smoker)


# Шуточный пример, из которого можно почерпнуть некоторые возможности ggplot


#Запустите код, расположенный между двумя линиями

#___________________________

circus <- function(n, p, cos2 = 0, sin2 =0, cos3 = 0, sin3 = 0){
  # n - number of points
  # p - period
  factor <- points <- data.frame(X=c(1:n), Y=c(1:n))
  k <- 0
  for (i in 1:n){
    factor$X[i] <- (i-1)/p - k
    if ((i/p - trunc(i/p))==0) k <- k + 1
  }

  factor$Y <- factor$X

  for (i in 1:n){
    points$X[i] <- cos(2*pi*factor$X[i]) + cos(cos2/4*pi*factor$X[i]) + cos(cos3/4*pi*factor$X[i])
    points$Y[i] <- sin(2*pi*factor$Y[i]) + sin(sin2/4*pi*factor$Y[i]) + sin(sin3/4*pi*factor$Y[i])
  }
  return(points)
}

bill <- circus(100, 100, 10.7, 15, 0, 0)
bill2 <- circus(100, 100, 10.7, 15, 0, 0)
cock_head <- circus(100, 100, 15, 15, 0, 15)
cock_head$X <- cock_head$X +1.6
cock_head$Y <- cock_head$Y +1.1
cock_beard <- circus(100, 100, 1, 5, 1, 1)
cock_crest <- circus(100, 100, 15, 30, 20, 40)
cock_pupil <- circus(100, 100, 0, 0, 0, 0)
forest <- circus(100, 100, 15, 200, 15, 100)

fir <- data.frame(x = 3, y = seq(-3, 4, length.out = 100))
fir$xend <- seq(3, 5, length.out = 100)
fir$yend <- 4 - fir$xend


fir2 <- data.frame(x = 3, y = seq(4, -3, length.out = 100))
fir2$xend <- seq(3, -3, length.out = 100)
fir2$yend <- fir$yend


ray <- data.frame(x=3, y = 5, angle = runif(100, 0, 2*pi), radius = rnorm(100, 1, 0.5))

stars <- data.frame(x = rnorm(30, 1, 5), y = rnorm(30, 11, 0.5) )

snow <- data.frame(x = rnorm(3000, 3, 10), y = rnorm(3000, -3, 0.1) )
snow$y[snow$y < -3] <-snow$y [snow$y < -3] + 0.3

ggplot() + geom_polygon(data = forest, aes(X*3, Y ), fill = "white", color = "black") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill="blue")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill="blue")) +
  xlab("") +
  ylab("") +
  ylim(-3, 12) +
  xlim(-7, 10) +
  geom_curve(data = fir, aes(x=x, y=y, xend = xend, yend = yend), curvature = -0, color = "darkgreen") +
  geom_curve(data = fir, aes(x=x, y=y +1, xend = xend, yend = yend + 1), curvature = -0, color = "darkgreen") +
  geom_curve(data = fir, aes(x=x, y=y -1, xend = xend, yend = yend - 1), curvature = -0, color = "darkgreen") +
  geom_curve(data = fir2, aes(x=x, y=y, xend = xend, yend = yend), curvature = -0, color = "darkgreen") +
  geom_curve(data = fir2, aes(x=x, y=y +1, xend = xend, yend = yend + 1), curvature = -0, color = "darkgreen") +
  geom_curve(data = fir2, aes(x=x, y=y -1, xend = xend, yend = yend - 1), curvature = -0, color = "darkgreen") +
  geom_spoke(data = ray, aes(x=x, y=y, angle = angle, radius = radius), color = "yellow") +
  geom_point(data  = stars, aes(x=x, y=y), color = "yellow", size=10, shape = "*") + geom_point(data  = snow, aes(x=x, y=y), color = "white") +
  geom_point(aes(x = rnorm(100,1,10), y=rnorm(100,2, 2)), shape=8, size=3, color="white") +
  geom_polygon(data = bill2, aes(X + 5, Y-0.5 +5), fill = "gold", color = "black") + geom_polygon(data = bill, aes(X+ 5, Y+5), fill = "gold", color = "black")  +
  geom_polygon(data = cock_head, aes(X+ 5, Y+5), fill = "orange", color = "black") +
  geom_polygon(data = cock_crest, aes(X*1.2 + 4+ 5, Y*1.2 +4+5), fill = "red", color = "black") +
  geom_polygon(data = cock_beard, aes(X/1.5+0.9+ 5, Y*1.2-3+5), fill = "red") +
  geom_polygon(data = cock_pupil, aes(X/4 + 1.6 + 5, Y/3 + 1.6+5), fill = "black") +
  geom_text(aes(x=0, y = 9), label = "Year of the roosteR", size = 10, color = "yellow") +
  geom_text(aes(x=0, y =7), label = "2017", size = 15, color = "white")
#___________________________

