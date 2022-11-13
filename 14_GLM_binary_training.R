
##########################################
#Самостоятельная работа

##########################################


#####################################3

# Множественная логистическая регрессия

surviv <- read.table("data/ICU.csv", header=TRUE, sep=";")


##Сделаем факторами те дискретные предикторы, которые обозначенны цифрами
surviv$PO2 <- factor(surviv$PO2)
surviv$PH <- factor(surviv$PH)
surviv$PCO <- factor(surviv$PCO)
surviv$BIC <- factor(surviv$BIC)
surviv$CRE <- factor(surviv$CRE)
surviv$LOC <- factor(surviv$LOC)


M1 <- glm(STA ~ , data = surviv)



##Задание: Проведите анализ девиансы для данной модели




##Задание: Подберите оптмальную модель




# Проверьте получится ли аналогичная модель, если воспользоваться автоматическим алгоритмом
step(, direction = )




##Задание: Проведите диагностику  финальной модели

_diagn <- fortify(M )


ggplot( _diagn, aes(x = .fitted, y =.stdresid)) + geom_point() + geom_smooth()







library(dplyr)

_diagn$group <- ntile( _diagn$.fitted, 10)

resi_and_fit <- _diagn %>%  group_by(group) %>%  summarise(mean_fit = mean(.fitted), mean_res = mean(.stdresid))

qplot(resi_and_fit$mean_fit, resi_and_fit$mean_res ) + geom_smooth()



#Визуализируем предсказания модели, взяв пр этом TYPE == Emergency

MyData = surviv %>% group_by(CAN,  TYP, PH, PCO, LOC) %>% do(data.frame(SYS = seq(min(.$SYS), max(.$SYS), length.out = 100)))

MyData$AGE <- mean(surviv$AGE)


MyData$Predicted <- predict( , newdata = MyData, type = "response" )



ggplot(MyData[MyData$TYP == "Emergency", ], aes(x=SYS, y = Predicted)) + geom_line() + facet_grid(LOC + PH + PCO ~ CAN, labeller = label_both) + scale_color_gradient(low = "green",  high = "red") + labs(label = list(x = "Давление в момент реанимации (SYS)", y = "Вероятность гибели", title = "Предсказания модели")) + theme_bw()



# Задание для самостоятельной работы в группах
# Зависит ливероятность встретить ящериц (PA) от степени изрезанности острова (PARATIO)
liz <- read.csv("data/polis.csv")



# Задание для самостоятельной работы в группах
# Как зависит вероятность долгосрочной безработицы от возраста?

library(catdata)
data("unemployment")
help(unemployment)

vignette("tree-unemployment")
