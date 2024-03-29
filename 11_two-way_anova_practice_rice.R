# Практикум по дисперсионному анализу
# Задание 1

# В датасете rice из пакета DAAG содержатся данные
# о росте риса (Perrine et al., 2001; Maindonald,
# Braun, 2015).

# Если не получится загрузить данные из пакета,
# скачайте файл rice с сайта курса.
# Краткое описание данных можно найти либо в справке,
# либо по ссылке
# https://www.rdocumentation.org/packages/DAAG/versions/1.24/topics/rice

# Постройте линейную модель, чтобы описать, как
# зависит сухая масса побегов риса от сорта,
# удобрений и взаимодействия этих факторов.

library(DAAG)
data(rice)
?rice

# Разведочный анализ #############################
# - все ли правильно открылось? какова структура объекта с данными?

# - являются ли дискретные предикторы факторами? Если нет - превратите их в факторы.

# - есть ли пропущенные значения?

# - есть ли подозрительные значениямя непрерывных переменных? (дот-плоты Кливленда)

# - каков объем выборки?

# - сколько наблюдений для каждого из уровней каждого дискретного фактора?
# (Т.е. сбалансированы ли объемы выборок?)


# Линейная модель ################################



# Диагностика модели #############################


# Дисперсионный анализ ###########################


# Пост хок тест ##################################


# График модели ##################################
