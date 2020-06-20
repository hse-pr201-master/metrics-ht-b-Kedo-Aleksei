library("lmtest")
library("mctest")
library("car")
library("plm")
library("forecast")
set.seed(123)

# №1  
# Считываем файл
setwd("D:/НИУ ВШЭ/3 курс/Эконометрика/Домашка")
Z <- read.csv("country_profile_variables.csv")
str(Z)
names(Z)
# Я хочу изучить, как связаны распространенность интернета и ВВП на душу населения. 
# Для этого в качестве зависимой переменной я беру количество индивидов, пользующихся интернетом на 100 жителей
# (Individuals.using.the.Internet..per.100.inhabitants.)

# №2
# Создаем матрицу регрессоров X
# В качестве непрерывных регрессоров я беру ВВП на душу населения, процент городского 
# населения и подписки на сотовую связь, потому что доступность интернета по логике зависит от
# доходов граждан, интернет более распространен в городах, и люди с интернетом имеют мобильную связь
# В качестве бинарных регрессоров я беру условие, что 14-ти летних больше 25% населения, потому что 
# молодежь более склонна к нововведениям. Взял именно 25, т.к. примерно одинаково стран, 
# в которых мх больше и в которых меньше 25%
# В качестве нелиныйных регрессоров я беру логарифм от территории в километрах квадратных, потому что, 
# по-моему, зависимость от размера страны есть, но она слабее чем линейная (от километров квадратных).

X <- Z[ , c("Individuals.using.the.Internet..per.100.inhabitants.",
            "GDP.per.capita..current.US..",
            "Urban.population....of.total.population.",
            "Mobile.cellular.subscriptions..per.100.inhabitants." )]

X$Mobile.cellular.subscriptions..per.100.inhabitants. <- as.numeric(X$Mobile.cellular.subscriptions..per.100.inhabitants.)
X$More.youth <- Z$Population.age.distribution..0.14...60..years.... > 25
X$Ln..Surface.area..km2. <- log(strtoi(Z$Surface.area..km2.))

# №3
# Графики
hist(X$GDP.per.capita..current.US..,
     breaks = 100,
     main = "Гистограмма: ВВП на душу населения",
     xlab = "ВВП на душу населения",
     ylab = "Количество наблюдений") # Присутствуют выбросы

hist(X$Urban.population....of.total.population.,
     breaks = 100,
     main = "Гистограмма: Процент городского населения",
     xlab = "Процент городского населения",
     ylab = "Количество наблюдений") # Есть наблюдения, равные 0, и наблюдения, приближенные и равные 100

Z %>% filter(Z$Urban.population....of.total.population. < 5) %>%
  select(country, Urban.population....of.total.population.)
Z %>% filter(Z$Urban.population....of.total.population. > 95) %>%
  select(country, Urban.population....of.total.population.)
# Решил не убирать эти наблюдения, они вполне валидны

hist(X$Mobile.cellular.subscriptions..per.100.inhabitants.,
     breaks = 100,
     main = "Гистограмма: Количество подписок на сотовую связь",
     xlab = "Количество подписок на сотовую связь",
     ylab = "Количество наблюдений") # Присутствуют выбросы и значения меньше 0

hist(as.numeric(X$More.youth),
     breaks = 100,
     main = "Гистограмма: Молодежь, да или нет?",
     xlab = "Молодежь, да или нет?",
     ylab = "Количество наблюдений") # Как и говорилось ранее, тут примерное равное количество нулей и единиц

hist(X$Ln..Surface.area..km2.,
     breaks = 100,
     main = "Гистограмма: Логарифм километров квадратных",
     xlab = "Логарифм километров квадратных",
     ylab = "Количество наблюдений") # Присутствуют нулевые значения

# Убираем строки с пустыми значениями (встречалось значение -99, его тоже считаем за пустое)
X <- X %>%
  filter(X$Mobile.cellular.subscriptions..per.100.inhabitants. > 0,
         X$Ln..Surface.area..km2. > 0,
         X$GDP.per.capita..current.US.. < 100000, # Удаляем выбросы
         X$Mobile.cellular.subscriptions..per.100.inhabitants. < 300) 
X
# Еще графики - проверяем, как изменились
hist(X$GDP.per.capita..current.US..,
     breaks = 100,
     main = "Гистограмма: ВВП на душу населения",
     xlab = "ВВП на душу населения",
     ylab = "Количество наблюдений")

hist(X$Mobile.cellular.subscriptions..per.100.inhabitants.,
     breaks = 100,
     main = "Гистограмма: Количество подписок на сотовую связь",
     xlab = "Количество подписок на сотовую связь",
     ylab = "Количество наблюдений")

hist(X$Ln..Surface.area..km2.,
     breaks = 100,
     main = "Гистограмма: Логарифм километров квадратных",
     xlab = "Логарифм километров квадратных",
     ylab = "Количество наблюдений")
# Графики стали выглядеть лучше без выбросов и отсутствующих наблюдений

# №4

model <- lm(X$Individuals.using.the.Internet..per.100.inhabitants. ~
            X$GDP.per.capita..current.US.. +
            X$Urban.population....of.total.population. +
            X$Mobile.cellular.subscriptions..per.100.inhabitants. +
            X$More.youth +
            X$Ln..Surface.area..km2.)

# Мультиколлинеорность
# vif
vif(model) # Значения каждой переменной меньше 2, скорее всего мультиколлинеарности нет

# Гетероскедастичность
# Тест Уайта (Бройша-Пагана)
bptest(model) # p-value = 0.1556 на уровне значимости 5% гипотеза о гомоскедастичности не отвергается 
# Тест Голдфелда-Квандта
gqtest(model, order.by = ~X$GDP.per.capita..current.US..) # p-value = 0.002005 На уровне значимости 5%
# гипотеза о гомоскедастичности отвергается

# Эндогенность

# Тест Хаусмана - источник: http://rstudio-pubs-static.s3.amazonaws.com/4752_487690a54aa84b54978aaef1d5483863.html
phtest(model, OLS2)
#

# №5 
# При помощи МНК
summary(model) # Значима только переменная Ln..Surface.area..km2.


# Часть 2 ARIMA 
# №1 
# Источник: http://www.fsb.miamioh.edu/lij14/690_s9.pdf
# ARIMA(1, 0, 0)
set.seed(123)
e1 = rnorm(120)
y1 = rep(0, 120)
y1[1] = e1[1]
for (t in 2:120) {
  y1[t] = 0.8 * y1[t-1] + e1[t]
}
# ARIMA(3, 0, 0)
set.seed(234)
e2 = rnorm(120)
y2 = rep(0, 120)
y2[1] = e2[1]
y2[2] = 0.1 * y2[1] + e2[2]
y2[3] = 0.1 * y2[2] + 0.2 * y2[1] + e2[3]
for (t in 4:120) {
  y2[t] = 0.1 * y2[t-1] + 0.2 * y2[t-2] + 0.3 * y2[t-3] + e2[t]
}
# ARIMA(0, 0, 2)
set.seed(345)
e3 = rnorm(120)
y3 = rep(0, 120)
y3[1] = e3[1]
for (t in 3:120) {
  y3[t] = y3[t-1] + e3[t] + 1.2 * e3[t-1] + 2 * e3[t-2] 
}
# Графики
ts.plot(y1)
ts.plot(y2)
ts.plot(y3)

# Первый ряд стационарен
# Второй ряд стационарен
# Третий ряд нестационарен

# №2
# ARIMA(0, 1, 2) Нет стац
set.seed(123)
e1 = rnorm(120)
y1 = rep(0, 120)
dy1 = rep(0, T)
dy1[1] = e1[1]
for (t in 2:120) {
  dy1[t] = dy1[t-1] + e1[t] + e1[t-1]
  y1[t] = dy[t]
}
# ARIMA(0, 0, 2) Есть стац
set.seed(345)
e3 = rnorm(120)
y3 = rep(0, 120)
for (t in 3:120) {
  y3[t] = e3[t] + e3[t-1]
  }
# ARIMA(3, 0, 0) Есть стац
set.seed(234)
e2 = rnorm(120)
y2 = rep(0, 120)
for (t in 4:120) {
  y2[t] = 0.1 * y2[t-1] + 0.2 * y2[t-2] + 0.3 * y2[t-3] + e2[t]
}
# Графики
ts.plot(y1)
ts.plot(y2)
ts.plot(y3)

# №3 Случайное блуждание
set.seed(567)
e2 = rnorm(120)
y2 = rep(0, 120)
y2[1] = e2[1]
for (t in 2:120) {
  y2[t] = y2[t-1] + e2[t]
}
ts.plot(y2)
# Случайное блуждание не имеет стационарных решений
