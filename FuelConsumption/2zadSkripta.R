path <- "C:/Users/Ivan/Desktop/sapP/AnabolicStatistics/FuelConsumption/car_specifications.csv"

require(nortest)
data <- read.csv(path)

data$fuel <- as.factor(data$fuel)
data$city.L.100km <- as.numeric(data$city.L.100km)
data$highway.L.100km <- as.numeric(data$highway.L.100km)
data$country <- as.factor(data$country)
data$continent <- as.factor(data$continent)

countries <- c("United States of America","France","Germany","Japan","Sweden") #IZBACILI ITALIJU I UK JER NEMA DOVOLJNO PODATAKA
continents <- unique(data$continent)

lillie.test(data$city.L.100km)
lillie.test(data$highway.L.100km)

for (country in countries) {
  print(lillie.test(data$city.L.100km[data$country == country]))
  print(lillie.test(data$highway.L.100km[data$country == country]))
}

for(continent in continents) {
  print(lillie.test((data$city.L.100km[data$continent == continent])))
  print(lillie.test((data$highway.L.100km[data$continent == continent])))
}


bartlett.test(data$city.L.100km ~ data$country)
bartlett.test(data)

boxplot(data$city.L.100km ~ data$country)
boxplot(data$highway.L.100km ~ data$country)

boxplot(data$city.L.100km ~ data$contient)
boxplot(data$highway.L.100km ~ data$continent)

model <- aov(data$city.L.100km ~ data$country)
summary(model)

model <- aov(data$highway.L.100km ~ data$country)
summary(model)

model <- aov(data$city.L.100km ~ data$continent)
summary(model)

model <- aov(data$highway.L.100km ~ data$continent)
summary(model)

