path <- "car_specifications.csv"

data <- read.csv(path)

data$fuel <- as.factor(data$fuel)
data$city.L.100km <- as.numeric(data$city.L.100km)
data$highway.L.100km <- as.numeric(data$highway.L.100km)
data$country <- as.factor(data$country)
data$continent <- as.factor(data$continent)

model <- aov(data$city.L.100km ~ data$country)
summary(model)

model <- aov(data$highway.L.100km ~ data$country)
summary(model)

model <- aov(data$city.L.100km ~ data$continent)
summary(model)

model <- aov(data$highway.L.100km ~ data$continent)
summary(model)

