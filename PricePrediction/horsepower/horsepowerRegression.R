# Učitavanje podataka
data <- read.csv("C:/Users/itsth/Downloads/car_specifications.csv")

data$make <- as.factor(data$make)
data$aspiration <- as.factor(data$aspiration)
data$num_of_doors <- as.factor(data$num_of_doors)
data$body_style <- as.factor(data$body_style)
data$drive_wheels <- as.factor(data$drive_wheels)
data$engine_location <- as.factor(data$engine_location)
data$fuel <- as.factor(data$fuel)
data$country <- as.factor(data$country)
data$continent <- as.factor(data$continent)

# sa levels printam sve kategorije ovih varijabli
#levels(data$make)
#levels(data$aspiration)

model <- lm(price ~ horsepower, data = data)

summary(model)
plot(model)
