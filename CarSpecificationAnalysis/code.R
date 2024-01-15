# Loading data
cardata = read.csv('/Users/dominik/Desktop/CarSpecificationAnalysis/datasets/car_specifications.csv')
cardata = na.omit(cardata)

require(nortest)
lillie.test(cardata$horsepower)

lillie.test(cardata$horsepower[cardata$drive.wheels == 'fwd'])
lillie.test(cardata$horsepower[cardata$drive.wheels == 'rwd'])
lillie.test(cardata$horsepower[cardata$drive.wheels == '4wd'])


hist(cardata$horsepower[cardata$drive.wheels=='fwd'])
hist(cardata$horsepower[cardata$drive.wheels=='rwd'])
hist(cardata$horsepower[cardata$drive.wheels=='4wd'])

bartlett.test(cardata$horsepower ~ cardata$drive.wheels)

var(cardata$horsepower[cardata$drive.wheels == 'fwd'])
var(cardata$horsepower[cardata$drive.wheels == 'rwd'])
var(cardata$horsepower[cardata$drive.wheels == '4wd'])

boxplot(cardata$horsepower ~ cardata$drive.wheels)

a = aov(cardata$horsepower ~ cardata$drive.wheels)
summary(a)

model = lm(horsepower ~ drive.wheels, data = cardata)
summary(model)

anova(model)

a = aov(cardata$horsepower ~ cardata$drive.wheels)

# Performing a post-hoc test (Tukey's test is commonly used)
posthoc = TukeyHSD(a)

print(posthoc)

