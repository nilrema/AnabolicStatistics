# Loading data
cardata = read.csv('./datasets/car_specifications.csv')
cardata = na.omit(cardata)  # Remove any rows with missing values

if(!dir.exists("graphs")) {
  dir.create("graphs")
}

# QQ-plots to check the normality of 'horsepower' distribution for different drive types
# QQ-plot for 'horsepower' for front-wheel drive (fwd)
png("graphs/qqplot_fwd.png")
qqnorm(cardata$horsepower[cardata$drive.wheels == 'fwd'], main = "Q-Q Plot for FWD Horsepower")
qqline(cardata$horsepower[cardata$drive.wheels == 'fwd'], col = "red")
dev.off()

# QQ-plot for 'horsepower' for rear-wheel drive (rwd)
png("graphs/qqplot_rwd.png")
qqnorm(cardata$horsepower[cardata$drive.wheels == 'rwd'], main = "Q-Q Plot for RWD Horsepower")
qqline(cardata$horsepower[cardata$drive.wheels == 'rwd'], col = "red")
dev.off()

# QQ-plot for 'horsepower' for four-wheel drive (4wd)
png("graphs/qqplot_4wd.png")
qqnorm(cardata$horsepower[cardata$drive.wheels == '4wd'], main = "Q-Q Plot for 4WD Horsepower")
qqline(cardata$horsepower[cardata$drive.wheels == '4wd'], col = "red")
dev.off()

# Load package for Lilliefors (Kolmogorov-Smirnov) normality test
require(nortest)

# Normality tests for 'horsepower' across different 'drive.wheels' categories
lillie.test(cardata$horsepower)
lillie.test(cardata$horsepower[cardata$drive.wheels == 'fwd'])
lillie.test(cardata$horsepower[cardata$drive.wheels == 'rwd'])
lillie.test(cardata$horsepower[cardata$drive.wheels == '4wd'])

# Kruskal-Wallis test to check for differences in 'horsepower' among the 3 drive wheel categories
kruskal.test(horsepower ~ drive.wheels, data = cardata)

# Histograms of 'horsepower' for each 'drive.wheels' category
png("graphs/histogram_fwd.png")  
hist(cardata$horsepower[cardata$drive.wheels=='fwd'], main="Histogram of FWD Horsepower", xlab="Horsepower")
dev.off()

png("graphs/histogram_rwd.png") 
hist(cardata$horsepower[cardata$drive.wheels=='rwd'], main="Histogram of RWD Horsepower", xlab="Horsepower")
dev.off()

png("graphs/histogram_4wd.png")  
hist(cardata$horsepower[cardata$drive.wheels=='4wd'], main="Histogram of 4WD Horsepower", xlab="Horsepower")
dev.off()

# Bartlett's test for homogeneity of variances across different 'drive.wheels' categories
bartlett.test(cardata$horsepower ~ cardata$drive.wheels)

# Variance calculations for 'horsepower' in each 'drive.wheels' category
var(cardata$horsepower[cardata$drive.wheels == 'fwd'])
var(cardata$horsepower[cardata$drive.wheels == 'rwd'])
var(cardata$horsepower[cardata$drive.wheels == '4wd'])

# Boxplot showing distribution of 'horsepower' across 'drive.wheels' categories
png("graphs/boxplot_horsepower.png")
boxplot(cardata$horsepower ~ cardata$drive.wheels, main="Boxplot of Horsepower by Drive Wheels", xlab="Drive Wheels", ylab="Horsepower", col="cyan")
dev.off()

# T-test for 'horsepower' between fwd and rwd
t.test(cardata$horsepower[cardata$drive.wheels == 'fwd'], 
       cardata$horsepower[cardata$drive.wheels == 'rwd'])

# T-test for 'horsepower' between rwd and 4wd
t.test(cardata$horsepower[cardata$drive.wheels == 'rwd'], 
       cardata$horsepower[cardata$drive.wheels == '4wd'])

# T-test for 'horsepower' between fwd and 4wd
t.test(cardata$horsepower[cardata$drive.wheels == 'fwd'], 
       cardata$horsepower[cardata$drive.wheels == '4wd'])

# Installing and Loading ggplot2 package
install.packages("ggplot2")
library(ggplot2)

# Density plot
density_plot <- ggplot(cardata, aes(x = horsepower, fill = drive.wheels)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Horsepower by Drive Wheels",
       x = "Horsepower",
       y = "Density") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")
ggsave("graphs/density_plot.png", plot = density_plot)

# Scatter plot
scatter_plot <- ggplot(cardata, aes(x = engine.size, y = horsepower)) +
  geom_point() +
  labs(title = "Scatter Plot of Engine Size vs Horsepower",
       x = "Engine Size",
       y = "Horsepower") +
  theme_minimal()
ggsave("graphs/scatter_plot.png", plot = scatter_plot)



