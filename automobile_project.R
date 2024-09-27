datasetfull <- read.csv("C:/Users/Sasha/OneDrive - purdue.edu/Desktop/PURDUE UNIVERSITY/Courses 2020-24/courses_Fall2023/STAT 512/automobile.data",header = FALSE)
names(datasetfull) <- c("symboling","normalized-losses","make","fuel-type","aspiration","num-of-doors","body-style","drive-wheels","engine-location","wheel-base","length","width","height","curb-weight","engine-type","num-of-cylinders","engine-size","fuel-system","bore","stroke","compression-ratio","horsepower","peak-rpm","city-mpg","highway-mpg","price")
View(datasetfull)
datasetfull<- datasetfull[datasetfull$price != '?', , drop = FALSE]
datasetfull$price[datasetfull$price == '?'] <- 0
datasetfull$price <- as.numeric(datasetfull$price)


# Research Question 1 Code #
X1<-datasetfull$height
X2<-datasetfull$width
X3<-datasetfull$length
Y<-datasetfull$price

#OLM model
answer<-lm(Y~X1 + X2 + X3, data=datasetfull)
summary(answer)

#Graphs and Influence Points
anova(answer)

library(car)
avPlots(answer)

influencePlot(answer)

student<-rstudent(answer)

library(lmtest)
X<-cbind(X1,X2,X3,Y)

#Tests
shapiro.test(X)

bptest(answer)

vif(answer)

install.packages("MASS")
library(MASS)
shapiro.test(residuals(answer))

bcmle <- boxcox(answer, lambda = seq(-3, 3, by = 0.1))  # Box-Cox transformation for lambda selection

optimal_lambda <- bcmle$x[which.max(bcmle$y)]  # Identifying the optimal lambda value for transformation

datasetfull$new_price = datasetfull$price ** optimal_lambda
answer2 <- lm(new_price~X1+X2+X3, datasetfull)
summary(answer2)

shapiro.test(residuals(answer2))

#exploratory data analysis:
pairs(datasetfull[, c("height", "width", "length")])

plot(answer2)


summary(answer2$residuals)

dffits(answer2)
dfbetas(answer2)
dfbetasPlots(answer2)

cooksd <- cooks.distance(answer2)
influential_points <- which(cooksd > 4/length(answer$residuals))
print(influential_points)
cooks.distance(answer2)


plot(answer2, pch=18, col="red", which=c(4))


# added variable for linearity
library(car)
avPlots(answer2)

#influencial points
lm.influence(answer2)$hat
influencePlot(answer2)

#Robust Regression
library(MASS)
model<-rlm(new_price~X1+X2+X3, datasetfull, psi=psi.bisquare)
summary(model)

#Bootstrap
fit_robust_regression <- function(data, indices, maxit=100) {
  data <- data[indices, ]
  model <- rlm(new_price~X1+X2+X3, data = datasetfull, maxit=maxit)
  coefficients(model)
}

library(boot)
boot_results <- boot(data = datasetfull, statistic = fit_robust_regression, R = 1000, maxit=100)
boot_results

#k-fold

# Research Question 2 #

install.packages("vctrs")
library(vctrs)
install.packages("caret")
library(caret)
train.control <- trainControl(method = "cv", number = 10)
step.mod <- train(new_price~X1+X2+X3, 
                  data = datasetfull, 
                  method = "leapBackward",
                  tuneGrid = data.frame(nvmax = 6),
                  trControl = train.control)

# Displaying the results of feature selection 
step.mod$results

datasetfull <- read.csv("C:/Users/sasha/Downloads/automobile/imports-85.data",header = FALSE)
names(datasetfull) <- c("symboling","normalized-losses","make","fuel-type","aspiration","num-of-doors","body-style","drive-wheels","engine-location","wheel-base","length","width","height","curb-weight","engine-type","num-of-cylinders","engine-size","fuel-system","bore","stroke","compression-ratio","horsepower","peak-rpm","city-mpg","highway-mpg","price")
datasetq2 <- read.csv("C:/Users/sasha/Downloads/automobile/data.csv",header = FALSE)
names(datasetq2) <- c("engine_type","num_of_cylinders","engine_size","price")

datasetq2 <- datasetq2[datasetq2$price != '?', , drop = FALSE]


library(dplyr)

# Define a mapping for replacement "eight" to 8
cylinder_mapping <- c("eight" = 8, "five" = 5, "four" = 4, "six" = 6, "three" = 3, "twelve" = 12, "two" = 2)

# Replace values in the 'num-of-cylinders' column using dplyr
datasetq2 <- datasetq2 %>%
  mutate(`num_of_cylinders` = ifelse(`num_of_cylinders` %in% names(cylinder_mapping), 
                                     cylinder_mapping[`num_of_cylinders`], 
                                     as.integer(`num_of_cylinders`)))
datasetq2$price <- as.numeric(datasetq2$price)

# Exploratory Data Analysis: Scatterplot matrix
pairs(datasetq2[, c("num_of_cylinders", "engine_size", "price")])


# Example boxplot by engine type
boxplot(price ~ num_of_cylinders, data = datasetq2)

boxplot(price~engine_size, data=datasetq2)


model <- lm(price ~ num_of_cylinders + engine_size + engine_type, data = datasetq2)
summary(model)

Call:
  lm(formula = price ~ num_of_cylinders + engine_size + engine_type, 
     data = datasetq2)
   
plot(model)



summary(model$residuals)

# Check p value
anova_result <- anova(model)
print(anova_result)

# Use the bptest function to perform the Breusch-Pagan test
bp_test <- bptest(model)


# Effect of Multicollnearity on sum of squares
anova(lm(imports$price~imports$num_of_cylinders,imports))

# Multicollinearity 
install.packages("car")
library(car)
vif_values <- vif(model)
print(vif_values)

# Check for heteroscedasticity
bp_test <- bptest(model)

wls_model <- lm(price ~ num_of_cylinders + engine_size + engine_type, data = datasetq2, weights = weights)

summary(wls_model)

# Cook's distance
cooksd <- cooks.distance(model)
influential_points <- which(cooksd > 4/length(model$residuals))
print(influential_points)

# Robust Regression Model: Remedial measures for influential cases

library(MASS)
new_model <- rlm(price ~ num_of_cylinders + engine_size + engine_type, data = datasetq2, psi = psi.bisquare)

summary(new_model)

cooksd <- cooks.distance(new_model)
influential_points <- which(cooksd > 4/length(new_model$residuals))

library(lmtest)
coeftest(new_model)

Bootstrap:
  library(boot)

# Define the bootstrap function
bs <- function(formula, data, indices) {
  d <- data[indices, ]
  fit <- lm(formula, data = d)
  return(coef(fit))
}

# Sample data
set.seed(123)
datasetq2 <- data.frame(
  price = rnorm(100),
  num_of_cylinders = rnorm(100),
  engine_size = rnorm(100),
  engine_type = sample(c("dohc", "dohcv", "l", "ohc", "ohcf", "ohcv", "rotor"), 100, replace = TRUE)
)

# Perform bootstrap resampling
results <- boot(
  data = datasetq2,
  statistic = bs,
  R = 1000,
  formula = price ~ num_of_cylinders + engine_size + engine_type
)

# Print the bootstrap results
print(results)

boot_ci <- boot.ci(results, type = "bca")
print(boot_ci)

# Define formula
formula <- price ~ num_of_cylinders + engine_size + engine_type

# Number of folds
k <- 5

# Create indices for k-fold cross-validation
folds <- cut(seq(1, nrow(datasetq2)), breaks = k, labels = FALSE)

# Perform k-fold cross-validation
cv_results <- sapply(1:k, function(i) {
  test_indices <- which(folds == i)
  train_data <- datasetq2[-test_indices, ]
  test_data <- datasetq2[test_indices, ]
  
  fit <- rlm(formula, data = train_data)
  
  predictions <- predict(fit, newdata = test_data)
  r_squared <- 1 - sum((test_data$price - predictions)^2) / sum((test_data$price - mean(test_data$price))^2)
  
  return(r_squared)
})

# Research Question 3 # 

library(car)
library(ALSM)
library(olsrr)
library(lmtest)
library(MASS)
dataset3 <- datasetfull[c("price", "city-mpg","highway-mpg", "compression-ratio", "curb-weight")]
rownames(dataset3) <- 1:nrow(dataset3)
model3 <- lm(price ~ `city-mpg` + `highway-mpg` + `compression-ratio` + `curb-weight`, data = dataset3)
pairs(dataset3[, c("city-mpg","highway-mpg", "compression-ratio", "curb-weight")])
summary(model3)
anova(model3)
vif(model3)


# Shapiro, BP, Box-cox transformation
shapiro.test(residuals(model3))
bptest(model3)
bcmle <- boxcox(model3, lambda = seq(-3,3,by = 0.1))
optimal_lambda <- bcmle$x[which.max(bcmle$y)]
dataset3$new_price = dataset3$price ** optimal_lambda


# Performed Shaprio and BP test and found out that 
# it fails both. Decided to use box-cox to transform price 
# into new_price by raising price to the power of optimal_lambda

model3 <- lm(new_price ~ `city-mpg` + `highway-mpg` + `compression-ratio` + `curb-weight`, data = dataset3)
summary(model3)
anova(model3)
shapiro.test(residuals(model3))
bptest(model3)
vif(model3)

# After the transformation, the new model with new_price 
# passes both shapiro and bp test. VIF stays the same.

# Stepwise
full_model <- lm(dataset3$new_price~.,data = dataset3)
summary(full_model)
ols_mallows_cp(model3,full_model)
stepresult <- step(model3, method = "both", trace = 1)
summary(stepresult)

# Influential points
cooksTest <- cooks.distance(model3)
infPointTest <- which(cooksTest > 4/length(model3$residuals))
print(unique(infPointTest))

dffitsVal <- 2 * sqrt(5/205)
unique(which((abs(dffits(model3)) > dffitsVal)))
influencePlot(model3)
plot(model3, pch=18, col="red", which=c(4))

infPointTest <- which(cooksTest > 4/length(model3$residuals))
print(unique(infPointTest))
dffitsVal <- 2 * sqrt(5/205)
unique(which((abs(dffits(model3)) > dffitsVal)))

# WLS
model3 <- lm(new_price ~ `city-mpg` + `highway-mpg` + `compression-ratio` + `curb-weight`, data = dataset3)
wts1 <- 1/fitted(lm(abs(residuals(model3))~`city-mpg` + `highway-mpg` + `compression-ratio` + `curb-weight`, dataset3))^2
modWLS1 <- lm(new_price ~ `city-mpg` + `highway-mpg` + `compression-ratio` + `curb-weight`, weight=wts1, data = dataset3)
summary(modWLS1)
par(mfrow = c(2,2))
plot(modWLS1)
par(mfrow = c(1,1))

# WLS influential points
cooksTestWLS <- cooks.distance(modWLS1)
infPointTestWLS <- which(cooksTestWLS > 4/length(modWLS1$residuals))
print(unique(infPointTestWLS))

dffitsVal <- 2 * sqrt(5/204)
unique(which((abs(dffits(modWLS1)) > dffitsVal)))
influencePlot(modWLS1)
plot(modWLS1, pch=18, col="red", which=c(4))

infPointTestWLS <- which(cooksTestWLS > 4/length(modWLS1$residuals))
print(unique(infPointTestWLS))
dffitsVal <- 2 * sqrt(5/204)
unique(which((abs(dffits(modWLS1)) > dffitsVal)))

# Ridge Regression
library(lmridge)
lmMod2 <- lmridge(new_price ~ `city-mpg` + `highway-mpg` + `compression-ratio` + `curb-weight`, data = as.data.frame(dataset3), K=seq(0,1,0.02))
plot(lmMod2)
vif(lmMod2)
#summary(lmMod2)


# Robust Regression
library(MASS)
dataset3names <- dataset3
names(dataset3names) <- c("new_price","city_mpg","highway_mpg","compression_ratio","curb_weight")

robustRegMod <- rlm(new_price~city_mpg+highway_mpg+compression_ratio+curb_weight, data = dataset3names, psi = psi.bisquare, maxit = 100)
summary(robustRegMod)


# WLS comparion and bootstrapping
par(mfrow = c(2,2))
plot(new_price ~ `city-mpg` + `highway-mpg` + `compression-ratio` + `curb-weight`, data=dataset3, main="Model Comparison")
lines(dataset3$new_price, predict(model3), col="red", lwd=2, lty=1)
lines(dataset3$new_price, predict(modWLS1), col="blue", lwd=2, lty=2)
legend(20, 100, legend=c("OLS","WLS"), col=c("red","blue"),lty=1:2)
par(mfrow = c(1,1))

library(boot)
library(lmridge)
library(MASS)

# return coefficients
boot.wlscoef <- function(data, indices, maxit=20) {
  data <- data[indices,]
  colnames(data)<-c("new_price","city-mpg","highway-mpg","compression-ratio","curb-weight")
  data.mod<-lm(new_price ~ `city-mpg` + `highway-mpg` + `compression-ratio` + `curb-weight`, data)
  wts1 <- 1/fitted(lm(abs(residuals(data.mod))~`city-mpg` + `highway-mpg` + `compression-ratio` + `curb-weight`, data))^2
  data.mod2<-lm(new_price ~ `city-mpg` + `highway-mpg` + `compression-ratio` + `curb-weight`, weight=wts1, data)
  return(coef(data.mod2))
}

bootModel <- boot(data=dataset3, statistic = boot.wlscoef, R=100, maxit=20)

# view results
bootModel
# see plot of coefficients
plot(bootModel, index=2)
# 95% confidence intervals
boot.ci(bootModel, type="perc", index=2)

# Hypothesis Test
library(lmtest)
coeftest(modWLS1)

coeftest(modWLS1)

# K folds
library(caret)
train.control <- trainControl(method = "cv", number = 10)
mod3KFolds <- train(new_price~`city-mpg` + `highway-mpg` + `compression-ratio` + `curb-weight`,
                    data = dataset3,
                    method = "leapBackward",
                    tuneGrid = data.frame(nvmax = 6),
                    trControl = train.control)

# Displaying the results of feature selection
mod3KFolds$results