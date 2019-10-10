### IMPORT THE DATASET
cars<- read.csv("~/FINALE/Data/cars.csv")
View(cars)

### RENAME THE COLUMNS
names(cars) <-c("name.of.car","speed.of.car", "distance.of.car")

### Create Squared Speed Column - the reason for this is to plot the squared speed to better identify outliers. 
cars$Squared.Speed<- cars$speed.of.car*cars$speed.of.car

### REMOVE THE OUTLIER
outliers <- boxplot(cars$distance.of.car, plot=FALSE)$out
show(outliers)
cars <- cars[-which(cars$distance.of.car %in% outliers),]

### DATASET CLEANED
plot(cars$distance.of.car, cars$Squared.Speed)

set.seed(123)
### SET TRAINING AND TESTING
trainSize<-round(nrow(cars)*0.7) 
testSize<-nrow(cars)-trainSize
training_indices<-sample(seq_len(nrow(cars)),size =trainSize)
trainSet<-cars[training_indices,]
testSet<-cars[-training_indices,] 

### LINEAR REGRESSION
MOD <- lm(distance.of.car ~ speed.of.car, trainSet)
coefficients(MOD)

COEFF.INTERCEPT <- coefficients(MOD)[1]
COEFF.SLOPE <- coefficients(MOD)[2]

Prediction <- predict(MOD,testSet)

Prediction

### NON LINEAR REGRESSION TO FIT THE MODEL BETTER
MOD <- lm(distance.of.car ~ Squared.Speed, trainSet)
coefficients(MOD)

COEFF.INTERCEPT1 <- coefficients(MOD)[1]
COEFF.SLOPE1 <- coefficients(MOD)[2]

### FINAL PLOT
  library(ggplot2)
speed.of.car2 <- testSet$Squared.Speed
ggplot(testSet,
       aes(x = Squared.Speed, y = distance.of.car)) + 
  geom_point(color = "blue") +
 geom_abline(intercept = COEFF.INTERCEPT1, slope = COEFF.SLOPE1) + 
ggtitle("opopopop")



### Fit lm model using 10-fold CV: model
model <- train(
  price~., diamonds,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 10,
    verboseIter = TRUE
  )
)
