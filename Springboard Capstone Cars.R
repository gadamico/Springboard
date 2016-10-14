install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)

cars <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/
                   auto-mpg/auto-mpg.data")
colnames(cars) <- c("mpg", "cylinders", "displacement", "horsepower", "weight",
                     "acceleration", "model year", "origin", "car name")

horses <- as.vector(cars$horsepower)

for (i in 1:398){
  if (horses[i] == "?"){
    horses[i] <- "NA"
  }
}
cars$horsepower <- horses

carsmakemodel <- separate(cars, "car name", c("make", "model"),
                          sep = " ", extra = "merge")

manufs <- as.vector(carsmakemodel$make)

for (i in 1:398){
  if ((manufs[i] == "chevy") | (manufs[i] == "chevroelt")){
    manufs[i] <- "chevrolet"
  }
  if (manufs[i] == "maxda"){
    manufs[i] <- "mazda"
  }
  if (manufs[i] == "mercedes-benz"){
    manufs[i] <- "mercedes"
  }
  if (manufs[i] == "toyouta"){
    manufs[i] <- "toyota"
  }
  if ((manufs[i] == "vokswagen") | (manufs[i] == "vw")){
    manufs[i] <- "volkswagen"
  }
  if (manufs[i] == "datsun"){
    manufs[i] <- "nissan"
  }
}

carsmakemodel$make <- manufs

ggplot(carsmakemodel, aes(x = carsmakemodel$cylinders, y = carsmakemodel$mpg)) + geom_point()
ggplot(carsmakemodel, aes(x = carsmakemodel$`model year`, y = carsmakemodel$mpg)) + geom_point()
ggplot(carsmakemodel, aes(x = carsmakemodel$weight, y = carsmakemodel$mpg)) + geom_point()

fourcyls <- carsmakemodel[carsmakemodel$cylinders == "4",]
sixcyls <- carsmakemodel[carsmakemodel$cylinders == "6",]
eightcyls <- carsmakemodel[carsmakemodel$cylinders == "8",]
398 - (nrow(fourcyls) + nrow(sixcyls) + nrow(eightcyls))
oddcyls <- carsmakemodel[!((carsmakemodel$cylinders == "4"
                            | carsmakemodel$cylinders == "6") | carsmakemodel$cylinders == "8"),]

ggplot(fourcyls, aes(x = fourcyls$make, y = fourcyls$mpg)) + geom_boxplot()
ggplot(sixcyls, aes(x = sixcyls$make, y = sixcyls$mpg)) + geom_boxplot()
ggplot(eightcyls, aes(x = eightcyls$make, y = eightcyls$mpg)) + geom_boxplot()
ggplot(carsmakemodel, aes(x = carsmakemodel$make, y = carsmakemodel$mpg)) + geom_boxplot()

mean(fourcyls$mpg)
mean(sixcyls$mpg)
mean(eightcyls$mpg)

amc <- carsmakemodel[carsmakemodel$make == "amc",]
ggplot(amc, aes(x = amc$model, y = amc$mpg)) + geom_point(col = amc$`model year`)

model1 <- lm(mpg ~ cylinders + `model year` + weight + make, data = carsmakemodel)
summary(model1)

model2 <- lm(mpg ~ `model year` + weight + make, data = carsmakemodel)
summary(model2)

model3 <- lm(mpg ~ weight, data = carsmakemodel)
summary(model3)

model4 <- lm(mpg ~ `model year`, data = carsmakemodel)
summary(model4)

set.seed(12)

train_part <- sample(seq_len(nrow(carsmakemodel)), size = 300)

trainset <- carsmakemodel[train_part,]
testset <- carsmakemodel[-train_part,]
trainmodel <- lm(mpg ~ weight, data = trainset)
summary(trainmodel)
MPGest <- testset$weight * -0.0062524 + 40.3879027
ggplot(testset, aes(x = testset$make)) + geom_line(aes(y = testset$mpg), col = "black") +
  geom_line(aes(y = MPGest), col = "red")

ggplot(testset, aes(x = testset$make, y = (testset$mpg - MPGest)/testset$mpg)) + geom_point()

ggplot(amc, aes(x = amc$`model year`, y = amc$mpg)) + geom_point()

honda <- carsmakemodel[carsmakemodel$make == "honda",]
ggplot(honda, aes(x = honda$`model year`, y = honda$mpg)) + geom_point()

oldsmobile <- carsmakemodel[carsmakemodel$make == "oldsmobile",]
ggplot(oldsmobile, aes(x = oldsmobile$`model year`, y = oldsmobile$mpg)) + geom_point()

datsun <- carsmakemodel[carsmakemodel$make == "datsun",]
renault <- carsmakemodel[carsmakemodel$make == "renault",]
volkswagen <- carsmakemodel[carsmakemodel$make == "volkswagen",]

plot(renault$model, renault$mpg)

mean(renault$mpg)

cadillac <- carsmakemodel[carsmakemodel$make == "cadillac",]


