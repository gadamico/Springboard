## Let's start by installing and loading some useful packages.

install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)


## Now we load in the dataset.  Note that we use 'read.table()' since this is a .data file.

cars <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/
                   auto-mpg/auto-mpg.data")

## It will be useful to have names for the columns.

colnames(cars) <- c("mpg", "cylinders", "displacement", "horsepower", "weight",
                     "acceleration", "model year", "origin", "car name")

## Now let's get rid of the '?'s that appear in a few places in the 'horsepower' column.

horses <- as.vector(cars$horsepower)

for (i in 1:398){
  if (horses[i] == "?"){
    horses[i] <- "NA"
  }
}
cars$horsepower <- horses

## The 'car name' column would be more useful if we could have access to the manufacturer
## name and the model name separately.  Note the use of 'extra = "merge"' in the function
## call.

carsmakemodel <- separate(cars, "car name", c("make", "model"),
                          sep = " ", extra = "merge")

## It's high time we did something about those misspellings of the car names.

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

## Since there are so many different manufacturers, it will also prove helpful to group
## them.  We can do this most intuitively by country of origin.

carsmakemodel[, "Country"] <- 1:398

for (i in 1:398){
  if (carsmakemodel[i, 9] == "nissan" | carsmakemodel[i, 9] == "subaru" |
            carsmakemodel[i, 9] == "honda" | carsmakemodel[i, 9] == "mazda" |
            carsmakemodel[i, 9] == "toyota"){
      carsmakemodel[i, 11] <- "Japan"}
  
  else if (carsmakemodel[i, 9] == "audi" | carsmakemodel[i, 9] == "bmw" |
            carsmakemodel[i, 9] == "mercedes" | carsmakemodel[i, 9] == "opel" |
            carsmakemodel[i, 9] == "volkswagen"){
      carsmakemodel[i, 11] <- "Germany"}
  
  else if (carsmakemodel[i, 9] == "saab" | carsmakemodel[i, 9] == "volvo"){
      carsmakemodel[i, 11] <- "Sweden"}
  
  else if (carsmakemodel[i, 9] == "amc" | carsmakemodel[i, 9] == "buick" |
            carsmakemodel[i, 9] == "cadillac" | carsmakemodel[i, 9] == "chevrolet" |
            carsmakemodel[i, 9] == "chrysler" | carsmakemodel[i, 9] == "dodge" |
            carsmakemodel[i, 9] == "ford" | carsmakemodel[i, 9] == "mercury" |
            carsmakemodel[i, 9] == "oldsmobile" | carsmakemodel[i, 9] == "plymouth" |
            carsmakemodel[i, 9] == "pontiac"){
      carsmakemodel[i, 11] <- "USA"}
  
  else if (carsmakemodel[i, 9] == "peugeot" | carsmakemodel[i, 9] == "renault"){
      carsmakemodel[i, 11] <- "France"}

  else {carsmakemodel[i, 11] <- "XX - Other"}
  }


## Time to do some exploring!  Let's make a few preliminary plots of MPG vs. some other
## variables to see if we can gain any insight into the general picture.

ggplot(carsmakemodel, aes(x = carsmakemodel$cylinders, y = carsmakemodel$mpg)) +
  geom_point(aes(color = carsmakemodel$Country))
ggplot(carsmakemodel, aes(x = carsmakemodel$`model year`, y = carsmakemodel$mpg)) +
  geom_point(aes(color = carsmakemodel$Country))
ggplot(carsmakemodel, aes(x = carsmakemodel$weight, y = carsmakemodel$mpg)) +
  geom_point(aes(color = carsmakemodel$Country))
ggplot(carsmakemodel, aes(x = carsmakemodel$acceleration, y = carsmakemodel$mpg)) +
  geom_point(aes(color = carsmakemodel$Country))


## Let's plot MPG means by country.

usamean <- mean(carsmakemodel[carsmakemodel$Country == "USA", 1])
germanymean <- mean(carsmakemodel[carsmakemodel$Country == "Germany", 1])
francemean <- mean(carsmakemodel[carsmakemodel$Country == "France", 1])
japanmean <- mean(carsmakemodel[carsmakemodel$Country == "Japan", 1])
swedenmean <- mean(carsmakemodel[carsmakemodel$Country == "Sweden", 1])

means <- c(francemean, germanymean, japanmean, swedenmean, usamean)
countries <- 1:5
countrymeans <- as.data.frame(cbind(means, countries))
countrymeans$countries <- c("France", "Germany", "Japan", "Sweden", "USA")
ggplot(countrymeans, aes(x = countries, y = means, fill = countries)) +
  geom_bar(stat = "identity")


## Let's now look at weight means by country.

usawtmean <- mean(carsmakemodel[carsmakemodel$Country == "USA", 5])
germanywtmean <- mean(carsmakemodel[carsmakemodel$Country == "Germany", 5])
francewtmean <- mean(carsmakemodel[carsmakemodel$Country == "France", 5])
japanwtmean <- mean(carsmakemodel[carsmakemodel$Country == "Japan", 5])
swedenwtmean <- mean(carsmakemodel[carsmakemodel$Country == "Sweden", 5])

wtmeans <- c(francewtmean, germanywtmean, japanwtmean, swedenwtmean, usawtmean)
wtcountries <- 1:5
countrywtmeans <- as.data.frame(cbind(wtmeans, wtcountries))
countrywtmeans$wtcountries <- c("France", "Germany", "Japan", "Sweden", "USA")
ggplot(countrywtmeans, aes(x = wtcountries, y = wtmeans, fill = wtcountries)) +
  geom_bar(stat = "identity")

## The first plot suggests that it will be useful to have access to the cars as sorted by
## the number of cylinders in the engines.  By far the most common engine types are the
## straight 4, the V6, and the V8.  But a few cars have 3 or 5 cylinders.

fourcyls <- carsmakemodel[carsmakemodel$cylinders == "4",]
sixcyls <- carsmakemodel[carsmakemodel$cylinders == "6",]
eightcyls <- carsmakemodel[carsmakemodel$cylinders == "8",]
398 - (nrow(fourcyls) + nrow(sixcyls) + nrow(eightcyls))

## Since there are only seven of these 3- and 5-cylinder cars, we'll put them together in
## one group.

oddcyls <- carsmakemodel[!((carsmakemodel$cylinders == "4"
                            | carsmakemodel$cylinders == "6") | carsmakemodel$cylinders == "8"),]

## Let's make some plots of MPG vs. make.

ggplot(fourcyls, aes(x = fourcyls$make, y = fourcyls$mpg)) + geom_boxplot()
ggplot(fourcyls, aes(x = fourcyls$Country, y = fourcyls$mpg)) + geom_boxplot()
ggplot(sixcyls, aes(x = sixcyls$make, y = sixcyls$mpg)) + geom_boxplot()
ggplot(sixcyls, aes(x = sixcyls$Country, y = sixcyls$mpg)) + geom_boxplot()
ggplot(eightcyls, aes(x = eightcyls$make, y = eightcyls$mpg)) + geom_boxplot()
ggplot(eightcyls, aes(x = eightcyls$Country, y = eightcyls$mpg)) + geom_boxplot()
ggplot(oddcyls, aes(x = oddcyls$make, y = oddcyls$mpg)) + geom_boxplot()
ggplot(oddcyls, aes(x = oddcyls$Country, y = oddcyls$mpg)) + geom_boxplot()
ggplot(carsmakemodel, aes(x = carsmakemodel$make, y = carsmakemodel$mpg)) + geom_boxplot()
ggplot(carsmakemodel, aes(x = carsmakemodel$Country, y = carsmakemodel$mpg)) + geom_boxplot()

mean(fourcyls$mpg)
mean(sixcyls$mpg)
mean(eightcyls$mpg)


## We can also investigate whether there are any significant _intra-manufacturer_
## differences in MPG across the various models of a single make.
## To this end it will be helpful to carve up subsets of carsmakemodel according
## to manufacturer.  I here omit the few
## manufacturers with very few representatives, such as 'hi' and 'triumph'.  The major
## standouts for high MPG will prove to be Honda, Nissan, Renault, and Volkswagen.

amc <- carsmakemodel[carsmakemodel$make == "amc",]
audi <- carsmakemodel[carsmakemodel$make == "audi",]
bmw <- carsmakemodel[carsmakemodel$make == "bmw",]
buick <- carsmakemodel[carsmakemodel$make == "buick",]
cadillac <- carsmakemodel[carsmakemodel$make == "cadillac",]
chevrolet <- carsmakemodel[carsmakemodel$make == "chevrolet",]
chrysler <- carsmakemodel[carsmakemodel$make == "chrysler",]
dodge <- carsmakemodel[carsmakemodel$make == "dodge",]
fiat <- carsmakemodel[carsmakemodel$make == "fiat",]
ford <- carsmakemodel[carsmakemodel$make == "ford",]
honda <- carsmakemodel[carsmakemodel$make == "honda",]
mazda <- carsmakemodel[carsmakemodel$make == "mazda",]
mercedes <- carsmakemodel[carsmakemodel$make == "mercedes",]
mercury <- carsmakemodel[carsmakemodel$make == "mercury",]
nissan <- carsmakemodel[carsmakemodel$make == "nissan",]
oldsmobile <- carsmakemodel[carsmakemodel$make == "oldsmobile",]
opel <- carsmakemodel[carsmakemodel$make == "opel",]
peugeot <- carsmakemodel[carsmakemodel$make == "peugeot",]
plymouth <- carsmakemodel[carsmakemodel$make == "plymouth",]
pontiac <- carsmakemodel[carsmakemodel$make == "pontiac",]
renault <- carsmakemodel[carsmakemodel$make == "renault",]
saab <- carsmakemodel[carsmakemodel$make == "saab",]
subaru <- carsmakemodel[carsmakemodel$make == "subaru",]
toyota <- carsmakemodel[carsmakemodel$make == "toyota",]
volkswagen <- carsmakemodel[carsmakemodel$make == "volkswagen",]
volvo <- carsmakemodel[carsmakemodel$make == "volvo",]


## Just some shortening for the sake of better visualizations.

chrysler[2, 10] <- "new yorker"
chrysler[4, 10] <- "lebaron t/c"
chrysler[6, 10] <- "lebaron medal"

honda[2, 10] <- "civ cvcc"
honda[4, 10] <- "acc cvcc"
honda[5, 10] <- "civ cvcc"
honda[6, 10] <- "acc lx"
honda[7, 10] <- "civ 1500 gl"
honda[9, 10] <- "civ 1300"
honda[13, 10] <- "civic"

nissan[1, 10] <- "510"
nissan[2, 10] <- "510"
nissan[4, 10] <- "510"
nissan[9, 10] <- "b210"
nissan[10, 10] <- "f10hb"
nissan[12, 10] <- "b210"
nissan[14, 10] <- "200"
nissan[17, 10] <- "510hb"
nissan[19, 10] <- "280"
nissan[20, 10] <- "210"
nissan[21, 10] <- "200"
nissan[22, 10] <- "810"
nissan[23, 10] <- "stanza"
nissan[24, 10] <- "310"

volkswagen[1, 10] <- "1131"
volkswagen[2, 10] <- "111"
volkswagen[3, 10] <- "3"
volkswagen[4, 10] <- "411"
volkswagen[5, 10] <- "sbtl"
volkswagen[6, 10] <- "dshr"
volkswagen[7, 10] <- "dshr"
volkswagen[8, 10] <- "rbt"
volkswagen[9, 10] <- "rbt"
volkswagen[10, 10] <- "rbt"
volkswagen[11, 10] <- "rbtc"
volkswagen[12, 10] <- "dshr"
volkswagen[13, 10] <- "rbtcd"
volkswagen[14, 10] <- "srco"
volkswagen[15, 10] <- "rbtc"
volkswagen[16, 10] <- "rbt"
volkswagen[17, 10] <- "rbtcd"
volkswagen[18, 10] <- "dshrd"
volkswagen[19, 10] <- "rbt"
volkswagen[21, 10] <- "rbtl"

pontiac[1, 10] <- "ctlna"
pontiac[2, 10] <- "ctlnab"
pontiac[3, 10] <- "sfri"
pontiac[4, 10] <- "fibrd"
pontiac[5, 10] <- "ctlna"
pontiac[6, 10] <- "gprix"
pontiac[7, 10] <- "ctlna"
pontiac[9, 10] <- "vtra"
pontiac[10, 10] <- "gprix"
pontiac[11, 10] <- "snbrd"
pontiac[12, 10] <- "phx"
pontiac[13, 10] <- "lmns"
pontiac[14, 10] <- "phx"
pontiac[15, 10] <- "j2000"
pontiac[16, 10] <- "phx"


## Plots of MPG by model and by model year.

ggplot(amc, aes(x = amc$model, y = amc$mpg)) + geom_point()
ggplot(amc, aes(x = amc$`model year`, y = amc$mpg)) + geom_point()
ggplot(audi, aes(x = audi$model, y = audi$mpg)) + geom_point()
ggplot(audi, aes(x = audi$`model year`, y = audi$mpg)) + geom_point()
ggplot(bmw, aes(x = bmw$model, y = bmw$mpg)) + geom_point()
ggplot(bmw, aes(x = bmw$`model year`, y = bmw$mpg)) + geom_point()
ggplot(buick, aes(x = buick$model, y = buick$mpg)) + geom_point()
ggplot(buick, aes(x = buick$`model year`, y = buick$mpg)) + geom_point()
ggplot(cadillac, aes(x = cadillacc$model, y = cadillac$mpg)) + geom_point()
ggplot(cadillac, aes(x = cadillac$`model year`, y = cadillac$mpg)) + geom_point()
ggplot(chevrolet, aes(x = chevrolet$model, y = chevrolet$mpg)) + geom_point()
ggplot(chevrolet, aes(x = chevrolet$`model year`, y = chevrolet$mpg)) + geom_point()
ggplot(chrysler, aes(x = chrysler$model, y = chrysler$mpg)) + geom_point()
ggplot(chrysler, aes(x = chrysler$`model year`, y = chrysler$mpg)) + geom_point()
ggplot(dodge, aes(x = dodge$model, y = dodge$mpg)) + geom_point()
ggplot(dodge, aes(x = dodge$`model year`, y = dodge$mpg)) + geom_point()
ggplot(fiat, aes(x = fiat$model, y = fiat$mpg)) + geom_point()
ggplot(fiat, aes(x = fiat$`model year`, y = fiat$mpg)) + geom_point()
ggplot(ford, aes(x = ford$model, y = ford$mpg)) + geom_point()
ggplot(ford, aes(x = ford$`model year`, y = ford$mpg)) + geom_point()
ggplot(honda, aes(x = honda$model, y = honda$mpg)) + geom_point()
ggplot(honda, aes(x = honda$`model year`, y = honda$mpg)) + geom_point()
ggplot(mazda, aes(x = mazda$model, y = mazda$mpg)) + geom_point()
ggplot(mazda, aes(x = mazda$`model year`, y = mazda$mpg)) + geom_point()
ggplot(mercedes, aes(x = mercedes$model, y = mercedes$mpg)) + geom_point()
ggplot(mercedes, aes(x = mercedes$`model year`, y = mercedes$mpg)) + geom_point()
ggplot(mercury, aes(x = mercury$model, y = mercury$mpg)) + geom_point()
ggplot(mercury, aes(x = mercury$`model year`, y = mercury$mpg)) + geom_point()
ggplot(nissan, aes(x = nissan$model, y = nissan$mpg)) + geom_point()
ggplot(nissan, aes(x = nissan$`model year`, y = nissan$mpg)) + geom_point()
ggplot(oldsmobile, aes(x = oldsmobile$model, y = oldsmobile$mpg)) + geom_point()
ggplot(oldsmobile, aes(x = oldsmobile$`model year`, y = oldsmobile$mpg)) + geom_point()
ggplot(opel, aes(x = opel$model, y = opel$mpg)) + geom_point()
ggplot(opel, aes(x = opel$`model year`, y = opel$mpg)) + geom_point()
ggplot(peugeot, aes(x = peugeot$model, y = peugeot$mpg)) + geom_point()
ggplot(peugeot, aes(x = peugeot$`model year`, y = peugeot$mpg)) + geom_point()
ggplot(plymouth, aes(x = plymouth$model, y = plymouth$mpg)) + geom_point()
ggplot(plymouth, aes(x = plymouth$`model year`, y = plymouth$mpg)) + geom_point()
ggplot(pontiac, aes(x = pontiac$model, y = pontiac$mpg)) + geom_point()
ggplot(pontiac, aes(x = pontiac$`model year`, y = pontiac$mpg)) + geom_point()
ggplot(renault, aes(x = renault$model, y = renault$mpg)) + geom_point()
ggplot(renault, aes(x = renault$`model year`, y = renault$mpg)) + geom_point()
ggplot(saab, aes(x = saab$model, y = saab$mpg)) + geom_point()
ggplot(saab, aes(x = saab$`model year`, y = saab$mpg)) + geom_point()
ggplot(subaru, aes(x = subaru$model, y = subaru$mpg)) + geom_point()
ggplot(subaru, aes(x = subaru$`model year`, y = subaru$mpg)) + geom_point()
ggplot(toyota, aes(x = toyota$model, y = toyota$mpg)) + geom_point()
ggplot(toyota, aes(x = toyota$`model year`, y = toyota$mpg)) + geom_point()
ggplot(volkswagen, aes(x = volkswagen$model, y = volkswagen$mpg)) + geom_point()
ggplot(volkswagen, aes(x = volkswagen$`model year`, y = volkswagen$mpg)) + geom_point()
ggplot(volvo, aes(x = volvo$model, y = volvo$mpg)) + geom_point()
ggplot(volvo, aes(x = volvo$`model year`, y = volvo$mpg)) + geom_point()


## Let's make some subsets of our dataset according to country of origin.

france <- carsmakemodel[carsmakemodel$Country == "France",]

germany <- carsmakemodel[carsmakemodel$Country == "Germany",]

japan <- carsmakemodel[carsmakemodel$Country == "Japan",]

sweden <- carsmakemodel[carsmakemodel$Country == "Sweden",]

usa <- carsmakemodel[carsmakemodel$Country == "USA",]

ggplot(france, aes(x = france$make, y = france$mpg)) + geom_boxplot()
ggplot(germany, aes(x = germany$make, y = germany$mpg)) + geom_boxplot()
ggplot(japan, aes(x = japan$make, y = japan$mpg)) + geom_boxplot()
ggplot(sweden, aes(x = sweden$make, y = sweden$mpg)) + geom_boxplot()
ggplot(usa, aes(x = usa$make, y = usa$mpg)) + geom_boxplot()


## Let's now construct a linear model to see if we can isolate the key variables.  Since it
## now seems that there is a fair bit of intra-national variation, we'll use 'make' instead
## of 'Country'.

model1 <- lm(mpg ~ cylinders + `model year` + weight + make, data = carsmakemodel)
summary(model1)

## The 'cylinders' variable is not listed as significant, presumably because of its lack
## of range.  But we've already looked at the number of cylinders anyway.  So let's drop
## it and try a new model.

model2 <- lm(mpg ~ `model year` + weight + make, data = carsmakemodel)
summary(model2)

model3 <- lm(mpg ~ weight, data = carsmakemodel)
summary(model3)

model4 <- lm(mpg ~ `model year`, data = carsmakemodel)
summary(model4)


## Let's now split our dataset into a traning part and a test part, using the first to
## make a prediction about what to expect for the second.  Since our process involves
## random sampling, we'll set the seed number for the sake of reproducibility.

## First Experiment with Weight only.

set.seed(12)

train_part <- sample(seq_len(nrow(carsmakemodel)), size = 300)

trainset <- carsmakemodel[train_part,]
testset <- carsmakemodel[-train_part,]
trainmodel <- lm(mpg ~ weight, data = trainset)
summary(trainmodel)


## The summary leads us to make the following predictive estimate.

MPGest <- testset$weight * -0.0077672 + 46.6415132


## Now let's construct manufacturer-defined subsets of our testset.

amctest <- testset[testset$make == "amc",]
amcmeans <- c("amc", mean(amctest$mpg), mean(amctest$weight))
auditest <- testset[testset$make == "audi",]
audimeans <- c("aud", mean(auditest$mpg), mean(auditest$weight))
bmwtest <- testset[testset$make == "bmw",]
bmwmeans <- c("bmw", mean(bmwtest$mpg), mean(bmwtest$weight))
buicktest <- testset[testset$make == "buick",]
buickmeans <- c("bui", mean(buicktest$mpg), mean(buicktest$weight))
cadillactest <- testset[testset$make == "cadillac",]
cadillacmeans <- c("clc", mean(cadillactest$mpg), mean(cadillactest$weight))
chevrolettest <- testset[testset$make == "chevrolet",]
chevroletmeans <- c("chv", mean(chevrolettest$mpg), mean(chevrolettest$weight))
chryslertest <- testset[testset$make == "chrysler",]
chryslermeans <- c("chr", mean(chryslertest$mpg), mean(chryslertest$weight))
dodgetest <- testset[testset$make == "dodge",]
dodgemeans <- c("dod", mean(dodgetest$mpg), mean(dodgetest$weight))
fiattest <- testset[testset$make == "fiat",]
fiatmeans <- c("fia", mean(fiattest$mpg), mean(fiattest$weight))
fordtest <- testset[testset$make == "ford",]
fordmeans <- c("frd", mean(fordtest$mpg), mean(fordtest$weight))
hondatest <- testset[testset$make == "honda",]
hondameans <- c("hda", mean(hondatest$mpg), mean(hondatest$weight))
mazdatest <- testset[testset$make == "mazda",]
mazdameans <- c("maz", mean(mazdatest$mpg), mean(mazdatest$weight))
mercedestest <- testset[testset$make == "mercedes",]
mercedesmeans <- c("mrs", mean(mercedestest$mpg), mean(mercedestest$weight))
mercurytest <- testset[testset$make == "mercury",]
mercurymeans <- c("mrc", mean(mercurytest$mpg), mean(mercurytest$weight))
nissantest <- testset[testset$make == "nissan",]
nissanmeans <- c("nis", mean(nissantest$mpg), mean(nissantest$weight))
oldsmobiletest <- testset[testset$make == "oldsmobile",]
oldsmobilemeans <- c("old", mean(oldsmobiletest$mpg), mean(oldsmobiletest$weight))
opeltest <- testset[testset$make == "opel",]
opelmeans <- c("ope", mean(opeltest$mpg), mean(opeltest$weight))
peugeottest <- testset[testset$make == "peugeot",]
peugeotmeans <- c("peu", mean(peugeottest$mpg), mean(peugeottest$weight))
plymouthtest <- testset[testset$make == "plymouth",]
plymouthmeans <- c("ply", mean(plymouthtest$mpg), mean(plymouthtest$weight))
pontiactest <- testset[testset$make == "pontiac",]
pontiacmeans <- c("pon", mean(pontiactest$mpg), mean(pontiactest$weight))
renaulttest <- testset[testset$make == "renault",]
renaultmeans <- c("ren", mean(renaulttest$mpg), mean(renaulttest$weight))
saabtest <- testset[testset$make == "saab",]
saabmeans <- c("sab", mean(saabtest$mpg), mean(saabtest$weight))
subarutest <- testset[testset$make == "subaru",]
subarumeans <- c("sub", mean(subarutest$mpg), mean(subarutest$weight))
toyotatest <- testset[testset$make == "toyota",]
toyotameans <- c("toy", mean(toyotatest$mpg), mean(toyotatest$weight))
volkswagentest <- testset[testset$make == "volkswagen",]
volkswagenmeans <- c("vks", mean(volkswagentest$mpg), mean(volkswagentest$weight))
volvotest <- testset[testset$make == "volvo",]
volvomeans <- c("vvo", mean(volvotest$mpg), mean(volvotest$weight))


## Now we'll put these means together in a new data frame.

makemeans <- as.data.frame(rbind(amcmeans, audimeans, bmwmeans, buickmeans, cadillacmeans,
                  chevroletmeans, chryslermeans, dodgemeans, fiatmeans, fordmeans,
                  hondameans, mazdameans, mercedesmeans, mercurymeans, nissanmeans,
                  oldsmobilemeans, opelmeans, peugeotmeans, plymouthmeans, pontiacmeans,
                  renaultmeans, saabmeans, subarumeans, toyotameans, volkswagenmeans,
                  volvomeans))


## Let's give the data frame's columns names.

colnames(makemeans) <- c("make", "avgMPG", "avgWT")


## We'll use the same equation from before in making our prediction.

avgMPGest <- as.numeric(as.vector(makemeans$avgWT)) * -0.0077672 + 46.6415132


quartz()
ggplot(makemeans, aes(x = makemeans$make)) +
  geom_point(aes(y = as.numeric(as.vector(makemeans$avgMPG))), col = "black") +
  geom_point(aes(y = avgMPGest), col = "red")


## Let's add in abbreviations of the makes for better visualizations here.

for (i in 1:98){
  if (testset[i, 9] == "audi"){
    testset[i, 9] <- "aud"}
  if (testset[i, 9] == "buick"){
    testset[i, 9] <- "bui"}
  if (testset[i, 9] == "cadillac"){
    testset[i, 9] <- "clc"}
  if (testset[i, 9] == "chevrolet"){
    testset[i, 9] <- "chv"}
  if (testset[i, 9] == "chrysler"){
    testset[i, 9] <- "chr"}
  if (testset[i, 9] == "dodge"){
    testset[i, 9] <- "dod"}
  if (testset[i, 9] == "fiat"){
    testset[i, 9] <- "fia"}
  if (testset[i, 9] == "ford"){
    testset[i, 9] <- "frd"}
  if (testset[i, 9] == "honda"){
    testset[i, 9] <- "hda"}
  if (testset[i, 9] == "mazda"){
    testset[i, 9] <- "maz"}
  if (testset[i, 9] == "mercedes"){
    testset[i, 9] <- "mrs"}
  if (testset[i, 9] == "mercury"){
    testset[i, 9] <- "mrc"}
  if (testset[i, 9] == "nissan"){
    testset[i, 9] <- "nis"}
  if (testset[i, 9] == "oldsmobile"){
    testset[i, 9] <- "old"}
  if (testset[i, 9] == "opel"){
    testset[i, 9] <- "ope"}
  if (testset[i, 9] == "peugeot"){
    testset[i, 9] <- "peu"}
  if (testset[i, 9] == "plymouth"){
    testset[i, 9] <- "ply"}
  if (testset[i, 9] == "pontiac"){
    testset[i, 9] <- "pon"}
  if (testset[i, 9] == "renault"){
    testset[i, 9] <- "ren"}
  if (testset[i, 9] == "saab"){
    testset[i, 9] <- "sab"}
  if (testset[i, 9] == "subaru"){
    testset[i, 9] <- "sub"}
  if (testset[i, 9] == "toyota"){
    testset[i, 9] <- "toy"}
  if (testset[i, 9] == "volkswagen"){
    testset[i, 9] <- "vks"}
  if (testset[i, 9] == "volvo"){
    testset[i, 9] <- "vvo"}
}

ggplot(testset, aes(x = testset$make)) + geom_line(aes(y = testset$mpg), col = "black") +
  geom_line(aes(y = MPGest), col = "red")

ggplot(testset, aes(x = testset$make, y = (testset$mpg - MPGest)/testset$mpg)) + geom_point()

## Our predictive model has performed rather well.


## Second Experiment with both Weight and Model Year.

set.seed(13)

train_part2 <- sample(seq_len(nrow(carsmakemodel)), size = 300)

trainset2 <- carsmakemodel[train_part2,]
testset2 <- carsmakemodel[-train_part2,]
trainmodel2 <- lm(mpg ~ weight + `model year`, data = trainset2)
summary(trainmodel2)


## The summary leads us to make the following predictive estimate.

MPGest2 <- testset2$weight * -0.006704 + testset2$`model year` * 0.7087 - 10.45


## As before, let's construct manufacturer-defined subsets of our testset.

amctest2 <- testset2[testset2$make == "amc",]
amcmeans2 <- c("amc", mean(amctest2$mpg), mean(amctest2$weight), mean(amctest2$`model year`))
auditest2 <- testset2[testset2$make == "audi",]
audimeans2 <- c("aud", mean(auditest2$mpg), mean(auditest2$weight), mean(auditest2$`model year`))
bmwtest2 <- testset2[testset2$make == "bmw",]
bmwmeans2 <- c("bmw", mean(bmwtest2$mpg), mean(bmwtest2$weight), mean(bmwtest2$`model year`))
buicktest2 <- testset2[testset2$make == "buick",]
buickmeans2 <- c("bui", mean(buicktest2$mpg), mean(buicktest2$weight), mean(bmwtest2$`model year`))
cadillactest2 <- testset2[testset2$make == "cadillac",]
cadillacmeans2 <- c("clc", mean(cadillactest2$mpg), mean(cadillactest2$weight), mean(cadillactest2$`model year`))
chevrolettest2 <- testset2[testset2$make == "chevrolet",]
chevroletmeans2 <- c("chv", mean(chevrolettest2$mpg), mean(chevrolettest2$weight), mean(chevrolettest2$`model year`))
chryslertest2 <- testset2[testset2$make == "chrysler",]
chryslermeans2 <- c("chr", mean(chryslertest2$mpg), mean(chryslertest2$weight), mean(chryslertest2$`model year`))
dodgetest2 <- testset2[testset2$make == "dodge",]
dodgemeans2 <- c("dod", mean(dodgetest2$mpg), mean(dodgetest2$weight))
fiattest2 <- testset2[testset2$make == "fiat",]
fiatmeans2 <- c("fia", mean(fiattest2$mpg), mean(fiattest2$weight), mean(fiattest2$`model year`))
fordtest2 <- testset2[testset2$make == "ford",]
fordmeans2 <- c("frd", mean(fordtest2$mpg), mean(fordtest2$weight), mean(fordtest2$`model year`))
hondatest2 <- testset2[testset2$make == "honda",]
hondameans2 <- c("hda", mean(hondatest2$mpg), mean(hondatest2$weight), mean(hondatest2$`model year`))
mazdatest2 <- testset2[testset2$make == "mazda",]
mazdameans2 <- c("maz", mean(mazdatest2$mpg), mean(mazdatest2$weight), mean(mazdatest2$`model year`))
mercedestest2 <- testset2[testset2$make == "mercedes",]
mercedesmeans2 <- c("mrs", mean(mercedestest2$mpg), mean(mercedestest2$weight), mean(mercedestest2$`model year`))
mercurytest2 <- testset2[testset2$make == "mercury",]
mercurymeans2 <- c("mrc", mean(mercurytest2$mpg), mean(mercurytest2$weight), mean(mercurytest2$`model year`))
nissantest2 <- testset2[testset2$make == "nissan",]
nissanmeans2 <- c("nis", mean(nissantest2$mpg), mean(nissantest2$weight), mean(nissantest2$`model year`))
oldsmobiletest2 <- testset2[testset2$make == "oldsmobile",]
oldsmobilemeans2 <- c("old", mean(oldsmobiletest2$mpg), mean(oldsmobiletest2$weight), mean(oldsmobiletest2$`model year`))
opeltest2 <- testset2[testset2$make == "opel",]
opelmeans2 <- c("ope", mean(opeltest2$mpg), mean(opeltest2$weight), mean(opeltest2$`model year`))
peugeottest2 <- testset2[testset2$make == "peugeot",]
peugeotmeans2 <- c("peu", mean(peugeottest2$mpg), mean(peugeottest2$weight), mean(peugeottest2$`model year`))
plymouthtest2 <- testset2[testset2$make == "plymouth",]
plymouthmeans2 <- c("ply", mean(plymouthtest2$mpg), mean(plymouthtest2$weight), mean(plymouthtest2$`model year`))
pontiactest2 <- testset2[testset2$make == "pontiac",]
pontiacmeans2 <- c("pon", mean(pontiactest2$mpg), mean(pontiactest2$weight), mean(pontiactest2$`model year`))
renaulttest2 <- testset2[testset2$make == "renault",]
renaultmeans2 <- c("ren", mean(renaulttest2$mpg), mean(renaulttest2$weight), mean(renaulttest2$`model year`))
saabtest2 <- testset2[testset2$make == "saab",]
saabmeans2 <- c("sab", mean(saabtest2$mpg), mean(saabtest2$weight), mean(saabtest2$`model year`))
subarutest2 <- testset2[testset2$make == "subaru",]
subarumeans2 <- c("sub", mean(subarutest2$mpg), mean(subarutest2$weight), mean(subarutest2$`model year`))
toyotatest2 <- testset2[testset2$make == "toyota",]
toyotameans2 <- c("toy", mean(toyotatest2$mpg), mean(toyotatest2$weight), mean(toyotatest2$`model year`))
volkswagentest2 <- testset2[testset2$make == "volkswagen",]
volkswagenmeans2 <- c("vks", mean(volkswagentest2$mpg), mean(volkswagentest2$weight), mean(volkswagentest2$`model year`))
volvotest2 <- testset2[testset2$make == "volvo",]
volvomeans2 <- c("vvo", mean(volvotest2$mpg), mean(volvotest2$weight), mean(volvotest2$`model year`))


## Now we'll put these means together in a new data frame.

makemeans2 <- as.data.frame(rbind(amcmeans2, audimeans2, bmwmeans2, buickmeans2,
                                  cadillacmeans2, chevroletmeans2, chryslermeans2,
                                  dodgemeans2, fiatmeans2, fordmeans2, hondameans2,
                                  mazdameans2, mercedesmeans2, mercurymeans2,
                                  nissanmeans2, oldsmobilemeans2, opelmeans2,
                                  peugeotmeans2, plymouthmeans2, pontiacmeans2,
                                  renaultmeans2, saabmeans2, subarumeans2, toyotameans2,
                                  volkswagenmeans2, volvomeans2))


## As before, let's give the data frame's columns names.

colnames(makemeans2) <- c("make", "avgMPG", "avgWT", "avgMY")


## We'll use the same equation from before in making our prediction.

avgMPGest2 <- as.numeric(as.vector(makemeans2$avgWT)) * -0.006704 +
  as.numeric(as.vector(makemeans2$avgMY)) * 0.7087 - 10.45

quartz()
ggplot(makemeans2, aes(x = makemeans2$make)) +
  geom_point(aes(y = as.numeric(as.vector(makemeans2$avgMPG))), col = "black") +
  geom_point(aes(y = avgMPGest2), col = "red")


## Again we'll put in our abbreviations.

for (i in 1:98){
  if (testset2[i, 9] == "audi"){
    testset2[i, 9] <- "aud"}
  if (testset2[i, 9] == "buick"){
    testset2[i, 9] <- "bui"}
  if (testset2[i, 9] == "cadillac"){
    testset2[i, 9] <- "clc"}
  if (testset2[i, 9] == "chevrolet"){
    testset2[i, 9] <- "chv"}
  if (testset2[i, 9] == "chrysler"){
    testset2[i, 9] <- "chr"}
  if (testset2[i, 9] == "dodge"){
    testset2[i, 9] <- "dod"}
  if (testset2[i, 9] == "fiat"){
    testset2[i, 9] <- "fia"}
  if (testset2[i, 9] == "ford"){
    testset2[i, 9] <- "frd"}
  if (testset2[i, 9] == "honda"){
    testset2[i, 9] <- "hda"}
  if (testset2[i, 9] == "mazda"){
    testset2[i, 9] <- "maz"}
  if (testset2[i, 9] == "mercedes"){
    testset2[i, 9] <- "mrs"}
  if (testset2[i, 9] == "mercury"){
    testset2[i, 9] <- "mrc"}
  if (testset2[i, 9] == "nissan"){
    testset2[i, 9] <- "nis"}
  if (testset2[i, 9] == "oldsmobile"){
    testset2[i, 9] <- "old"}
  if (testset2[i, 9] == "opel"){
    testset2[i, 9] <- "ope"}
  if (testset2[i, 9] == "peugeot"){
    testset2[i, 9] <- "peu"}
  if (testset2[i, 9] == "plymouth"){
    testset2[i, 9] <- "ply"}
  if (testset2[i, 9] == "pontiac"){
    testset2[i, 9] <- "pon"}
  if (testset2[i, 9] == "renault"){
    testset2[i, 9] <- "ren"}
  if (testset2[i, 9] == "saab"){
    testset2[i, 9] <- "sab"}
  if (testset2[i, 9] == "subaru"){
    testset2[i, 9] <- "sub"}
  if (testset2[i, 9] == "toyota"){
    testset2[i, 9] <- "toy"}
  if (testset2[i, 9] == "volkswagen"){
    testset2[i, 9] <- "vks"}
  if (testset2[i, 9] == "volvo"){
    testset2[i, 9] <- "vvo"}
}

ggplot(testset2, aes(x = testset2$make)) + geom_line(aes(y = testset2$mpg), col = "black") +
  geom_line(aes(y = MPGest2), col = "red")

ggplot(testset2, aes(x = testset2$make, y = (testset2$mpg - MPGest2)/testset2$mpg)) + geom_point()

## Again our predictive model looks pretty good.

## END