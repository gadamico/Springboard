library(dplyr)
titanic_original <- read.csv("~/Desktop/Springboard/titanic3.csv")
titanic_clean <- mutate(titanic_original,
    embarked = replace(embarked, (embarked == '' | embarked == 'NA'), 'S'))
titanic_clean <- mutate(titanic_clean,
    age = replace(age, (age == '' | age == 'NA'),
    mean(titanic_clean$age, na.rm = TRUE)))
titanic_clean <- mutate(titanic_clean,
    boat = replace(boat, boat == '', NA))
titanic_clean <- titanic_clean %>%
  mutate(cabin, has_cabin_number = ifelse(cabin == 'NA' | cabin == '', "0", "1"))
write.csv(titanic_clean, file = "titanic.clean.csv")