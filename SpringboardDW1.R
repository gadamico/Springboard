install.packages("tidyr")
install.packages("dplyr")
library(tidyr)
library(dplyr)
refine_original <- read.csv("~/Desktop/refine_original.csv")
View(refine_original)
refine_clean <- mutate(refine_original, company = tolower(company))
refine_clean <- mutate(refine_clean, company = replace(company,
                  (company == "phillips" | company == "phllips" |
                  company == "phillps" | company == "fillips" |
                  company == "phlips"), "philips"))
refine_clean <- mutate(refine_clean, company = replace(company,
                  (company == "akz0" | company == "ak zo"), "akzo"))
refine_clean <- mutate(refine_clean, company = replace(company,
                  company == "unilver", "unilever"))
refine_clean <- separate(refine_clean, Product.code...number, c("code",
                  "number"), sep = '-', remove = TRUE)
refine_clean <- mutate(refine_clean, product = code)
for (r in 1:25) {
  if (refine_clean[r, 2] == "p") {
    refine_clean[r, 8] = "Smartphone"}
  else if (refine_clean[r, 2] == "v") {
    refine_clean[r, 8] = "TV"}
  else if (refine_clean[r, 2] == "x") {
    refine_clean[r, 8] = "laptop"}
  else {refine_clean[r, 8] = "tablet"}
}

refine_clean <- unite(refine_clean, "full_address", c(address, city, country),
                      sep = ', ')
refine_clean <- refine_clean %>%
  mutate(company, company_philips = ifelse(company == "philips", "1","0")) %>%
  mutate(company, company_akzo = ifelse(company == "akzo", "1" , "0")) %>%
  mutate(company, company_van_houten = ifelse(company == "van houten", "1" , "0")) %>%
  mutate(company, company_van_houten = ifelse(company == "van houten", "1" , "0")) %>%
  mutate(company, company_unilever = ifelse(company == "unilever", "1" , "0")) %>%
  mutate(product, product_smartphone = ifelse(product == "Smartphone", "1" , "0")) %>%
  mutate(product, product_tv = ifelse(product == "TV", "1" , "0")) %>%
  mutate(product, product_laptop = ifelse(product == "Laptop", "1" , "0")) %>%
  mutate(product, product_tablet = ifelse(product == "Tablet", "1" , "0"))
write.csv(refine_clean, file = "refine_clean.csv")
