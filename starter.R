library(tidyverse)

bank<-read.csv("https://raw.githubusercontent.com/PatLittle/CIND119-group-project/main/bank_marketing/bank.csv")

str(bank)
colSums(is.na(bank))


bank_clean<- bank %>% mutate_if(is.character, factor)
  

