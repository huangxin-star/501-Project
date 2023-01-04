library(tidyverse)
customers<-read_delim('./data/Customer Review.csv',delim = ',')
summary(customers)
# calculate sd
sd(customers$ProductPrice)
sd(customers$ReviewRating)
sd(customers$UserAge)