library(tidyverse)
customers<-read_delim('./data/Customer Review.csv',delim = ',')
summary(customers)
# calculate sd
sd(customers$ProductPrice)
sd(customers$ReviewRating)
sd(customers$UserAge)
# unique category items
length(unique(customers$RetailerCity))
length(unique(customers$RetailerState))
length(unique(customers$UserOccupation))
length(unique(customers$RetailerZip))
length(unique(customers$RetailerName))
length(unique(customers$ManufacturerName))
length(unique(customers$UserID))
length(unique(customers$ProductModelName))
length(unique(customers$ProductCategory))
# detect outliers
library(gridExtra)
p1<-ggplot(customers,aes(y=ProductPrice))+geom_boxplot(outlier.colour = "red",outlier.shape = 8,outlier.size = 4)
p2<-ggplot(customers,aes(y=RetailerZip))+geom_boxplot(outlier.colour = "red",outlier.shape = 8,outlier.size = 4)
p3<-ggplot(customers,aes(y=UserAge))+geom_boxplot(outlier.colour = "red",outlier.shape = 8,outlier.size = 4)
p4<-ggplot(customers,aes(y=ReviewRating))+geom_boxplot(outlier.colour = "red",outlier.shape = 8,outlier.size = 4)

grid.arrange(p1,p2,p3,p4,nrow=2,ncol=2)

mean_price=mean(customers$ProductPrice)
mean_review=mean(customers$ReviewRating)

IQRCal<-function(Q1,Q3){
  IQR = Q3-Q1
  S = IQR*1.5
  UB = Q3+S
  LB = Q1-S
  result = c(LB,UB)
  return (result)
}

price_IQR=IQRCal(349,999)
review_IQR=IQRCal(4,5)
#replace the outliers with mean
customers$ProductPrice.fix<-ifelse(between(customers$ProductPrice,price_IQR[1],price_IQR[2]),customers$ProductPrice,mean_price)
customers$ReviewRating.fix<-ifelse(between(customers$ReviewRating,review_IQR[1],review_IQR[2]),customers$ReviewRating,mean_review)

ggplot(customers, aes(x=ReviewRating)) + geom_bar()

# Chi-square Test
subset_data<-customers[c('ProductCategory','ReviewRating')]
ProductCategory <- table(subset_data)
chisq.test(ProductCategory, correct=F)

subset_data<-customers[c('ProductPrice','ReviewRating')]
ProductPrice <- table(subset_data)
chisq.test(ProductPrice, correct=F)

subset_data<-customers[c('RetailerCity','ReviewRating')]
RetailerCity <- table(subset_data)
chisq.test(RetailerCity, correct=F) 

subset_data<-customers[c('ManufacturerName','ReviewRating')]
ManufacturerName <- table(subset_data)
chisq.test(ManufacturerName, correct=F)

subset_data<-customers[c('UserAge','ReviewRating')]
UserAge <- table(subset_data)
chisq.test(UserAge, correct=F)

subset_data<-customers[c('UserGender','ReviewRating')]
UserGender <- table(subset_data)
chisq.test(UserGender, correct=F)

subset_data<-customers[c('UserOccupation','ReviewRating')]
UserOccupation <- table(subset_data)
chisq.test(UserOccupation, correct=F)


#测试能否push

ttt