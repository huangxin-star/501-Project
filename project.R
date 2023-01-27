library(tidyverse)
customers<-read_delim('data/Customer Review.csv',delim = ',')
summary(customers)
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

price_IQR=IQRCal(quantile(customers$ProductPrice,0.25),quantile(customers$ProductPrice,0.75))
review_IQR=IQRCal(quantile(customers$ReviewRating,0.25),quantile(customers$ReviewRating,0.75))

#replace the outliers with mean
customers$ProductPrice.fix<-ifelse(between(customers$ProductPrice,price_IQR[1],price_IQR[2]),customers$ProductPrice,mean_price)
customers$ReviewRating.fix<-ifelse(between(customers$ReviewRating,review_IQR[1],review_IQR[2]),customers$ReviewRating,mean_review)