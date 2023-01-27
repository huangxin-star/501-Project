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



# convert category data to numerical data
customers$RetailerName.cov <- as.numeric(factor(customers$RetailerName))
customers$ManufacturerName.cov <-  as.numeric(factor(customers$ManufacturerName))
customers$ProductModelName.cov <- as.numeric(factor(customers$ProductModelName))
customers$ProductCategory.cov <- as.numeric(factor(customers$ProductCategory))
customers$RetailerCity.cov <- as.numeric(factor(customers$RetailerCity))
customers$RetailerState.cov <- as.numeric(factor(customers$RetailerState))
customers$ProductOnSale.cov <- as.numeric(factor(customers$ProductOnSale))
customers$UserGender.cov <- as.numeric(factor(customers$UserGender))
customers$UserOccupation.cov <- as.numeric(factor(customers$UserOccupation))
customers$ManufacturerRebate.cov <- as.numeric(factor(customers$ManufacturerRebate))

# new dataframe including all numerical data
customers_new <- data_frame(customers$RetailerName.cov,
                            customers$ManufacturerName.cov,
                            customers$ProductModelName.cov,
                            customers$ProductCategory.cov,
                            customers$RetailerCity.cov,
                            customers$RetailerState.cov,
                            customers$ProductOnSale.cov,
                            customers$UserGender.cov,
                            customers$UserOccupation.cov,
                            customers$ManufacturerRebate.cov,
                            customers$ProductPrice.fix,
                            customers$UserAge,
                            customers$ReviewRating
                            )
# 生成相关性矩阵
cor_matrix <- cor(customers_new)
# 画出相关性矩阵
library(corrplot)
corrplot(cor_matrix)
