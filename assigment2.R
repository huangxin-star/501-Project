library(tidyverse)
customers<-read_delim('data/marketing_campaign.csv')
summary(customers)
customers_cluster<-select(customers,Year_Birth,Income,MntWines,MntFruits,MntMeatProducts,MntFishProducts,MntSweetProducts,MntGoldProds)

# 转换年龄
customers_cluster$age<-(2022-customers_cluster$Year_Birth)
customers_cluster<-customers_cluster%>%drop_na()

library(gridExtra)
p1<-ggplot(customers_cluster,aes(y=age))+geom_boxplot(outlier.colour = "red",outlier.shape = 8,outlier.size = 4)
p2<-ggplot(customers_cluster,aes(y=Income))+geom_boxplot(outlier.colour = "red",outlier.shape = 8,outlier.size = 4)
p3<-ggplot(customers_cluster,aes(y=MntWines))+geom_boxplot(outlier.colour = "red",outlier.shape = 8,outlier.size = 4)
p4<-ggplot(customers_cluster,aes(y=MntFruits))+geom_boxplot(outlier.colour = "red",outlier.shape = 8,outlier.size = 4)
p5<-ggplot(customers_cluster,aes(y=MntMeatProducts))+geom_boxplot(outlier.colour = "red",outlier.shape = 8,outlier.size = 4)
p6<-ggplot(customers_cluster,aes(y=MntFishProducts))+geom_boxplot(outlier.colour = "red",outlier.shape = 8,outlier.size = 4)
p7<-ggplot(customers_cluster,aes(y=MntSweetProducts))+geom_boxplot(outlier.colour = "red",outlier.shape = 8,outlier.size = 4)
p8<-ggplot(customers_cluster,aes(y=MntGoldProds))+geom_boxplot(outlier.colour = "red",outlier.shape = 8,outlier.size = 4)


grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,nrow=4,ncol=2)