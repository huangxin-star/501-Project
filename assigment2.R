library(tidyverse)
customers<-read_delim('data/marketing_campaign.csv')
summary(customers)
customers_cluster<-select(customers,Year_Birth,Income,MntWines,MntFruits,MntMeatProducts,MntFishProducts,MntSweetProducts,MntGoldProds)

# 转换年龄
customers_cluster$age<-(2022-customers_cluster$Year_Birth)
customers_cluster<-customers_cluster%>%drop_na()
print(customers_cluster)

library(gridExtra)
# p1<-ggplot(customers_cluster,aes(y=age))+geom_boxplot(outlier.colour = "red",outlier.shape = 8,outlier.size = 4)
# p2<-ggplot(customers_cluster,aes(y=Income))+geom_boxplot(outlier.colour = "red",outlier.shape = 8,outlier.size = 4)
# p3<-ggplot(customers_cluster,aes(y=MntWines))+geom_boxplot(outlier.colour = "red",outlier.shape = 8,outlier.size = 4)
# p4<-ggplot(customers_cluster,aes(y=MntFruits))+geom_boxplot(outlier.colour = "red",outlier.shape = 8,outlier.size = 4)
# p5<-ggplot(customers_cluster,aes(y=MntMeatProducts))+geom_boxplot(outlier.colour = "red",outlier.shape = 8,outlier.size = 4)
# p6<-ggplot(customers_cluster,aes(y=MntFishProducts))+geom_boxplot(outlier.colour = "red",outlier.shape = 8,outlier.size = 4)
# p7<-ggplot(customers_cluster,aes(y=MntSweetProducts))+geom_boxplot(outlier.colour = "red",outlier.shape = 8,outlier.size = 4)
# p8<-ggplot(customers_cluster,aes(y=MntGoldProds))+geom_boxplot(outlier.colour = "red",outlier.shape = 8,outlier.size = 4)
# 

# grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,nrow=4,ncol=2)
ggplot(customers_cluster,aes(y=MntGoldProds))+geom_boxplot(outlier.colour = "blue",outlier.shape = 8,outlier.size = 4)
# 去除离群值
# replace_outlier_with_mean <- function(df) {
#   columns=names(df)
#   for (x in columns){
#     cl=df[[x]]
#     Q1=quantile(cl,0.25)
#     Q3=quantile(cl,0.75)
#     mn=mean(cl)
#     S = (Q3-Q1)*1.5
#     UB = Q3+S
#     LB = Q1-S
#     cl[which(cl>UB|cl<LB)]<-mn
#     # df[[x]] <- replace(df[[x]], !between(df[[x]],LB,UB), mean(df[[x]]))
#   }
# }
# customers_pre<-customers_cluster%>%mutate(across(.fns = replace_outlier_with_mean))
for (i in 1:ncol(customers_cluster)){
  print(class(customers_cluster[,i]))
  cl=unlist(customers_cluster[1,])
  Q1=quantile(cl,0.25)
  Q3=quantile(cl,0.75)
  mn=mean(cl)
  S = (Q3-Q1)*1.5
  UB = Q3+S
  LB = Q1-S
  for (y in customers_cluster[,i]){
    ifelse(between(y,LB,UB),y,mn)
  }
  # customers_cluster[,i][which(customers_cluster[,i]>UB|customers_cluster[,i]<LB)]<-mn
}
ggplot(customers_cluster,aes(y=MntGoldProds))+geom_boxplot(outlier.colour = "red",outlier.shape = 8,outlier.size = 4)
customers_nor<-as.data.frame(scale(customers_cluster))

test<-function(x){
  Q1=quantile(x,0.25)
  Q3=quantile(x,0.75)
  mn=mean(x)
  S=1.5*(Q3-Q1)
  LB=Q3-S
  UB=Q3+S
  x[which(x>UB|x<LB)]<-mn
}

