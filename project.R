install.packages("flexclust")
install.packages("factoextra")
install.packages("NbClust")
library(tidyverse)
library(caret)
library(cluster)
library(factoextra)
library(NbClust)
library(readxl)
customers<-read_delim('data/Customer Review.csv',delim = ',')
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
install.packages(grid)
library(gridExtra)
p1<-ggplot(customers,aes(y=ProductPrice))+geom_boxplot(outlier.colour = "red",outlier.shape = 8,outlier.size = 4)
p2<-ggplot(customers,aes(y=RetailerZip))+geom_boxplot(outlier.colour = "red",outlier.shape = 8,outlier.size = 4)
p3<-ggplot(customers,aes(y=UserAge))+geom_boxplot(outlier.colour = "red",outlier.shape = 8,outlier.size = 4)
p4<-ggplot(customers,aes(y=ReviewRating))+geom_boxplot(outlier.colour = "red",outlier.shape = 8,outlier.size = 4)



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


## Assignment2 PartB
male <- length(which(customers$UserGender=='Male'))
female <- length(which(customers$UserGender=='Female'))
sex <- read_excel("data/sex.xlsx")
# EDA（这里我会接着补充）
p <- ggplot(sex,aes((sex),number))
p+geom_bar(stat = 'identity',aes(fill=sex))


# convert category data to numerical data
# 1. ProductCategory
productCategory_one <- customers$ProductCategory == 'TV'
customers[productCategory_one, "ProductCategory.fix"] <- 1
productCategory_two <- customers$ProductCategory == 'Tablet'
customers[productCategory_two, "ProductCategory.fix"] <- 2
productCategory_three <- customers$ProductCategory == 'Smart Phone'
customers[productCategory_three, "ProductCategory.fix"] <- 3
productCategory_four <- customers$ProductCategory == 'Laptop'
customers[productCategory_four, "ProductCategory.fix"] <- 4

# 2. UserGender
UserGender_one <- customers$UserGender == 'Male'
customers[UserGender_one, "UserGender.fix"] <- 1
UserGender_two <- customers$UserGender == 'Female'
customers[UserGender_two, "UserGender.fix"] <- 2

# 3.UserOccuption
customers$UserOccupation <- str_to_title(customers$UserOccupation)
UserOccupation_one <- customers$UserOccupation == 'Manager'
customers[UserOccupation_one, "UserOccupation.fix"] <- 1
UserOccupation_two <- customers$UserOccupation == 'Secretary'
customers[UserOccupation_two, "UserOccupation.fix"] <- 2
UserOccupation_three <- customers$UserOccupation == 'Programmer'
customers[UserOccupation_three, "UserOccupation.fix"] <- 3
UserOccupation_four <- customers$UserOccupation == 'Student'
customers[UserOccupation_four, "UserOccupation.fix"] <- 4
UserOccupation_five <- customers$UserOccupation == 'Accountant'
customers[UserOccupation_five, "UserOccupation.fix"] <- 5
UserOccupation_six <- customers$UserOccupation == 'Unknown'
customers[UserOccupation_six, "UserOccupation.fix"] <- 0

# 4.ManufacturerName
ManufacturerName_one <- customers$ManufacturerName == 'Samsung'
customers[ManufacturerName_one, "ManufacturerName.fix"] <- 1
ManufacturerName_two <- customers$ManufacturerName == 'Microsoft'
customers[ManufacturerName_two, "ManufacturerName.fix"] <- 2
ManufacturerName_three <- customers$ManufacturerName == 'HP'
customers[ManufacturerName_three, "ManufacturerName.fix"] <- 3
ManufacturerName_four <- customers$ManufacturerName == 'Apple'
customers[ManufacturerName_four, "ManufacturerName.fix"] <- 4
ManufacturerName_five <- customers$ManufacturerName == 'Dell'
customers[ManufacturerName_five, "ManufacturerName.fix"] <- 5
ManufacturerName_six <- customers$ManufacturerName == 'LG'
customers[ManufacturerName_six, "ManufacturerName.fix"] <- 6


# Clusting Test(这里写聚类的代码)
# 我聚合了一下产品和用户职业
new <- data_frame(customers$UserOccupation.fix,customers$ProductCategory.fix)
head(new)

d <- dist(new)  # 计算距离 默认欧式距离
fit_average <- hclust(d, method="average") # 聚类
plot(fit_average, hang = -1,  main = "Average Linkage Clustering")

# 围绕中心点的划分（PAM）
data(new)
nu_pam <- pam(new, 2, metric="euclidean")
fviz_cluster(nu_pam, new)

#确定聚类个数
set.seed(123)
fviz_nbclust(new, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)

# 第一种制图方式
fit.km <- kmeans(new, centers = 4, nstart = 25)
plot(new, col = fit.km$cluster, pch = 19, frame = FALSE,
     main = "K-means with k = 4")

# 第二种（好看）
points(fit.km$centers, col = 1:2, pch = 8, cex = 3)
fviz_cluster(fit.km, data = new, 
             ellipse = T, # 增加椭圆
             ellipse.type = "t", # 椭圆类型
             geom = "point", # 只显示点不要文字
             palette = "lancet", # 支持超多配色方案
             ggtheme = theme_bw() # 支持更换主题
)



# wriiten by ZM 
# 提取需要的列
customers_cluster=select(customers,ProductCategory.fix,UserGender.fix,UserOccupation.fix,ManufacturerName.fix,ProductPrice.fix,ReviewRating.fix)
customers_nor<-as.data.frame(scale(customers_cluster))
bss <- numeric()
wss <- numeric()
set.seed(1234)

for(i in 1:10){
  
  # For each k, calculate betweenss and tot.withinss
  bss[i] <- kmeans(customers_nor, centers=i)$betweenss
  wss[i] <- kmeans(customers_nor, centers=i)$tot.withinss
  
}


# Between-cluster sum of squares vs Choice of k
p3 <- qplot(1:10, bss, geom=c("point", "line"), 
            xlab="Number of clusters", ylab="Between-cluster sum of squares") +
  scale_x_continuous(breaks=seq(0, 10, 1)) +
  theme_bw()

# Total within-cluster sum of squares vs Choice of k
p4 <- qplot(1:10, wss, geom=c("point", "line"),
            xlab="Number of clusters", ylab="Total within-cluster sum of squares") +
  scale_x_continuous(breaks=seq(0, 10, 1)) +
  theme_bw()



# Subplot
grid.arrange(p3, p4, ncol=2)
set.seed(1234)
cutomers_k3 <- kmeans(customers_nor, centers=3)

aggregate(customers_cluster, by=list(cutomers_k3$cluster), mean)

library(GGally)

# Clustering 
ggpairs(cbind(customers_cluster, Cluster=as.factor(cutomers_k3$cluster)),
        columns=1:6, aes(colour=Cluster, alpha=0.5),
        lower=list(continuous="points"),
        upper=list(continuous="blank"),
        axisLabels="none", switch="both") +
  theme_bw()


