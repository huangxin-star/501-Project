library(tidyverse)
library(caret)
library(factoextra)
library(reshape2)

customers<-read_delim('data/marketing_campaign.csv')
summary(customers)
customers_cluster<-select(customers,Year_Birth,Income,MntWines,MntFruits,MntMeatProducts,MntFishProducts,MntSweetProducts,MntGoldProds)

# Part A
library(tidyverse)
library("dplyr")
library(caret)
#convert numerical value to categorical value
customer$ReviewRating <- as.factor(customer$ReviewRating )
str(customer)

#split
set.seed(333)
sample <- sample(c(TRUE, FALSE), nrow(customer), replace=TRUE, prob=c(0.7,0.3))
train  <- customer[sample, ]
test   <- customer[!sample, ]

train$ReviewRating  <- make.names(train$ReviewRating)
train$ReviewRating
set.seed(333)
train_control <- trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = defaultSummary)
model <- train(ReviewRating~., data = train, method = 'knn', tuneGrid = expand.grid(k = seq(1, 50, 2)), 
               trControl = train_control, metric = "Accuracy")
print(model)
# Part B
# EDA
customers['Child']=customers$Kidhome+customers$Teenhome
customers['Age']= 2022-customers$Year_Birth
customers['accepted']=customers$AcceptedCmp1+customers$AcceptedCmp2+customers$AcceptedCmp3+customers$AcceptedCmp4+customers$AcceptedCmp5
customers=customers[c(-1,-2,-6,-7,-8,-10,-11,-12,-13,-14,-15,-21,-22,-23,-24,-25,-27,-28)]
hist(customers$Income,40,col="#adcae6")

income=customers$Income[customers$Income<200000]
customers['total_spent']=customers$MntMeatProducts+customers$MntFishProducts+customers$MntWines+customers$MntFruits+customers$MntSweetProducts+customers$MntGoldProds
boxplot(income,
        main = "customer income",
        xlab = "Income",
        ylab = "",
        col = "#adcae6",
        border = "blue",
        horizontal = TRUE,
        notch = TRUE
)
scale_income <- scale(customers$Income)
scale_total_spent <- scale(customers$total_spent)
plot(scale_income,scale_total_spent,col='#adcae6',xlab='Income',ylab='spent')

hist(customers$Age,50,col="#adcae6")
boxplot(customers$Age,
        main = "customer Age",
        xlab = "Age",
        ylab = "",
        col = "#adcae6",
        border = "blue",
        horizontal = TRUE,
        notch = TRUE
)
hist(customers$total_spent,50,col="#adcae6")
boxplot(customers$total_spent,
        main = "total spent by the customer",
        xlab = "total",
        ylab = "",
        col = "#adcae6",
        border = "blue",
        horizontal = TRUE,
        notch = TRUE
)
eduplot = ggplot(customers, aes(x=Education,y=Income,fill=Education))+ylim(0,200000)+geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=T)
eduplot

mplot = ggplot(customers, aes(x=Marital_Status,y=Income,fill=Marital_Status))+ylim(0,180000)+geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=T)
mplot
# Transform
# Because of some variables are category，so we need to convert the data to dummy variables
dmy <- dummyVars(" ~ .", data = customers, fullRank = T)
dat_transformed <- data.frame(predict(dmy, newdata = customers))
# Correlation matrix 
test <- read_delim('data/marketing_campaign.csv')
test=test[c(-1,-2,-6,-7,-8,-10,-11,-12,-13,-14,-15,-21,-22,-23,-24,-25,-27,-28)]

melt(cor(test))%>%
  ggplot(aes(Var1,Var2,fill=value))+
  geom_tile()+
  scale_fill_gradient2(low = "firebrick4", high = "steelblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1),
        axis.title = element_blank())
corr<-cor(dfc[,1:14],method="pearson")
corrplot(corr)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(corr,method="color",#调整为正方形
         col=col(200), #颜色调整
         type="upper", #保留右上部分
         order="hclust", #层次聚类
         addCoef.col = "black", #添加相关系数
         tl.col="black", tl.srt=45, #修改字体
         diag=FALSE)#去掉自身相关
# selecting special values.
# The income is not necessary,because we want to find expand rather than income.
glimpse(dat_transformed)
dfc=dat_transformed[c(7,8,13,14,15,16,17,18,19,20,21,22,23,24)]
head(dfc)
# Data analysis
# Histogram for each Attribute
dfc %>%
  gather(Attributes, value, 1:14) %>%
  ggplot(aes(x=value, fill=Attributes)) +
  geom_histogram(colour="black", show.legend=FALSE) +
  facet_wrap(~Attributes, scales="free_x") +
  labs(x="Values", y="Frequency",
       title="Customers Attributes - Histograms") +
  theme_bw()
# Density plot for each Attribute
# Boxplot for each Attribute --ignore age and total income 
dfc %>%
  gather(Attributes, values, c(1:10,13:14)) %>%
  ggplot(aes(x=reorder(Attributes, values, FUN=median), y=values, fill=Attributes)) +
  geom_boxplot(show.legend=FALSE) +
  labs(title="Customers Attributes - Boxplots") +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank()) +
  ylim(0, 50) +
  coord_flip()
# K Means Clustering
# First,make sure correct K value.
# The Income variable has some missing values. since the percentage is very low. I will remove them.
# some values are empty,so I remove them.
customers <- na.omit(customers)
# Second, find the optimal number of clusters
fviz_nbclust(dfc,kmeans,method="wss")+geom_vline(xintercept=3,linetype=2)
set.seed(123)
km.res <- kmeans(dfc, 3, nstart = 10)
print(km.res$centers)
# To cluster
fviz_cluster(km.res, dfc, geom = "point",ellipse.type = "t",repel = TRUE, ellipse = T)

fit.km <- kmeans(dfc, centers = 3, nstart = 25)
points(fit.km$centers, col = 1:2, pch = 8, cex = 3)
fviz_cluster(fit.km, data = dfc, 
             ellipse = T, # 增加椭圆
             ellipse.type = "t", # 椭圆类型
             geom = "point", # 只显示点不要文字
             palette = "default", # 支持超多配色方案
             ggtheme = theme_bw() # 支持更换主题
)
# To calculate the center cluster for each item
dfc['cluster']=as.factor(km.res$cluster)
attach(dfc)
# plot the box 
spentplot1 = ggplot(dfc, aes(x=cluster,y=total_spent,fill=cluster))+geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=T)
spentplot2 = ggplot(dfc, aes(x=cluster,y=Age,fill=cluster))+geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=T)
numdealplot3 = ggplot(dfc, aes(x=cluster,y=NumDealsPurchases,fill=cluster))+geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=T)
numwebplot4 = ggplot(dfc, aes(x=cluster,y=NumWebPurchases,fill=cluster))+geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=T)
numcatplot5 = ggplot(dfc, aes(x=cluster,y=NumCatalogPurchases,fill=cluster))+geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=T)
numcatplot6 = ggplot(dfc, aes(x=cluster,y=NumStorePurchases,fill=cluster))+geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=T)
numcatplot7 = ggplot(dfc, aes(x=cluster,y= NumWebVisitsMonth,fill=cluster))+geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=T)
grid.arrange(spentplot1, spentplot2,numdealplot3,numwebplot4,numcatplot5,numcatplot6,numcatplot7, ncol=4)

