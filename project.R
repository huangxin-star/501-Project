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
IQRCal<-function(Q1,Q3){
  IQR = Q3-Q1
  S = IQR*1.5
  UB = Q3+S
  LB = Q1-S
  result = c(LB,UB)
  return (result)
}

price_IQR=IQRCal(quantile(customers$ProductPrice,0.25),quantile(customers$ProductPrice,0.75))

#replace the outliers with mean
customers$ProductPrice.fix<-ifelse(between(customers$ProductPrice,price_IQR[1],price_IQR[2]),customers$ProductPrice,mean_price)
ggplot(customers, aes(x=ReviewRating)) + geom_bar()

# 生成词云
install.packages("tm") # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
install.packages("syuzhet") # for sentiment analysis
install.packages("ggplot2") # for plotting graphs
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")


TextDoc <- Corpus(VectorSource(customers$ReviewText))
# 替代特殊符号
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")
# 转换为小写
TextDoc <- tm_map(TextDoc, content_transformer(tolower))
# 移除数字
TextDoc <- tm_map(TextDoc, removeNumbers)
# Remove English common stopwords
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
# Remove your own stop word
# specify your custom stopwords as a character vector
TextDoc <- tm_map(TextDoc, removeWords, c("s", "company", "team", "https","tco", "will"))
# Remove punctuations
TextDoc <- tm_map(TextDoc, removePunctuation)
# Eliminate extra white spaces
TextDoc <- tm_map(TextDoc, stripWhitespace)
# Text stemming - which reduces words to their root form
TextDoc <- tm_map(TextDoc, stemDocument)
# Build a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)
# Sort by descearing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- tibble(word = names(dtm_v),freq=dtm_v)
head(dtm_d,5)

# 作图
dtm_d %>% slice(1:15) %>%
  ggplot(aes(x=reorder(word, freq), y=freq)) +
  geom_bar(stat="identity", aes(fill=freq), show.legend=F) +
  geom_label(aes(label=freq)) +
  scale_fill_gradient(low="#58D68D", high="#239B56") +
  labs(x="word", y="Number of word", title="most frequent words") +
  coord_flip() +
  theme_bw()

# generate word cloud
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min)

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
customers_cor <- customers[,c(12,15,18,19,20,21,22,23,24,25,26,27,28)]

# 生成相关性矩阵
cor_matrix <- cor(customers_cor)
# 画出相关性矩阵
library(corrplot)
corrplot(cor_matrix)
# 提取相关列
customer_fin <- customers[,c(12,15,19,20,21,28)]
# 标准化
scale(customer_fin)
# 转化成二分类问题
customer_fin$ReviewRating <- ifelse(customer_fin$ReviewRating>=1&customer_fin$ReviewRating<=3,0,1)
# 导入相关库
library(rpart)
library(rpart.plot)
install.packages('rpart.plot')

# model training
ggplot(customer_fin, aes(x=ReviewRating)) + geom_bar()
table(customer_fin$ReviewRating)
# 导入unbalanced包
install.packages("ROSE")
# 数据集
customer_fin$ReviewRating <- as.factor(customer_fin$ReviewRating)
# 划分数据集
sample <- sample(c(TRUE, FALSE), nrow(customer_fin), replace=TRUE, prob=c(0.7,0.3))
train  <- customer_fin[sample, ]
test   <- customer_fin[!sample, ]

# 组合采样
balance.both <- ovun.sample(ReviewRating~.,data =train,
                            N = nrow(train),p=0.5,seed=1,method = "both")$data
# 查看均衡处理后的数据
ggplot(balance.both, aes(x=ReviewRating)) + geom_bar()


# 不均衡处理训练模型
# fit1 <- train(ReviewRating~.,data = train,method="knn")
# 均衡处理后训练模型(KNN)
# 设置默认参数
control_params <- trainControl(
  method = "cv",number = 10,search = "grid"
)
set.seed(333)
knn_fit_before <- train(ReviewRating~.,data = balance.both,method="knn",trControl=control_params,tuneGrid = expand.grid(k=2))
knn_predict_before <- predict(knn_fit_before,test)
knn_cm <- confusionMatrix(knn_predict_before,test$ReviewRating)
roc_knn <- roc(as.numeric(test$ReviewRating)-1,as.numeric(knn_predict_before)-1)
plot(roc_knn, print.auc=TRUE, auc.polygon=TRUE,legacy.axes=TRUE, grid=c(0.1, 0.2), 
     grid.col=c("green", "red"), max.auc.polygon=TRUE, 
     auc.polygon.col="#f3ae8f", print.thres=TRUE,
     thresholds="best") 
knn_recall <- knn_cm$byClass["Recall"]
# 调整参数后（roc）
knn_fit_after <- train(ReviewRating~.,data = balance.both,method="knn",
                               trControl = control_none,tuneGrid = expand.grid(k=5),
                               metric = "Accuracy")
knn_predict_after <- predict(knn_fit_after,test)
knn_cm_after <- confusionMatrix(knn_predict_after,test$ReviewRating)
knn_recall_after <- knn_cm_after$byClass["Recall"]

# 均衡处理后训练模型(dt)
dt_fit_before <- train(ReviewRating~.,data = balance.both,method="rpart",
                       trControl=control_params,
                       tuneGrid = expand.grid(cp=0.05))
dt_predict_before <- predict(dt_fit_before,test)
dt_cm <- confusionMatrix(dt_predict_before,test$ReviewRating)
roc_dt <- roc(as.numeric(test$ReviewRating)-1,as.numeric(dt_predict_before)-1)
plot(roc_dt, print.auc=TRUE, auc.polygon=TRUE,legacy.axes=TRUE, grid=c(0.1, 0.2), 
     grid.col=c("green", "red"), max.auc.polygon=TRUE, 
     auc.polygon.col="#f3ae8f", print.thres=TRUE,
     thresholds="best") 
dt_recall <- dt_cm$byClass["Recall"]
# 调整参数后（roc）
dt_fit_after <- train(ReviewRating~.,data = balance.both,method="rpart",
                       trControl = control_none,tuneGrid = expand.grid(cp=0.03),
                       metric = "Accuracy")
dt_predict_after <- predict(dt_fit_after,test)
dt_cm_after <- confusionMatrix(dt_predict_after,test$ReviewRating)
dt_recall_after <- dt_cm_after$byClass["Recall"]

# 均衡处理后训练模型(xgb)
xgb_fit_before <- train(ReviewRating~.,data = balance.both,method="xgbLinear",
                        trControl=control_params,
                        tuneGrid = expand.grid(nrounds= 100,
                                               eta=0.1,
                                               lambda=1e-04, 
                                               alpha=0))
xgb_predict_before <- predict(xgb_fit_before,test)
xgb_cm <- confusionMatrix(xgb_predict_before,test$ReviewRating)
roc_xgb <- roc(as.numeric(test$ReviewRating)-1,as.numeric(xgb_predict_before)-1)
plot(roc_xgb, print.auc=TRUE, auc.polygon=TRUE,legacy.axes=TRUE, grid=c(0.1, 0.2), 
     grid.col=c("green", "red"), max.auc.polygon=TRUE, 
     auc.polygon.col="#f3ae8f", print.thres=TRUE,
     thresholds="best") 
xgb_recall <- xgb_cm$byClass["Recall"]
# 调整参数后（roc）
xgb_fit_after <- train(ReviewRating~.,data = balance.both,method="xgbLinear",
                      trControl = control_params,tuneGrid = expand.grid(nrounds=150, 
                                                                      eta= 0.3, 
                                                                      lambda=1e-04, 
                                                                      alpha=0),
                                                                      metric = "Accuracy")
xgb_predict_after <- predict(xgb_fit_after,test)
xgb_cm_after <- confusionMatrix(xgb_predict_after,test$ReviewRating)
xgb_recall_after <- xgb_cm_after$byClass["Recall"]
# 均衡处理后训练模型(naive_bayes)
nb_fit_before <- train(ReviewRating~.,data = balance.both,method="naive_bayes",
                       trControl = control_params,tuneGrid = expand.grid(laplace =c(0,1),
                                                                         usekernel = c("True","False"), 
                                                                         adjust=c(0,1)),
                       metric = "Accuracy")
                       
nb_predict_before <- predict(nb_fit_before,test)
nb_cm <- confusionMatrix(nb_predict_before,test$ReviewRating)
roc_nb <- roc(as.numeric(test$ReviewRating)-1,as.numeric(nb_predict_before)-1)
plot(roc_nb, print.auc=TRUE, auc.polygon=TRUE,legacy.axes=TRUE, grid=c(0.1, 0.2), 
     grid.col=c("green", "red"), max.auc.polygon=TRUE, 
     auc.polygon.col="#f3ae8f", print.thres=TRUE,
     thresholds="best") 
nb_recall <- nb_cm$byClass["Recall"]
# 调整参数后（roc）
nb_fit_after <- train(ReviewRating~.,data = balance.both,method="naive_bayes",
                       trControl = control_params,tuneGrid = expand.grid(laplace =0,
                                                                         usekernel = "False", 
                                                                         adjust=1),
                       metric = "Accuracy")
nb_predict_after <- predict(nb_fit_after,test)
nb_cm_after <- confusionMatrix(nb_predict_after,test$ReviewRating)
nb_recall_after <- nb_cm_after$byClass["Recall"]

# 均衡处理后训练模型(svmLinear3)
# install kernlab package if not installed
install.packages("kernlab")
# load the package
library(kernlab)
# train the model
svm_fit_before <- ksvm(ReviewRating ~ ., data = balance.both,
                       type = "C-svc", prob.model = TRUE,trainControl=control_params,cost = 0.5,
                       loss = "L1", 
                       weight = 1 )
svm_predict_before <- predict(svm_fit_before,test)
svm_cm <- confusionMatrix(svm_predict_before,test$ReviewRating)
roc_svm <- roc(as.numeric(test$ReviewRating)-1,as.numeric(svm_predict_before)-1)
plot(roc_svm, print.auc=TRUE, auc.polygon=TRUE,legacy.axes=TRUE, grid=c(0.1, 0.2), 
     grid.col=c("green", "red"), max.auc.polygon=TRUE, 
     auc.polygon.col="#f3ae8f", print.thres=TRUE,
     thresholds="best") 
svm_recall <- svm_cm$byClass["Recall"]
# 调整参数后（roc）
svm_fit_after <- ksvm(ReviewRating ~ ., data = balance.both,
                       type = "C-svc", prob.model = TRUE,trainControl=control_params,cost = 0.25,
                       loss = "L2", 
                       weight = 1 )
svm_predict_after <- predict(svm_fit_after,test)
svm_cm_after <- confusionMatrix(svm_predict_after,test$ReviewRating)
svm_recall_after <- svm_cm_after$byClass["Recall"]

# 不均衡处理预测
# predict1 <- predict(fit1,test)
# 均衡处理预测
# predict2 <- predict(fit2,test)
# 不均衡处理预测结果混淆矩阵
#confusionMatrix(predict1,test$ReviewRating)
# 均衡处理预测结果混淆矩阵
#confusionMatrix(predict2,test$ReviewRating)


library(caretEnsemble)

library(ROCR)
roc1<- roc(data$ReviewRating,data$UserAge)
roc2<- roc(data$ReviewRating,data$ManufacturerName.cov)
roc3<- roc(data$ReviewRating,data$ProductModelName.cov)
roc4<- roc(data$ReviewRating,data$ProductCategory.cov)
roc5<- roc(data$ReviewRating,data$ProductPrice.fix)

plot(roc1,col="red",legacy.axes=T)
plot(roc2, add=TRUE, col="blue")
plot(roc3, add=TRUE, col="green")
plot(roc4, add=TRUE, col="black")
plot(roc5, add=TRUE, col="yellow")

round(auc(roc1),3)##AUC
round(ci(roc1),3)##95%CI
round(auc(roc2),3)##AUC
round(ci(roc2),3)##95%CI
round(auc(roc3),3)##AUC
round(ci(roc3),3)##95%CI
round(auc(roc4),3)##AUC
round(ci(roc4),3)##95%CI
round(auc(roc5),3)##AUC
round(ci(roc5),3)##95%CI
legend("bottomright", legend=c("roc1-auc0.539","roc2-auc0.514","roc3-auc0.573","roc4-auc0.436","roc5-auc0.558"),
       col=c("red","blue","green","black"),lty=1)
roc.test(roc1,roc4)



# ROC
pred1 <- predict(knn_fit_before, test, type = "prob")[,2]
perf1 <- performance(prediction(pred1, as.numeric(test$ReviewRating)), "tpr", "fpr")
pred2 <- predict(nb_fit_before, test, type = "prob")[,2]
perf2 <- performance(prediction(pred2, as.numeric(test$ReviewRating)), "tpr", "fpr")
pred3 <- predict(xgb_fit_before, test, type = "prob")[,2]
perf3 <- performance(prediction(pred3, as.numeric(test$ReviewRating)), "tpr", "fpr")
pred4 <- predict(dt_fit_before, test, type = "prob")[,2]
perf4 <- performance(prediction(pred4, as.numeric(test$ReviewRating)), "tpr", "fpr")
pred5 <- predict(svm_fit_before, test, type = "prob")[,2]
perf5 <- performance(prediction(pred5, as.numeric(test$ReviewRating)), "tpr", "fpr")
# 绘制ROC曲线
plot(perf1, color = "red", lwd = 2, main = "ROC Curve",
     xlab = "False Positive Rate", ylab = "True Positive Rate")
plot(perf2, add=TRUE, col="blue")
plot(perf3, add=TRUE, col="green")
plot(perf4, add=TRUE, col="pink")
plot(perf5, add=TRUE, col="orange")
legend(x = 0.7, y = 0.3, legend = c("Knn", "Nb", "Xgb","Descison Tree",'SVM'),
       col = c("red", "blue", "green","pink",'orange'), lty = 1, cex = 0.8)

# 绘制调优之后的曲线
# knn_fit_after <- train(ReviewRating~.,data = balance.both,method="knn",
#                     trControl = trainControl(method = "cv",number = 10,search = "grid"),	
#                     metric = "Accuracy")
pred_knn <- predict(knn_fit_after, test, type = "prob")[,2]
perf_knn <- performance(prediction(pred_knn, as.numeric(test$ReviewRating)), "tpr", "fpr")
pred_df <- predict(dt_fit_after, test,type = "prob")[,2]
perf_df <- performance(prediction(pred_df, as.numeric(test$ReviewRating)), "tpr", "fpr")
pred_svm <- predict(svm_fit_after, test,type = "prob")[,2]
perf_svm <- performance(prediction(pred_svm, as.numeric(test$ReviewRating)), "tpr", "fpr")
pred_nb <- predict(nb_fit_after, test,type = "prob")[,2]
perf_nb <- performance(prediction(pred_nb, as.numeric(test$ReviewRating)), "tpr", "fpr")
pred_xgb <- predict(xgb_fit_after, test,type = "prob")[,2]
perf_xgb <- performance(prediction(pred_xgb, as.numeric(test$ReviewRating)), "tpr", "fpr")
plot(perf_knn, color = "red", lwd = 2, main = "ROC Curve",
     xlab = "False Positive Rate", ylab = "True Positive Rate")
plot(perf_df, add=TRUE, col="blue")
plot(perf_svm, add=TRUE, col="green")
plot(perf_nb, add=TRUE, col="pink")
plot(perf_xgb, add=TRUE, col="orange")

legend(x = 0.7, y = 0.3, legend = c("Knn", "Nb", "Xgb","Descison Tree",'SVM'),
       col = c("red", "blue", "green","pink",'orange'), lty = 1, cex = 0.8)



