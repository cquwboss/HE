library(mlbench)
library(caret)
library(ggplot2)
library(randomForest)
library(readr)
library(corrplot)
library(car)
#è¯»å–æ•°æ®
data<- read.csv("C:\\Users\\admin\\Desktop\\482.csv")
#»®·ÖÊý¾Ý¼¯
set.seed(101)
ind = sample(2,nrow(data),replace = TRUE,prob = c(0.8,0.2))
train=data[ind == 1,]
test=data[ind == 2,]
#å› å­ç›¸å…³æ€§ã€é‡è¦æ€§åˆ†æžã€çƒ­åŠ›å›¾ã€å…±çº¿æ€§è¯Šæ–?
ma1<-cor(data)
ma2<-cor(data[,1],data[2:22])
ma1
ma2
corrplot(corr=ma1,method="color",type="lower")
a=lm(score~., data=data)
summary(a)
importance = varImp(a,scale = FALSE)
pre<-predict(a,data)
outlierTest(lm(pm1~data$score))
vif(a)

#Ö÷³É·Ö·ÖÎö
data <- read.csv("C:\\Users\\admin\\Desktop\\370.csv", header = T)  
pr <- princomp(data[,-1], cor = T,¡¡scores = T)
#¹Û²ìÖ÷³É·Ö·ÖÎöµÄÏêÏ¸Çé¿ö
summary(pr, loadings = T)
#Çó³öcor(df)µÄÌØÕ÷ÖµºÍÌØÕ÷ÏòÁ¿
y=eigen(cor(data))  
#Êä³öÌØÕ÷Öµ£¬È·¶¨Âú×ãÌØÕ÷Öµ£¾1.0µÄÖ÷³É·Ö¸öÊý£¨8¸ö£©
y$values 
#²é¿´ÀÛ¼Æ¹±Ï×ÂÊ£¬È·¶¨Âú×ãÀÛ¼Æ¹±Ï×ÂÊ£¾0.85µÄÖ÷³É·Ö¸öÊý£¨16¸ö£©
sum(y$values[1:15])/sum(y$values)
#Êä³öÇ°16¸öÖ÷³É·Ö£¨ÀÛ¼Æ¹±Ï×ÂÊ£¾0.85£©µÄÔØºÉ¾ØÕó
pr$loadings[,1:8]   
#ËéÊ¯Í¼£¬ÌØÕ÷Öµ£¾1.0µÄÖ÷³É·Ö¸öÊý£¨8¸ö£©
screeplot(pr,type = "lines") 
#»æÖÆÖ÷³É·Ö·½ÏòÍ¼/É¢µãÍ¼
biplot(pr) 
#Êä³öÇ°16¸öÖ÷³É·ÖµÄµÃ·Ö
s=pr$scores[,1:8] 
#¼ÆËã×ÛºÏµÃ·Ö
scores=0.0
for (i in 1:8)
  scores=(y$values[i]*s[,i])/(sum(y$values[1:8]))+scores
#Êä³ö×ÛºÏµÃ·ÖÐÅÏ¢
cbind(s,scores) 
#¼ÆËãµÃµ½¸÷¸öÑù±¾Ö÷³É·ÖµÄÊý¾Ý
pca_data <- predict(pr)
#Ð´ÈëÊý¾ÝÎÄ¼þ£¬Ö®ºó¿ÉÒÔÀûÓÃÖ÷³É·ÖÒò×Ó¹¹½¨Ä£ÐÍ
com1<-data.frame(data[,1],s)
write.csv(com1,file="C:\\Users\\admin\\Desktop\\com11.csv",row.names=TRUE)
com2<-data.frame(data[,1],pca_data[,1:16])
write.csv(com2,file="C:\\Users\\admin\\Desktop\\com22.csv",row.names=TRUE)

ma21<-cor(com2)
ma22<-cor(com1[,1],com2[2:8])
ma21
ma22
corrplot(corr=ma21,method="color",type="lower")

#éšæœºæ•°ç§å­ï¼Œå¯éšä¾¿è®¾ç½®ï¼ˆå³é€‰æ ·æœ¬æ—¶çš„é—´éš”ï¼‰
data1<- read.csv("C:\\Users\\admin\\Desktop\\com22.csv")
set.seed(101)
#»®·ÖÊý¾Ý¼¯
ind = sample(2,nrow(data1),replace = TRUE,prob = c(0.8,0.2))
train=data1[ind == 1,]
test=data1[ind == 2,]
#¶à´ÎKÕÛ½»²æÑéÖ¤,Èç5ÕÛ400´Î½»²æÑéÖ¤
folds<-createMultiFolds(y=data$score,k=5,times=400) 
for(i in 1:5){
  traindata<-data[-folds[[i]],]
  testdata<-data[folds[[i]],]
    rf<-randomForest(score~., traindata,mtry=3,ntree=850, nPerm=10, proximity=TRUE, importance=TRUE) 
    pm<-predict(rf,testdata)
  l<-lm(pm~test$score)
  summary(l)
  if(er<min) {
    min =er     
    num=i }
}
print(min)
print(num)

# å…¨éƒ¨é»˜è®¤ï¼Œfuntionsæ˜¯åšéšæœºæ£®æž—çš„å›žå½?
control <- rfeControl(functions=rfFuncs, method="boot632",number=50,verbose = FALSE, returnResamp = "final")
#[2:22]å?21ä¸ªå› å­æ‰€åœ¨åˆ—,[,1]å³Yï¼Œc(1:22)å³ç•™ä¸‹çš„å˜é‡ä»?1ç•™åˆ°22
results <- rfe(train[,2:17],train[,1], sizes=c(1:17), rfeControl=control)
# è¾“å‡ºç»“æžœ
print(results)
# åˆ—å‡ºç­›é€‰å‡ºçš„å˜é‡?
predictors(results)
# ç”»å‡ºæ›²çº¿
plot(results, type=c("g", "o"))

#ä¼˜åŒ–è¶…å‚æ•?
#æ‰¾æœ€ä¼˜mtry
#mtry_fit<- randomForest(score~Mi7+Mi6+Po8+Po9+Pr1+Mi5+Po6+Mi4+Po5+Mi3+Mi2+Pr3+Pr2+Po2+Po1+Po7+Po3+Po10+Po4+Mi1+Pr4, data=data, mtry=i)
n<-length(names(train))
set.seed(9)
min=1000
num=0
for (i in 1:(n-1)){
  mtry_fit<- randomForest(score~., data=train, mtry=i,na.action=na.roughfix)
  err<-mean(mtry_fit$mse)#å¦‚æžœæ˜¯åˆ†ç±»æ”¹mseä¸ºerr.rate
  print(err)
  if(err<min) {
    min =err     
    num=i }
}
print(min)
print(num)
#numå³ä¸ºmtryä¸ªæ•°
#å¸¦å…¥mtryï¼Œå°è¯•å¯»æ‰¾ntree
ntree_fit<-randomForest(score~.,data=train,mtry=2,ntree=1000,na.action=na.roughfix)
plot(ntree_fit)

#å½¢æˆå‚æ•°ä¼˜åŒ–åŽçš„éšæœºæ£®æž—æ¨¡åž‹å¹¶ç›¸çœ‹æ¨¡åž‹å‚æ•°å’Œå› å­é‡è¦æ€?
#rÈ±Ê§Êý¾Ý²å²¹
dataI<-rfImpute(score~.,data=data, iter=10, ntree=500)
ind = sample(2,nrow(dataI),replace = TRUE,prob = c(0.8,0.2))
train=dataI[ind == 1,]
test=dataI[ind == 2,]
#°´È«²¿²å²¹Êý¾Ý½¨Ä£ÆÀ¼ÛÄâºÏ¶È
rfm<-randomForest(score~., data=dataI,mtry=5,ntree=500, nPerm=30, proximity=TRUE, importance=TRUE) 
rfm
varImpPlot(rfm)
#åšæ¨¡åž‹é¢„æµ?
pm1<-predict(rfm,dataI)
plot(dataI$score,pm1)
abline(lm(pm1~dataI[,1]),col="red")
summary(lm(pm1~dataI$score))
#È¥³ýÊý¾ÝÒì³£Öµ
outlier<-outlierTest(lm(pm1~dataI$score))
outlier
data2<-dataI[!dataI[,n]>=outlier_limup & dataI[,n]<=outlier_limdown,]

#°´8£º2ÑµÁ·²âÊÔÄ£ÐÍ
rfm<-randomForest(score~., data=train,mtry=7,ntree=500, nPerm=10, na.action=na.roughfix, proximity=TRUE, importance=TRUE) 
rfm
varImpPlot(rfm)
#åšæ¨¡åž‹é¢„æµ?
pm1<-predict(rfm,test,na.action=na.roughfix)
plot(test$score,pm1)
abline(lm(pm1~test[,1]),col="red")
summary(lm(pm1~test$score))

#é¢„æµ‹ç»“æžœä¸Žå®žé™…æˆç»©çš„æ‹Ÿåˆå…³ç³»
c<-data.frame(data1$V,pm1)
write.csv(c,file="C:\\Users\\admin\\Desktop\\2781.csv",row.names=TRUE)
p <- ggplot(data = c, aes(x = data1$V, y = pm1)) + 
  geom_point() + 
  geom_smooth(method = "lm", color = "red")+
                annotate("text", x = 75, y = 94, parse = TRUE, 
                         label = "y == 0.603937*x+36.34351", size = 4) +
                annotate("text", x = 75, y = 95, parse = TRUE, 
                         label = "atop(R^2==0.9465)", size = 4)
p

