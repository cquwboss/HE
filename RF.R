library(mlbench)
library(caret)
library(ggplot2)
library(randomForest)
library(readr)
library(corrplot)
library(car)
#读取数据
data<- read.csv("C:\\Users\\admin\\Desktop\\482.csv")
#�������ݼ�
set.seed(101)
ind = sample(2,nrow(data),replace = TRUE,prob = c(0.8,0.2))
train=data[ind == 1,]
test=data[ind == 2,]
#因子相关性、重要性分析、热力图、共线性诊�?
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

#���ɷַ���
data <- read.csv("C:\\Users\\admin\\Desktop\\370.csv", header = T)  
pr <- princomp(data[,-1], cor = T,��scores = T)
#�۲����ɷַ�������ϸ���
summary(pr, loadings = T)
#���cor(df)������ֵ����������
y=eigen(cor(data))  
#�������ֵ��ȷ����������ֵ��1.0�����ɷָ�����8����
y$values 
#�鿴�ۼƹ����ʣ�ȷ�������ۼƹ����ʣ�0.85�����ɷָ�����16����
sum(y$values[1:15])/sum(y$values)
#���ǰ16�����ɷ֣��ۼƹ����ʣ�0.85�����غɾ���
pr$loadings[,1:8]   
#��ʯͼ������ֵ��1.0�����ɷָ�����8����
screeplot(pr,type = "lines") 
#�������ɷַ���ͼ/ɢ��ͼ
biplot(pr) 
#���ǰ16�����ɷֵĵ÷�
s=pr$scores[,1:8] 
#�����ۺϵ÷�
scores=0.0
for (i in 1:8)
  scores=(y$values[i]*s[,i])/(sum(y$values[1:8]))+scores
#����ۺϵ÷���Ϣ
cbind(s,scores) 
#����õ������������ɷֵ�����
pca_data <- predict(pr)
#д�������ļ���֮������������ɷ����ӹ���ģ��
com1<-data.frame(data[,1],s)
write.csv(com1,file="C:\\Users\\admin\\Desktop\\com11.csv",row.names=TRUE)
com2<-data.frame(data[,1],pca_data[,1:16])
write.csv(com2,file="C:\\Users\\admin\\Desktop\\com22.csv",row.names=TRUE)

ma21<-cor(com2)
ma22<-cor(com1[,1],com2[2:8])
ma21
ma22
corrplot(corr=ma21,method="color",type="lower")

#随机数种子，可随便设置（即选样本时的间隔）
data1<- read.csv("C:\\Users\\admin\\Desktop\\com22.csv")
set.seed(101)
#�������ݼ�
ind = sample(2,nrow(data1),replace = TRUE,prob = c(0.8,0.2))
train=data1[ind == 1,]
test=data1[ind == 2,]
#���K�۽�����֤,��5��400�ν�����֤
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

# 全部默认，funtions是做随机森林的回�?
control <- rfeControl(functions=rfFuncs, method="boot632",number=50,verbose = FALSE, returnResamp = "final")
#[2:22]�?21个因子所在列,[,1]即Y，c(1:22)即留下的变量�?1留到22
results <- rfe(train[,2:17],train[,1], sizes=c(1:17), rfeControl=control)
# 输出结果
print(results)
# 列出筛选出的变�?
predictors(results)
# 画出曲线
plot(results, type=c("g", "o"))

#优化超参�?
#找最优mtry
#mtry_fit<- randomForest(score~Mi7+Mi6+Po8+Po9+Pr1+Mi5+Po6+Mi4+Po5+Mi3+Mi2+Pr3+Pr2+Po2+Po1+Po7+Po3+Po10+Po4+Mi1+Pr4, data=data, mtry=i)
n<-length(names(train))
set.seed(9)
min=1000
num=0
for (i in 1:(n-1)){
  mtry_fit<- randomForest(score~., data=train, mtry=i,na.action=na.roughfix)
  err<-mean(mtry_fit$mse)#如果是分类改mse为err.rate
  print(err)
  if(err<min) {
    min =err     
    num=i }
}
print(min)
print(num)
#num即为mtry个数
#带入mtry，尝试寻找ntree
ntree_fit<-randomForest(score~.,data=train,mtry=2,ntree=1000,na.action=na.roughfix)
plot(ntree_fit)

#形成参数优化后的随机森林模型并相看模型参数和因子重要�?
#rȱʧ���ݲ岹
dataI<-rfImpute(score~.,data=data, iter=10, ntree=500)
ind = sample(2,nrow(dataI),replace = TRUE,prob = c(0.8,0.2))
train=dataI[ind == 1,]
test=dataI[ind == 2,]
#��ȫ���岹���ݽ�ģ������϶�
rfm<-randomForest(score~., data=dataI,mtry=5,ntree=500, nPerm=30, proximity=TRUE, importance=TRUE) 
rfm
varImpPlot(rfm)
#做模型预�?
pm1<-predict(rfm,dataI)
plot(dataI$score,pm1)
abline(lm(pm1~dataI[,1]),col="red")
summary(lm(pm1~dataI$score))
#ȥ�������쳣ֵ
outlier<-outlierTest(lm(pm1~dataI$score))
outlier
data2<-dataI[!dataI[,n]>=outlier_limup & dataI[,n]<=outlier_limdown,]

#��8��2ѵ������ģ��
rfm<-randomForest(score~., data=train,mtry=7,ntree=500, nPerm=10, na.action=na.roughfix, proximity=TRUE, importance=TRUE) 
rfm
varImpPlot(rfm)
#做模型预�?
pm1<-predict(rfm,test,na.action=na.roughfix)
plot(test$score,pm1)
abline(lm(pm1~test[,1]),col="red")
summary(lm(pm1~test$score))

#预测结果与实际成绩的拟合关系
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

