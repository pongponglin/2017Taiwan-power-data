Data_class_outlier = read.csv('Data_class_outlier.csv')

library(rpart) 

Data_class_outlier$平均教育程度 = Data_class_outlier$博士比例*9+Data_class_outlier$碩士比例*6+Data_class_outlier$大學比例*4+Data_class_outlier$大學以下比例*0
Data_class_outlier$扶養比 = (Data_class_outlier$青少年比例+Data_class_outlier$老年比例)/Data_class_outlier$老年比例
i=1

tem = Data_class_outlier %>% 
  filter(分群 == i) %>% 
  select(行政區域, 中位數, 第一分位數, 第三分位數, 標準差, med, 商家數, 博士比例, 碩士比例, 大學比例,
  大學以下比例,  平均屋齡, 每戶平均老年人口數.人., 有偶比例..., 每戶平均人數,  平均教育程度,
  扶養比,用電狀態)



centered_tem <- scale(tem[,-c(1,dim(tem)[2])],center=TRUE,scale=TRUE)

tem_normalize <- cbind(centered_tem, {Data_class_outlier %>% filter(分群 == i)}$用電狀態) %>% 
  as.data.frame()


control<-rpart.control(minisplit=10,minbucket=3,xval=0)#minisplit就是node #minbucket就是葉子裡面要有幾個
treeorig<-rpart(V17~. ,data=tem_normalize,method="class",control=control) 


library(rpart.plot) 
par(family="STHeitiTC-Light")
prp(treeorig,         # 模型
    faclen=0,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    shadow.col="gray",  # 最下面的節點塗上陰影
    # number of correct classifications / number of observations in that node
    extra=2)  
treeorig$cptable




