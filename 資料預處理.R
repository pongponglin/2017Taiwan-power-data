
setwd("/Users/xiaopingguo/Downloads/")
Data = read.csv('final5.csv', header = T, stringsAsFactors = F)

Data$平均教育程度 = Data$博士比例*9+Data$碩士比例*6+Data$大學比例*4+Data$大學以下比例*0
Data$扶養比 = (Data$青少年比例+Data$老年比例)/Data$老年比例

library(dplyr)
Data = Data[, -which(names(Data) =="gen_sum")]
Data = Data[, -which(names(Data) =="戶數")]
Data = Data[, -which(names(Data) =="壯年人口")]
Data = Data[, -which(names(Data) =="老年人口")]
Data = Data[, -which(names(Data) =="X__2")]
Data = Data[, -which(names(Data) =="綜合所得總額")]
Data = Data[, -which(names(Data) =="納稅單位")]
Data = Data[, -which(names(Data) =="X1人一宅宅數.宅.")]
Data = Data[, -which(names(Data) =="X2人一宅宅數.宅.")]
Data = Data[, -which(names(Data) =="X3人一宅宅數.宅.")]
Data = Data[, -which(names(Data) =="X4人一宅宅數.宅.")]
Data = Data[, -which(names(Data) =="X5人一宅宅數.宅.")]
Data = Data[, -which(names(Data) =="X6人以上一宅宅數.宅.")]
Data = Data[, -which(names(Data) =="無老年人口戶數.戶.")]
Data = Data[, -which(names(Data) =="一位老年人口戶數.戶.")]
Data = Data[, -which(names(Data) =="二位老年人口戶數.戶.")]
Data = Data[, -which(names(Data) =="三位以上老年人口戶數.戶.")]

Data = Data[, -which(names(Data) =="總計")]
Data = Data[, -which(names(Data) =="僅老年人口居住宅數.宅.")]
Data = Data[, -which(names(Data) =="一名老年人口宅數.宅.")]
Data = Data[, -which(names(Data) =="二名老年人口宅數.宅.")]
Data = Data[, -which(names(Data) =="三名以上老年人口宅數.宅.")]
Data = Data[, -which(names(Data) =="總戶長數.人.")]
Data = Data[, -which(names(Data) =="有偶.人.")]
Data = Data[, -which(names(Data) =="離婚.人.")]
Data = Data[, -which(names(Data) =="喪偶.人.")]
Data = Data[, -which(names(Data) =="未婚.人.")]
Data = Data[, -which(names(Data) =="婚姻關係消滅.人.")]
Data = Data[, -which(names(Data) =="設有戶籍宅數.宅..x")]
#Data = Data[, -which(names(Data) =="商家數")]
Data = Data[, -which(names(Data) =="公園數")]
Data = Data[, -which(names(Data) =="博士")]
Data = Data[, -which(names(Data) =="碩士")]
Data = Data[, -which(names(Data) =="大學")]
Data = Data[, -which(names(Data) =="大學以下")]
Data = Data[, -which(names(Data) =="公園坪數")]
Data = Data[, -which(names(Data) =="青少年人口")]
Data = Data[, -which(names(Data) =="X1人一宅宅數比例...")]
Data = Data[, -which(names(Data) =="X2人一宅宅數比例...")]
Data = Data[, -which(names(Data) =="X3人一宅宅數比例...")]
Data = Data[, -which(names(Data) =="X4人一宅宅數比例...")]
Data = Data[, -which(names(Data) =="X5人一宅宅數比例...")]
Data = Data[, -which(names(Data) =="X6人以上一宅宅數比例...")]
Data = Data[, -which(names(Data) =="一位老年人口戶數比例...")]
Data = Data[, -which(names(Data) =="一名老年人口宅數比例...")]
Data = Data[, -which(names(Data) =="二位老年人口戶數比例...")]
Data = Data[, -which(names(Data) =="二名老年人口宅數比例...")]
Data = Data[, -which(names(Data) =="三位以上老年人口戶數比例...")]
Data = Data[, -which(names(Data) =="三名以上老年人口宅數比例...")]
Data = Data[, -which(names(Data) =="婚姻關係消滅比例...")]
Data = Data[, -which(names(Data) =="喪偶比例...")]
Data = Data[, -which(names(Data) =="變異係數")]
Data = Data[, -which(names(Data) =="離婚比例...")]
Data = Data[, -which(names(Data) =="未婚比例...")]
#Data = Data[, -which(names(Data) =="第一分位數")]
#Data = Data[, -which(names(Data) =="第三分位數")]
Data = Data[, -which(names(Data) =="平均數")]
Data = Data[, -which(names(Data) =="壯年比例")]
Data = Data[, -which(names(Data) =="老年比例")]
Data = Data[, -which(names(Data) =="青少年比例")]
#Data = Data[, -which(names(Data) =="博士比例")]
#Data = Data[, -which(names(Data) =="碩士比例")]
#Data = Data[, -which(names(Data) =="大學以下比例")]
#Data = Data[, -which(names(Data) =="大學比例")]
Data = Data[, -which(names(Data) =="設有戶籍宅數之平均人口數.人.")]
Data = Data[, -which(names(Data) =="無老年人口戶數比例...")]


Result = step2(pre_processing(Data), 5)


##### 匯出 分群後的原始檔案
Data = read.csv('final5.csv', header = T, stringsAsFactors = F)
Data$分群 = Result[[3]]

write.csv(Data,'final6_class.csv', row.names = F,  fileEncoding = 'UTF-8')



names(Data)
g = read.csv('final6_class.csv')



#Data = Data[, -which(substr(names(Data), 1,1) == 'X')[-c(1:2)]]


pre_processing <- function(Data){
  if(is.na(Data) %>% sum() != 0){
    ans = readline('NA is in Data, we may replace NA with 0, please reply y or n ')
    
    state = TRUE
    while(state){
      if(ans == 'y'){
        Data[is.na(Data)] <- 0
        
        state = FALSE
      }else if(ans == 'n'){
        output = NULL
        break
      }else{
        ans = readline('please type y or n')
      }
    }
    
  }
  
  
  {Data[,-c(1:4)] -> tem_Data } %>%   # 刪掉前四行非變數 
    apply(2, class)  %>%              # 看變數格式是否正確
    as.vector() -> title
  
  if ( ! unique(title) %in% c('numeric','factor')){
    print('Data structure is not all numeric or factor')
    output = NULL
  }
  
  threshold = 0.85   # corvariance higher than threshold, then => colinear
  
  if ( TRUE %in% {cor(tem_Data) %>% is.na() %>% as.vector()} ){
    M = cor(tem_Data) %>% is.na()
    output = names(tem_Data)[ apply(M, 2, sum) == dim(M)-1 ]  # 哪些變數造成cor(M) = NA
    print('these variables have some mistack, causing corvariance matrix = NA')
    
  }else{
    if ( sum(abs(cor(tem_Data) ) > threshold) > 0 ){
      cat('There are', sum(abs(cor(tem_Data) ) > threshold), 'pairs of variable which
               covariance is higher than ', threshold, '. Do you want to see that? y or n :')
      ans1 = readline( 'y or n :')
      if(ans1 == 'y'){
        M = abs(cor(tem_Data))
        row = {which(M > threshold) - 1}  %% dim(M)[1] +1
        col = {which(M > threshold) - 1} %/% dim(M)[1] + 1
        row != col-> index
        row <- row[index] ; col <- col[index] 
        for(i in 1:length(row)){
          cat( row.names(M)[row[i]] , 'and', colnames(M)[col[i]], 'have higher covariance \n' )
        }
        ans2 = readline('Go ahead ? y or n :')
      }else{
        ans2 = readline('Go ahead ? y or n :')
      }
      
      if(ans2 == 'n'){output = pairs(tem_Data)}
    }else{
      print('Variables do no collinear')
    }
    
  }

  
  fit <- lm(log(戶均用電) ~ . , data= tem_Data )
  
  summary(fit) # show results
  coefficients(fit)
  predict = step(fit, direction = "backward", trace=FALSE ) 
  a = predict$coefficients %>% names()  
  
  predict = step(fit, direction = "forward", trace=FALSE ) 
  b = predict$coefficients %>% names()
  output = intersect(b,a)
  
  
  return(output)
}


######################################################################

step2 <- function(x,z=4){
  cluster_Data = Data[, which( names(Data) %in% x[-1] )]
  
  
  
  # 32 個變數 => 分群 => 每群在轉成指標
  # 轉成指標 => 分群
  
  
  library(cluster)
  centered.y <- scale(cluster_Data,center=TRUE,scale=TRUE)
  y<-daisy(cluster_Data, stand=T)
  agn<-agnes(y,metric="euclidean",method="ward")
  plot(agn,which.plots=2)
  
  k.cl <- kmeans(centered.y , centers=z,nstart = 2000)
  centered.y<-data.frame(centered.y)
  centered.y$cluster<-k.cl$cluster
  
  
  
  
  
  return(list( 'score' = k.cl, 'table' =   table(k.cl$cluster), 'class' = k.cl$cluster ))
}
