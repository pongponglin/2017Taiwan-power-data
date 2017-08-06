#### 找 outlier 

Data_class = read.csv('final6_class.csv')

### testing


threshold1 = 1000 
threshold2 = 1200 
threshold3 = 1250 
threshold4 = 1400 
threshold5 = 950 




par(mfrow = c(2,3))
M = matrix(0,4,5) %>% as.data.frame()
names(M) = c('第一群','第二群','第三群','第四群','第五群')
row.names(M) = c('每戶用電平均', '正常用電每戶用電平均', '高用電每戶用電平均', '高用電戶占的比例')
M = t(M)

######################################################

X = {Data_class %>% filter(分群 == 1)}$戶均用電  ;
X %>% hist(breaks = seq(450, 2600,50))
abline(v = threshold1, col = 2)   # 第五群  


M[1,1] = mean(X) 
M[1,2] = X[X<threshold1] %>% mean() 
M[1,3] = X[X>threshold1] %>% mean()
M[1,4] = sum(X > threshold1) / length(X)


######################################################

X = {Data_class %>% filter(分群 == 2)}$戶均用電  ;
X %>% hist(breaks = seq(450, 2600,50))
abline(v = threshold2, col = 2)   # 第五群  

sum(X > threshold2) / length(X)


M[2,1] = mean(X) 
M[2,2] = X[X<threshold2] %>% mean() 
M[2,3] = X[X>threshold2] %>% mean()
M[2,4] = sum(X > threshold2) / length(X)

######################################################

X = {Data_class %>% filter(分群 == 3)}$戶均用電  ;
X %>% hist(breaks = seq(450, 2600,50))
abline(v = threshold3, col = 2)   # 第五群  


M[3,1] = mean(X) 
M[3,2] = X[X<threshold3] %>% mean() 
M[3,3] = X[X>threshold3] %>% mean()
M[3,4] = sum(X > threshold3) / length(X)


######################################################

X = {Data_class %>% filter(分群 == 4)}$戶均用電
X %>% hist(breaks = seq(450, 2600,50))
abline(v = threshold4, col = 2)   # 第五群  


M[4,1] = mean(X) 
M[4,2] = X[X<threshold4] %>% mean() 
M[4,3] = X[X>threshold4] %>% mean()
M[4,4] = sum(X > threshold4) / length(X)


######################################################

X = {Data_class %>% filter(分群 == 5)}$戶均用電
X %>% hist(breaks = seq(450, 2600,50))
abline(v = threshold5, col = 2)   # 第五群  


M[5,1] = mean(X) 
M[5,2] = X[X<threshold5] %>% mean() 
M[5,3] = X[X>threshold5] %>% mean()
M[5,4] = sum(X > threshold5) / length(X)


############################################################
############################################################


Data_class$用電狀態 = 0
for(i in 1:dim(Data_class)[1]){
 
  if(Data_class$戶均用電[i] > get(eval(paste0('threshold',Data_class$分群[i])))){
    Data_class[i, dim(Data_class)[2]] <- '高用電'
  }else{
    Data_class[i, dim(Data_class)[2]] <- '正常or低用電'
  }
}



###################################
# 存擋

rbind(Data_class$戶均用電, Data_class$分群, Data_class$用電狀態) %>% t() %>% View()

write.csv(Data_class, 'Data_class_outlier.csv',row.names = F,  fileEncoding = 'UTF-8')


rbind(M, c(mean(Data_class$戶均用電), NULL, NULL, NULL))




