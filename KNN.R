#knn:최근접이웃, 레이블 없는 데이터를 레이블된 유사한 그룹에 할당
#예-개인별 선호도 예측, 병 진단
getwd()
#setwd("c:/다른경로") 작업디렉토리를 변경
wbcd<-read.csv("D:/R_work/wisc_bc_data.csv", stringsAsFactors = FALSE)
str(wbcd)
wbcd<-wbcd[-1]
str(wbcd)

table(wbcd$diagnosis)
str(wbcd$diagnosis)
wbcd$diagnosis<-factor(wbcd$diagnosis, 
                       levels = c('B', 'M'),
                       labels=c('Benign', 'Malignant'))
wbcd$diagnosis
str(wbcd$diagnosis)
wbcd

round(prop.table(table(wbcd$diagnosis))*100,
      digits = 1)
summary(wbcd[c("radius_mean", "area_mean", 
               "smoothness_mean")])

normalize <- function(x){
  return ((x-min(x)) / (max(x)-min(x)))
}

normalize(c(1,2,3,4,5))
normalize(c(10,20,30,40,50))

str(wbcd)
wbcd_n<-lapply(wbcd[2:31],normalize)#데이터에 특정 함수를 적용해라
wbcd_n<-as.data.frame(wbcd_n)

str(wbcd_n)
summary(wbcd_n$area_mean)

#569건: train:1~469, test:470~569
wbcd_train<-wbcd_n[1:469,]
wbcd_test<-wbcd_n[470:569,]

wbcd_train_labels<-wbcd[1:469,1]
wbcd_test_labels<-wbcd[470:569,1]

head(wbcd_train,5)
head(wbcd_test,5)
head(wbcd_train_labels,5)
head(wbcd_test_labels,5)

#knn모델링:단순히 입력데이터를 구조화된 형식으로 저장
#knn에서 데이터를 분류하기 위한 함수: class
install.packages("class")
library(class)

table(wbcd$diagnosis)

wbcd_test_pred<-knn(train=wbcd_train, test=wbcd_test,
                    cl=wbcd_train_labels, k=21) #K는 임의로 ㅈ

# 모델 성능 평가줌
CrossTable(x=wbcd_test_labels, y=wbcd_test_pred, prop.chisq=FALSE)

