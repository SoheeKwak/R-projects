subject_name<-c('Jone','Jane','Steve')
#c:벡터 생성 함수
subject_name
temperature<-c(98.1, 98.6, 101.4)
temperature[2]
flu_status<-c(FALSE, FALSE, TRUE)
flu_status[2:3]
flu_status[-3] #3번째 위치를 제외한 나머지 

temperature[c(FALSE, FALSE, TRUE)]

#범주형
gender<-factor(c('MALE', 'FEMALE','MALE'))
gender
blood<-factor(c('O','AB','A'), 
              levels = c('A','B','O','AB'))
blood

#순서있는 팩터형
symptoms<-factor(c('SEVERE', 'MILD','MODERATE'),
                 levels = c('MILD','MODERATE','SEVERE'),
                 ordered = TRUE)
symptoms

#리스트:데이터형이 짬뽕~
gender[1]

subject1<-list(fullname=subject_name[1],
               temperature=temperature[1],
               flu_status=flu_status[1],
               gender=gender[1],
               blood=blood[1],
               symptoms=symptoms[1])
subject1[2]
subject1[[2]]
subject1$temperature
subject1[c('temperature', "flu_status")]

pt_data<-data.frame(subject_name,temperature,flu_status,
                    gender,blood,symptoms,stringsAsFactors = FALSE)
str(pt_data)#변수에 저장된 데이터 구조
pt_data[,1]#1열
pt_data[1,]#1행
pt_data[,]#전체
pt_data[c(1,3)] #1열과 3열
pt_data[c(1,3), c("temperature", "gender")]
pt_data[-2,c(-1,-3,-5)]#2행, 1,3,5열을 제외

#행렬
m<-matrix(c(1,2,3,4), nrow=2)
m<-matrix(c(1,2,3,4,5,6), ncol=2)#3행 2열
m
m[1,1]
m[3,2]
m[1,]
m[,1]

ls() #메모리에 있는 변수 목록 확인
rm(m, subject1)
ls()
m
rm(list=ls())
ls()

usedcars<-read.csv('d:/r_work/usedcars.csv',
                   stringsAsFactors = FALSE)
str(usedcars)
#요약통계(6 number)
summary(usedcars$year)
summary(usedcars[c("price","mileage")])

(36000+44000+56000)/3
mean(c(36000,44000,56000))
median(c(36000,44000,56000))#중위수
range(usedcars$price)#최소값 최대값
diff(range(usedcars$price))#최대-최소
IQR(usedcars$price)
quantile(usedcars$price)

#분위수를 얻고자 하는 경우에 probs옵션 지정
quantile(usedcars$price, probs = c(0.01, 0.99))
#seq함수
quantile(usedcars$price, seq(from=0,to=1,by=0.20))

boxplot(usedcars$price,
        main="Boxplot of UCP",
        ylab="price($)")
boxplot(usedcars$mileage,
        main="Boxplot of UCM",
        ylab="Odometer(mi.)")

#historygram
hist(usedcars$price, main=
       "Hist of UCP",xlab = 'price')

var(usedcars$price)
sd(usedcars$price)

table(usedcars$year)
table(usedcars$model)
table(usedcars$color)

model_table<-table(usedcars$model)
prop.table(model_table) #표 비율

color_table<-table(usedcars$color)
color_pct<-prop.table(color_table)*100
round(color_pct,digits = 1)

plot(x=usedcars$mileage, y=usedcars$price,
     main="scatterplot",xlab='uso',
     ylab='usp')

str(usedcars)
usedcars$conservative<-usedcars$color %in% 
  c("Black", "Gray", "Silver", "White")
table(usedcars$conservative)

install.packages("gmodels")
library(gmodels)
CrossTable(x=usedcars$model, 
           y=usedcars$conservative)