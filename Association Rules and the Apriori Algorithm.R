# 연관규칙
install.packages("arules")
library(arules)
#groceries <- read.csv("D:/R_work/groceries.csv", sep=",")
#groceries
groceries <- read.transactions("D:/R_work/groceries.csv", sep=",")
summary(groceries) 

inspect(groceries[1:5])
itemFrequencyPlot(groceries, support=0.1)
#support
itemFrequencyPlot(groceries, topN=15)
image(sample(groceries, 100))

#suppot=0.1, confidence=0.8 (default설정): 이 데이터에서 confidence가 너무 높음. 재조정 할 필요
apriori(groceries)

groceryrules<-apriori(groceries, parameter = list(support=0.006, confidence=0.25, minlen=2))

#minlen: 2개 미만의 아이템을 갖는 규칙을 제외
#ex) {}->전유
groceryrules
summary(groceryrules)
inspect(groceryrules[1:3])

inspect(sort(groceryrules, by="lift")[1:10])

# 연관 규칙의 부분집합
#berry가 다른 어떤 아이템과 자주 구매
berryrules<-subset(groceryrules, items %in% "berries")
inspect(berryrules)

write(groceryrules, file = "D:/R_work/groceryrules.csv", sep=",", quote=TRUE, row.names=FALSE)
#convert the rule set to a data frame
groceryrules_df<-as(groceryrules, "data.frame")
str(groceryrules_df)
