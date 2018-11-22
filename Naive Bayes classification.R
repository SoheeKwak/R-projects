## Naive Bayes Classification

sms_raw <- read.csv("D:/R_work/sms_test.txt", stringsAsFactors = FALSE)
str(sms_raw)
sms_raw$type<-factor(sms_raw$type) #타입변환(char->factor)
str(sms_raw)
table(sms_raw$type)

# 텍스트 마이닝 패키지를 사용해서 corpus 생성
install.packages("tm")
library(tm) # require(tm)

# 데이터 준비: 텍스트 데이터 정리 & 표준화
#메일: 단어, 공백, 구두점, 숫자 등으로 구성된 텍스트 문자열
#위 작업을 tm패키지에서 제공

#텍스트 처리
#1) 코퍼스(텍스트 문서의 모음) 생성-이메일 코퍼스-> VCorpus()함수로 생성
#sms_raw$text벡터에서 vectorSource리더함수를 사용->소스객체를 생성->VCorpus()함수에 전달
sms_corpus <- VCorpus(VectorSource(sms_raw$text))
print(sms_corpus) #Content: documents: 60 =>훈련데이터가 60개
str(sms_corpus)

#inspect함수를 이용해서 메시지의 내용을 참조
inspect(sms_corpus[1:2]) #sms_corpus의 1,2번째 메시지 요약

#실제 텍스트 내용을 보고 싶다면 문자 함수로 변경
as.character(sms_corpus[[1]])
lapply(sms_corpus[1:2], as.character)


#Remove punctuations etc.
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower)) #shif to lower case
as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[[1]])

#Remove numbers
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)

#Remove stop words(to, and, or, but, ...)
sms_corpus_clean <-tm_map(sms_corpus_clean, removeWords, stopwords()) #stopwords(): list of stop words
as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[[1]])

sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)
as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[[1]])

removePunctuation("hi,hello...world!") #"hihelloworld"
replacePunctuation <- function(x){gsub("[[:punct:]]"," ",x)} #punctuation-> space; gsub(pattern, replacement, x); [:punct:]:Punctuation characters:! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~.
replacePunctuation("hi,hello...world!")  #"hi hello   world "

#Remove suffix: (ex)learn learns learning learned -> learn 
install.packages("SnowballC") 
library(SnowballC)
wordStem(c("learn","learned","learning","learns"))

sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)

inspect(sms_corpus_clean[1:2])
as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[[1]])

sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)

#교정 전과 후의 상태 비교
as.character(sms_corpus[[3]])
as.character(sms_corpus_clean[[3]])


#텍스트를 단어로 나누는 작업(토큰화), 토큰:텍스트 문자열의 요소(단어) #document(corpus)-term(word token)행렬로 변호
sms_dtm <- DocumentTermMatrix(sms_corpus_clean) #참고로 단어-문서 행렬로 변환해주는 함수:TermDocumentMatrix()
#예시(DTM;DocumentTermMatrix)
#메일번호 ball band night.....
#    1    0    0    1.......
#    2    0    0    1.......
#    3    0    1    1.......
#    4    0    1    1.......
sms_dtm  #<<DocumentTermMatrix (documents: 60, terms: 330) 행:60개, 열330

#전처리를 수행하지 않았다면 았파라미터를 설정하여 단어-문서 행렬로 변환하는 과정에서 전처리를 수행할 수 있음
sms_dtm2 <- DocumentTermMatrix(sms_corpus, control = list(
  tolower=TRUE, 
  removeNumbers=TRUE,
  stopwords=TRUE,
  removePunctuation=TRUE,
  stemming=TRUE))


sms_dtm3 <- DocumentTermMatrix(sms_corpus, control = list(
  tolower=TRUE, 
  removeNumbers=TRUE,
  stopwords=function(x){removeWords(x,stopwords())},
  removePunctuation=TRUE,
  stemming=TRUE))

sms_dtm
sms_dtm2
sms_dtm3


#inspect(sms_dtm)
sms_dtm_train <- sms_dtm[1:50,]
sms_dtm_train

sms_dtm_test <- sms_dtm[51:60,]
sms_dtm_test

sms_train_labels <- sms_raw[1:50,]$type
sms_test_labels <- sms_raw[51:60,]$type

sms_train_labels
sms_test_labels

#prop.table()
table(sms_train_labels)
table(sms_test_labels)

prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

install.packages("wordcloud")
library(wordcloud)
wordcloud(sms_corpus_clean, random.order = FALSE, random.color = T, colors = brewer.pal(9,"Set1"))  #min.freq = 3:최소3번 등장하는 단어만 출력

#sms_raw에 있는 모든 베시지에 대해 spam/ham 분류
spam <- subset(sms_raw, type=="spam")
ham <- subset(sms_raw, type=="ham")

spam
print("=============================================================")
ham

wordcloud(spam$text, random.order=FALSE, random.color=T, colors=brewer.pal(9,"Set1"))
wordcloud(ham$text, random.order=FALSE, random.color=T, colors=brewer.pal(9,"Set1"))



##최소 2번 이상 등장한 단어를 포함하는 문자 벡터를 구함
#DocumentTermMatrix (documents: 50, terms: 330)
sms_freq_words <- findFreqTerms(sms_dtm_train, lowfreq = 2)
str(sms_freq_words)

sms_dtm_train[,sms_freq_words] #terms: 62
sms_dtm_train #terms: 330

#최소 2개의 문서에서 나타나는 단어들만 추출
sms_dtm_freq_train <- sms_dtm_train[,sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[,sms_freq_words]

#나이브 베이즈 분류기는 범주형 특징 데이터에 대해서만 훈련
inspect(sms_dtm_freq_train)

#셀의 값을 숫자가 아닌 단어가 나타났는지의 여부로 변경(YES/NO)
convert_counts <- function(x){x<-ifelse(x>0, "YES", "NO")}

#Margin=1 행방향, 2:열방향
sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_train

sms_test <- apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)
sms_test


######################## 모델링 #######################################

## sms메시지가 스팸일 확률을 추정. 단어의 존재 or부재
#나이브베이지 패키지 : e1071
install.packages("e1071")
library(e1071)

sms_classifier <- naiveBayes(sms_train, sms_train_labels, laplace = 1)
sms_test_pred <- predict(sms_classifier, sms_test)

sms_test_pred

library(gmodels)
CrossTable(sms_test_pred, sms_test_labels, dnn = c('predicted','actual'))











