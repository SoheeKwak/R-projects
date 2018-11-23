## 텍스트 분석(감성분석)

install.packages("tidytext")
install.packages("tidyr")
library(tidytext)
library(tidyr)
#AFINN 감정어휘 사전
get_sentiments("afinn")
#감정어휘에 대해 -5(가장 부정적)~5(가장 긍정적)
AFINN<-data.frame(get_sentiments("afinn"))
AFINN
hist(AFINN$score, breaks = 20, col='blue')

#렉시콘 감정어휘 사전(pos, neg)
get_sentiments("bing")

##emolex 감정어휘 사전(pos, neg,anger, fear, surprise, sadness, joy, disgust, trust 등)
get_sentiments("nrc")

oplex<-data.frame(get_sentiments("bing"))
table(oplex$sentiment)

emolex<-data.frame(get_sentiments("nrc"))
table(emolex$sentiment)

emolex$word[emolex$sentiment=='anger']


library("tm")
library("stringr")
library("dplyr")

my.text.location <- "D:/R_work/papers"
mypaper<-VCorpus(DirSource(my.text.location), 
                 readerControl = list(laguage="en"))
mypaper # documents: 24 files
inspect(mypaper)

mytxt<-c(rep((NA),24))
mytxt

for (i in 1:24){
  mytxt[i]<-as.character(mypaper[[i]][1])
}
mytxt

#Transformation to 24 rows, 2 columns
my.df.text<- data_frame(paper.id=1:24, doc=mytxt)
my.df.text

#Document-Words matrix
# %>% (pipe operation) formation: "variable %>% function"
my.df.text.word<-my.df.text %>% unnest_tokens(word,doc) #unnest_tokens: document->words)
my.df.text.word #Shifting from 24*2 document-> to 3,505 x 2 words

# Sentiment Analysis by applying data to sentiment words dic.(get_sentiments("bing"))
# Merge data(my.df.text.word) with dic. through inner_join fundction ->Joining, by = "word"
myresult.sa<-my.df.text.word %>% 
  inner_join(get_sentiments("bing"))
myresult.sa

myresult.sa<-my.df.text.word %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word,paper.id, sentiment) %>% #numbers of words
  spread(sentiment, n, fill=0) #fill=0 : fill zero into the data not observed
#Identify the  numbers of variable "sentiment" by features (either positive or negative)
myresult.sa

#The parameter of sentiment term dic is "Word". But our analysis parameter is "document"

myresult.sa<-my.df.text.word %>% 
  inner_join(get_sentiments("bing"))
myresult.sa
myresult.sa<-myresult.sa %>% count(word,paper.id, sentiment)
myresult.sa
myresult.sa<-myresult.sa %>% spread(sentiment, n, fill=0)
myresult.sa


#Calculate the sum of positive sentiment terms and negative terms by document, using "summerise" function
#Caculate the differences btw positive and negative terms
myagg<-summarise(group_by(myresult.sa, paper.id),
          pos.sum=sum(positive),
          neg.sum=sum(negative),
          pos.sent=pos.sum-neg.sum)

myagg

#Intergration of mata-data of paper ducument with the results of sentiment analysis
myfilenames <- list.files(path=my.text.location,pattern = NULL, all.files = TRUE)

paper.name<-myfilenames[3:26]
pub.year<-as.numeric(unlist(str_extract_all(paper.name, "[[:digit:]]{4}")))
paper.id<-1:24
pub.year.df<-data.frame(paper.id, paper.name, pub.year)
pub.year.df

myagg<- merge(myagg,pub.year.df, by='paper.id',all=TRUE)
myagg

myagg.long <- reshape(myagg, idvar = 'paper.id', varying = list(2:4), timevar = 'category', v.names = 'value', direction = 'long')
myagg.long

myagg.long$cate[myagg.long$category==1]<-'positive words'
myagg.long$cate[myagg.long$category==2]<-'negative words'
myagg.long$cate[myagg.long$category==3]<-'positive score'


library('ggplot2')
ggplot(data=myagg.long, aes(x=pub.year, y=value))+
  geom_bar(stat = 'identity')+
  labs(s='publication year', y='value')+
  scale_x_continuous(limits = c(2009,2015))+
  facet_grid(cate~.)












