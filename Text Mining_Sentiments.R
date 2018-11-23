install.packages("tidytext")
install.packages("tidyr")
library(tidytext)
library(tidyr)
#AFINN 감정어휘 사전
get_sentiments("afinn")
#감정어휘에 대해 -5(가장 부정적)~5(가장 긍정적)
AFINN<-data.frame(get_sentiments("afinn"))
AFINN
hist(AFINN$score, breaks = 20,col='blue')

#렉시콘 감정어휘 사전(pos, neg)
get_sentiments("bing")

##emolex 감정어휘 사전(pos, neg,anger, fear, 
#surprise, sadness, joy, disgust, trust 등)
get_sentiments("nrc")

oplex<-data.frame(get_sentiments("bing"))
table(oplex$sentiment)

emolex<-data.frame(get_sentiments("nrc"))
table(emolex$sentiment)

emolex$word[emolex$sentiment=='anger']

#matplotlib.org



