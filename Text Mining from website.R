

install.packages("rvest")
install.packages("stringr")
library(rvest)
library(stringr)
paper2016<-read_html("http://www.dbpia.co.kr/Journal/ArticleDetail/NODE07079332")
k.title<-paper2016 %>% html_node("h3") %>% html_text()
k.title

E.title<-paper2016 %>% html_node(".h3_sub_scr") %>% html_text()
E.title
#infoWrap > div > div.h3_sub_scr

author<-paper2016 %>% html_node(".writeInfo") %>% html_text()
author
author<-str_replace_all(author, "\r\n[[:space:]]+","")
author

abstract<-paper2016 %>% html_node(".con_txt") %>% html_text()
abstract
abstract.divide<-str_split(str_replace(abstract,"^\r\n[[:space:]]+" ,""),"\r\n")
k.abstract<-abstract.divide[[1]][2]
e.abstract<-abstract.divide[[1]][3]

k.abstract
e.abstract