# 텍스트마이닝 in r : 리스트 형식
myvector <- c(1:6, 'a') #문자가 있으면 요소가 모두 문자화
myvector #"1" "2" "3" "4" "5" "6" "a"

mylist <- list(1:6,"a") 
mylist


#형태소-> 단어-> 문장-> 문단->문서
#텍스트마이닝에서는 통상적으로 list 함수를 씀
obj1<-1:4
obj2<-6:10
obj3<-list(obj1, obj2)
obj3
class(obj1)

mylist<-list(obj1, obj2, obj3)
mylist[[3]][1] #리스트 참조 시 [] 리스트를 구함
mylist[[3]][[1]] #[[]]벡터를 구함
mylist[[3]][[1]][3]

#unlist함수: 리스트를 벡터형식으로 변환해 주는 함수(사용 주의)
myvector<-c(1:6,'a')
mylist<-list(1:6, 'a')
class(myvector)
unlist(mylist)
myvector==unlist(mylist)

mean(mylist[[1]][1:6])
mean(unlist(mylist)[1:6]) #unlist: NA 벡터형식의 문자라서 평균을 구할 수 없음

name1<-"Donald"
myspace<-" "
name2<-"Trump"
list(name1, myspace, name2)
unlist(list(name1, myspace, name2))

# 단어들을 조합하여 문장을 구성, 또는 문장->문단, 문단->문서...텍스트 마이닝 작업시 위와 같은 형식으로 활용

#데이터 속성 부여
name<-c('갑','을','병','정')
gender<-c(2,1,1,2)
mydata<-data.frame(name, gender)
mydata

#메타데이터(data of data)입력 in r : attr()
attr(mydata$name,"what the variable means")<-"응답자 이름"
mydata$name

attr(mydata$gender,"what the variable means")<-"응답자 성별"
mydata$gender

myvalues<-gender
for(i in 1:length(gender)){
  myvalues[i]<-ifelse(gender[i]==1, "남성","여성")
}

attr(mydata$gender, "what the value means")<-myvalues
mydata$gender

#속성값 추출
attr(mydata$gender, "what the value means")
mydata$gender.character<-attr(mydata$gender, "what the value means")
mydata

mylist<- list(1:4,6:10, list(1:4,6:10))
mylist
lapply(mylist[[3]], mean) #lapply:각 해당 리스트 요소들에 대한 계산(여기선 평균)을 하게 해줌

letters[3]
LETTERS[3]
letters[1:26]
LETTERS[1:26]

tolower("Eye for eye")
toupper("Eye for eye")

nchar('korea')
nchar('korea', type='bytes')

nchar('한국')
nchar('한국', type='bytes')

nchar('korea ')
nchar('korea\t')
nchar('korea\t',type = 'bytes')
x<-'korea, republic of'
nchar(x)
x<-'korea, \nrepublic of'
nchar('korea, \nrepublic of')

#단어 단위로 문장을 분해
mysentence<-'Learning R is so interesting'
strsplit(mysentence, split = ' ')

#문자 단위로 단어를 분해
mywords<-strsplit(mysentence, split = ' ')
class(mywords)
strsplit(mywords[[1]][5], split = '')

myletters <-list(rep(NA,5))
for(i in 1:5){
  myletters[i]<-strsplit(mywords[[1]][i], split = '')
}
myletters


#문자를 원래의 형태로 조합
paste(myletters[[1]], collapse = '')

mywords2<-list(rep(NA,5))

for(i in 1:5){성
  mywords2[i]<-paste(myletters[[1]][i], collapse = '')
}
#5개의 단어를 공백으로 구분하여 합쳐서 문장을 구ㅅ
mywords2
paste(mywords2, collapse = ' ')
mywords2


R_wiki<-"R is a programming language and free software environment for statistical computing and graphics supported by the R Foundation for Statistical Computing.[6] The R language is widely used among statisticians and data miners for developing statistical software[7] and data analysis.[8] Polls, data mining surveys and studies of scholarly literature databases, show substantial increases in popularity in recent years.[9] As of August 2018, R ranks 18th in the TIOBE index, a measure of popularity of programming languages.[10]

A GNU package[11], source code for the R software environment is written primarily in C, Fortran and R itself[12] and is freely available under the GNU General Public License. Pre-compiled binary versions are provided for various operating systems. Although R has a command line interface, there are several graphical user interfaces, such as RStudio, an Integrated development environment"

R_wiki_para <- strsplit(R_wiki, split='\n')
R_wiki_para

#문단을 문장 단위로 분해
R_wiki_sent<-strsplit(R_wiki_para[[1]], split = '\\.')
R_wiki_sent


#단어 단위로 분해
R_wiki_word <-list(NA,NA)
for(i in 1:2){
  R_wiki_word[[i]]<-strsplit(R_wiki_sent[[i]],split = ' ')
}
R_wiki_word
R_wiki_word[[1]][[2]][3]


mysentence<-"Learning R is so interesting"
regexpr('ing', mysentence)

#시작 위치
loc.begin <-as.vector(regexpr('ing', mysentence))
loc.begin 

#패턴 길이
loc.length <-as.vector(attr(regexpr('ing', mysentence),"match.length"))
loc.length

#끝나는 위치
loc.end<-lo.begin+loc.length-1
loc.end

#패턴(ing)이 등장하는 모든 위치가 추출
gregexpr('ing',mysentence)

#패턴이 등장한 횟ㅅ
length(gregexpr('ing',mysentence[[1]]))

#시작위치
loc.begin<-as.vector(gregexpr('ing',mysentence[[1]]))
loc.begin

#패턴길이
loc.length<-as.vector(attr(regexpr('ing', mysentence[[1]]),"match.length"))
loc.length


#끝나는 위치
loc.end<-lo.begin+loc.length-1
loc.end


regexpr('interesting', mysentence)
regexec('interesting', mysentence)

#문장 벡터화
mysentences<- unlist(R_wiki_sent) 

#패턴의 위치
regexpr("software", mysentences)

#패턴의 위치
gregexpr("software", mysentences)



mytemp <-regexpr("software", mysentences)
my.begin<- as.vector(mytemp)
my.begin

#-1 -> NA
my.begin[my.begin==-1]<-NA
my.begin

#끝위치
my.end<-my.begin+as.vector(attr(mytemp,"match.length"))-1
my.end

mylocs<-matrix(NA, nrow=length(my.begin), ncol = 2)
mylocs

colnames(mylocs)<-c('begin','end')
mylocs

rownames(mylocs)<-paste('sentence', 1:length(my.begin),sep='.')
mylocs

for(i in 1:length(my.begin)){
  mylocs[i,]<-cbind(my.begin[i], my.end[i])
}
mylocs


#다른 방법
grep('software', mysentences)
grepl('software', mysentences)

#sub함수: 특정 문자를 다른 문자로 변경
sub('ing','ING', mysentence) #첫번째 검색된 것만 교체
gsub('ing','ING', mysentence) #검색된 모든 것 교체


#고유명사 처리
sent1<-R_wiki_sent[[1]][1]
sent1
new.sent1<-gsub("R Foundation for Statistical Computing",
                "R Foundation for Statistical Computing",
                sent1)

new.sent1
sum(table(strsplit(sent1,split = ' ')))
sum(table(strsplit(new.sent1,split = ' ')))

#문장에서 특정 단어 삭제
drop.sent1<-gsub("and |by |for |the","",new.sent1)
new.sent1
drop.sent1

mypattern<-regexpr('ing', mysentence)
mypattern
regmatches(mysentence, mypattern)

mypattern<-gregexpr('ing', mysentence)
regmatches(mysentence, mypattern)


mypattern<-regexpr('ing', mysentence)
regmatches(mysentence, mypattern, invert = TRUE)

mypattern<-gregexpr('ing', mysentence)
regmatches(mysentence, mypattern, invert = TRUE)

strsplit(mysentence,split = 'ing') #ing를 기준으로 좌우로 나눔

gsub('ing','', mysentence)

substr(mysentences, 1,30)

my2sentence <- c("Learning R is so interesting",
                 "He is fascinating singer")
mypattern0<- gregexpr('ing', my2sentence)
mypattern0


mypattern1<-gregexpr('[[:alpha:]](ing)',my2sentence)
mypattern1

mypattern1<-gregexpr('[[:digit:]](ing)',my2sentence)
mypattern1

mypattern1<-gregexpr('[[:upper:]](ing)',my2sentence)
mypattern1

mypattern1<-gregexpr('[[:lower:]](ing)',my2sentence)
mypattern1

#ing앞에 올 알파벳을 출ㄹ
mypattern1<-gregexpr('[[:alpha:]](ing)',my2sentence)
regmatches(my2sentence, mypattern1)

#ing앞에 오는 최소 1번 이상의 알파벳을 출ㄹ
mypattern2<-gregexpr('[[:alpha:]]+(ing)',my2sentence)
regmatches(my2sentence, mypattern2)

#[[:alpha:]]+(ing)형식으로 끝나야 출ㄹ
mypattern3<-gregexpr('[[:alpha:]]+(ing)\\b',my2sentence)
regmatches(my2sentence, mypattern3)

mypattern<-gregexpr('[[:alpha:]]+(ing)\\b',mysentences)
myings <- regmatches(mysentences, mypattern)
myings
table(unlist(myings))


mypattern<-gregexpr('[[:alpha:]]+(ing)\\b',tolower(mysentences)
myings <- regmatches(tolower(mysentences), mypattern)
myings
table(unlist(myings))

#stat이 포함된 표혀
mypattern<-gregexpr('(stat)[[:alpha:]]+',tolower(mysentences)
regmatches(tolower(mysentences), mypattern)



