# 사전 추가
library(tm); library("KoNLP"); # rJava
useSejongDic(); # useNIADic()

# 데이터 로드 
news<-readLines('C:/Users/okso6/temp/ADP/PART 06 실습용 데이터/키워드_뉴스.txt')

# Corpus 형태로 변환 
# news.corpus <-VCorpus(news) 
# Error in VCorpus(news) : inherits(x, "Source") is not TRUE
news.corpus <-VCorpus(VectorSource(news))

# 데이터 살펴보기 
inspect(news.corpus)
news.corpus[[1]]$content

# 전처리 - Corpus 대상 
clean_txt <- function(x){
    x<-tm_map(x,removeNumbers) # 숫자 제거
    x<-tm_map(x,removePunctuation) # 문장부호 제거    
    x<-tm_map(x,stripWhitespace) # 공백제거
    return (x)}
clean.news <- clean_txt(news.corpus)
clean.news[[1]]$content

# 전처리 - Corpus 대상 X
clean_txt2 <- function(x){
    x<-removeNumbers(x) # 숫자 제거
    x<-removePunctuation(x) # 문장부호 제거    
    x<-stripWhitespace(x) # 공백제거
    x<-gsub("[^[:alnum:]]", " ", x)
    return (x)}
clean2.news <- clean_txt2(news) # Corpus 아님 
clean2.news[1]

# gsub로 특수문자 제거 
txt2 <- gsub("[[:punct:]]", "", clean.news[[1]])
txt2

# 사전 추가
buildDictionary(ext_dic="sejong",
user_dic=data.frame(c('스르륵'),c('mag'))) # 단어, 품사태그셋 
mergeUserDic(data.frame(c('스르륵'),c('mag')))

# 명사추출
extractNoun(clean2.news[1])

# 형태소 분석
MorphAnalyzer(clean2.news[1])
# 형태소 분리 SimplePos09 SimplePos22
library("stringr") 
doc1<-paste(SimplePos22(clean2.news[1]))
doc2<-str_match(doc1, "([가-힣]+)/PA") # 형용사 추출 
doc2[!is.na(doc2[,2])]

# 어간 추출
stemDocument(); stemCompletion()

# TDM
VC.news<-VCorpus(VectorSource(clean2.news))
TDM.news<-TermDocumentMatrix(VC.news)
dim(TDM.news); inspect(TDM.news[1:3,])

# 명사만 추출하여 TDM
words <- function(doc){
    doc<-as.character(doc)
    extractNoun(doc)}
TDM.news2 <- TermDocumentMatrix(VC.news, control = list(tokenize=words))
inspect(TDM.news2)

# TDM 빈도 체크
tdm2<-as.matrix(TDM.news2)
tdm3<-rowSums(tdm2)
tdm4<-tdm3[order(tdm3, decreasing = T)]
tdm5<-tdm4[1:20]

# 빈도 시각화
barplot(tdm5[1:5], main="title", col=rainbow(5))
legend("topright", names(tdm5[1:5]), fill=rainbow(5))

# 연관성 
findAssocs(TDM.news2, '빅데이터', 0.8)

# 워드클라우드
library(wordcloud)
wordcloud(words = names(tdm5), freq = tdm5, min.freq = 2, random.order = F, colors = brewer.pal(8, 'Dark2'))

# 명사 추출 빈도top10 시각화 
barplot(tdm5[1:10], main="Noun top 10")

# 2음절 이상 -> 시각화 
Noun <- as.vector(unlist(extractNoun(clean2.news)))
Noun2 <- Noun[nchar(Noun) >= 2]
result <- data.frame(sort(table(Noun2), decreasing = TRUE))
wordcloud(result$Noun2, result$Freq, colors = brewer.pal(8, "Dark2"), min.freq = 10)

# 기간별 빈도
blog <-read.csv('C:/Users/okso6/temp/ADP/모의고사 R코드 및 데이터/모의고사 3회/공구 블로그 댓글.txt', sep='\t')
blog$Date <- substr(as.character(blog$Date), 1, 10) # Date: 2014.02.14\t 
blog$Date <- as.Date(blog$Date, format='%Y.%m.%d')
blog$month <- as.numeric(format(blog$Date, '%m'))
blog_2 <- subset(blog, blog$month == 2) # 2월 
# 2월의 명사 빈도
clean_txt3 <- function(x){
    x<-removeNumbers(x) # 숫자 제거
    x<-removePunctuation(x) # 문장부호 제거    
    x<-stripWhitespace(x) # 공백제거
    x<- gsub("[^[:alnum:]]", " ", x)
    x<- gsub("[[:punct:]]", "", x) 
    x<- gsub("\u3160", "", x) 
    return(x)}
blog_2$Content <- clean_txt3(blog_2$Content)

Nn <- as.vector(unlist(extractNoun(blog_2$Content)))
Nn2 <- Nn[nchar(Nn) >= 2]
table.NnCnt <- head(sort(table(Nn2), decreasing = T), 5)
pie(table.NnCnt)

# 사전으로 사전 추가 
dict <- readLines('C:/Users/okso6/temp/ADP/모의고사 R코드 및 데이터/모의고사 3회/사전.txt')
user_dict <- data.frame(dict, "ncn") # ncn 비서술형명사
buildDictionary(ext_dic = "woorimalsam", user_dic = user_dict, replace_usr_dic = T)