library(wordcloud)
library(wordcloud2)
library(stringr)
library(KoNLP)


# 데이터 준비
title <- read.csv(file="crawl_title.csv", header=TRUE, fileEncoding = 'UTF-8')
title



# 1. 배부된 자료에 연습 문제를 확인하고 워드 크라우드 작성
# 데이터 정제 작업을 더 추가할 것.

seoul <- title$TITLE


seoul2 <- extractNoun(seoul)

seoul3 <- unlist(seoul2)

data4 <- str_replace_all(seoul3,"[^[:alpha:]]","")
data4 <- gsub(" ","",data4)


# 제거 목록 파일 불러오기
seoul_gsub <- readLines('서울명소gsub.txt')

# 제거
for(i in 1:length(seoul_gsub)){
  data4 <- gsub(seoul_gsub[i],"",data4)
}

# 2글자 이상만 남기기
data4 <- Filter(function(x){nchar(x)>=2},data4)
data4

# 파일로 저장 후 다시 읽어오기
write(data4, 'title_2.txt')
data5 <- read.table('title_2.txt')

wordcount <- table(data5)

View(sort(wordcount, decreasing = T))


# wordcloud 그리기
palete <- brewer.pal(9,"Set3")

wordcloud(names(wordcount), # 출력할 단어들
          freq=wordcount,   # 언급된 빈도수
          scale=c(5,1),     # 글자 크기 5부터 1
          rot.per=0.4,     # 회전 단어 비욜
          min.freq = 1,     # 최소 단위(1번만 언급된 것도 표현하겠다는 뜻)
          random.order = F, # 출력되는 순서를 임의로 지정할 것인지 여부
          random.color = T, # 색상 랜덤 사용 여부
          colors=palete)    # 색상 정보

legend(0.3,1,
       "서울 명소",
       fill=NA,
       border=NA,
       bg='white',
       text.col='red',
       text.font=2,
       box.col='red')



wordcloud2(wordcount,
           size = 1.2,
           color=palete)



# 2. 배부된 baseball_news.xlsx 파일을 이용해서 타이틀 data를 활용해 워드 크라우드 작성

# 데이터 불러오기
library(readxl)
library(dplyr)
raw_baseball <- read_excel(path='baseball_news.xlsx')
View(raw_baseball)

b_title <- raw_baseball %>% select(title)

b_title1 <- c() # 빈 벡터 생성

# 모든 title에 대해서 명사 추출하여 하나의 파일로 만들기
for(i in 1:nrow(b_title)){
  b_title1 <- c(b_title1,extractNoun(b_title[i,]))
}

title2<- str_replace_all(b_title1,"[^[:alpha:]]","")
title2 <- gsub(" ","",title2)

# 2글자 이상인 것만 남기기
title2 <- Filter(function(x){nchar(x)>=2},title2)

# 파일로 저장 후 다시 읽어오기
write(title2, 'title2_2.txt')
title3 <- read.table('title2_2.txt')
View(title3)

# 언급 빈도 구하기
wordcount <- table(title3)

View(sort(wordcount, decreasing = T))



# wordcloud 그리기
palete <- brewer.pal(9,"Set3")

wordcloud(names(wordcount), # 출력할 단어들
          freq=wordcount,   # 언급된 빈도수
          scale=c(7,1),     # 글자 크기 5부터 1
          rot.per=0.4,     # 회전 단어 비욜
          min.freq = 1,     # 최소 단위(1번만 언급된 것도 표현하겠다는 뜻)
          random.order = F, # 출력되는 순서를 임의로 지정할 것인지 여부
          random.color = T, # 색상 랜덤 사용 여부
          colors=palete)    # 색상 정보

legend(0.3,1,
       "야구 뉴스",
       fill=NA,
       border=NA,
       bg='white',
       text.col='red',
       text.font=2,
       box.col='red')


wordcloud2(wordcount,
           size = 1.2,
           color=palete)
