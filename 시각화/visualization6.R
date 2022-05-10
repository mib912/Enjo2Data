library(dplyr)
library(ggplot2)
library(tidyverse)
library(scales)
library(RColorBrewer)

##################################################################
### 대여일시에 따른 대여건수 비교

## 1월

# ,를 없앤 뒤 숫자형으로 변환
d_jan$대여건수 <- gsub(',','',d_jan$대여건수)
d_jan$대여건수 <- as.numeric(d_jan$대여건수)
View(d_jan)
str(d_jan)

# 대여일시 중 월, 일만 나타날 수 있도록 문자형 변환하여 substr 사용
ggplot(d_jan, aes(x=substr(as.character(대여일시),6,10), y=대여건수)) +
  geom_bar(stat='identity', fill = '#FFC0CB') +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) +
  xlab('대여일시') +
  ggtitle('2021년 1월 대여건수') + 
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))
  


## 2월
d_feb$대여건수 <- gsub(',','',d_feb$대여건수)
d_feb$대여건수 <- as.numeric(d_feb$대여건수)


ggplot(d_feb, aes(x=substr(as.character(대여일시),6,10), y=대여건수)) +
  geom_bar(stat='identity', fill = '#FFC0CB') +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) +
  xlab('대여일시') +
  ggtitle('2021년 2월 대여건수') + 
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))



## 3월
d_mar$대여건수 <- gsub(',','',d_mar$대여건수)
d_mar$대여건수 <- as.numeric(d_mar$대여건수)


ggplot(d_mar, aes(x=substr(as.character(대여일시),6,10), y=대여건수)) +
  geom_bar(stat='identity', fill = '#FFC0CB') +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) +
  xlab('대여일시') +
  ggtitle('2021년 3월 대여건수') + 
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))


## 4월
d_apr$대여건수 <- gsub(',','',d_apr$대여건수)
d_apr$대여건수 <- as.numeric(d_apr$대여건수)


ggplot(d_apr, aes(x=substr(as.character(대여일시),6,10), y=대여건수)) +
  geom_bar(stat='identity', fill = '#FFC0CB') +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) +
  xlab('대여일시') +
  ggtitle('2021년 4월 대여건수') + 
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))


## 5월
d_may$대여건수 <- gsub(',','',d_may$대여건수)
d_may$대여건수 <- as.numeric(d_may$대여건수)


ggplot(d_may, aes(x=substr(as.character(대여일시),6,10), y=대여건수)) +
  geom_bar(stat='identity', fill = '#B0E0E6') +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) +
  xlab('대여일시') +
  ggtitle('2021년 5월 대여건수') + 
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))


## 6월
d_june$대여건수 <- gsub(',','',d_june$대여건수)
d_june$대여건수 <- as.numeric(d_june$대여건수)


ggplot(d_june, aes(x=substr(as.character(대여일시),6,10), y=대여건수)) +
  geom_bar(stat='identity', fill = '#B0E0E6') +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) +
  xlab('대여일시') +
  ggtitle('2021년 6월 대여건수') + 
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))




## 7월
d_july$대여건수 <- gsub(',','',d_july$대여건수)
d_july$대여건수 <- as.numeric(d_july$대여건수)


ggplot(d_july, aes(x=substr(as.character(대여일시),6,10), y=대여건수)) +
  geom_bar(stat='identity', fill = '#B0E0E6') +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) +
  xlab('대여일시') +
  ggtitle('2021년 7월 대여건수') + 
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))



## 8월
d_aug$대여건수 <- gsub(',','',d_aug$대여건수)
d_aug$대여건수 <- as.numeric(d_aug$대여건수)


ggplot(d_aug, aes(x=substr(as.character(대여일시),6,10), y=대여건수)) +
  geom_bar(stat='identity', fill = '#B0E0E6') +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) +
  xlab('대여일시') +
  ggtitle('2021년 8월 대여건수') + 
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))



## 9월
d_sep$대여건수 <- gsub(',','',d_sep$대여건수)
d_sep$대여건수 <- as.numeric(d_sep$대여건수)


ggplot(d_sep, aes(x=substr(as.character(대여일시),6,10), y=대여건수)) +
  geom_bar(stat='identity', fill = '#DDA0DD') +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) +
  xlab('대여일시') +
  ggtitle('2021년 9월 대여건수') + 
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))



## 10월
d_oct$대여건수 <- gsub(',','',d_oct$대여건수)
d_oct$대여건수 <- as.numeric(d_oct$대여건수)


ggplot(d_oct, aes(x=substr(as.character(대여일시),6,10), y=대여건수)) +
  geom_bar(stat='identity', fill = '#DDA0DD') +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) +
  xlab('대여일시') +
  ggtitle('2021년 10월 대여건수') + 
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))


## 11월
d_nov$대여건수 <- gsub(',','',d_nov$대여건수)
d_nov$대여건수 <- as.numeric(d_nov$대여건수)


ggplot(d_nov, aes(x=substr(as.character(대여일시),6,10), y=대여건수)) +
  geom_bar(stat='identity', fill = '#DDA0DD') +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) +
  xlab('대여일시') +
  ggtitle('2021년 11월 대여건수') + 
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))



## 12월
d_dec$대여건수 <- gsub(',','',d_dec$대여건수)
d_dec$대여건수 <- as.numeric(d_dec$대여건수)


ggplot(d_dec, aes(x=substr(as.character(대여일시),6,10), y=대여건수)) +
  geom_bar(stat='identity', fill = '#DDA0DD') +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) +
  xlab('대여일시') +
  ggtitle('2021년 12월 대여건수') + 
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))



##########################################################################
## 분기별로 대여건수 나타내기

# 3달씩 나누어 각 변수에 저장
test1 <- rbind(d_jan, d_feb, d_mar)
test2 <- rbind(d_apr, d_may, d_june)
test3 <- rbind(d_july, d_aug, d_sep)
test4 <- rbind(d_oct, d_nov, d_dec)

str(test1)
View(month(test1$대여일시))

# 1분기 대여건수
df1 <- test1 %>% group_by(month(test1$대여일시)) %>% summarise(mean_num = round(mean(대여건수),2))
View(df1)

names(df1) <- c('대여일시(월)', '대여건수')
q1 <- subset(df1, select=c(대여건수))
q1 <- round(mean(q1$대여건수),2)
q1

# 2분기 대여건수
df2 <- test2 %>% group_by(month(test2$대여일시)) %>% summarise(mean_num = round(mean(대여건수),2))
View(df2)

names(df2) <- c('대여일시(월)', '대여건수')

q2 <- subset(df2, select=c(대여건수))
q2 <- round(mean(q2$대여건수),2)
q2

# 3분기 대여건수
df3 <- test3 %>% group_by(month(test3$대여일시)) %>% summarise(mean_num = round(mean(대여건수),2))
View(df3)

names(df3) <- c('대여일시(월)', '대여건수')

q3 <- subset(df3, select=c(대여건수))
q3 <- round(mean(q3$대여건수),2)
q3

# 4분기 대여건수
df4 <- test4 %>% group_by(month(test4$대여일시)) %>% summarise(mean_num = round(mean(대여건수),2))
View(df4)

names(df4) <- c('대여일시(월)', '대여건수')

q4 <- subset(df4, select=c(대여건수))
q4 <- round(mean(q4$대여건수),2)
q4

# 각 분기별 데이터 합치기
qs <- c(q1,q2,q3,q4)
seasons <- c('1분기', '2분기', '3분기', '4분기')
df_q <- data.frame(qs, seasons)


ggplot(data=df_q, aes(x=seasons, y=qs, fill=as.factor(seasons))) +
  geom_bar(stat='identity') +
  xlab('') +
  ylab('대여건수') +
  labs(fill = '분기') +
  ggtitle('2021년 분기 별 대여건수 비교') +
  theme(plot.title=element_text(size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12)) +
  geom_label(aes(label=qs), nudge_y = 1.1)
  



##################################################################
# boxplot으로 나타내기

names = c('1월', '2월', '3월', '4월')
col = c('#FFB6C1', '#FFE4E1', '#FFEFD5', '#F0F8FF')

boxplot(d_jan$대여건수,d_feb$대여건수, d_mar$대여건수, d_apr$대여건수,
        main ='1~4월 대여건수 비교', names = names,
        col=col,
        ylab='대여건수')

boxplot(d_may$대여건수,d_june$대여건수, d_july$대여건수, d_aug$대여건수,
        main ='5~8월 대여건수 비교', names = names,
        col=col,
        ylab='대여건수')

boxplot(d_sep$대여건수,d_oct$대여건수, d_nov$대여건수, d_dec$대여건수,
        main ='9~12월 대여건수 비교', names = names,
        col=col,
        ylab='대여건수')

######################################################################
# 1년데이터에 대해 boxplot으로 나타내기

names = c('1월', '2월', '3월', '4월', '5월','6월','7월','8월','9월','10월','11월','12월')
col = c('#FFB6C1', '#FFE4E1', '#FFEFD5', '#F0F8FF', '#D8BFD8', '#FFC0CB',
        '#B0E0E6', '#87CEFA', '#E6E6FA', '#B0E0E6', '#6495ED', '#F58F98')
boxplot(d_jan$대여건수,d_feb$대여건수, d_mar$대여건수, d_apr$대여건수,
        d_may$대여건수,d_june$대여건수, d_july$대여건수, d_aug$대여건수,
        d_sep$대여건수,d_oct$대여건수, d_nov$대여건수, d_dec$대여건수,
        main ='2021년 월별 대여건수 비교',
        names = names,
        col = col,
        ylab='대여건수')


