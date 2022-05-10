library(dplyr)
library(ggplot2)
library(tidyverse)
library(scales)
library(RColorBrewer)

#######################################################################
## 대여시간 별 운동량 비교

## 1월
str(t_jan)

# 사용시간 , 운동량을 수치형으로 변환
t_jan24$사용시간 <- as.numeric(t_jan24$사용시간)
t_jan24$운동량 <- as.numeric(t_jan24$운동량)

# 결측치 처리
t_jan24 <- na.omit(t_jan24)

# 대여시간별 평균운동량 나타냄
e_1 <- t_jan24 %>%
  group_by(대여시간) %>% summarise(mean_ex = round(mean(운동량),2))

View(e_1)


e1 <- ggplot(e_1, aes(x=대여시간, y=mean_ex)) +
  geom_line(color='#9370DB', size=3) +
  xlab('대여시간') +
  ylab('운동량 평균') +
  ggtitle('대여시간 별 평균 운동량 비교(1월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))


e1


## 2월

t_feb24$사용시간 <- as.numeric(t_feb24$사용시간)
t_feb24$운동량 <- as.numeric(t_feb24$운동량)
t_feb24 <- na.omit(t_feb24)

e_2 <- t_feb24 %>%
  group_by(대여시간) %>% summarise(mean_ex = round(mean(운동량),2))

View(e_2)


e2 <- ggplot(e_2, aes(x=대여시간, y=mean_ex)) +
  geom_line(color='#9370DB', size=3) +
  xlab('대여시간') +
  ylab('운동량 평균') +
  ggtitle('대여시간 별 평균 운동량 비교(2월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))


e2


## 3월

t_mar24$사용시간 <- as.numeric(t_mar24$사용시간)
t_mar24$운동량 <- as.numeric(t_mar24$운동량)
t_mar24 <- na.omit(t_mar24)

e_3 <- t_mar24 %>%
  group_by(대여시간) %>% summarise(mean_ex = round(mean(운동량),2))

View(e_3)


e3 <- ggplot(e_3, aes(x=대여시간, y=mean_ex)) +
  geom_line(color='#9370DB', size=3) +
  xlab('대여시간') +
  ylab('운동량 평균') +
  ggtitle('대여시간 별 평균 운동량 비교(3월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))


e3


## 4월

t_apr24$사용시간 <- as.numeric(t_apr24$사용시간)
t_apr24$운동량 <- as.numeric(t_apr24$운동량)
t_apr24 <- na.omit(t_apr24)

e_4 <- t_apr24 %>%
  group_by(대여시간) %>% summarise(mean_ex = round(mean(운동량),2))

View(e_4)


e4 <- ggplot(e_4, aes(x=대여시간, y=mean_ex)) +
  geom_line(color='#9370DB', size=3) +
  xlab('대여시간') +
  ylab('운동량 평균') +
  ggtitle('대여시간 별 평균 운동량 비교(4월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))


e4


## 5월

t_may24$사용시간 <- as.numeric(t_may24$사용시간)
t_may24$운동량 <- as.numeric(t_may24$운동량)
t_may24 <- na.omit(t_may24)

e_5 <- t_may24 %>%
  group_by(대여시간) %>% summarise(mean_ex = round(mean(운동량),2))

View(e_5)


e5 <- ggplot(e_5, aes(x=대여시간, y=mean_ex)) +
  geom_line(color='#778899', size=3) +
  xlab('대여시간') +
  ylab('운동량 평균') +
  ggtitle('대여시간 별 평균 운동량 비교(5월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))


e5


## 6월

t_june24$사용시간 <- as.numeric(t_june24$사용시간)
t_june24$운동량 <- as.numeric(t_june24$운동량)
t_june24 <- na.omit(t_june24)

e_6 <- t_june24 %>%
  group_by(대여시간) %>% summarise(mean_ex = round(mean(운동량),2))

View(e_6)


e6 <- ggplot(e_6, aes(x=대여시간, y=mean_ex)) +
  geom_line(color='#778899', size=3) +
  xlab('대여시간') +
  ylab('운동량 평균') +
  ggtitle('대여시간 별 평균 운동량 비교(6월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))


e6

## 7월

t_july24$사용시간 <- as.numeric(t_july24$사용시간)
t_july24$운동량 <- as.numeric(t_july24$운동량)
t_july24 <- na.omit(t_july24)

e_7 <- t_july24 %>%
  group_by(대여시간) %>% summarise(mean_ex = round(mean(운동량),2))

View(e_7)


e7 <- ggplot(e_7, aes(x=대여시간, y=mean_ex)) +
  geom_line(color='#778899', size=3) +
  xlab('대여시간') +
  ylab('운동량 평균') +
  ggtitle('대여시간 별 평균 운동량 비교(7월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))

e7

## 8월
t_aug24$사용시간 <- as.numeric(t_aug24$사용시간)
t_aug24$운동량 <- as.numeric(t_aug24$운동량)
t_aug24 <- na.omit(t_aug24)

e_8 <- t_aug24 %>%
  group_by(대여시간) %>% summarise(mean_ex = round(mean(운동량),2))

View(e_8)


e8 <- ggplot(e_8, aes(x=대여시간, y=mean_ex)) +
  geom_line(color='#778899', size=3) +
  xlab('대여시간') +
  ylab('운동량 평균') +
  ggtitle('대여시간 별 평균 운동량 비교(8월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))

e8

## 9월
t_sep24$사용시간 <- as.numeric(t_sep24$사용시간)
t_sep24$운동량 <- as.numeric(t_sep24$운동량)
t_sep24 <- na.omit(t_sep24)

e_9 <- t_sep24 %>%
  group_by(대여시간) %>% summarise(mean_ex = round(mean(운동량),2))

View(e_9)


e9 <- ggplot(e_9, aes(x=대여시간, y=mean_ex)) +
  geom_line(color='#87CEEB', size=3) +
  xlab('대여시간') +
  ylab('운동량 평균') +
  ggtitle('대여시간 별 평균 운동량 비교(9월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))

e9


## 10월
t_oct24$사용시간 <- as.numeric(t_oct24$사용시간)
t_oct24$운동량 <- as.numeric(t_oct24$운동량)
t_oct24 <- na.omit(t_oct24)

e_10 <- t_oct24 %>%
  group_by(대여시간) %>% summarise(mean_ex = round(mean(운동량),2))

View(e_10)


e10 <- ggplot(e_10, aes(x=대여시간, y=mean_ex)) +
  geom_line(color='#87CEEB', size=3) +
  xlab('대여시간') +
  ylab('운동량 평균') +
  ggtitle('대여시간 별 평균 운동량 비교(10월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))

e10

## 11월
t_nov24$사용시간 <- as.numeric(t_nov24$사용시간)
t_nov24$운동량 <- as.numeric(t_nov24$운동량)
t_nov24 <- na.omit(t_nov24)

e_11 <- t_nov24 %>%
  group_by(대여시간) %>% summarise(mean_ex = round(mean(운동량),2))

View(e_11)


e11 <- ggplot(e_11, aes(x=대여시간, y=mean_ex)) +
  geom_line(color='#87CEEB', size=3) +
  xlab('대여시간') +
  ylab('운동량 평균') +
  ggtitle('대여시간 별 평균 운동량 비교(11월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))

e11


## 12월
t_dec24$사용시간 <- as.numeric(t_dec24$사용시간)
t_dec24$운동량 <- as.numeric(t_dec24$운동량)
t_dec24 <- na.omit(t_dec24)

e_12 <- t_dec24 %>%
  group_by(대여시간) %>% summarise(mean_ex = round(mean(운동량),2))

View(e_12)


e12 <- ggplot(e_12, aes(x=대여시간, y=mean_ex)) +
  geom_line(color='#87CEEB', size=3) +
  xlab('대여시간') +
  ylab('운동량 평균') +
  ggtitle('대여시간 별 평균 운동량 비교(12월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))

e12




##################################################################
# boxplot으로 이상치 확인

names = c('1월', '2월', '3월', '4월')
col = c('#FFB6C1', '#FFE4E1', '#FFEFD5', '#F0F8FF')

# boxplot으로 각 달의 평균운동량 비교
boxplot(e_1$mean_ex,e_2$mean_ex, e_3$mean_ex, e_4$mean_ex,
        main ='1~4월 대여시간 별 운동량 비교', names = names,
        col=col,
        ylab='평균 운동량')
  
  
names = c('5월', '6월', '7월', '8월')
col = c('#FFB6C1', '#FFE4E1', '#FFEFD5', '#F0F8FF')
boxplot(e_5$mean_ex,e_6$mean_ex, e_7$mean_ex, e_8$mean_ex,
        main ='5~8월 대여시간 별 운동량 비교', names = names,
        col=col,
        ylab='평균 운동량')


names = c('9월', '10월', '11월', '12월')
col = c('#FFB6C1', '#FFE4E1', '#FFEFD5', '#F0F8FF')
boxplot(e_9$mean_ex,e_10$mean_ex, e_11$mean_ex, e_12$mean_ex,
        main ='9~12월 대여시간 별 운동량 비교', names = names,
        col=col,
        ylab='평균 운동량',
        ylim=c(30,200)) # ylim의 최대값을 늘릴 수록 이상치 증가




####################################################################
## 분기별 평균 운동량 비교

seasons <- c('1분기', '2분기', '3분기', '4분기')
q1 <- c(round(mean(e_1$mean_ex),2), round(mean(e_2$mean_ex),2), round(mean(e_3$mean_ex),2))
q1 <- round(mean(q1),2)

q2 <- c(round(mean(e_4$mean_ex),2), round(mean(e_5$mean_ex),2), round(mean(e_6$mean_ex),2))
q2 <- round(mean(q2),2)

q3 <- c(round(mean(e_7$mean_ex),2), round(mean(e_8$mean_ex),2), round(mean(e_9$mean_ex),2))
q3 <- round(mean(q3),2)

q4 <- c(round(mean(e_10$mean_ex),2), round(mean(e_11$mean_ex),2), round(mean(e_12$mean_ex),2))
q4 <- round(mean(q4),2)

qs <- c(q1,q2,q3,q4)

# 분기별(q1~q4) 값들의 평균을 낸 뒤 데이터프레임 생성
df_q <- data.frame(qs, seasons)

# 분기별로 나누어 평균운동량 막대그래프로 표현현
ggplot(data=df_q, aes(x=seasons, y=qs)) +
  geom_bar(stat='identity', fill = col) +
  xlab('') +
  ylab('평균 운동량') +
  labs(fill = '분기') +
  ggtitle('2021년 분기 별 평균운동량 비교') +
  theme(plot.title=element_text(size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12)) +
  geom_label(aes(label=qs), nudge_y = 1.1)
