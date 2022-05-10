library(dplyr)
library(ggplot2)
library(tidyverse)
library(scales)
library(RColorBrewer)

#######################################################################
## 사용시간이 가장 높거나 낮은 시간대별 탐색


## 1월
str(t_jan)
t_jan24$사용시간 <- as.numeric(t_jan24$사용시간)

u_1 <- t_jan24 %>%
  group_by(대여시간) %>% summarise(mean_use = round(mean(사용시간),2))

View(u_1)


u1 <- ggplot(u_1, aes(x=대여시간, y=mean_use)) +
  geom_line(color='#FA8072', size=3) +
  xlab('대여시간') +
  ylab('사용시간 평균') +
  ggtitle('대여시간 별 평균 사용시간 비교(1월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))


u1


## 2월
t_feb24$사용시간 <- as.numeric(t_feb24$사용시간)

t_feb24 <- na.omit(t_feb24)

u_2 <- t_feb24 %>%
  group_by(대여시간) %>% summarise(mean_use = round(mean(사용시간),2))

View(u_2)


u2 <- ggplot(u_2, aes(x=대여시간, y=mean_use)) +
  geom_line(color='#FA8072', size=3) +
  xlab('대여시간') +
  ylab('사용시간 평균') +
  ggtitle('대여시간 별 평균 사용시간 비교(2월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))

u2


## 3월
t_mar24$사용시간 <- as.numeric(t_mar24$사용시간)

t_mar24 <- na.omit(t_mar24)

u_3<- t_mar24 %>%
  group_by(대여시간) %>% summarise(mean_use = round(mean(사용시간),2))

View(u_3)


u3 <- ggplot(u_3, aes(x=대여시간, y=mean_use)) +
  geom_line(color='#FA8072', size=3) +
  xlab('대여시간') +
  ylab('사용시간 평균') +
  ggtitle('대여시간 별 평균 사용시간 비교(3월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))

u3



## 4월
t_apr24$사용시간 <- as.numeric(t_apr24$사용시간)

t_apr24 <- na.omit(t_apr24)

u_4 <- t_apr24 %>%
  group_by(대여시간) %>% summarise(mean_use = round(mean(사용시간),2))

View(u_4)


u4 <- ggplot(u_4, aes(x=대여시간, y=mean_use)) +
  geom_line(color='#FA8072', size=3) +
  xlab('대여시간') +
  ylab('사용시간 평균') +
  ggtitle('대여시간 별 평균 사용시간 비교(4월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))

u4

## 5월
t_may24$사용시간 <- as.numeric(t_may24$사용시간)

t_may24 <- na.omit(t_may24)

u_5 <- t_may24 %>%
  group_by(대여시간) %>% summarise(mean_use = round(mean(사용시간),2))

View(u_5)


u5 <- ggplot(u_5, aes(x=대여시간, y=mean_use)) +
  geom_line(color='#3CB371', size=3) +
  xlab('대여시간') +
  ylab('사용시간 평균') +
  ggtitle('대여시간 별 평균 사용시간 비교(5월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))

u5

## 6월
t_june24$사용시간 <- as.numeric(t_june24$사용시간)

t_june24 <- na.omit(t_june24)

u_6 <- t_june24 %>%
  group_by(대여시간) %>% summarise(mean_use = round(mean(사용시간),2))

View(u_6)


u6 <- ggplot(u_6, aes(x=대여시간, y=mean_use)) +
  geom_line(color='#3CB371', size=3) +
  xlab('대여시간') +
  ylab('사용시간 평균') +
  ggtitle('대여시간 별 평균 사용시간 비교(6월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))

u6

## 7월
t_july24$사용시간 <- as.numeric(t_july24$사용시간)

t_july24 <- na.omit(t_july24)

u_7 <- t_july24 %>%
  group_by(대여시간) %>% summarise(mean_use = round(mean(사용시간),2))

View(u_7)


u7 <- ggplot(u_7, aes(x=대여시간, y=mean_use)) +
  geom_line(color='#3CB371', size=3) +
  xlab('대여시간') +
  ylab('사용시간 평균') +
  ggtitle('대여시간 별 평균 사용시간 비교(7월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))

u7


## 8월
t_aug24$사용시간 <- as.numeric(t_aug24$사용시간)

t_aug24 <- na.omit(t_aug24)

u_8 <- t_aug24 %>%
  group_by(대여시간) %>% summarise(mean_use = round(mean(사용시간),2))

View(u_8)


u8 <- ggplot(u_8, aes(x=대여시간, y=mean_use)) +
  geom_line(color='#3CB371', size=3) +
  xlab('대여시간') +
  ylab('사용시간 평균') +
  ggtitle('대여시간 별 평균 사용시간 비교(8월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))

u8


## 9월
t_sep24$사용시간 <- as.numeric(t_sep24$사용시간)

t_sep24 <- na.omit(t_sep24)

u_9 <- t_sep24 %>%
  group_by(대여시간) %>% summarise(mean_use = round(mean(사용시간),2))

View(u_9)


u9 <- ggplot(u_9, aes(x=대여시간, y=mean_use)) +
  geom_line(color='#6495ED', size=3) +
  xlab('대여시간') +
  ylab('사용시간 평균') +
  ggtitle('대여시간 별 평균 사용시간 비교(9월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))

u9

## 10월
t_oct24$사용시간 <- as.numeric(t_oct24$사용시간)

t_oct24 <- na.omit(t_oct24)

u_10 <- t_oct24 %>%
  group_by(대여시간) %>% summarise(mean_use = round(mean(사용시간),2))

View(u_10)


u10 <- ggplot(u_10, aes(x=대여시간, y=mean_use)) +
  geom_line(color='#6495ED', size=3) +
  xlab('대여시간') +
  ylab('사용시간 평균') +
  ggtitle('대여시간 별 평균 사용시간 비교(10월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))

u10


## 11월
t_nov24$사용시간 <- as.numeric(t_nov24$사용시간)

t_nov24 <- na.omit(t_nov24)

u_11 <- t_nov24 %>%
  group_by(대여시간) %>% summarise(mean_use = round(mean(사용시간),2))

View(u_11)


u11 <- ggplot(u_11, aes(x=대여시간, y=mean_use)) +
  geom_line(color='#6495ED', size=3) +
  xlab('대여시간') +
  ylab('사용시간 평균') +
  ggtitle('대여시간 별 평균 사용시간 비교(11월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))

u11


## 12월
t_dec24$사용시간 <- as.numeric(t_dec24$사용시간)

t_dec24 <- na.omit(t_dec24)

u_12 <- t_dec24 %>%
  group_by(대여시간) %>% summarise(mean_use = round(mean(사용시간),2))

View(u_12)


u12 <- ggplot(u_12, aes(x=대여시간, y=mean_use)) +
  geom_line(color='#6495ED', size=3) +
  xlab('대여시간') +
  ylab('사용시간 평균') +
  ggtitle('대여시간 별 평균 사용시간 비교(12월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))

u12



#########################################################################
## 분기 별 평균 사용시간 비교

seasons <- c('1분기', '2분기', '3분기', '4분기')
q1 <- c(round(mean(u_1$mean_use),2), round(mean(u_2$mean_use),2), round(mean(u_3$mean_use),2))
q1 <- round(mean(q1),2)

q2 <- c(round(mean(u_4$mean_use),2), round(mean(u_5$mean_use),2), round(mean(u_6$mean_use),2))
q2 <- round(mean(q2),2)

q3 <- c(round(mean(u_7$mean_use),2), round(mean(u_8$mean_use),2), round(mean(u_9$mean_use),2))
q3 <- round(mean(q3),2)

q4 <- c(round(mean(u_10$mean_use),2), round(mean(u_11$mean_use),2), round(mean(u_12$mean_use),2))
q4 <- round(mean(q4),2)

qs <- c(q1,q2,q3,q4)

# 분기별(q1~q4) 값들의 평균을 낸 뒤 데이터프레임 생성
df_q <- data.frame(qs, seasons)


ggplot(data=df_q, aes(x=seasons, y=qs, fill=as.factor(seasons))) +
  geom_bar(stat='identity') +
  xlab('') +
  ylab('평균 사용시간') +
  labs(fill = '분기') +
  ggtitle('2021년 분기 별 평균사용시간 비교') +
  theme(plot.title=element_text(size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12)) +
  geom_label(aes(label=qs), nudge_y = 1.1)