library(dplyr)
library(ggplot2)
library(tidyverse)
library(scales)
library(RColorBrewer)



## 1월에 대여시간별(빌린 시간) 평균 이동거리 비교 ##
# 결측치 확인
sum(is.na(t_jan)) # 0

# 대여시간별로 이동거리 평균을 구해 mean_move열 생성
df <- t_jan %>%
  group_by(대여시간) %>% summarise(mean_move = round(mean(이동거리),2)) %>% 
  arrange(desc(mean_move))
View(df)


## x축 값에 따라 그래프가 표현 (case 1: 대여시간 그룹화)

label = c('1 (0~6시)', '2 (7~10시)', '3 (11~13시)', '4 (14~17시)', '5 (18~20시)', '6 (21~23시)')

# 대여시간, 이동거리 평균을 각 축으로 둠
p1 <- ggplot(df, aes(x=as.factor(대여시간), y=mean_move, fill = as.factor(대여시간))) +
  geom_bar(stat='identity') + 
  labs(fill = '대여시간') +
  xlab('대여시간') +
  ylab('이동거리 평균') +
  ggtitle('대여시간 별 평균 이동거리 비교(1월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12)) +
  geom_label(aes(label=mean_move), nudge_y = 1.1)


p1 + scale_fill_brewer(palette = 'Pastel1', labels =label)

# 2021년 1월에는 4, 즉, 14시 부터 17시까지 따릉이를 대여한 사람들의 이동거리 평균이 가장 높은 것을 알 수 있다.
# 대여시간이 0시부터 6시까지는 '새벽시간'으로 간주하여 1로 인코딩하였고
# 대여시간이 오전 7시부터 10시까지는 '출근시간'으로 간주하여 2로 인코딩하였으며
# 대여시간이 11시부터 13시까지는 3으로, 대여시간이 14시부터 17시까지는 4로 인코딩하였다.
# 대여시간이 18시부터 20시까지는 5, 대여시간이 21시부터 23시까지는 6으로 인코딩하였다.
# 이때, 대여시간이 2인 경우, 즉, 출근시간 때 따릉이를 대여한 사용자들의 평균 이동거리가 가장 적은것을 알 수 있다.


## case 2: 대여시간을 24시간으로 나타냈을 때

# 24시간대별로 이동거리 평균 나타내기
df_1 <- t_jan24 %>% 
  group_by(대여시간) %>% summarise(mean_move = mean(이동거리))


p1_24 <- ggplot(df_1, aes(x=대여시간, y=mean_move)) +
  geom_line(color='#3CB371', size=3) + 
  xlab('대여시간') +
  ylab('이동거리 평균') +
  ggtitle('대여시간 별 평균 이동거리 비교(1월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))

p1_24

# 2021년 1월에는 오전 5시, 7-8시 사이의 평균 이동거리가 가장 낮고 14-15시 사이의 평균 이동거리가 가장 높은 것을 알 수 있다.


## 2021년 2월 대여시간(빌린시간)별 평균 이동거리 비교
sum(is.na(t_feb)) # 374

t_feb$이동거리 <- as.numeric(t_feb$이동거리)
t_feb <- na.omit(t_feb)

# 결측치를 제외한 값들로 평균 이동거리 구함
df2 <- t_feb %>% 
  group_by(대여시간) %>% summarise(mean_move = round(mean(이동거리, na.rm=TRUE),2)) %>% 
  arrange(desc(mean_move))

# 대여시간이 NA인 행 제거
df2 <- df2[-c(7),]

str(t_feb)
str(df)

View(df2)

## x축 값에 따라 그래프가 표현 (case 1: 대여시간 그룹화)

label = c('1 (0~6시)', '2 (7~10시)', '3 (11~13시)', '4 (14~17시)', '5 (18~20시)', '6 (21~23시)')

p2 <- ggplot(df2, aes(x=as.factor(대여시간), y=mean_move, fill = as.factor(대여시간))) +
  geom_bar(stat='identity') + 
  labs(fill = '대여시간') +
  xlab('대여시간') +
  ylab('이동거리 평균') +
  ggtitle('대여시간 별 평균 이동거리 비교(2월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12)) +
  geom_label(aes(label=mean_move), nudge_y = 1.1)


p2 + scale_fill_brewer(palette = 'Pastel1', labels =label)


## case 2: 대여시간을 24시간으로 나타냈을 때
str(t_feb24)
t_feb24$이동거리 <- as.numeric(t_feb24$이동거리)
t_feb24$대여시간 <- as.numeric(t_feb24$대여시간)


t_feb24 <- na.omit(t_feb24)

df_2 <- t_feb24 %>% 
  group_by(대여시간) %>% summarise(mean_move = mean(이동거리)) %>% 
  arrange(desc(mean_move))


p2_24 <- ggplot(df_2, aes(x=대여시간, y=mean_move)) +
  geom_line(color='#3CB371', size=3) + 
  xlab('대여시간') +
  ylab('이동거리 평균') +
  ggtitle('대여시간 별 평균 이동거리 비교(2월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))

p2_24



## 2021년 3월 대여시간(빌린시간)별 평균 이동거리 비교

t_mar$이동거리 <- as.numeric(t_mar$이동거리)
sum(is.na(t_mar$이동거리))

df3 <- t_mar %>% 
  group_by(대여시간) %>% summarise(mean_move = round(mean(이동거리, na.rm = T),2)) %>% 
  arrange(desc(mean_move))

df3 <- df3[-c(7),]
View(df3)


## x축 값에 따라 그래프가 표현 (case 1: 대여시간 그룹화)

label = c('1 (0~6시)', '2 (7~10시)', '3 (11~13시)', '4 (14~17시)', '5 (18~20시)', '6 (21~23시)')

p3 <- ggplot(df3, aes(x=as.factor(대여시간), y=mean_move, fill = as.factor(대여시간))) +
  geom_bar(stat='identity') + 
  labs(fill = '대여시간') +
  xlab('대여시간') +
  ylab('이동거리 평균') +
  ggtitle('대여시간 별 평균 이동거리 비교(3월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12)) +
  geom_label(aes(label=mean_move), nudge_y = 1.1)


p3 + scale_fill_brewer(palette = 'Pastel1', labels =label)



## case 2: 대여시간을 24시간으로 나타냈을 때
str(t_feb24)
t_mar24$이동거리 <- as.numeric(t_mar24$이동거리)
t_mar24$대여시간 <- as.numeric(t_mar24$대여시간)

t_mar24 <- na.omit(t_mar24)

df_3 <- t_mar24 %>% 
  group_by(대여시간) %>% summarise(mean_move = mean(이동거리)) %>% 
  arrange(desc(mean_move))

View(df_3)

p3_24 <- ggplot(df_3, aes(x=대여시간, y=mean_move)) +
  geom_line(color='#3CB371', size=3) + 
  xlab('대여시간') +
  ylab('이동거리 평균') +
  ggtitle('대여시간 별 평균 이동거리 비교(3월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))

p3_24



## 2021년 4월 대여시간(빌린시간)별 평균 이동거리 비교

t_apr$이동거리 <- as.numeric(t_apr$이동거리)
sum(is.na(t_apr$이동거리)) # 0

df4 <- t_apr %>% 
  group_by(대여시간) %>% summarise(mean_move = round(mean(이동거리, na.rm = T),2)) %>% 
  arrange(desc(mean_move))

View(df4)


## x축 값에 따라 그래프가 표현 (case 1: 대여시간 그룹화)

label = c('1 (0~6시)', '2 (7~10시)', '3 (11~13시)', '4 (14~17시)', '5 (18~20시)', '6 (21~23시)')

p4 <- ggplot(df4, aes(x=as.factor(대여시간), y=mean_move, fill = as.factor(대여시간))) +
  geom_bar(stat='identity') + 
  labs(fill = '대여시간') +
  xlab('대여시간') +
  ylab('이동거리 평균') +
  ggtitle('대여시간 별 평균 이동거리 비교(4월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12)) +
  geom_label(aes(label=mean_move), nudge_y = 1.1)


p4 + scale_fill_brewer(palette = 'Pastel1', labels =label)



## case 2: 대여시간을 24시간으로 나타냈을 때
str(t_apr24)
t_apr24$이동거리 <- as.numeric(t_apr24$이동거리)
t_apr24$대여시간 <- as.numeric(t_apr24$대여시간)

t_apr24 <- na.omit(t_apr24)

df_4 <- t_apr24 %>% 
  group_by(대여시간) %>% summarise(mean_move = mean(이동거리)) %>% 
  arrange(desc(mean_move))

View(df_4)

p4_24 <- ggplot(df_4, aes(x=대여시간, y=mean_move)) +
  geom_line(color='#3CB371', size=3) + 
  xlab('대여시간') +
  ylab('이동거리 평균') +
  ggtitle('대여시간 별 평균 이동거리 비교(4월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))

p4_24



## 2021년 5월 대여시간(빌린시간)별 평균 이동거리 비교

t_may$이동거리 <- as.numeric(t_may$이동거리)
sum(is.na(t_may$이동거리)) # 0

df5 <- t_may %>% 
  group_by(대여시간) %>% summarise(mean_move = round(mean(이동거리, na.rm = T),2)) %>% 
  arrange(desc(mean_move))

View(df5)


## x축 값에 따라 그래프가 표현 (case 1: 대여시간 그룹화)

label = c('1 (0~6시)', '2 (7~10시)', '3 (11~13시)', '4 (14~17시)', '5 (18~20시)', '6 (21~23시)')

p5 <- ggplot(df5, aes(x=as.factor(대여시간), y=mean_move, fill = as.factor(대여시간))) +
  geom_bar(stat='identity') + 
  labs(fill = '대여시간') +
  xlab('대여시간') +
  ylab('이동거리 평균') +
  ggtitle('대여시간 별 평균 이동거리 비교(5월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12)) +
  geom_label(aes(label=mean_move), nudge_y = 1.1)


p5 + scale_fill_brewer(palette = 'Spectral', labels =label)



## case 2: 대여시간을 24시간으로 나타냈을 때
str(t_may24)
t_may24$이동거리 <- as.numeric(t_may24$이동거리)
t_may24$대여시간 <- as.numeric(t_may24$대여시간)

t_may24 <- na.omit(t_may24)

df_5 <- t_may24 %>% 
  group_by(대여시간) %>% summarise(mean_move = mean(이동거리)) %>% 
  arrange(desc(mean_move))

View(df_5)

p5_24 <- ggplot(df_5, aes(x=대여시간, y=mean_move)) +
  geom_line(color='#4AA8D8', size=3) + 
  xlab('대여시간') +
  ylab('이동거리 평균') +
  ggtitle('대여시간 별 평균 이동거리 비교(5월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))

p5_24



## 2021년 6월 대여시간(빌린시간)별 평균 이동거리 비교

t_june$이동거리 <- as.numeric(t_june$이동거리)
sum(is.na(t_june$이동거리)) # 0

t_june <- na.omit(t_june)

df6 <- t_june %>% 
  group_by(대여시간) %>% summarise(mean_move = round(mean(이동거리, na.rm = T),2)) %>% 
  arrange(desc(mean_move))

View(df6)


## x축 값에 따라 그래프가 표현 (case 1: 대여시간 그룹화)

label = c('1 (0~6시)', '2 (7~10시)', '3 (11~13시)', '4 (14~17시)', '5 (18~20시)', '6 (21~23시)')

p6 <- ggplot(df6, aes(x=as.factor(대여시간), y=mean_move, fill = as.factor(대여시간))) +
  geom_bar(stat='identity') + 
  labs(fill = '대여시간') +
  xlab('대여시간') +
  ylab('이동거리 평균') +
  ggtitle('대여시간 별 평균 이동거리 비교(6월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12)) +
  geom_label(aes(label=mean_move), nudge_y = 1.1)


p6 + scale_fill_brewer(palette = 'Spectral', labels =label)



## case 2: 대여시간을 24시간으로 나타냈을 때
str(t_june24)
t_june24$이동거리 <- as.numeric(t_june24$이동거리)
t_june24$대여시간 <- as.numeric(t_june24$대여시간)

t_june24 <- na.omit(t_june24)

df_6 <- t_june24 %>% 
  group_by(대여시간) %>% summarise(mean_move = mean(이동거리)) %>% 
  arrange(desc(mean_move))

View(df_6)

p6_24 <- ggplot(df_6, aes(x=대여시간, y=mean_move)) +
  geom_line(color='#4AA8D8', size=3) + 
  xlab('대여시간') +
  ylab('이동거리 평균') +
  ggtitle('대여시간 별 평균 이동거리 비교(6월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))

p6_24


## 2021년 7월 대여시간(빌린시간)별 평균 이동거리 비교

t_july$이동거리 <- as.numeric(t_july$이동거리)
sum(is.na(t_july$이동거리)) # 0

t_july <- na.omit(t_july)

df7 <- t_july %>% 
  group_by(대여시간) %>% summarise(mean_move = round(mean(이동거리, na.rm = T),2)) %>% 
  arrange(desc(mean_move))

View(df7)


## x축 값에 따라 그래프가 표현 (case 1: 대여시간 그룹화)

label = c('1 (0~6시)', '2 (7~10시)', '3 (11~13시)', '4 (14~17시)', '5 (18~20시)', '6 (21~23시)')

p7 <- ggplot(df7, aes(x=as.factor(대여시간), y=mean_move, fill = as.factor(대여시간))) +
  geom_bar(stat='identity') + 
  labs(fill = '대여시간') +
  xlab('대여시간') +
  ylab('이동거리 평균') +
  ggtitle('대여시간 별 평균 이동거리 비교(7월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12)) +
  geom_label(aes(label=mean_move), nudge_y = 1.1)


p7 + scale_fill_brewer(palette = 'Spectral', labels =label)



## case 2: 대여시간을 24시간으로 나타냈을 때
str(t_july24)
t_july24$이동거리 <- as.numeric(t_july24$이동거리)
t_july24$대여시간 <- as.numeric(t_july24$대여시간)

t_july24 <- na.omit(t_july24)

df_7 <- t_july24 %>% 
  group_by(대여시간) %>% summarise(mean_move = mean(이동거리)) %>% 
  arrange(desc(mean_move))

View(df_7)

p7_24 <- ggplot(df_7, aes(x=대여시간, y=mean_move)) +
  geom_line(color='#4AA8D8', size=3) + 
  xlab('대여시간') +
  ylab('이동거리 평균') +
  ggtitle('대여시간 별 평균 이동거리 비교(7월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))

p7_24



## 2021년 8월 대여시간(빌린시간)별 평균 이동거리 비교

t_aug$이동거리 <- as.numeric(t_aug$이동거리)
sum(is.na(t_aug$이동거리)) # 0

t_aug <- na.omit(t_aug)

df8 <- t_aug %>% 
  group_by(대여시간) %>% summarise(mean_move = round(mean(이동거리, na.rm = T),2)) %>% 
  arrange(desc(mean_move))

View(df8)


## x축 값에 따라 그래프가 표현 (case 1: 대여시간 그룹화)

label = c('1 (0~6시)', '2 (7~10시)', '3 (11~13시)', '4 (14~17시)', '5 (18~20시)', '6 (21~23시)')

p8 <- ggplot(df8, aes(x=as.factor(대여시간), y=mean_move, fill = as.factor(대여시간))) +
  geom_bar(stat='identity') + 
  labs(fill = '대여시간') +
  xlab('대여시간') +
  ylab('이동거리 평균') +
  ggtitle('대여시간 별 평균 이동거리 비교(8월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12)) +
  geom_label(aes(label=mean_move), nudge_y = 1.1)


p8 + scale_fill_brewer(palette = 'Spectral', labels =label)



## case 2: 대여시간을 24시간으로 나타냈을 때
str(t_aug24)
t_aug24$이동거리 <- as.numeric(t_aug24$이동거리)
t_aug24$대여시간 <- as.numeric(t_aug24$대여시간)

t_aug24 <- na.omit(t_aug24)

df_8 <- t_aug24 %>% 
  group_by(대여시간) %>% summarise(mean_move = mean(이동거리)) %>% 
  arrange(desc(mean_move))

View(df_8)

p8_24 <- ggplot(df_8, aes(x=대여시간, y=mean_move)) +
  geom_line(color='#4AA8D8', size=3) + 
  xlab('대여시간') +
  ylab('이동거리 평균') +
  ggtitle('대여시간 별 평균 이동거리 비교(8월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))

p8_24


## 2021년 9월 대여시간(빌린시간)별 평균 이동거리 비교

t_sep$이동거리 <- as.numeric(t_sep$이동거리)
sum(is.na(t_sep$이동거리)) # 0

t_sep <- na.omit(t_sep)

df9 <- t_sep %>% 
  group_by(대여시간) %>% summarise(mean_move = round(mean(이동거리, na.rm = T),2)) %>% 
  arrange(desc(mean_move))

View(df9)


## x축 값에 따라 그래프가 표현 (case 1: 대여시간 그룹화)

label = c('1 (0~6시)', '2 (7~10시)', '3 (11~13시)', '4 (14~17시)', '5 (18~20시)', '6 (21~23시)')

p9 <- ggplot(df9, aes(x=as.factor(대여시간), y=mean_move, fill = as.factor(대여시간))) +
  geom_bar(stat='identity') + 
  labs(fill = '대여시간') +
  xlab('대여시간') +
  ylab('이동거리 평균') +
  ggtitle('대여시간 별 평균 이동거리 비교(9월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12)) +
  geom_label(aes(label=mean_move), nudge_y = 1.1)


p9 + scale_fill_brewer(palette = 'Greens', labels =label)



## case 2: 대여시간을 24시간으로 나타냈을 때
str(t_sep24)
t_sep24$이동거리 <- as.numeric(t_sep24$이동거리)
t_sep24$대여시간 <- as.numeric(t_sep24$대여시간)

t_sep24 <- na.omit(t_sep24)

df_9 <- t_sep24 %>% 
  group_by(대여시간) %>% summarise(mean_move = mean(이동거리)) %>% 
  arrange(desc(mean_move))

View(df_9)

p9_24 <- ggplot(df_9, aes(x=대여시간, y=mean_move)) +
  geom_line(color='#FFA048', size=3) + 
  xlab('대여시간') +
  ylab('이동거리 평균') +
  ggtitle('대여시간 별 평균 이동거리 비교(9월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))

p9_24



## 2021년 10월 대여시간(빌린시간)별 평균 이동거리 비교

t_oct$이동거리 <- as.numeric(t_oct$이동거리)
sum(is.na(t_oct$이동거리)) # 0

t_oct <- na.omit(t_oct)

df10 <- t_oct %>% 
  group_by(대여시간) %>% summarise(mean_move = round(mean(이동거리, na.rm = T),2)) %>% 
  arrange(desc(mean_move))

View(df10)


## x축 값에 따라 그래프가 표현 (case 1: 대여시간 그룹화)

label = c('1 (0~6시)', '2 (7~10시)', '3 (11~13시)', '4 (14~17시)', '5 (18~20시)', '6 (21~23시)')

p10 <- ggplot(df10, aes(x=as.factor(대여시간), y=mean_move, fill = as.factor(대여시간))) +
  geom_bar(stat='identity') + 
  labs(fill = '대여시간') +
  xlab('대여시간') +
  ylab('이동거리 평균') +
  ggtitle('대여시간 별 평균 이동거리 비교(10월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12)) +
  geom_label(aes(label=mean_move), nudge_y = 1.1)


p10 + scale_fill_brewer(palette = 'Greens', labels =label)



## case 2: 대여시간을 24시간으로 나타냈을 때
str(t_oct24)
t_oct24$이동거리 <- as.numeric(t_oct24$이동거리)
t_oct24$대여시간 <- as.numeric(t_oct24$대여시간)

t_oct24 <- na.omit(t_oct24)

df_10 <- t_oct24 %>% 
  group_by(대여시간) %>% summarise(mean_move = mean(이동거리)) %>% 
  arrange(desc(mean_move))

View(df_10)

p10_24 <- ggplot(df_10, aes(x=대여시간, y=mean_move)) +
  geom_line(color='#FFA048', size=3) + 
  xlab('대여시간') +
  ylab('이동거리 평균') +
  ggtitle('대여시간 별 평균 이동거리 비교(10월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))

p10_24



## 2021년 11월 대여시간(빌린시간)별 평균 이동거리 비교

t_nov$이동거리 <- as.numeric(t_nov$이동거리)
sum(is.na(t_nov$이동거리)) # 0

t_nov <- na.omit(t_nov)

df11 <- t_nov %>% 
  group_by(대여시간) %>% summarise(mean_move = round(mean(이동거리, na.rm = T),2)) %>% 
  arrange(desc(mean_move))

View(df11)


## x축 값에 따라 그래프가 표현 (case 1: 대여시간 그룹화)

label = c('1 (0~6시)', '2 (7~10시)', '3 (11~13시)', '4 (14~17시)', '5 (18~20시)', '6 (21~23시)')

p11 <- ggplot(df11, aes(x=as.factor(대여시간), y=mean_move, fill = as.factor(대여시간))) +
  geom_bar(stat='identity') + 
  labs(fill = '대여시간') +
  xlab('대여시간') +
  ylab('이동거리 평균') +
  ggtitle('대여시간 별 평균 이동거리 비교(11월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12)) +
  geom_label(aes(label=mean_move), nudge_y = 1.1)


p11 + scale_fill_brewer(palette = 'Greens', labels =label)



## case 2: 대여시간을 24시간으로 나타냈을 때
str(t_nov24)
t_nov24$이동거리 <- as.numeric(t_nov24$이동거리)
t_nov24$대여시간 <- as.numeric(t_nov24$대여시간)

t_nov24 <- na.omit(t_nov24)

df_11 <- t_nov24 %>% 
  group_by(대여시간) %>% summarise(mean_move = mean(이동거리)) %>% 
  arrange(desc(mean_move))

View(df_11)

p11_24 <- ggplot(df_11, aes(x=대여시간, y=mean_move)) +
  geom_line(color='#FFA048', size=3) + 
  xlab('대여시간') +
  ylab('이동거리 평균') +
  ggtitle('대여시간 별 평균 이동거리 비교(11월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))

p11_24


## 2021년 12월 대여시간(빌린시간)별 평균 이동거리 비교

t_dec$이동거리 <- as.numeric(t_dec$이동거리)
sum(is.na(t_dec$이동거리)) # 0

t_dec <- na.omit(t_dec)

df12 <- t_dec %>% 
  group_by(대여시간) %>% summarise(mean_move = round(mean(이동거리, na.rm = T),2)) %>% 
  arrange(desc(mean_move))

View(df12)


## x축 값에 따라 그래프가 표현 (case 1: 대여시간 그룹화)

label = c('1 (0~6시)', '2 (7~10시)', '3 (11~13시)', '4 (14~17시)', '5 (18~20시)', '6 (21~23시)')

p12 <- ggplot(df12, aes(x=as.factor(대여시간), y=mean_move, fill = as.factor(대여시간))) +
  geom_bar(stat='identity') + 
  labs(fill = '대여시간') +
  xlab('대여시간') +
  ylab('이동거리 평균') +
  ggtitle('대여시간 별 평균 이동거리 비교(12월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12)) +
  geom_label(aes(label=mean_move), nudge_y = 1.1)


p12 + scale_fill_brewer(palette = 'Greens', labels =label)



## case 2: 대여시간을 24시간으로 나타냈을 때
str(t_dec24)
t_dec24$이동거리 <- as.numeric(t_dec24$이동거리)
t_dec24$대여시간 <- as.numeric(t_dec24$대여시간)

t_dec24 <- na.omit(t_dec24)

df_12 <- t_dec24 %>% 
  group_by(대여시간) %>% summarise(mean_move = mean(이동거리)) %>% 
  arrange(desc(mean_move))

View(df_12)

p12_24 <- ggplot(df_12, aes(x=대여시간, y=mean_move)) +
  geom_line(color='#FFA048', size=3) + 
  xlab('대여시간') +
  ylab('이동거리 평균') +
  ggtitle('대여시간 별 평균 이동거리 비교(12월)') +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12))

p12_24


######################################################################
## 분기 별 평균이동거리 비교

seasons <- c('1분기', '2분기', '3분기', '4분기')
q1 <- c(round(mean(df$mean_move),2), round(mean(df2$mean_move),2), round(mean(df3$mean_move),2))
q1 <- round(mean(q1),2)

q2 <- c(round(mean(df4$mean_move),2), round(mean(df5$mean_move),2), round(mean(df6$mean_move),2))
q2 <- round(mean(q2),2)

q3 <- c(round(mean(df7$mean_move),2), round(mean(df8$mean_move),2), round(mean(df9$mean_move),2))
q3 <- round(mean(q3),2)

q4 <- c(round(mean(df10$mean_move),2), round(mean(df11$mean_move),2), round(mean(df12$mean_move),2))
q4 <- round(mean(q4),2)

qs <- c(q1,q2,q3,q4)

# 분기별(q1~q4) 값들의 평균을 낸 뒤 데이터프레임 생성
df_q <- data.frame(qs, seasons)


ggplot(data=df_q, aes(x=seasons, y=qs, fill=as.factor(seasons))) +
  geom_bar(stat='identity') +
  xlab('') +
  ylab('평균 이동거리') +
  labs(fill = '분기') +
  ggtitle('2021년 분기 별 평균이동거리 비교') +
  theme(plot.title=element_text(size=18,
                                hjust=0.5,
                                vjust=2,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12)) +
  geom_label(aes(label=qs), nudge_y = 1.1)
