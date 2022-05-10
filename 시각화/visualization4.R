library(dplyr)
library(ggplot2)
library(tidyverse)
library(scales)
library(RColorBrewer)

#######################################################################
## 대여구분코드 비율 확인

## 1월
# 대여시간, 대여구분코드만 추출
c1 <- subset(t_jan, select = c(대여시간, 대여구분코드))
View(c1)
str(df1)

# 대여구분코드 비율 확인
# 1: 정기권, 2: 일일권, 3: 단체권
label <- c('정기권', '일일권', '단체권')

# 대여구분코드 별 행 수 구하고 비율 구하기
df1 <- c1 %>% 
  group_by(대여구분코드) %>% summarise(n=n()) %>%
  mutate(pct=round(n/sum(n)*100,1))

View(df1)

# factor형으로 변환
df1$대여구분코드 <- as.factor(df1$대여구분코드)


# ggplot을 이용하여 파이그래프를 그리기 위함
bp1 <- ggplot(df1, aes(x="", y=pct, fill=대여구분코드)) +
  geom_bar(width=0.5, stat='identity')
bp1

# 파이그래프
pie1 <- bp1 + coord_polar('y', start=0) +
  labs(title='1월 대여구분코드 비율', x='', y='') +
  theme_void() +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=1.5,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12)) +
  geom_text(aes(label=paste0(pct,'%')), position = position_stack(vjust=0.5))


pie1 + scale_fill_manual(values = c('#FFA07A','#FFB6C1','#98Fb98'), labels=label)


## 2월
# 대여시간, 대여구분코드만 추출
c2 <- subset(t_feb, select = c(대여시간, 대여구분코드))
c2 <- na.omit(c2)

# 대여구분코드 별 행 수 구하고 비율 구하기
df2 <- c2 %>% 
  group_by(대여구분코드) %>% summarise(n=n()) %>%
  mutate(pct=round(n/sum(n)*100,1))

View(df2)

# factor형으로 변환
df2$대여구분코드 <- as.factor(df2$대여구분코드)


# ggplot을 이용하여 파이그래프를 그리기 위함
bp2 <- ggplot(df2, aes(x="", y=pct, fill=대여구분코드)) +
  geom_bar(width=0.5, stat='identity')
bp2

# 파이그래프
pie2 <- bp2 + coord_polar('y', start=0) +
  labs(title='2월 대여구분코드 비율', x='', y='') +
  theme_void() +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=1.5,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12)) +
  geom_text(aes(label=paste0(pct,'%')), position = position_stack(vjust=0.5))

pie2 + scale_fill_manual(values = c('#FFA07A','#FFB6C1','#98Fb98'), labels=label)


## 3월
# 대여시간, 대여구분코드만 추출
c3 <- subset(t_mar, select = c(대여시간, 대여구분코드))
c3 <- na.omit(c3)

# 대여구분코드 별 행 수 구하고 비율 구하기
df3 <- c3 %>% 
  group_by(대여구분코드) %>% summarise(n=n()) %>%
  mutate(pct=round(n/sum(n)*100,1))

View(df3)

# factor형으로 변환
df3$대여구분코드 <- as.factor(df3$대여구분코드)

# ggplot을 이용하여 파이그래프를 그리기 위함
bp3 <- ggplot(df3, aes(x="", y=pct, fill=대여구분코드)) +
  geom_bar(width=0.5, stat='identity')
bp3

# 파이그래프
pie3 <- bp3 + coord_polar('y', start=0) +
  labs(title='3월 대여구분코드 비율', x='', y='') +
  theme_void() +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=1.5,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12)) +
  geom_text(aes(label=paste0(pct,'%')), position = position_stack(vjust=0.5))

pie3 + scale_fill_manual(values = c('#FFA07A','#FFB6C1','#98Fb98'), labels=label)


## 4월
# 대여시간, 대여구분코드만 추출
c4 <- subset(t_apr, select = c(대여시간, 대여구분코드))
c4 <- na.omit(c4)

# 대여구분코드 별 행 수 구하고 비율 구하기
df4 <- c4 %>% 
  group_by(대여구분코드) %>% summarise(n=n()) %>%
  mutate(pct=round(n/sum(n)*100,1))

View(df4)

# factor형으로 변환
df4$대여구분코드 <- as.factor(df4$대여구분코드)

# ggplot을 이용하여 파이그래프를 그리기 위함
bp4 <- ggplot(df4, aes(x="", y=pct, fill=대여구분코드)) +
  geom_bar(width=0.5, stat='identity')
bp4

# 파이그래프
pie4 <- bp4 + coord_polar('y', start=0) +
  labs(title='4월 대여구분코드 비율', x='', y='') +
  theme_void() +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=1.5,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12)) +
  geom_text(aes(label=paste0(pct,'%')), position = position_stack(vjust=0.5))

pie4 + scale_fill_manual(values = c('#FFA07A','#FFB6C1','#98Fb98'), labels=label)


## 5월
# 대여시간, 대여구분코드만 추출
c5 <- subset(t_may, select = c(대여시간, 대여구분코드))
c5 <- na.omit(c5)

# 대여구분코드 별 행 수 구하고 비율 구하기
df5 <- c5 %>% 
  group_by(대여구분코드) %>% summarise(n=n()) %>%
  mutate(pct=round(n/sum(n)*100,1))

View(df5)

# factor형으로 변환
df5$대여구분코드 <- as.factor(df5$대여구분코드)

# ggplot을 이용하여 파이그래프를 그리기 위함
bp5 <- ggplot(df5, aes(x="", y=pct, fill=대여구분코드)) +
  geom_bar(width=0.5, stat='identity')
bp5

# 파이그래프
pie5 <- bp5 + coord_polar('y', start=0) +
  labs(title='5월 대여구분코드 비율', x='', y='') +
  theme_void() +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=1.5,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12)) +
  geom_text(aes(label=paste0(pct,'%')), position = position_stack(vjust=0.5))

pie5 + scale_fill_manual(values = c('#FFDAB9','#FA8072','#B0C4DE'), labels=label)


## 6월
# 대여시간, 대여구분코드만 추출
c6 <- subset(t_june, select = c(대여시간, 대여구분코드))
c6 <- na.omit(c6)

# 대여구분코드 별 행 수 구하고 비율 구하기
df6 <- c6 %>% 
  group_by(대여구분코드) %>% summarise(n=n()) %>%
  mutate(pct=round(n/sum(n)*100,1))

View(df6)

# factor형으로 변환
df6$대여구분코드 <- as.factor(df6$대여구분코드)

# ggplot을 이용하여 파이그래프를 그리기 위함
bp6 <- ggplot(df6, aes(x="", y=pct, fill=대여구분코드)) +
  geom_bar(width=0.5, stat='identity')
bp6

# 파이그래프
pie6 <- bp6 + coord_polar('y', start=0) +
  labs(title='6월 대여구분코드 비율', x='', y='') +
  theme_void() +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=1.5,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12)) +
  geom_text(aes(label=paste0(pct,'%')), position = position_stack(vjust=0.5))

pie6 + scale_fill_manual(values = c('#FFDAB9','#FA8072','#B0C4DE'), labels=label)


## 7월
# 대여시간, 대여구분코드만 추출
c7 <- subset(t_july, select = c(대여시간, 대여구분코드))
c7 <- na.omit(c7)

# 대여구분코드 별 행 수 구하고 비율 구하기
df7 <- c7 %>% 
  group_by(대여구분코드) %>% summarise(n=n()) %>%
  mutate(pct=round(n/sum(n)*100,1))

View(df7)

# factor형으로 변환
df7$대여구분코드 <- as.factor(df7$대여구분코드)

# ggplot을 이용하여 파이그래프를 그리기 위함
bp7 <- ggplot(df7, aes(x="", y=pct, fill=대여구분코드)) +
  geom_bar(width=0.5, stat='identity')
bp7

# 파이그래프
pie7 <- bp7 + coord_polar('y', start=0) +
  labs(title='7월 대여구분코드 비율', x='', y='') +
  theme_void() +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=1.5,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12)) +
  geom_text(aes(label=paste0(pct,'%')), position = position_stack(vjust=0.5))

pie7 + scale_fill_manual(values = c('#FFDAB9','#FA8072','#B0C4DE'), labels=label)


## 8월
# 대여시간, 대여구분코드만 추출
c8 <- subset(t_aug, select = c(대여시간, 대여구분코드))
c8 <- na.omit(c8)

# 대여구분코드 별 행 수 구하고 비율 구하기
df8 <- c8 %>% 
  group_by(대여구분코드) %>% summarise(n=n()) %>%
  mutate(pct=round(n/sum(n)*100,1))

View(df8)

# factor형으로 변환
df8$대여구분코드 <- as.factor(df8$대여구분코드)

# ggplot을 이용하여 파이그래프를 그리기 위함
bp8 <- ggplot(df8, aes(x="", y=pct, fill=대여구분코드)) +
  geom_bar(width=0.5, stat='identity')
bp8

# 파이그래프
pie8 <- bp8 + coord_polar('y', start=0) +
  labs(title='8월 대여구분코드 비율', x='', y='') +
  theme_void() +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=1.5,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12)) +
  geom_text(aes(label=paste0(pct,'%')), position = position_stack(vjust=0.5))

pie8 + scale_fill_manual(values = c('#FFDAB9','#FA8072','#B0C4DE'), labels=label)


## 9월
# 대여시간, 대여구분코드만 추출
c9 <- subset(t_sep, select = c(대여시간, 대여구분코드))
c9 <- na.omit(c9)

# 대여구분코드 별 행 수 구하고 비율 구하기
df9 <- c9 %>% 
  group_by(대여구분코드) %>% summarise(n=n()) %>%
  mutate(pct=round(n/sum(n)*100,1))

View(df9)

# factor형으로 변환
df9$대여구분코드 <- as.factor(df9$대여구분코드)

# ggplot을 이용하여 파이그래프를 그리기 위함
bp9 <- ggplot(df9, aes(x="", y=pct, fill=대여구분코드)) +
  geom_bar(width=0.5, stat='identity')
bp9

# 파이그래프
pie9 <- bp9 + coord_polar('y', start=0) +
  labs(title='9월 대여구분코드 비율', x='', y='') +
  theme_void() +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=1.5,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12)) +
  geom_text(aes(label=paste0(pct,'%')), position = position_stack(vjust=0.5))

pie9 + scale_fill_manual(values = c('#98FB98','#00FA9A','#66CDAA'), labels=label)


## 10월
# 대여시간, 대여구분코드만 추출
c10 <- subset(t_oct, select = c(대여시간, 대여구분코드))
c10 <- na.omit(c10)

# 대여구분코드 별 행 수 구하고 비율 구하기
df10 <- c10 %>% 
  group_by(대여구분코드) %>% summarise(n=n()) %>%
  mutate(pct=round(n/sum(n)*100,1))

View(df10)

# factor형으로 변환
df10$대여구분코드 <- as.factor(df10$대여구분코드)

# ggplot을 이용하여 파이그래프를 그리기 위함
bp10 <- ggplot(df10, aes(x="", y=pct, fill=대여구분코드)) +
  geom_bar(width=0.5, stat='identity')
bp10

# 파이그래프
pie10 <- bp10 + coord_polar('y', start=0) +
  labs(title='10월 대여구분코드 비율', x='', y='') +
  theme_void() +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=1.5,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12)) +
  geom_text(aes(label=paste0(pct,'%')), position = position_stack(vjust=0.5))

pie10 + scale_fill_manual(values = c('#98FB98','#00FA9A','#66CDAA'), labels=label)


## 11월
# 대여시간, 대여구분코드만 추출
c11 <- subset(t_nov, select = c(대여시간, 대여구분코드))
c11 <- na.omit(c11)

# 대여구분코드 별 행 수 구하고 비율 구하기
df11 <- c11 %>% 
  group_by(대여구분코드) %>% summarise(n=n()) %>%
  mutate(pct=round(n/sum(n)*100,1))

View(df11)

# factor형으로 변환
df11$대여구분코드 <- as.factor(df11$대여구분코드)

# ggplot을 이용하여 파이그래프를 그리기 위함
bp11 <- ggplot(df11, aes(x="", y=pct, fill=대여구분코드)) +
  geom_bar(width=0.5, stat='identity')
bp11

# 파이그래프
pie11 <- bp11 + coord_polar('y', start=0) +
  labs(title='11월 대여구분코드 비율', x='', y='') +
  theme_void() +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=1.5,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12)) +
  geom_text(aes(label=paste0(pct,'%')), position = position_stack(vjust=0.5))

pie11 + scale_fill_manual(values = c('#98FB98','#00FA9A','#66CDAA'), labels=label)



## 12월
# 대여시간, 대여구분코드만 추출
c12 <- subset(t_dec, select = c(대여시간, 대여구분코드))
c12 <- na.omit(c12)

# 대여구분코드 별 행 수 구하고 비율 구하기
df12 <- c12 %>% 
  group_by(대여구분코드) %>% summarise(n=n()) %>%
  mutate(pct=round(n/sum(n)*100,1))

View(df12)

# factor형으로 변환
df12$대여구분코드 <- as.factor(df12$대여구분코드)

# ggplot을 이용하여 파이그래프를 그리기 위함
bp12 <- ggplot(df12, aes(x="", y=pct, fill=대여구분코드)) +
  geom_bar(width=0.5, stat='identity')
bp12

# 파이그래프
pie12 <- bp12 + coord_polar('y', start=0) +
  labs(title='12월 대여구분코드 비율', x='', y='') +
  theme_void() +
  theme(plot.title=element_text(family='serif',
                                size=18,
                                hjust=0.5,
                                vjust=1.5,
                                face='bold',
                                color="darkblue")) +
  theme(axis.title = element_text(face='bold', size=12)) +
  geom_text(aes(label=paste0(pct,'%')), position = position_stack(vjust=0.5))

pie12 + scale_fill_manual(values = c('#98FB98','#00FA9A','#66CDAA'), labels=label)