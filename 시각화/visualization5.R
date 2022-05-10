library(dplyr)
library(ggplot2)
library(tidyverse)
library(scales)
library(RColorBrewer)

#########################################################
## 따릉이 이용 연령대 비율 확인

## 1월
# 대여시간, 연령대코드만 추출
a1 <- subset(t_jan, select = c(대여시간, 연령대코드))
View(a1)

# 연령대비율 비율 확인
# ~10대: 1, 20대: 2, 30대: 3, 40대: 4, 50대:5, 60대이상: 6
label <- c('~10대', '20대', '30대', '40대', '50대', '60대 이상')

# 연령대코드 별 행 수 구하고 비율 구하기
df1 <- a1 %>% 
  group_by(연령대코드) %>% summarise(n=n()) %>%
  mutate(pct=round(n/sum(n)*100,1))

View(df1)

# factor형으로 변환
df1$연령대코드 <- as.factor(df1$연령대코드)


lbls1 <- paste(label, df1$pct)
lbls1 <- paste(lbls1, '%', sep='')

pie(df1$pct, init.angle = 90, col = c('#AFEEEE','#87CEFA','#00CED1','#9370DB', '#FFB6C1', '#FFE4E1'), label=lbls1,
    main='1월 이용자 연령대 비율')



## 2월
# 대여시간, 연령대코드만 추출
a2 <- subset(t_feb, select = c(대여시간, 연령대코드))
View(a2)

# 연령대비율 비율 확인
# ~10대: 1, 20대: 2, 30대: 3, 40대: 4, 50대:5, 60대이상: 6
label <- c('~10대', '20대', '30대', '40대', '50대', '60대 이상')

# 연령대코드 별 행 수 구하고 비율 구하기
df2 <- a2 %>% 
  group_by(연령대코드) %>% summarise(n=n()) %>%
  mutate(pct=round(n/sum(n)*100,1))

View(df2)

lbls2 <- paste(label, df2$pct)
lbls2 <- paste(lbls2, '%', sep='')

pie(df2$pct, init.angle = 90, col = c('#AFEEEE','#87CEFA','#00CED1','#9370DB', '#FFB6C1', '#FFE4E1'), label=lbls2,
    main='2월 이용자 연령대 비율')



## 3월
# 대여시간, 연령대코드만 추출
a3 <- subset(t_mar, select = c(대여시간, 연령대코드))
View(a3)

# 연령대코드 별 행 수 구하고 비율 구하기
df3 <- a3 %>% 
  group_by(연령대코드) %>% summarise(n=n()) %>%
  mutate(pct=round(n/sum(n)*100,1))

View(df3)

lbls3 <- paste(label, df3$pct)
lbls3 <- paste(lbls3, '%', sep='')

pie(df3$pct, init.angle = 90, col = c('#AFEEEE','#87CEFA','#00CED1','#9370DB', '#FFB6C1', '#FFE4E1'), label=lbls3,
    main='3월 이용자 연령대 비율')



## 4월
# 대여시간, 연령대코드만 추출
a4 <- subset(t_apr, select = c(대여시간, 연령대코드))
View(a4)

# 연령대코드 별 행 수 구하고 비율 구하기
df4 <- a4 %>% 
  group_by(연령대코드) %>% summarise(n=n()) %>%
  mutate(pct=round(n/sum(n)*100,1))

View(df4)

lbls4 <- paste(label, df4$pct)
lbls4 <- paste(lbls4, '%', sep='')

pie(df4$pct, init.angle = 90, col = c('#AFEEEE','#87CEFA','#00CED1','#9370DB', '#FFB6C1', '#FFE4E1'), label=lbls4,
    main='4월 이용자 연령대 비율')


## 5월
# 대여시간, 연령대코드만 추출
a5 <- subset(t_may, select = c(대여시간, 연령대코드))
View(a5)

# 연령대코드 별 행 수 구하고 비율 구하기
df5 <- a5 %>% 
  group_by(연령대코드) %>% summarise(n=n()) %>%
  mutate(pct=round(n/sum(n)*100,1))

View(df5)

lbls5 <- paste(label, df5$pct)
lbls5 <- paste(lbls5, '%', sep='')

pie(df5$pct, init.angle = 90, col = c('#E0FFFF','#F0F8FF','#FFF0F5','#FFFAF0', '#F5FFFA', '#F8F8FF'), label=lbls5,
    main='5월 이용자 연령대 비율')


## 6월
# 대여시간, 연령대코드만 추출
a6 <- subset(t_june, select = c(대여시간, 연령대코드))
View(a6)

# 연령대코드 별 행 수 구하고 비율 구하기
df6 <- a6 %>% 
  group_by(연령대코드) %>% summarise(n=n()) %>%
  mutate(pct=round(n/sum(n)*100,1))

View(df6)

lbls6 <- paste(label, df6$pct)
lbls6 <- paste(lbls6, '%', sep='')

pie(df6$pct, init.angle = 90, col = c('#E0FFFF','#F0F8FF','#FFF0F5','#FFFAF0', '#F5FFFA', '#F8F8FF'), label=lbls6,
    main='6월 이용자 연령대 비율')



## 7월
# 대여시간, 연령대코드만 추출
a7 <- subset(t_july, select = c(대여시간, 연령대코드))
View(a7)

# 연령대코드 별 행 수 구하고 비율 구하기
df7 <- a7 %>% 
  group_by(연령대코드) %>% summarise(n=n()) %>%
  mutate(pct=round(n/sum(n)*100,1))

View(df7)

lbls7 <- paste(label, df7$pct)
lbls7 <- paste(lbls7, '%', sep='')

pie(df7$pct, init.angle = 90, col = c('#E0FFFF','#F0F8FF','#FFF0F5','#FFFAF0', '#F5FFFA', '#F8F8FF'), label=lbls7,
    main='7월 이용자 연령대 비율')


## 8월
# 대여시간, 연령대코드만 추출
a8 <- subset(t_aug, select = c(대여시간, 연령대코드))
View(a8)

# 연령대코드 별 행 수 구하고 비율 구하기
df8 <- a8 %>% 
  group_by(연령대코드) %>% summarise(n=n()) %>%
  mutate(pct=round(n/sum(n)*100,1))

View(df8)

lbls8 <- paste(label, df8$pct)
lbls8 <- paste(lbls8, '%', sep='')

pie(df8$pct, init.angle = 90, col = c('#E0FFFF','#F0F8FF','#FFF0F5','#FFFAF0', '#F5FFFA', '#F8F8FF'), label=lbls8,
    main='8월 이용자 연령대 비율')


## 9월
# 대여시간, 연령대코드만 추출
a9 <- subset(t_sep, select = c(대여시간, 연령대코드))
View(a9)

# 연령대코드 별 행 수 구하고 비율 구하기
df9 <- a9 %>% 
  group_by(연령대코드) %>% summarise(n=n()) %>%
  mutate(pct=round(n/sum(n)*100,1))

View(df9)

lbls9 <- paste(label, df9$pct)
lbls9 <- paste(lbls9, '%', sep='')

pie(df9$pct, init.angle = 90, col = c('#FFF0F5','#FFFACD','#E6E6FA','#ADD8E6', '#FFb6c1', '#DDA0DD'), label=lbls9,
    main='9월 이용자 연령대 비율')


## 10월
# 대여시간, 연령대코드만 추출
a10 <- subset(t_oct, select = c(대여시간, 연령대코드))
View(a10)

# 연령대코드 별 행 수 구하고 비율 구하기
df10 <- a10 %>% 
  group_by(연령대코드) %>% summarise(n=n()) %>%
  mutate(pct=round(n/sum(n)*100,1))

View(df10)

lbls10 <- paste(label, df10$pct)
lbls10 <- paste(lbls10, '%', sep='')

pie(df10$pct, init.angle = 90, col = c('#FFF0F5','#FFFACD','#E6E6FA','#ADD8E6', '#FFb6c1', '#DDA0DD'), label=lbls10,
    main='10월 이용자 연령대 비율')


## 11월
# 대여시간, 연령대코드만 추출
a11 <- subset(t_nov, select = c(대여시간, 연령대코드))
View(a11)

# 연령대코드 별 행 수 구하고 비율 구하기
df11 <- a11 %>% 
  group_by(연령대코드) %>% summarise(n=n()) %>%
  mutate(pct=round(n/sum(n)*100,1))

View(df11)

lbls11 <- paste(label, df11$pct)
lbls11 <- paste(lbls11, '%', sep='')

pie(df11$pct, init.angle = 90, col = c('#FFF0F5','#FFFACD','#E6E6FA','#ADD8E6', '#FFb6c1', '#DDA0DD'), label=lbls11,
    main='11월 이용자 연령대 비율')


## 12월
# 대여시간, 연령대코드만 추출
a12 <- subset(t_dec, select = c(대여시간, 연령대코드))
View(a12)

# 연령대코드 별 행 수 구하고 비율 구하기
df12 <- a12 %>% 
  group_by(연령대코드) %>% summarise(n=n()) %>%
  mutate(pct=round(n/sum(n)*100,1))

View(df12)

lbls12 <- paste(label, df12$pct)
lbls12 <- paste(lbls12, '%', sep='')

pie(df12$pct, init.angle = 90, col = c('#FFF0F5','#FFFACD','#E6E6FA','#ADD8E6', '#FFb6c1', '#DDA0DD'), label=lbls12,
    main='12월 이용자 연령대 비율')