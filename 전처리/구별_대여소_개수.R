setwd("플젝")
getwd()

library(dplyr)
gu <- read.csv('대여소별전처리_rent_place2.csv')

gu_no <- gu %>% select(대여대여소번호, 보관소.대여소.명, 자치구, 상세주소, 위도, 경도) %>% group_by(자치구) %>% count(자치구)
View(gu_no)

gu_no

library(ggplot2)

ggplot(gu_no, aes(자치구, n))+
  geom_bar(stat='identity', fill='grey')+
  labs(title = '자치구별 대여소 수',
       x = '자치구명',
       y = '대여소 수')+
  theme(axis.text.x = element_text(size=10),
        plot.title = element_text(hjust = 0.5))
  

