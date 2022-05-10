setwd("플젝")
getwd()


library(ggplot2)

library(reshape2)

# 1번 방법 
work<-read.csv('강서구6.csv')
View(head(work))


melt(work, id.vars='시간대', measure.vars=c('평일','주말.공휴일'))

df1<-melt(work, id.vars='시간대', 
          measure.vars=c('평일','주말.공휴일'),
          variable.name = '구분',
          value.name = '대여건수')
View(df1)



ggplot(df1, aes(시간대, 대여건수))+
  geom_line(aes(color = 구분), size=2)
