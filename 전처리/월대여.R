library(reshape2)

# 1번 방법 
m_ren <-read.csv('월대여.csv')


melt(m_ren, id.vars='월', measure.vars=('대여건수'))

df1<-melt(m_ren, id.vars='월', 
          measure.vars=('대여건수'),
          variable.name = '구분',
          value.name = '대여건수')

library('tidyverse')
c('대여'='#FF0000',
  '반납'='#0000FF') -> k_palette

ggplot(df1, aes(월, 대여건수))+
  geom_bar(stat='identity',position = position_dodge(0.5), size=.2, width=0.5)+
  coord_cartesian(xlim=c(1,12),ylim=c(700000,3500000))+
  scale_x_continuous(breaks=seq(1,12,1))+
  labs(title = '2021년 월별 대여건수',
       x = '월',
       y = '대여건수')+
  theme(axis.text.x = element_text(size=10),
        plot.title = element_text(hjust = 0.5))
