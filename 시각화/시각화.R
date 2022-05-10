setwd("플젝")
getwd()


rent<-read.csv('구별전처리_rent1.csv')
View(rent)

# rownames(rent)=c('강남구','강동구', '강북구', '강서구', '관악구', '광진구', '구로구', '금천구',  '노원구', '도봉구',  '동대문구','동작구',  '마포구','서대문구',  '서초구', '성동구', '성북구',  '송파구', '양천구', '영등포구','용산구',  '은평구','종로구', '중구', '중랑구')

colnames(rent)=c('자치구','1월','2월','3월','4월','5월','6월','7월','8월','9월','10월','11월','12월','Total')


# rent$Total <- rowSums(rent)


library(ggplot2)

ggplot(rent, aes(자치구, Total))+
  geom_bar(stat='identity',fill='red')+
  labs(title = '2021년 자치구별 대여 건수',
       x = '자치구명',
       y = '총 대여 건수')+
  theme(axis.text.x = element_text(size=10),
        plot.title = element_text(hjust = 0.5))

################################################################################

return <-read.csv('구별전처리_return1.csv')
View(return)


#rownames(return)=c('강남구','강동구', '강북구', '강서구', '관악구', '광진구', '구로구', '금천구',  '노원구', '도봉구',  '동대문구','동작구',  '마포구','서대문구',  '서초구', '성동구', '성북구',  '송파구', '양천구', '영등포구','용산구',  '은평구','종로구', '중구', '중랑구')

colnames(return)=c('자치구','1월','2월','3월','4월','5월','6월','7월','8월','9월','10월','11월','12월','Total')

ggplot(return, aes(자치구, Total))+
  geom_bar(stat='identity',fill='blue')+
  labs(title = '2021년 자치구별 반납 건수',
       x = '자치구명',
       y = '총 반납 건수')+
  theme(axis.text.x = element_text(size=10),
        plot.title = element_text(hjust = 0.5))




################################################################################
library(reshape2)

# 1번 방법 
rent_return2 <-read.csv('구별전처리_rent_return2.csv')
df <-rent_return2 %>% select(자치구,대여,반납)
df

melt(df, id.vars='자치구', measure.vars=c('대여','반납'))

df1<-melt(df, id.vars='자치구', 
          measure.vars=c('대여','반납'),
          variable.name = '구분',
          value.name = '건수')

library('tidyverse')
c('대여'='#FF0000',
  '반납'='#0000FF') -> k_palette

ggplot(df1, aes(자치구, 건수, fill=구분))+
  geom_bar(stat='identity',position = position_dodge(0.5), size=.2, width=0.5)+
  labs(title = '2021년 자치구별 대여·반납 건수',
       x = '자치구명',
       y = '연간 총 대여·반납')+
  theme(axis.text.x = element_text(size=10),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values=k_palette)




# 2번 방법 

rent_return1 <-read.csv('구별전처리_rent_return1.csv')

barplot(cbind(Rent_Total,Return_Total)~자치구,rent_return1,beside=T,
        main="자치구별 대여,반납 건수",xlab="자치구",ylab="대여, 반납",col=rainbow(2))+
                  legend("topright",legend=c("대여","반납"),fill=rainbow(2))


#####################################
rent_count <-read.csv('구별전처리_rent_count3.csv')
df <-rent_count  %>% select(자치구,대여소수,대여수)
df
library(reshape2)
melt(df, id.vars='자치구', measure.vars=c('대여소수','대여수'))

df1<-melt(df, id.vars='자치구', 
          measure.vars=c('대여소수','대여수'),
          variable.name = '구분',
          value.name = '건수')

library('tidyverse')
c('대여소수'='#FF0000',
  '대여수'='#0000FF') -> k_palette

ggplot(df1, aes(자치구, 건수, fill=구분))+
  geom_bar(stat='identity',position = position_dodge(0.5), size=.2, width=0.5)+
  labs(title = '2021년 자치구별 대여·반납 건수',
       x = '자치구명',
       y = '연간 총 대여·반납')+
  theme(axis.text.x = element_text(size=10),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values=k_palette)
