setwd("플젝")
getwd()


rent01 <- read.csv('공공자전거_대여이력_정보_2101.csv')
head(rent01)
View(head(rent01))

rent02 <- read.csv('공공자전거_대여이력_정보_2102.csv')
rent03 <- read.csv('공공자전거_대여이력_정보_2103.csv')
rent04 <- read.csv('공공자전거_대여이력_정보_2104.csv')
rent05 <- read.csv('공공자전거_대여이력_정보_2105.csv')
rent06 <- read.csv('공공자전거_대여이력_정보_2106.csv')
rent07 <- read.csv('공공자전거_대여이력_정보_2107.csv')
rent08 <- read.csv('공공자전거_대여이력_정보_2108.csv')
rent09 <- read.csv('공공자전거_대여이력_정보_2109.csv')
rent10 <- read.csv('공공자전거_대여이력_정보_2110.csv')
rent11 <- read.csv('공공자전거_대여이력_정보_2111.csv')
rent12 <- read.csv('공공자전거_대여이력_정보_2112.csv')

# install.packages("readxl")
# library(readxl)

place <- read.csv('공공자전거_대여소_정보.csv')
View(head(place))


str(rent01)

#rent <- rbind(rent01, rent02, rent03, rent04, rent05, rent06, rent07, rent08, rent09, rent10, rent11, rent12)
#View(head(rent))
#View(tail(rent))

library(dplyr)
rent01<- rename(rent01, 대여대여소번호=대여.대여소번호, 대여소명=대여.대여소명)
head(rent01)

rent02<- rename(rent02, 대여대여소번호=대여.대여소번호, 대여소명=대여.대여소명)
rent03<- rename(rent03, 대여대여소번호=대여.대여소번호, 대여소명=대여.대여소명)
rent04<- rename(rent04, 대여대여소번호=대여.대여소번호, 대여소명=대여.대여소명)
rent05<- rename(rent05, 대여대여소번호=대여.대여소번호, 대여소명=대여.대여소명)
rent06<- rename(rent06, 대여대여소번호=대여.대여소번호, 대여소명=대여.대여소명)
rent07<- rename(rent07, 대여대여소번호=대여.대여소번호, 대여소명=대여.대여소명)
rent08<- rename(rent08, 대여대여소번호=대여.대여소번호, 대여소명=대여.대여소명)
rent09<- rename(rent09, 대여대여소번호=대여.대여소번호, 대여소명=대여.대여소명)
rent10<- rename(rent10, 대여대여소번호=대여.대여소번호, 대여소명=대여.대여소명)
rent11<- rename(rent11, 대여대여소번호=대여.대여소번호, 대여소명=대여.대여소명)
rent12<- rename(rent12, 대여대여소번호=대여.대여소번호, 대여소명=대여.대여소명)



rent01_t <- rent01 %>% select(대여일시,대여대여소번호,대여소명)
head(rent01_t)

rent02_t <- rent02 %>% select(대여일시,대여대여소번호,대여소명)
rent03_t <- rent03 %>% select(대여일시,대여대여소번호,대여소명)
rent04_t <- rent04 %>% select(대여일시,대여대여소번호,대여소명)
rent05_t <- rent05 %>% select(대여일시,대여대여소번호,대여소명)
rent06_t <- rent06 %>% select(대여일시,대여대여소번호,대여소명)
rent07_t <- rent07 %>% select(대여일시,대여대여소번호,대여소명)
rent08_t <- rent08 %>% select(대여일시,대여대여소번호,대여소명)
rent09_t <- rent09 %>% select(대여일시,대여대여소번호,대여소명)
rent10_t <- rent10 %>% select(대여일시,대여대여소번호,대여소명)
rent11_t <- rent11 %>% select(대여일시,대여대여소번호,대여소명)
rent12_t <- rent12 %>% select(대여일시,대여대여소번호,대여소명)

return01_t<- rent01 %>% select(반납일시,반납대여소번호,반납대여소명)
head(return01_t)

return02_t<- rent02 %>% select(반납일시,반납대여소번호,반납대여소명)
return03_t<- rent03 %>% select(반납일시,반납대여소번호,반납대여소명)
return04_t<- rent04 %>% select(반납일시,반납대여소번호,반납대여소명)
return05_t<- rent05 %>% select(반납일시,반납대여소번호,반납대여소명)
return06_t<- rent06 %>% select(반납일시,반납대여소번호,반납대여소명)
return07_t<- rent07 %>% select(반납일시,반납대여소번호,반납대여소명)
return08_t<- rent08 %>% select(반납일시,반납대여소번호,반납대여소명)
return09_t<- rent09 %>% select(반납일시,반납대여소번호,반납대여소명)
return10_t<- rent10 %>% select(반납일시,반납대여소번호,반납대여소명)
return11_t<- rent11 %>% select(반납일시,반납대여소번호,반납대여소명)
return12_t<- rent12 %>% select(반납일시,반납대여소번호,반납대여소명)




rent01_t <- rent01_t %>% filter(대여대여소번호>=102)
head(rent01_t)

rent02_t <- rent02_t %>% filter(대여대여소번호>=102)
rent03_t <- rent03_t %>% filter(대여대여소번호>=102)
rent04_t <- rent04_t %>% filter(대여대여소번호>=102)
rent05_t <- rent05_t %>% filter(대여대여소번호>=102)
rent06_t <- rent06_t %>% filter(대여대여소번호>=102)
rent07_t <- rent07_t %>% filter(대여대여소번호>=102)
rent08_t <- rent08_t %>% filter(대여대여소번호>=102)
rent09_t <- rent09_t %>% filter(대여대여소번호>=102)
rent10_t <- rent10_t %>% filter(대여대여소번호>=102)
rent11_t <- rent11_t %>% filter(대여대여소번호>=102)
rent12_t <- rent12_t %>% filter(대여대여소번호>=102)


return01_t <- return01_t %>% filter(반납대여소번호>=102)
head(return01_t)
return02_t <- return02_t %>% filter(반납대여소번호>=102)
return03_t <- return03_t %>% filter(반납대여소번호>=102)
return04_t <- return04_t %>% filter(반납대여소번호>=102)
return05_t <- return05_t %>% filter(반납대여소번호>=102)
return06_t <- return06_t %>% filter(반납대여소번호>=102)
return07_t <- return07_t %>% filter(반납대여소번호>=102)
return08_t <- return08_t %>% filter(반납대여소번호>=102)
return09_t <- return09_t %>% filter(반납대여소번호>=102)
return10_t <- return10_t %>% filter(반납대여소번호>=102)
return11_t <- return11_t %>% filter(반납대여소번호>=102)
return12_t <- return12_t %>% filter(반납대여소번호>=102)

head(place)
str(place)

place1<- rename(place, 대여대여소번호=대여소.번호)
head(place1)

place2<- rename(place, 반납대여소번호=대여소.번호)
head(place2)



 
rent_t1 <- full_join(rent01_t,place1, by='대여대여소번호')
head(rent_t1)
View(head(rent_t))
View(tail(rent_t))


rent_t2 <- full_join(rent02_t,place1, by='대여대여소번호')
rent_t3 <- full_join(rent03_t,place1, by='대여대여소번호')
rent_t4 <- full_join(rent04_t,place1, by='대여대여소번호')
rent_t5 <- full_join(rent05_t,place1, by='대여대여소번호')
rent_t6 <- full_join(rent06_t,place1, by='대여대여소번호')
rent_t7 <- full_join(rent07_t,place1, by='대여대여소번호')
rent_t8 <- full_join(rent08_t,place1, by='대여대여소번호')
rent_t9 <- full_join(rent09_t,place1, by='대여대여소번호')
rent_t10 <- full_join(rent10_t,place1, by='대여대여소번호')
rent_t11 <- full_join(rent11_t,place1, by='대여대여소번호')
rent_t12 <- full_join(rent12_t,place1, by='대여대여소번호')
head(rent_t12)
# typeof(rent12_t$대여대여소번호) # "integer"


# <character>> -> <integer>> 변환
return02_t$반납대여소번호<-as.integer(return02_t$반납대여소번호)
return03_t$반납대여소번호<-as.integer(return03_t$반납대여소번호)
return04_t$반납대여소번호<-as.integer(return04_t$반납대여소번호)
return05_t$반납대여소번호<-as.integer(return05_t$반납대여소번호)
return06_t$반납대여소번호<-as.integer(return06_t$반납대여소번호)


return_t1 <- full_join(return01_t,place2, by='반납대여소번호')
head(return_t)
View(head(return_t))
View(tail(return_t))
# typeof(return01_t$반납대여소번호) # "integer"


return_t2 <- full_join(return02_t,place2, by='반납대여소번호')
return_t3 <- full_join(return03_t,place2, by='반납대여소번호')
return_t4 <- full_join(return04_t,place2, by='반납대여소번호')
return_t5 <- full_join(return05_t,place2, by='반납대여소번호')
return_t6 <- full_join(return06_t,place2, by='반납대여소번호')
return_t7 <- full_join(return07_t,place2, by='반납대여소번호')
return_t8 <- full_join(return08_t,place2, by='반납대여소번호')
return_t9 <- full_join(return09_t,place2, by='반납대여소번호')
return_t10 <- full_join(return10_t,place2, by='반납대여소번호')
return_t11 <- full_join(return11_t,place2, by='반납대여소번호')
return_t12 <- full_join(return12_t,place2, by='반납대여소번호')


typeof(rent_1) # [1] "list"

rent_1 <- rent_t1 %>% select(대여일시,대여대여소번호,대여소명,자치구,상세주소,위도,경도)
View(head(rent_1))



rent_2 <- rent_t2 %>% select(대여일시,대여대여소번호,대여소명,자치구,상세주소,위도,경도)
rent_3 <- rent_t3 %>% select(대여일시,대여대여소번호,대여소명,자치구,상세주소,위도,경도)
rent_4 <- rent_t4 %>% select(대여일시,대여대여소번호,대여소명,자치구,상세주소,위도,경도)
rent_5 <- rent_t5 %>% select(대여일시,대여대여소번호,대여소명,자치구,상세주소,위도,경도)
rent_6 <- rent_t6 %>% select(대여일시,대여대여소번호,대여소명,자치구,상세주소,위도,경도)
rent_7 <- rent_t7 %>% select(대여일시,대여대여소번호,대여소명,자치구,상세주소,위도,경도)
rent_8 <- rent_t8 %>% select(대여일시,대여대여소번호,대여소명,자치구,상세주소,위도,경도)
rent_9 <- rent_t9 %>% select(대여일시,대여대여소번호,대여소명,자치구,상세주소,위도,경도)
rent_10 <- rent_t10 %>% select(대여일시,대여대여소번호,대여소명,자치구,상세주소,위도,경도)
rent_11 <- rent_t11 %>% select(대여일시,대여대여소번호,대여소명,자치구,상세주소,위도,경도)
rent_12 <- rent_t12 %>% select(대여일시,대여대여소번호,대여소명,자치구,상세주소,위도,경도)


memory.limit(size=NA)
memory.limit(size=56000)
memory.limit(size=NA)


rent_1<-na.omit(rent_1)
rent_2<-na.omit(rent_2)
rent_3<-na.omit(rent_3)
rent_4<-na.omit(rent_4)
rent_5<-na.omit(rent_5)
rent_6<-na.omit(rent_6)
rent_7<-na.omit(rent_7)
rent_8<-na.omit(rent_8)
rent_9<-na.omit(rent_9)
rent_10<-na.omit(rent_10)
rent_11<-na.omit(rent_11)
rent_12<-na.omit(rent_12)

rent_1_c<- count(rent_1, 자치구)
rent_2_c<- count(rent_2, 자치구)
rent_3_c<- count(rent_3, 자치구)
rent_4_c<- count(rent_4, 자치구)
rent_5_c<- count(rent_5, 자치구)
rent_6_c<- count(rent_6, 자치구)
rent_7_c<- count(rent_7, 자치구)
rent_8_c<- count(rent_8, 자치구)
rent_9_c<- count(rent_9, 자치구)
rent_10_c<- count(rent_10, 자치구)
rent_11_c<- count(rent_11, 자치구)
rent_12_c<- count(rent_12, 자치구)
rent_12_c

rent_d_1 <- as.data.frame(rent_1_c)
View(rent_d_1)
rent_d_2 <- as.data.frame(rent_2_c)
rent_d_3 <- as.data.frame(rent_3_c)
rent_d_4 <- as.data.frame(rent_4_c)
rent_d_5 <- as.data.frame(rent_5_c)
rent_d_6 <- as.data.frame(rent_6_c)
rent_d_7 <- as.data.frame(rent_7_c)
rent_d_8 <- as.data.frame(rent_8_c)
rent_d_9 <- as.data.frame(rent_9_c)
rent_d_10 <- as.data.frame(rent_10_c)
rent_d_11 <- as.data.frame(rent_11_c)
rent_d_12 <- as.data.frame(rent_12_c)

rent <- cbind(rent_d_1$n,rent_d_2$n,rent_d_3$n,rent_d_4$n,rent_d_5$n,rent_d_6$n,rent_d_7$n,rent_d_8$n,rent_d_9$n,rent_d_10$n,rent_d_11$n,rent_d_12$n)
View(rent) # 월별 자치구 대여 빈도 
typeof(rent)

library(tidyverse)
write_csv(rent, '구별전처리.csv')
rent_d <- data.frame(rent)
typeof(rent_d)
rent <- as.data.frame(rent_d)
typeof(rent)
write_csv(rent, '구별전처리_rent.csv')
View(rent)


############################## 일단 여기까지는 됨#####################################


#########################################################################################

# 번호열은 그냥 두고 자치구를 별도의 열로 추가해야함 

rownames(rent)=c('강남구','강동구', '강북구', '강서구', '관악구', '광진구', '구로구', '금천구',  '노원구', '도봉구',  '동대문구','동작구',  '마포구','서대문구',  '서초구', '성동구', '성북구',  '송파구', '양천구', '영등포구','용산구',  '은평구','종로구', '중구', '중랑구')

colnames(rent)=c('자치구', '1월','2월','3월','4월','5월','6월','7월','8월','9월','10월','11월','12월')

View(rent)
typeof(rent)



# 첫번째 열 제외하고 계산하기 ########## 여기서부터 하기 ########################
#rowSums(rent)
#typeof(rowSums(rent))

#a <- as.data.frame(rowSums(rent))
#View(a)

#rent <- cbind(rent, a$`rowSums(rent)`)
#View(rent)

#colnames(rent)=c('1월','2월','3월','4월','5월','6월','7월','8월','9월','10월','11월','12월','Total')
#View(rent)
#rent_d <- as.data.frame(rent)


library(ggplot2)

ggplot(rent_d, aes(자치구, Total))+
  geom_bar(stat='identity',fill='red')
#################################################################################################


return_1 <- return_t1 %>% select(반납일시,반납대여소번호,반납대여소명,자치구,상세주소,위도,경도)
View(head(return_1))
count(return_1, 자치구)


return_2 <- return_t2 %>% select(반납일시,반납대여소번호,반납대여소명,자치구,상세주소,위도,경도)
return_3 <- return_t3 %>% select(반납일시,반납대여소번호,반납대여소명,자치구,상세주소,위도,경도)
return_4 <- return_t4 %>% select(반납일시,반납대여소번호,반납대여소명,자치구,상세주소,위도,경도)
return_5 <- return_t5 %>% select(반납일시,반납대여소번호,반납대여소명,자치구,상세주소,위도,경도)
return_6 <- return_t6 %>% select(반납일시,반납대여소번호,반납대여소명,자치구,상세주소,위도,경도)
return_7 <- return_t7 %>% select(반납일시,반납대여소번호,반납대여소명,자치구,상세주소,위도,경도)
return_8 <- return_t8 %>% select(반납일시,반납대여소번호,반납대여소명,자치구,상세주소,위도,경도)
return_9 <- return_t9 %>% select(반납일시,반납대여소번호,반납대여소명,자치구,상세주소,위도,경도)
return_10 <- return_t10 %>% select(반납일시,반납대여소번호,반납대여소명,자치구,상세주소,위도,경도)
return_11 <- return_t11 %>% select(반납일시,반납대여소번호,반납대여소명,자치구,상세주소,위도,경도)
return_12 <- return_t12 %>% select(반납일시,반납대여소번호,반납대여소명,자치구,상세주소,위도,경도)


return_1<-na.omit(return_1)
return_2<-na.omit(return_2)
return_3<-na.omit(return_3)
return_4<-na.omit(return_4)
return_5<-na.omit(return_5)
return_6<-na.omit(return_6)
return_7<-na.omit(return_7)
return_8<-na.omit(return_8)
return_9<-na.omit(return_9)
return_10<-na.omit(return_10)
return_11<-na.omit(return_11)
return_12<-na.omit(return_12)

return_1_c<- count(return_1, 자치구)
return_2_c<- count(return_2, 자치구)
return_3_c<-count(return_3, 자치구)
return_4_c<-count(return_4, 자치구)
return_5_c<-count(return_5, 자치구)
return_6_c<-count(return_6, 자치구)
return_7_c<-count(return_7, 자치구)
return_8_c<-count(return_8, 자치구)
return_9_c<-count(return_9, 자치구)
return_10_c<-count(return_10, 자치구)
return_11_c<-count(return_11, 자치구)
return_12_c<-count(return_12, 자치구)

return_d_1 <- as.data.frame(return_1_c)
View(return_d_1)
return_d_2 <- as.data.frame(return_2_c)
return_d_3 <- as.data.frame(return_3_c)
return_d_4 <- as.data.frame(return_4_c)
return_d_5 <- as.data.frame(return_5_c)
return_d_6 <- as.data.frame(return_6_c)
return_d_7 <- as.data.frame(return_7_c)
return_d_8 <- as.data.frame(return_8_c)
return_d_9 <- as.data.frame(return_9_c)
return_d_10 <- as.data.frame(return_10_c)
return_d_11 <- as.data.frame(return_11_c)
return_d_12 <- as.data.frame(return_12_c)

return <- cbind(return_d_1$n,return_d_2$n,return_d_3$n,return_d_4$n,return_d_5$n,return_d_6$n,return_d_7$n,return_d_8$n,return_d_9$n,return_d_10$n,return_d_11$n,return_d_12$n)
View(return) # 월별 자치구 반납 빈도 
# library(tidyverse)
write_csv(return, '구별전처리_return.csv')
return_d <- data.frame(return)
typeof(return_d)
return <- as.data.frame(return_d)
typeof(return)
write_csv(return, '구별전처리_return.csv')
View(return)

rownames(return)=c('강남구','강동구', '강북구', '강서구', '관악구', '광진구', '구로구', '금천구',  '노원구', '도봉구',  '동대문구','동작구',  '마포구','서대문구',  '서초구', '성동구', '성북구',  '송파구', '양천구', '영등포구','용산구',  '은평구','종로구', '중구', '중랑구')

colnames(return)=c('1월','2월','3월','4월','5월','6월','7월','8월','9월','10월','11월','12월')

View(return)


rowSums(return)
typeof(rowSums(return))

a <- as.data.frame(rowSums(return))
View(a)

return <- cbind(return, a$`rowSums(return)`)
View(return)

colnames(return)=c('1월','2월','3월','4월','5월','6월','7월','8월','9월','10월','11월','12월','Total')
View(return)


#############################################################################################

library(ggplot2)

typeof(rent)

# rent %>% group


ggplot(rent, 
       mapping = aes(x, y))

           
           
           
rent_1 %>% group_by(자치구) %>% 
  ggplot(aes(자치구, 대여대여소번호)) + 
  geom_bar(stat='identity',fill='red')+
  labs(title = '자치구별 대여 횟수',
       x = '자치구명',
       y = '대여 횟수')+
  theme(panel.background = element_rect(fill='white'))


#return_t1_1 %>% group_by(자치구) %>% 
  ggplot(aes(자치구, 반납대여소번호)) +
  geom_bar(stat='identity',fill='blue')+
  labs(title = '자치구별 반납 횟수',
       x = '자치구명',
       y = '반납 횟수')
