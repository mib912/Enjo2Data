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

place <- read.csv('공공자전거_대여소_정보.csv')


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
tail(rent_t1)
View(head(rent_t1))
View(tail(rent_t1))

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


# <character>> -> <integer>> 변환
return02_t$반납대여소번호<-as.integer(return02_t$반납대여소번호)
return03_t$반납대여소번호<-as.integer(return03_t$반납대여소번호)
return04_t$반납대여소번호<-as.integer(return04_t$반납대여소번호)
return05_t$반납대여소번호<-as.integer(return05_t$반납대여소번호)
return06_t$반납대여소번호<-as.integer(return06_t$반납대여소번호)



return_t1 <- full_join(return01_t,place2, by='반납대여소번호')
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


rent_1_c<- count(rent_1, 대여대여소번호)
rent_2_c<- count(rent_2, 대여대여소번호)
rent_3_c<- count(rent_3, 대여대여소번호)
rent_4_c<- count(rent_4, 대여대여소번호)
rent_5_c<- count(rent_5, 대여대여소번호)
rent_6_c<- count(rent_6, 대여대여소번호)
rent_7_c<- count(rent_7, 대여대여소번호)
rent_8_c<- count(rent_8, 대여대여소번호)
rent_9_c<- count(rent_9, 대여대여소번호)
rent_10_c<- count(rent_10, 대여대여소번호)
rent_11_c<- count(rent_11, 대여대여소번호)
rent_12_c<- count(rent_12, 대여대여소번호)
head(rent_1_c)


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



# rent <- cbind(rent_d_1$n,rent_d_2$n,rent_d_3$n,rent_d_4$n,rent_d_5$n,rent_d_6$n,rent_d_7$n,rent_d_8$n,rent_d_9$n,rent_d_10$n,rent_d_11$n,rent_d_12$n)



rent1 <-full_join(rent_d_1,rent_d_2,by='대여대여소번호')
View(return1)

rent3 <-full_join(rent_d_3,rent_d_4,by='대여대여소번호')
View(return3)

rent5 <-full_join(rent_d_5,rent_d_6,by='대여대여소번호')
View(return5)

rent7 <-full_join(rent_d_7,rent_d_8,by='대여대여소번호')
View(return7)

rent9 <-full_join(rent_d_9,rent_d_10,by='대여대여소번호')
View(return9)

rent11 <-full_join(rent_d_11,rent_d_12,by='대여대여소번호')
View(return11)


rent1_1 <-full_join(rent1,rent3,by='대여대여소번호')
View(return1_1)

rent1_5 <-full_join(rent5,rent7,by='대여대여소번호')


rent1_9 <-full_join(rent9,rent11,by='대여대여소번호')


rent2 <-full_join(rent1_1,rent1_5,by='대여대여소번호')
View(return2)

rent <-full_join(rent2,rent1_9,by='대여대여소번호')
View(head(rent))
colnames(rent)=c('대여대여소번호','1월','2월','3월','4월','5월','6월','7월','8월','9월','10월','11월','12월')
View(head(rent))

typeof(rent)

library(tidyverse)

rent_d <- data.frame(rent)
typeof(rent_d)
rent <- as.data.frame(rent_d)
typeof(rent)
#write_csv(rent, '대여소별전처리_rent.csv')
View(rent)

#install.packages("readr")
library(readr)

write.csv(rent, '대여소별전처리_rent.csv',fileEncoding = 'cp949')

head(place1)

rent_place<-full_join(rent, place1,by='대여대여소번호')
View(head(rent_place))

write.csv(rent_place, '대여소별전처리_rent_place.csv',fileEncoding = 'cp949')




##########################################################################


###########################################################################

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
head(return_1)
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

return_1_c<-count(return_1, 반납대여소번호)
return_2_c<-count(return_2, 반납대여소번호)
return_3_c<-count(return_3, 반납대여소번호)
return_4_c<-count(return_4, 반납대여소번호)
return_5_c<-count(return_5, 반납대여소번호)
return_6_c<-count(return_6, 반납대여소번호)
return_7_c<-count(return_7, 반납대여소번호)
return_8_c<-count(return_8, 반납대여소번호)
return_9_c<-count(return_9, 반납대여소번호)
return_10_c<-count(return_10, 반납대여소번호)
return_11_c<-count(return_11, 반납대여소번호)
return_12_c<-count(return_12, 반납대여소번호)

return_d_1 <- as.data.frame(return_1_c)
View(return_d_12)
View(return_d_5)
View(return_d_6)
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



return1 <-full_join(return_d_1,return_d_2,by='반납대여소번호')
View(return1)

return3 <-full_join(return_d_3,return_d_4,by='반납대여소번호')
View(return3)

return5 <-full_join(return_d_5,return_d_6,by='반납대여소번호')
View(return5)

return7 <-full_join(return_d_7,return_d_8,by='반납대여소번호')
View(return7)

return9 <-full_join(return_d_9,return_d_10,by='반납대여소번호')
View(return9)

return11 <-full_join(return_d_11,return_d_12,by='반납대여소번호')
View(return11)


return1_1 <-full_join(return1,return3,by='반납대여소번호')
View(return1_1)

return1_5 <-full_join(return5,return7,by='반납대여소번호')


return1_9 <-full_join(return9,return11,by='반납대여소번호')


return2 <-full_join(return1_1,return1_5,by='반납대여소번호')
View(return2)

return <-full_join(return2,return1_9,by='반납대여소번호')
View(head(return))

colnames(return)=c('반납대여소번호','1월','2월','3월','4월','5월','6월','7월','8월','9월','10월','11월','12월')
View(head(return))

typeof(return)

library(tidyverse)

return_d <- data.frame(return)
typeof(return_d)
return <- as.data.frame(return_d)
typeof(return)
# write_csv(return, '대여소별전처리_return.csv')
View(head(return))


library(readr)

write.csv(return, '대여소별전처리_return.csv',fileEncoding = 'cp949')


head(place2)

return_place<-full_join(return, place2,by='반납대여소번호')
View(head(return_place))

write.csv(return_place, '대여소별전처리_return_place.csv',fileEncoding = 'cp949')
