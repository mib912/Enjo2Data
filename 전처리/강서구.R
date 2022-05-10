setwd("플젝")
getwd()

rent <- read.csv('공공자전거 대여이력 정보_시간_2105.csv')

place <- read.csv('공공자전거_대여소_정보.csv')


library(dplyr)
rent01<- rename(rent, 대여대여소번호=대여.대여소번호, 대여소명=대여.대여소명)
head(rent01)


rent01_t <- rent01 %>% select(대여일시,대여대여소번호,대여소명, 반납일시, 반납대여소번호, 반납대여소명, 이용시간, 이용거리 )
View(head(rent01_t))

rent01_t$대여날짜<-substr(rent01_t$대여일시, 0,10)
View(head(rent01_t))

rent01_t$대여시간<-substr(rent01_t$대여일시, 12,16)


rent01_t$반납날짜<-substr(rent01_t$반납일시, 0,10)


rent01_t$반납시간<-substr(rent01_t$반납일시, 12,16)
View(head(rent01_t))

rent_r <- rent01_t %>% select(대여날짜, 대여시간,대여대여소번호,대여소명, 반납날짜, 반납시간, 반납대여소번호, 반납대여소명, 이용시간, 이용거리 )
View(head(rent_r))



place1<- rename(place, 대여대여소번호=대여소.번호)

typeof(rent_r$대여대여소번호)
rent_r$대여대여소번호<-as.integer(rent_r$대여대여소번호)

rent_r2 <- full_join(rent_r,place1, by='대여대여소번호')

View(head(rent_r2))

rent_rs <- rent_r2 %>% filter(자치구=='강서구')
View(head(rent_rs))

rent_rs1<-na.omit(rent_rs)

write.csv(rent_rs, '강서구.csv',fileEncoding = 'cp949')

write.csv(rent_rs1, '강서구1.csv',fileEncoding = 'cp949')













