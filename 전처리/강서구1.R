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

rent01_t$대여시<-substr(rent01_t$대여일시, 12,13)

rent01_t$대여분<-substr(rent01_t$대여일시, 15,16)


rent01_t$반납날짜<-substr(rent01_t$반납일시, 0,10)


rent01_t$반납시<-substr(rent01_t$반납일시, 12,13)
rent01_t$반납분<-substr(rent01_t$반납일시, 15,16)
View(head(rent01_t))

rent01_t$대여분구분<-ifelse(rent_r$대여분<=30, "1", "2")
rent01_t$반납분구분<-ifelse(rent_r$반납분<=30, "1", "2")

rent01_t$이용시간구분<-ifelse(rent_r$이용시간<=10, "1",
                       ifelse(rent_r$이용시간<=20, "2","3"))

rent01_t$이용거리구분<-ifelse(rent_r$이용거리<=100, "1",
                        ifelse(rent_r$이용시간<=3000, "2","3"))

rent_r <- rent01_t %>% select(대여날짜, 대여시,대여분,대여분구분, 대여대여소번호,대여소명, 반납날짜, 반납시,반납분, 반납분구분,반납대여소번호, 반납대여소명, 이용시간,이용시간구분, 이용거리,이용거리구분 )

View(head(rent_r))
rent_rs<-na.omit(rent_r)


place1<- rename(place, 대여대여소번호=대여소.번호)

typeof(rent_rs$대여대여소번호)
rent_rs$대여대여소번호<-as.integer(rent_rs$대여대여소번호)
rent_rs1<-na.omit(rent_rs)
rent_r2 <- full_join(rent_rs1,place1, by='대여대여소번호')

View(head(rent_r2))

rent_rs <- rent_r2 %>% filter(자치구=='강서구')
View(head(rent_rs))

rent_rs1<-na.omit(rent_rs)


write.csv(rent_rs1, '강서구4.csv',fileEncoding = 'cp949')
