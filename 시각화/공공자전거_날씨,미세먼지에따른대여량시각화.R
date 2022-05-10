getwd()
setwd('./조별프로젝트/data')

# 데이터 불러오기
total_data <- read.csv('total.csv', header=T, fileEncoding = "utf-8")
total_data <- data.frame(total_data)
total_data <- subset(total_data, select=c(대여일시, 대여건수,미세먼지,상태,강수량.mm.,날씨))


# 관련 패키지
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)
library(ggmap)
library(tidyverse)
library(colorspace)

weather1 = total_data %>% filter(날씨 == 1)
weather1

# 대여건수에 붙은 , 제거 후 num 형태로 바꿔주기
wea_t1 <- gsub(",","",weather1$대여건수)
class(wea_t1)
num_wea <- as.numeric(wea_t1)
wea1=mean(num_wea)

weather2 = total_data %>% filter(날씨 == 2)
weather2

# 대여건수에 붙은 , 제거 후 num 형태로 바꿔주기
wea_t2 <- gsub(",","",weather2$대여건수)
class(wea_t2)
num_wea2 <- as.numeric(wea_t2)
wea2=mean(num_wea2)

weather3 = total_data %>% filter(날씨 == 3)
weather3

# 대여건수에 붙은 , 제거 후 num 형태로 바꿔주기
wea_t3 <- gsub(",","",weather3$대여건수)
class(wea_t3)
num_wea3 <- as.numeric(wea_t3)
wea3=mean(num_wea3)

weather4 = total_data %>% filter(날씨 == 4)
weather4

# 대여건수에 붙은 , 제거 후 num 형태로 바꿔주기
wea_t4 <- gsub(",","",weather4$대여건수)
class(wea_t4)
num_wea4 <- as.numeric(wea_t4)
wea4=mean(num_wea4)

weather5 = total_data %>% filter(날씨 == 5)
weather5

# 대여건수에 붙은 , 제거 후 num 형태로 바꿔주기
wea_t5 <- gsub(",","",weather5$대여건수)
class(wea_t5)
num_wea5 <- as.numeric(wea_t5)
wea5=mean(num_wea5)

wea_a <- c("맑음","약한비","비","강한비","폭우")
wea_b=rbind(wea1,wea2,wea3,wea4,wea5)

wea_c <- data.frame(wea_a,wea_b)
names(wea_c) <- c('날씨','대여횟수')
wea_c

ggplot(data=wea_c,aes(x=날씨, y=대여횟수)) +
  geom_bar(stat='identity', fill='lightblue', colour='black',width=0.5)+
  theme(panel.background = element_rect(fill='white',colour='black'))+
  ggtitle("강수량에 따른 따릉이 대여 평균")+
  theme(plot.title = element_text(size=15, hjust=0.5))+
  scale_x_discrete(limits=c("맑음","약한비","비","강한비","폭우"))


# 전체 데이터 boxplot으로 나타내기
gsub(",","",total_data$대여건수)
boxplot(num_wea,num_wea2,num_wea3,num_wea4,num_wea5,
        main="강수량에 따른 하루 대여량",
        xlab='날씨',
        ylab='대여횟수',
        names=c('맑음','약한비','비','강한비','폭우'),
        col='lightblue',
        alpha=0.2)

################################미세먼지

# 각 키워드별 데이터 분리
dust1 = total_data %>% filter(상태 == '좋음')
# 2021년의 경우 미세먼지 data에 좋음이 뜬 날이 없음.
dust1 <- as.numeric(dust1)

dust2 = total_data %>% filter(상태 == '보통')
dust2 # 총 164개의 data 
dust2 <- gsub(",","",dust2$대여건수)
dust2 <- as.numeric(dust2)
dust2_m = mean(dust2) # bar 그래프를 그리기 위해 데이터 평균 추출
dust2_m

dust3 = total_data %>% filter(상태 == '나쁨')
dust3
dust3 <- gsub(",","",dust3$대여건수)
dust3 <- as.numeric(dust3) #num화
dust3_m = mean(dust3) 
dust3_m

dust4 = total_data %>% filter(상태 == '매우나쁨')
dust4
dust4 <- gsub(",","",dust4$대여건수)
dust4 <- as.numeric(dust4)
dust4_m = mean(dust4) 
dust4_m

boxplot(dust2,dust3,dust4,
        main="미세먼지에 따른 하루 대여량 평균",
        xlab='미세먼지농도',
        ylab='대여횟수',
        names=c('보통','나쁨','매우나쁨'),
        col='lightpink',
        alpha=0.2)

# 각 평균 data 합쳐주기
dust_m <- rbind(dust2_m,dust3_m,dust4_m)
dust_a <- c("보통","나쁨","매우나쁨")
dust_b <- data.frame(dust_a,dust_m)
names(dust_b) <- c('대기질','대여횟수')
dust_b
dust_b$대여횟수<-round(dust_b$대여횟수) # 소수점 정수로 바꾸기


ggplot(data=dust_b,aes(x=대기질, y=대여횟수)) +
  geom_col(position='dodge',colour='black',fill='pink',width=0.5)+
  geom_text(aes(label = 대여횟수), vjust = -0.2, color = "black")+
  theme(panel.background = element_rect(fill='white',colour='black'))
  ggtitle("미세먼지에 따른 따릉이 대여 평균")+
  theme(plot.title = element_text(size=15, hjust=0.5))+
  scale_x_discrete(limits=c("보통","나쁨","매우나쁨"))



ggplot(dust_b, aes(x='', y=대여횟수, fill=대기질))+
  geom_bar(stat='identity')+
  theme_void()+
  coord_polar('y', start=0)+
  geom_text(aes(label=paste0(round(대여횟수,1))),
            position=position_stack(vjust=0.5))+
  scale_fill_manual(values=c("#FF66B2","#CC0066","#FFCCFF"))+
  ggtitle("미세먼지에 따른 따릉이 대여 평균")+
  theme(plot.title = element_text(size=15, hjust=0.5))



