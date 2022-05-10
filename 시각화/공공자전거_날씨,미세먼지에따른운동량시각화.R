getwd()
setwd('./조별프로젝트/data')

# 데이터 불러오기
bicycle <- read.csv('last1.csv', header=T, fileEncoding = "utf-8")
View(bicycle)

# 관련 패키지
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)
library(ggmap)
library(tidyverse)
library(colorspace)



# 운동량 값 num화
bicycle$운동량 <- as.numeric(bicycle$운동량)
bicycle

# 운동량에서 값이 0 인 것은 측정이 안 된 것이므로 제외(전처리)
bicycle_a <- bicycle %>% filter(운동량 > 0)
bicycle_a

# 각 날씨에 따른 운동량 평균 구하기
bicycle1 <- bicycle_a %>% filter(날씨 == 1)
bicycle2 <- bicycle_a %>% filter(날씨 == 2)
bicycle3 <- bicycle_a %>% filter(날씨 == 3)
bicycle4 <- bicycle_a %>% filter(날씨 == 4)
bicycle5 <- bicycle_a %>% filter(날씨 == 5)

# 운동량 총합
bicycle1a <- sum(bicycle1$운동량)
bicycle2a <- sum(bicycle2$운동량)
bicycle3a <- sum(bicycle3$운동량)
bicycle4a <- sum(bicycle4$운동량)
bicycle5a <- sum(bicycle5$운동량)

# 운동량과 날씨 데이터로 만들기
wea_a <- c("맑음","약한비","비","강한비","폭우")
bicyclee <- rbind(bicycle1a,bicycle2a,bicycle3a,bicycle4a,bicycle5a)
bicyclee
wea_c <- data.frame(wea_a,bicyclee)
names(wea_c) <- c('날씨','운동량') # 컬럼 이름 변경경
wea_c


ggplot(data=wea_c,aes(x=날씨, y=운동량)) +
  geom_bar(stat='identity', fill='lightblue', colour='black',width=0.5)+
  theme(panel.background = element_rect(fill='white',colour='black'))+
  ggtitle("강수량에 따른 따릉이 이동거리(m)")+
  theme(plot.title = element_text(size=15, hjust=0.5))+
  scale_x_discrete(limits=c("맑음","약한비","비","강한비","폭우"))


# 운동량 값 num화
bicycle$운동량 <- as.numeric(bicycle$운동량)
bicycle

# 운동량에서 값이 0 인 것은 측정이 안 된 것이므로 제외(전처리)
bicycle_a <- bicycle %>% filter(운동량 > 0)
bicycle_a

# 각 날씨에 따른 운동량 평균 구하기
dust1 <- bicycle_a %>% filter(상태 == '보통')
dust2 <- bicycle_a %>% filter(상태 == '나쁨')
dust3 <- bicycle_a %>% filter(상태 == '매우나쁨')



# 운동량 총합
dust1a <- sum(dust1$운동량)
dust2a <- sum(dust2$운동량)
dust3a <- sum(dust3$운동량)


# 운동량과 날씨 데이터로 만들기
dust_a <- c("보통","나쁨","매우나쁨")

bicycl <- rbind(dust1a,dust2a,dust3a)
bicycl
wea_d <- data.frame(dust_a,bicycl)
names(wea_d) <- c('미세먼지','운동량') # 컬럼 이름 변경
wea_d

ggplot(data=wea_d,aes(x=미세먼지, y=운동량)) +
  geom_bar(stat='identity', fill='lightpink', colour='black',width=0.5)+
  theme(panel.background = element_rect(fill='white',colour='black'))+
  ggtitle("미세먼지에 따른 따릉이 이동거리(m)")+
  theme(plot.title = element_text(size=15, hjust=0.5))+
  scale_x_discrete(limits=c("보통","나쁨","매우나쁨"))

