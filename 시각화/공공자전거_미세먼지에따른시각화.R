getwd()
setwd('./조별프로젝트/data')

# 데이터 불러오기
total_data <- read.csv('total.csv', header=T, fileEncoding = "utf-8")
total_data
total_data <- subset(total_data, select=c(대여일시, 대여건수,미세먼지,상태))
View(total_data)
gsub(",","",total_data$대여건수)

# 관련 패키지
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)
library(ggmap)
library(tidyverse)
library(colorspace)

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
dust4_m = mean(dust3) 
dust4_m

boxplot(dust2,dust3,dust4,
        main="미세먼지에 따른 대여량 변화",
        xlab='미세먼지농도',
        ylab='대여횟수',
        names=c('보통','나쁨','매우나쁨'),
        col='green',
        fill='orange',
        alpha=0.2)

# 각 평균 data 합쳐주기
dust_m <- rbind(dust2_m,dust3_m,dust4_m)
dust_a <- c("보통","나쁨","매우나쁨")
dust_b <- data.frame(dust_a,dust_m)
names(dust_b) <- c('대기질','대여횟수')
dust_b

ggplot(data=dust_b,aes(x=대기질, y=대여횟수)) +
  geom_bar(stat='identity', fill='orange',
           width=0.5)+
  theme(panel.background = element_rect(fill='white',colour='black'))

