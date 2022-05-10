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

bicycle1<-filter(bicycle, !grepl('N', 성별))

bicycle2<-filter(bicycle, !grepl('미상', 연령대코드))

# 성별에 따른 따릉이 이용자
g<-round(table(bicycle1['성별'])/sum(table(bicycle1['성별']))*100,1)
# 성별 코드 백분율로 변경
g_data <- data.frame(g)
g_data
table(bicycle1['성별'])

pie(table(bicycle1['성별']),
    main="성별에 따른 서울시 공공자전거 이용률",
    labels=paste(g_data$Var1,g_data$Freq,'%'))
    
barplot(table(bicycle1['성별']))



# 연령대별 따릉이 이용자 수
h<-round(table(bicycle['연령대코드'])/sum(table(bicycle['연령대코드']))*100,1)
# 연령대 코드 백분율로 바꾸기
data<-data.frame(h)
View(data)

barplot(table(bicycle2['연령대코드']))
pie(table(bicycle2['연령대코드']),
    main="연령대별 서울시 공공자전거 이용률",
    labels=paste(data$Var1,data$Freq,'%'))
  



# 성별에 따른 미세먼지 각 단계에서의 이용자 수
ggplot(bicycle1, aes(x=상태, fill=성별))+
  geom_bar()+
  theme(panel.background = element_rect(fill='white',colour='black'))+
  scale_x_discrete(limits=c("보통", "나쁨", "매우나쁨"))+
  ggtitle("성별에 따른 따릉이 이용률")
#################################################
ggplot(bicycle1)+
  geom_bar(mapping=aes(x=성별, fill=상태),position="dodge",colour='black')+
  theme(panel.background = element_rect(fill='white',colour='black'))+
  scale_fill_brewer(palette="Pastel1")+
  ggtitle("미세먼지에 따른 남녀 이용량")+
  theme(plot.title = element_text(size=15, hjust=0.5))+
  labs(x = "성별",
       y = "서울시 따릉이 이용량")



# 연령대에 따른 미세먼지 각 단계에서의 이용자 수
ggplot(bicycle2)+
  geom_bar(mapping=aes(x=상태, fill=연령대코드),position='dodge',colour='black')+
  theme(panel.background = element_rect(fill='white',colour='black'))+
  scale_fill_brewer(palette="Pastel1")+
  ggtitle("미세먼지에 따른 연령별 이용량")+
  theme(plot.title = element_text(size=15, hjust=0.5))+
  labs(x = "미세먼지농도",
       y = "따릉이 이용량")+
  scale_x_discrete(limits=c("보통", "나쁨", "매우나쁨"))


# 연령대에 따른 강수량에서의 이용자 수
ggplot(bicycle2)+
  geom_bar(mapping=aes(x=날씨, fill=연령대코드),position='dodge',colour='black')+
  theme(panel.background = element_rect(fill='white',colour='black'))+
  scale_fill_brewer(palette="Pastel1")+
  ggtitle("강수량에 따른 연령별 이용량")+
  theme(plot.title = element_text(size=15, hjust=0.5))+
  scale_x_discrete(limits=c("맑음", "약한비","비","강한비","폭우"))+
  labs(x = "강수량",
       y = "따릉이 이용량")


# 성별에 따른 강수량에서의 이용자 수
p <- ggplot(bicycle1)+
      geom_bar(mapping=aes(x=날씨, fill=성별),position='dodge',colour='black')+
      theme(panel.background = element_rect(fill='white',colour='black'))+
      scale_fill_brewer(palette="Pastel1")+
      ggtitle("강수량에 따른 성별 이용량")+
      theme(plot.title = element_text(size=15, hjust=0.5))+
      scale_x_discrete(limits=c("맑음", "약한비","비","강한비","폭우"))+
      labs(x = "강수량",
          y = "따릉이 이용량")
p



