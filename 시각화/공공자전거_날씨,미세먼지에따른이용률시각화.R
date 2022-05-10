# 미세먼지,날씨 합치기
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
#install.packages('beeswarm',dependencies = TRUE)
library(beeswarm)

# 운동량 값 num화
bicycle$운동량 <- as.numeric(bicycle$운동량)
bicycle

# 운동량에서 값이 0 인 것은 측정이 안 된 것이므로 제외(전처리)
bicycle_a <- bicycle %>% filter(운동량 > 0)
bicycle_a

# 각 날씨에 따른 운동량 평균 구하기
bicycle1 <- bicycle_a %>% filter(날씨 == "맑음")
bicycle2 <- bicycle_a %>% filter(날씨 == "비")
bicycle3 <- bicycle_a %>% filter(날씨 == "폭우")


# 운동량 총합
bicycle1a <- sum(bicycle1$운동량)
bicycle2a <- sum(bicycle2$운동량)
bicycle3a <- sum(bicycle3$운동량)


# 운동량과 날씨 데이터로 만들기
wea_a <- c("맑음","비","폭우")
bicyclee <- rbind(bicycle1a,bicycle2a,bicycle3a)
bicyclee
wea_c <- data.frame(wea_a,bicyclee)
names(wea_c) <- c('날씨','운동량') # 컬럼 이름 변경경
wea_c


ggplot(data=wea_c,aes(x=날씨, y=운동량)) +
  geom_bar(stat='identity', fill='lightblue', colour='black',width=0.5)+
  theme(panel.background = element_rect(fill='white',colour='black'))+
  ggtitle("강수량에 따른 따릉이 이동거리(m)")+
  theme(plot.title = element_text(size=15, hjust=0.5))+
  scale_x_discrete(limits=c("맑음","비","폭우"))


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


# 강수량과 미세먼지에 따른 따릉이 이용량
ggplot(bicycle)+
  geom_bar(mapping=aes(x=상태, fill=날씨),position='dodge',colour='black')+
  theme(panel.background = element_rect(fill='white',colour='black'))+
  scale_fill_brewer(palette="Pastel1")+
  ggtitle("강수량, 미세먼지 데이터에 따른 따릉이 대여량 변화")+
  theme(plot.title = element_text(size=15, hjust=0.5))+
  scale_x_discrete(limits=c("보통", "나쁨","매우나쁨"))+
  labs(x = "강수량",
       y = "따릉이 이용량")

# 날씨에 따른 미세먼지 상태에서의 대여량 뽑기
a <- table(bicycle$날씨 == '맑음' & bicycle$상태 == '보통')
a
b <- table(bicycle$날씨 == '맑음' & bicycle$상태 == '나쁨')
c <- table(bicycle$날씨 == '맑음' & bicycle$상태 == '매우나쁨')
d <- table(bicycle$날씨 == '비' & bicycle$상태 == '보통')
e <- table(bicycle$날씨 == '비' & bicycle$상태 == '나쁨')
f <- table(bicycle$날씨 == '비' & bicycle$상태 == '매우나쁨')
g <- table(bicycle$날씨 == '폭우' & bicycle$상태 == '보통')
h <- table(bicycle$날씨 == '폭우' & bicycle$상태 == '나쁨')
i=0
i
k <- rbind(a,b,c,d,e,f,g,h)
k <- as.numeric(k)
k
k <- k[9:16]

k <- c(k,0)
k
l <- c("맑음&보통","맑음&나쁨","맑음&매우나쁨",'비&보통','비&나쁨','비&매우나쁨','폭우&보통','폭우&나쁨','폭우&매우나쁨')
k

m <- data.frame(l,k)
m$k <- as.numeric(m$k)
m$k<-prop.table(m$k) # 백분율로 표현
m

ggplot(data=m,aes(x=l, y=k)) +
  geom_bar(stat='identity', fill='orange',
           width=0.8)+
  theme(panel.background = element_rect(fill='white',colour='black'))+
  labs(x = "미세먼지&강수량",
       y = "따릉이 이용량")+
  ggtitle("미세먼지&강수량에 따른 이용량")+
  theme(plot.title = element_text(size=15, hjust=0.5))

ggplot(bicycle, 
       aes(x =날씨  , y = 상태 , size = 운동량)) %>%
  + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1))


#################################



# 그래프 시각화


## 예시
# Load ggpubr package
#install.packages("ggpubr")
library(ggpubr)

# 그래프 표로 만들기
weat <-matrix(m$k,nrow=3,byrow=T)
rownames(weat) = c('맑음','비','폭우')
colnames(weat) = c('보통','나쁨','매우나쁨')
weat
names(dimnames(weat))=c('날씨','미세먼지') # 행과 열에 이름 붙여주기

# 행렬을 표로 전환 및 데이터프레임으로 만들기 
df_wea<-as.data.frame(as.table(weat))
df_wea$Freq <- round(df_wea$Freq,3)*100 # 백분위 표시 후 퍼센트로 바꿔주기
names(df_wea) <- c("날씨",'미세먼지',"이용률")
df_wea

## 그래프 시각화 ( ggballoonplot이용 )
ggballoonplot(
  data = df_wea, 
  x = "날씨", 
  y = "미세먼지",
  size = "이용률",
  size.range = c(10, 20), 
  fill = "green",
  show.label = TRUE, 
  rotate.x.text = FALSE, 
  legend = "none"
)

# ggplot을 통한 그래프 시각화
g <- ggplot(df_wea, aes(날씨, 미세먼지)) + geom_point(aes(size = 이용률), color="#FFCC00") + theme_bw() + xlab("날씨") + ylab("미세먼지")
g + scale_size_continuous(range=c(10,30)) + geom_text(aes(label = paste(이용률,'%')))+ theme(axis.text=element_text( face = 'bold', size = 10))+
  ggtitle("날씨와 미세먼지에 따른 따릉이 이용률")+
  theme(plot.title = element_text(size=20, hjust=0.5))+ 
  theme(legend.title = element_blank()) +   # remove legend title
  theme(legend.position = 'none')



                                                      