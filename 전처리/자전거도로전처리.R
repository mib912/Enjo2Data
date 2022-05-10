
library(dplyr)

raw <- read.table('./data/서울시 자전거도로 현황 통계(2020).txt', fileEncoding = 'utf-8', sep="\t", header=T)
road <- raw

# 불 필요한 행 삭제
road <- road[-c(1,2,3,29,30,31,32),]

# 불 필요한 열 삭제
road <- road[,-c(1,2)]

# 열 이름 바꾸기
header <- c('자치구','구간합계','길이합계','자전거전용도로_구간','자전거전용도로_길이','자전거보행자겸용도로_구간','자전거보행자겸용도로_길이','자전거전용차로_구간','자전거전용차로_길이','자전거우선도로_구간','자전거우선도로_길이')

names(road) <- header

# '-'를 0으로 바꾸기
road[road=='-'] <- 0

# 행 인덱스 초기화
rownames(road)=NULL

View(road)

# 저장하고 읽어오기 테스트
# write.csv(road,"./data/road.csv")
# read.csv("./data/road.csv",header=T)

