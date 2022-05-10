# 패키지
library(ggmap)
library(dplyr)
# install.packages('corrplot')
library(corrplot)

setwd('전공프로젝트')

# 데이터 불러오기
road <- read.csv('./pre_data/구별자전거도로.csv', header=T,row.names=1)
rent_map <- read.csv('./pre_data/공공자전거_1월(대여소별대여건수,위도경도).csv', header=T, row.names=1)

rent <- read.csv('./pre_data/구별전처리_rent1.csv', header=T,row.names=1)

# 자치구별 대여 건수 구하기
gu_cnt <- rent_map %>% select(c(대여건수, 자치구)) %>% 
  group_by(자치구) %>% 
  summarise(대여건수=sum(대여건수))

# 조인을 통해 자치구별 대여건수와 자치구별 자전거 도로 정보 합치기
gu_road <- left_join(road,gu_cnt,id='자치구')
str(gu_road)

mat <- as.matrix(gu_road[-1]) #  자치구열은 제외 후 행렬
road_cor <- cor(mat)



################################################################################
# 상관행렬 그래프
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(road_cor,
         title = '자전거도로와 월 대여건수 상관관계', 
         method = "color",        # 색깔로 표현
         col = col(200),          # 색상 200개 선정
         type = "lower",          # 왼쪽 아래 행렬만 표시
         addCoef.col = "black",   # 상관계수 색깔
         tl.col = "black",        # 변수명 색깔
         tl.srt = 45,             # 변수명 45도 기울임
         diag = F,                # 대각 행렬 제외
         mar=c(0,0,1,0) )         # 제목 보이게 설정정

#### 일부분씩 보기
# 1 자치구별 모든 자전거 도로 구간 합계, 길이 합계를 대여건수와 비교
df1 <- gu_road %>% select(c(자치구,구간합계,길이합계,대여건수))

# 상관행렬을 만들기 위해서 자치구열은 제외하고 상관행렬 만들기
mat <- as.matrix(df1[-1]) #  자치구열은 제외 후 행렬
road_cor <- cor(mat)

# 상관행렬 그래프
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(road_cor,
         method = "color",        # 색깔로 표현
         col = col(200),          # 색상 200개 선정
         order = "hclust",        # 유사한 상관계수끼리 군집화
         addCoef.col = "black",   # 상관계수 색깔
         tl.col = "black",        # 변수명 색깔
         tl.srt = 45)             # 변수명 45도 기울임


####
names(gu_road)
# [1] "자치구"                    "구간합계"                 
# [3] "길이합계"                  "자전거전용도로_구간"      
# [5] "자전거전용도로_길이"       "자전거보행자겸용도로_구간"
# [7] "자전거보행자겸용도로_길이" "자전거전용차로_구간"      
# [9] "자전거전용차로_길이"       "자전거우선도로_구간"      
# [11] "자전거우선도로_길이"       "대여건수" 

# 2 
df2 <- gu_road %>% select(c(자치구,자전거전용도로_구간,자전거전용도로_길이,자전거보행자겸용도로_구간,자전거보행자겸용도로_길이, 대여건수))

# 상관행렬을 만들기 위해서 자치구열은 제외하고 상관행렬 만들기
mat <- as.matrix(df2[-1]) #  자치구열은 제외 후 행렬
road_cor <- cor(mat)

# 상관행렬 그래프
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(road_cor,
         method = "color",        # 색깔로 표현
         col = col(200),          # 색상 200개 선정
         order = "hclust",        # 유사한 상관계수끼리 군집화
         addCoef.col = "black",   # 상관계수 색깔
         tl.col = "black",        # 변수명 색깔
         tl.srt = 45)             # 변수명 45도 기울임



#####################################################################################
# batplot

sel <- gu_road %>% select(c(자치구, 구간합계, 길이합계, 대여건수))

# 한번에 나타내기 위해서 long 형으로 변환

sel$대여건수 <- round(sel$대여건수/1000,0)
library(reshape2)
sel_melt <- melt(sel, 
     id.vars = '자치구',
     measure.vars = c('구간합계','길이합계','대여건수'))


ggplot(data=sel_melt,aes(x=자치구,group=variable, fill=variable)) + 
  geom_bar(aes(y=value), stat='identity', position='dodge') +
  ylab("") +
  theme(legend.title=element_blank()) +
  theme_bw() + labs(title = "1월 자치구별 자전거도로와 대여건수") +
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5))


#####################################################################################










