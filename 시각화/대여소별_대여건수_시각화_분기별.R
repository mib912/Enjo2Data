# 시각화
library(ggmap)
library(dplyr)

# 월별대여이력, 대여소 정보 데이터 불러오기
rent_cnt <- read.csv('./pre_data/공공자전거_대여소별_월별_대여건수.csv', header=T)
station1 <- read.csv('./pre_data/공공자전거대여소_21년1월기준.csv', header=T)
station2 <- read.csv('./pre_data/공공자전거대여소_21년12월기준.csv', header=T)

# 분기별로 분리
q1 <- rent_cnt %>% filter(month==10|month==11|month==12) %>% group_by(대여소번호) %>% summarise(분기별_대여건수=sum(대여건수)) 

# 대여건수와 대여소정보 join
rent_map <- left_join(q1,station1,by='대여소번호')

# 대여소의 위도 경도 정보가 없는 데이터만 추출
out <- rent_map %>% filter(is.na(lon)) %>% select(c(대여소번호, 분기별_대여건수))

# station2의 정보와 join하여 채워주기
rent_map2 <- left_join(out,station2,by='대여소번호')

# rent_map에서 위도 경도가 있는 것만 불러오기
rent_map1 <- rent_map %>% filter(!(is.na(lon)))

rent_info <- rbind(rent_map1, rent_map2)
rent_info <- rent_info %>% filter(!(is.na(lon))) # 위도경도정보있는것만 사용

rent_info <- rent_info[,-3] # 불필요한 열 제거

# 데이터 저장
write.csv(rent_info,"./pre_data/공공자전거_4분기(대여소별대여건수,위도경도).csv")
# =========================================================================
# 지도 시각화
library(ggmap)
library("ggplot2")
library("RColorBrewer")

# 불러오기
rent_map <- read.csv('./pre_data/공공자전거_4분기(대여소별대여건수,위도경도).csv', header=T)

# 1.  register_google(key=발급받은api키) 
register_google(key='AIzaSyCI68-2aSaiMwvQyZByTWigayUFrJZ6SW8')


# 팔레트 컬러
myPalette <- colorRampPalette(brewer.pal(11, "YlOrRd"))


# 2. 구글 지도의 위치(좌표) 정보 얻어오기
seoul_map <- get_map("seoul", zoom=11, , maptype="toner", source="stamen")
ggmap(seoul_map, extent = "device") + 
  geom_point(data=rent_map, aes(x=lon,y=lat, colour=분기별_대여건수, size=분기별_대여건수),alpha=0.8) +
  scale_colour_gradientn(colours = myPalette(100), limits=c(1, 50000))+ 
  ggtitle("4분기 대여소별 대여건수") +
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"))

