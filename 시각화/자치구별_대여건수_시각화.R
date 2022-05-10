## 자치구별 월별 대여건수

# 패키지
library(ggmap)
library(dplyr)


# 데이터 불러오기
rent_map <- read.csv('./pre_data/공공자전거_1월(대여소별대여건수,위도경도).csv', header=T)
seoul_id <- read.csv('./data/seoul_id.csv', header=T)

seoul_id <- rename(seoul_id,자치구=시군구명)

# 자치구별 대여 건수 구하기
gu_cnt <- rent_map %>% select(c(대여건수, 자치구)) %>% 
  group_by(자치구) %>% 
  summarise(대여건수=sum(대여건수))


seoul_gu_cnt <- left_join(gu_cnt,seoul_id,id='자치구')

# 
# 
# # 구별 단계구분도
# seoul_map <- read_excel('./data/서울_map.xlsx')
# names(seoul_map)
# head(seoul_map)
# 
# 
# 
# # map_id = code 는 양쪽 파일에 모두 있어야 함
# 
# # 서울시 각 자치구의 행정코드 DATA가 있어야 함
# # 행정코드DATA를 seoul_map에서 추출
# seoul_map_1 <- seoul_map %>% select(code,name)
# head(seoul_map_1)
# 
# # 행정코드와 자치구명이 중복되어서 여러번 나타나므로 자치구별 1개씩 추출
# seoul_code <- seoul_map_1[!duplicated(seoul_map_1$name),]
# 
# # 양쪽 파일에서 변수명 일치
# seoul_code <- rename(seoul_code,자치구=name)
# 
# # 조인
# gu_rent_gg <- left_join(gu_cnt,seoul_code,id='자치구')
# 
# # 단계도를 그릴 data : gu_rent_gg
# # 지도 data : seoul_map

# 자치구 위도경도 데이터

# raw <- read.table('./data/자치구_위도경도.txt', fileEncoding = 'utf-8', sep="\t", header=T)
# road <- raw
# 
# road <- road %>% select(c('시군구명','X','Y'))
# names(road) <- c('자치구','lon','lat')
# # 조인
# gu_rent_gg <- left_join(gu_rent_gg,road,id='code')


################################################

install.packages("raster")
install.packages("rgeos")
install.packages("maptools")
install.packages("rgdal")
install.packages('sf')
install.packages('sp')

library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(ggplot2)
library(sf)
library(sp)

map_korea <- readOGR("./data/SIG_202101/TL_SCCO_SIG.shp")#지리 정보 데이터셋

map_info <- map_korea@data
head(map_info)

map <- spTransform(map_korea, CRSobj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))

new_map <- fortify(map, region = 'SIG_CD')
View(new_map)
            
new_map$id <- as.numeric(new_map$id)
seoul_map <- new_map[new_map$id <= 11740,]


gu <- left_join(seoul_map,seoul_gu_cnt,id='id')


ggplot() + 
  geom_polygon(data = gu, aes(x=long, y=lat, group=group, fill=대여건수)) + 
  geom_text(data=gu_rent_gg, aes(x=lon, y=lat, label=paste(자치구))) + 
  scale_fill_gradient(low = "#ffe5e5", high = "#ff3232", space = "Lab", guide = "colourbar") +
  theme_bw() + labs(title = "1월 자치구별 대여건수") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5))
  



