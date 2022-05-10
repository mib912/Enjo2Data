setwd("플젝")

getwd()

rent <- read.csv('대여소별전처리_rent_place.csv')

rent[is.na(rent)] <- 0
write.csv(rent, '대여소별전처리_rent_place1.csv',fileEncoding = 'cp949')


return<- read.csv('대여소별전처리_return_place.csv')

return[is.na(return)] <- 0
write.csv(return, '대여소별전처리_return_place1.csv',fileEncoding = 'cp949')



####################################
# ggmap
library(ggmap)
# 구글 맵 사용 방법
# 발급 받은 GOOGLE API 키 등록
# 1. register_google(key=발급받은 API 키) -> 내 컴퓨터의 register라는 저장소에 등록
register_google(key='AIzaSyAPdXEniXjxUXFsBGm_wq2JxKl-95TyH0g')

# 메모장에 붙인거
# AIzaSyDrj02-aXNpOevc9TRfNDE8qyyG2WTegYU

# 2. 구글 지도의 위치(좌표)정보 얻어오기
# 변수 <- get_googlemap('대표지역명',maptype=)
# 위 좌표 정보를 이용해서 지도를 그리기 때문에 변수에 저장
ggseoul <- get_googlemap('seoul',maptype='terrain')

ggseoul <- get_googlemap('seoul',maptype='roadmap')

ggseoul <- get_googlemap('seoul',maptype='hybrid')

# 3. 지도 호출(지도 그리기)
# ggmap(좌표정보)
ggmap(ggseoul)

#################################################
# geocode() 함수
??geocode
# 특정지역의 위경도 데이터를 요청해서 반환하는 함수 

geocode('seoul')

#############
# 대여소 위치 표시 

# 위경도 데이터 읽어오기
rent_map <- read.csv('대여소별전처리_rent_place2.csv', header=T)
View(head(rent_map))

# 구글키 등록
register_google(key='AIzaSyAPdXEniXjxUXFsBGm_wq2JxKl-95TyH0g')



# 특정 위치 기준으로 구글 지도 정보 얻어오기
# 서울기준
seoul <- get_googlemap('서울', 
                          zoom=13,
                          maptype='roadmap')

seoul
seo.map <- ggmap(seoul)

seo.map

# 지도위에 대여소를 point로 표시
#seo.map.point<-seo.map + geom_point(data=rent_map, 
                                      #aes(x=대여대여소번호, y=Total),
                                      #size=1,
                                      #color='red')
#seo.map.point





seoul_map <- get_map("seoul", zoom=11, maptype="roadmap")

ggmap(seoul_map) + 
  geom_point(data=rent_map, aes(x=위도,y=경도,size=Total), 
             alpha=0.3)

library(dplyr)
typeof(rent_map$위도)
rent_map$위도<-as.numeric(rent_map$위도)
rent_map$경도<-as.numeric(rent_map$경도)
rent_map1<- rename(rent_map, lat=위도, lon=경도)

seoul_map <- get_map("seoul", zoom=11, maptype="roadmap")

ggmap(seoul_map) + 
  geom_point(data=rent_map1, aes(x=lon,y=lat,size=Total), 
             alpha=0.3)

ggmap(seoul_map) + 
  geom_point(data=rent_map1, aes(x=lon,y=lat), 
             alpha=0.3, color='navy')

# return_map <- read.csv('대여소별전처리_return_place2.csv')





