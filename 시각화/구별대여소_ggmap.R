setwd("플젝")
getwd()

library(dplyr)
rent_map <- read.csv('대여소별전처리_rent_place2.csv')
return_map <- read.csv('대여소별전처리_return_place2.csv')


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
typeof(rent_map$위도)
rent_map$위도<-as.numeric(rent_map$위도)
rent_map$경도<-as.numeric(rent_map$경도)
rent_map1<- rename(rent_map, lat=위도, lon=경도)

##############################
# 강남구
gu_gn <- rent_map1 %>% 
  filter(자치구=='강남구') 

# 구글키 등록
register_google(key='AIzaSyAPdXEniXjxUXFsBGm_wq2JxKl-95TyH0g')

gangnamgu <- get_googlemap('강남구', 
                          zoom=13,
                          maptype='roadmap')
gangnamgu
ggmap(gangnamgu)

ggmap(gangnamgu) + 
  geom_point(data=gu_gn, aes(x=lon,y=lat,size=Total), 
             alpha=0.3, color='red')


# scale_fill_brewer(palette="Reds") 



##############################
# 강서구
gu_gs <- rent_map1 %>% 
  filter(자치구=='강서구') 

# 구글키 등록
register_google(key='AIzaSyAPdXEniXjxUXFsBGm_wq2JxKl-95TyH0g')

gangseogu <- get_googlemap('강서구', 
                           zoom=13,
                           maptype='roadmap')
gangseogu
ggmap(gangseogu)

ggmap(gangseogu) + 
  geom_point(data=gu_gs, aes(x=lon,y=lat,size=Total), 
             alpha=0.3, color='red')

##############################
#강남구,강동구,강북구,강서구,관악구,광진구,구로구,금천구,노원구,도봉구,동대문구,동작구,마포구,서대문구,서초구,성동구,성북구,송파구,양천구,영등포구,용산구,은평구,종로구,중구 ,중랑구
##############################
# 강서구
gu_gs <- rent_map1 %>% 
  filter(자치구=='강서구') 

# 구글키 등록
register_google(key='AIzaSyAPdXEniXjxUXFsBGm_wq2JxKl-95TyH0g')

gangseogu <- get_googlemap('강서구', 
                           zoom=13,
                           maptype='roadmap')
gangseogu
ggmap(gangseogu)

ggmap(gangseogu) + 
  geom_point(data=gu_gs, aes(x=lon,y=lat,size=Total), 
             alpha=0.3, color='red')

##############################



# 대여건수가 높은 상위 100개 위치 

rent_top <- rent_map1 %>% arrange(desc(Total)) 
rent_top100 <-head(rent_top,100)
View(rent_top100)
gc <- geocode(enc2utf8(rent_top100$보관소.대여소.명))

# 구글키 등록
register_google(key='AIzaSyAPdXEniXjxUXFsBGm_wq2JxKl-95TyH0g')

rent_top100_map <- get_googlemap('seoul', zoom=11,maptype='roadmap',markers = gc)
rent_top100_map
ggmap(rent_top100_map)

ggmap(rent_top100_map) + 
  geom_point(data=rent_top100, aes(x=lon,y=lat,size=Total), 
             alpha=0.3, color='red')
##############
##############################
# 대여건수가 높은 상위 50개 위치 

rent_top <- rent_map1 %>% arrange(desc(Total)) 
rent_top50 <-head(rent_top,50)
View(rent_top50)


# 구글키 등록
register_google(key='AIzaSyAPdXEniXjxUXFsBGm_wq2JxKl-95TyH0g')

rent_top50_map <- get_googlemap('seoul', zoom=11,maptype='roadmap')
rent_top50_map
ggmap(rent_top50_map)

ggmap(rent_top50_map) + 
  geom_point(data=rent_top50, aes(x=lon,y=lat,size=Total), 
             alpha=0.3, color='red')
##############
# 강동구
gu_gd <- rent_map1 %>% 
  filter(자치구=='강동구') 

# 구글키 등록
register_google(key='AIzaSyAPdXEniXjxUXFsBGm_wq2JxKl-95TyH0g')

gangdonggu <- get_googlemap('강동구', 
                           zoom=13,
                           maptype='roadmap')
gangdonggu
ggmap(gangdonggu)

ggmap(gangdonggu) + 
  geom_point(data=gu_gd, aes(x=lon,y=lat,size=Total), 
             alpha=0.3, color='red')
##############
# 강북구
gu_gb <- rent_map1 %>% 
  filter(자치구=='강북구') 

# 구글키 등록
register_google(key='AIzaSyAPdXEniXjxUXFsBGm_wq2JxKl-95TyH0g')

gangbukgu <- get_googlemap('강북구', 
                            zoom=13,
                            maptype='roadmap')
gangbukgu
ggmap(gangbukgu)

ggmap(gangbukgu) + 
  geom_point(data=gu_gb, aes(x=lon,y=lat,size=Total), 
             alpha=0.3, color='red')
##############
# 관악구
gu_ga <- rent_map1 %>% 
  filter(자치구=='관악구') 

# 구글키 등록
register_google(key='AIzaSyAPdXEniXjxUXFsBGm_wq2JxKl-95TyH0g')

gwanakgu <- get_googlemap('관악구', 
                            zoom=13,
                            maptype='roadmap')
gwanakgu
ggmap(gwanakgu)

ggmap(gwanakgu) + 
  geom_point(data=gu_ga, aes(x=lon,y=lat,size=Total), 
             alpha=0.3, color='red')


##############
# 광진구
gu_gj <- rent_map1 %>% 
  filter(자치구=='광진구') 

# 구글키 등록
register_google(key='AIzaSyAPdXEniXjxUXFsBGm_wq2JxKl-95TyH0g')

gangjingu <- get_googlemap('광진구', 
                            zoom=13,
                            maptype='roadmap')
gangjingu
ggmap(gangjingu)

ggmap(gangjingu) + 
  geom_point(data=gu_gj, aes(x=lon,y=lat,size=Total), 
             alpha=0.3, color='red')


##############
# 구로구
gu_gr <- rent_map1 %>% 
  filter(자치구=='구로구') 

# 구글키 등록
register_google(key='AIzaSyAPdXEniXjxUXFsBGm_wq2JxKl-95TyH0g')

gurogu <- get_googlemap('구로구', 
                            zoom=13,
                            maptype='roadmap')
gurogu
ggmap(ggurogu)

ggmap(gurogu) + 
  geom_point(data=gu_gr, aes(x=lon,y=lat,size=Total), 
             alpha=0.3, color='red')


##############
# 금천구
gu_gc <- rent_map1 %>% 
  filter(자치구=='금천구') 

# 구글키 등록
register_google(key='AIzaSyAPdXEniXjxUXFsBGm_wq2JxKl-95TyH0g')

geumchungu <- get_googlemap('금천구', 
                            zoom=13,
                            maptype='roadmap')
geumchungu
ggmap(geumchungu)

ggmap(geumchungu) + 
  geom_point(data=gu_gc, aes(x=lon,y=lat,size=Total), 
             alpha=0.3, color='red')


##############
# 노원구
gu_nw <- rent_map1 %>% 
  filter(자치구=='노원구') 

# 구글키 등록
register_google(key='AIzaSyAPdXEniXjxUXFsBGm_wq2JxKl-95TyH0g')

nowongu <- get_googlemap('노원구', 
                            zoom=13,
                            maptype='roadmap')
nowongu
ggmap(nowongu)

ggmap(nowongu) + 
  geom_point(data=gu_nw, aes(x=lon,y=lat,size=Total), 
             alpha=0.3, color='red')

##############
# 도봉구
gu_db <- rent_map1 %>% 
  filter(자치구=='도봉구') 

# 구글키 등록
register_google(key='AIzaSyAPdXEniXjxUXFsBGm_wq2JxKl-95TyH0g')

dobonggu <- get_googlemap('도봉구', 
                            zoom=13,
                            maptype='roadmap')
dobonggu
ggmap(dobonggu)

ggmap(dobonggu) + 
  geom_point(data=gu_db, aes(x=lon,y=lat,size=Total), 
             alpha=0.3, color='red')


##############
# 동대문구
gu_ddm <- rent_map1 %>% 
  filter(자치구=='동대문구') 

# 구글키 등록
register_google(key='AIzaSyAPdXEniXjxUXFsBGm_wq2JxKl-95TyH0g')

ddmgu <- get_googlemap('동대문구', 
                            zoom=13,
                            maptype='roadmap')
ddmgu
ggmap(ddmgu)

ggmap(ddmgu) + 
  geom_point(data=gu_ddm, aes(x=lon,y=lat,size=Total), 
             alpha=0.3, color='red')



##############
# 동작구
gu_djg <- rent_map1 %>% 
  filter(자치구=='동작구') 

# 구글키 등록
register_google(key='AIzaSyAPdXEniXjxUXFsBGm_wq2JxKl-95TyH0g')

dongjakgu <- get_googlemap('동작구', 
                            zoom=13,
                            maptype='roadmap')
dongjakgu
ggmap(dongjakgu)

ggmap(dongjakgu) + 
  geom_point(data=gu_djg, aes(x=lon,y=lat,size=Total), 
             alpha=0.3, color='red')



##############
# 마포구
gu_mp <- rent_map1 %>% 
  filter(자치구=='마포구') 

# 구글키 등록
register_google(key='AIzaSyAPdXEniXjxUXFsBGm_wq2JxKl-95TyH0g')

mapogu <- get_googlemap('마포구', 
                            zoom=13,
                            maptype='roadmap')
mapogu
ggmap(mapogu)

ggmap(mapogu) + 
  geom_point(data=gu_mp, aes(x=lon,y=lat,size=Total), 
             alpha=0.3, color='red')



##############
# 서대문구
gu_sd <- rent_map1 %>% 
  filter(자치구=='서대문구') 

# 구글키 등록
register_google(key='AIzaSyAPdXEniXjxUXFsBGm_wq2JxKl-95TyH0g')

sdmgu <- get_googlemap('서대문구', 
                            zoom=13,
                            maptype='roadmap')
sdmgu
ggmap(sdmgu)

ggmap(sdmgu) + 
  geom_point(data=gu_sd, aes(x=lon,y=lat,size=Total), 
             alpha=0.3, color='red')



##############
# 서초구
gu_sc <- rent_map1 %>% 
  filter(자치구=='서초구') 

# 구글키 등록
register_google(key='AIzaSyAPdXEniXjxUXFsBGm_wq2JxKl-95TyH0g')

seochogu <- get_googlemap('서초구', 
                            zoom=13,
                            maptype='roadmap')
seochogu 
ggmap(seochogu)

ggmap(seochogu) + 
  geom_point(data=gu_sc, aes(x=lon,y=lat,size=Total), 
             alpha=0.3, color='red')



##############
# 성동구
gu_sdk <- rent_map1 %>% 
  filter(자치구=='성동구') 

# 구글키 등록
register_google(key='AIzaSyAPdXEniXjxUXFsBGm_wq2JxKl-95TyH0g')

sungdonggu <- get_googlemap('성동구', 
                            zoom=13,
                            maptype='roadmap')
sungdonggu
ggmap(sungdonggu)

ggmap(sungdonggu) + 
  geom_point(data=gu_sdk, aes(x=lon,y=lat,size=Total), 
             alpha=0.3, color='red')



##############
# 성북구
gu_sbk <- rent_map1 %>% 
  filter(자치구=='성북구') 

# 구글키 등록
register_google(key='AIzaSyAPdXEniXjxUXFsBGm_wq2JxKl-95TyH0g')

sungbukgu <- get_googlemap('성북구', 
                            zoom=13,
                            maptype='roadmap')
sungbukgu
ggmap(sungbukgu)

ggmap(sungbukgu) + 
  geom_point(data=gu_sbk, aes(x=lon,y=lat,size=Total), 
             alpha=0.3, color='red')



##############
# 송파구
gu_sp <- rent_map1 %>% 
  filter(자치구=='송파구') 

# 구글키 등록
register_google(key='AIzaSyAPdXEniXjxUXFsBGm_wq2JxKl-95TyH0g')

songpagu <- get_googlemap('송파구', 
                            zoom=13,
                            maptype='roadmap')
songpagu
ggmap(songpagu)

ggmap(songpagu) + 
  geom_point(data=gu_sp, aes(x=lon,y=lat,size=Total), 
             alpha=0.3, color='red')




##############
# 양천구
gu_yc <- rent_map1 %>% 
  filter(자치구=='양천구') 

# 구글키 등록
register_google(key='AIzaSyAPdXEniXjxUXFsBGm_wq2JxKl-95TyH0g')

ycgu <- get_googlemap('양천구', 
                            zoom=13,
                            maptype='roadmap')
ycgu
ggmap(ycgu)

ggmap(ycgu) + 
  geom_point(data=gu_yc, aes(x=lon,y=lat,size=Total), 
             alpha=0.3, color='red')



##############
# 영등포구
gu_ydp <- rent_map1 %>% 
  filter(자치구=='영등포구') 

# 구글키 등록
register_google(key='AIzaSyAPdXEniXjxUXFsBGm_wq2JxKl-95TyH0g')

ydpgu <- get_googlemap('영등포구', 
                            zoom=13,
                            maptype='roadmap')
ydpgu
ggmap(ydpgu)

ggmap(ydpgu) + 
  geom_point(data=gu_ydp, aes(x=lon,y=lat,size=Total), 
             alpha=0.3, color='red')



##############
# 용산구
gu_ysk <- rent_map1 %>% 
  filter(자치구=='용산구') 

# 구글키 등록
register_google(key='AIzaSyAPdXEniXjxUXFsBGm_wq2JxKl-95TyH0g')

ysgu <- get_googlemap('용산구', 
                            zoom=13,
                            maptype='roadmap')
ysgu
ggmap(ysgu)

ggmap(ysgu) + 
  geom_point(data=gu_ysk, aes(x=lon,y=lat,size=Total), 
             alpha=0.3, color='red')




##############
# 은평구
gu_ep <- rent_map1 %>% 
  filter(자치구=='은평구') 

# 구글키 등록
register_google(key='AIzaSyAPdXEniXjxUXFsBGm_wq2JxKl-95TyH0g')

epgu <- get_googlemap('은평구', 
                            zoom=13,
                            maptype='roadmap')
epgu
ggmap(epgu)

ggmap(epgu) + 
  geom_point(data=gu_ep, aes(x=lon,y=lat,size=Total), 
             alpha=0.3, color='red')



##############
# 종로구
gu_jr <- rent_map1 %>% 
  filter(자치구=='종로구') 

# 구글키 등록
register_google(key='AIzaSyAPdXEniXjxUXFsBGm_wq2JxKl-95TyH0g')

jrgu <- get_googlemap('종로구', 
                            zoom=14,
                            maptype='roadmap')
jrgu
ggmap(jrgu)

ggmap(jrgu) + 
  geom_point(data=gu_jr, aes(x=lon,y=lat,size=Total), 
             alpha=0.3, color='red')




##############
# 중구
gu_jk <- rent_map1 %>% 
  filter(자치구=='중구') 

# 구글키 등록
register_google(key='AIzaSyAPdXEniXjxUXFsBGm_wq2JxKl-95TyH0g')

junggu <- get_googlemap('중구, 서울', 
                            zoom=14,
                            maptype='roadmap')
junggu
ggmap(junggu)

ggmap(junggu) + 
  geom_point(data=gu_jk , aes(x=lon,y=lat,size=Total), 
             alpha=0.3, color='red')




##############
# 중랑구
gu_jrk <- rent_map1 %>% 
  filter(자치구=='중랑구') 

# 구글키 등록
register_google(key='AIzaSyAPdXEniXjxUXFsBGm_wq2JxKl-95TyH0g')

jrkgu <- get_googlemap('중랑구', 
                            zoom=14,
                            maptype='roadmap')
jrkgu
ggmap(jrkgu)

ggmap(jrkgu) + 
  geom_point(data=gu_jrk, aes(x=lon,y=lat,size=Total), 
             alpha=0.3, color='red')
