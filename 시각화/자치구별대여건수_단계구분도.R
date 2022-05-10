setwd("플젝")
getwd()


library(dplyr)



rent_map <- read.csv('대여소별전처리_rent_place2.csv')
return_map <- read.csv('대여소별전처리_return_place2.csv')
seoul_id <- read.csv('seoul_id.csv', header=T)

seoul_id <- rename(seoul_id,자치구=시군구명)


# 자치구별 대여 건수 구하기
gu_cnt <- rent_map %>% select(c(X1월,X2월,X3월,X4월,X5월,X6월,X7월,X8월,X9월,X10월,X11월,X12월,Total, 자치구)) %>% 
  group_by(자치구) %>% 
  summarise(Total=sum(Total))

# 자치구별 반납 건수 구하기
gu_cnt_r <- return_map %>% dplyr::select(c(X1월,X2월,X3월,X4월,X5월,X6월,X7월,X8월,X9월,X10월,X11월,X12월,Total, 자치구)) %>% 
  group_by(자치구) %>% 
  summarise(Total=sum(Total))

seoul_gu_cnt_r <- left_join(gu_cnt_r,seoul_id,id='자치구')

library(ggplot2)
# install.packages('maps')
library(maps)


# 패키지

library(ggmap)


# gu_map <- read.csv('대여소별전처리_rent_place2.csv')


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
library(sf)
library(sp)

map_korea <- readOGR("./SIG_202101/TL_SCCO_SIG.shp")#지리 정보 데이터셋

map_info <- map_korea@data
head(map_info)

map <- spTransform(map_korea, CRSobj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))


new_map <- fortify(map, region = 'SIG_CD')
View(new_map)

new_map$id <- as.numeric(new_map$id)
seoul_map <- new_map[new_map$id <= 11740,]

gu <- left_join(seoul_map,seoul_gu_cnt,id='id')
gu_r <- left_join(seoul_map,seoul_gu_cnt_r,id='id')

typeof(rent_map$위도)
rent_map$위도<-as.numeric(rent_map$위도)
rent_map$경도<-as.numeric(rent_map$경도)
rent_map1<- rename(rent_map, lat=위도, lon=경도)


typeof(return_map$위도)
return_map$위도<-as.numeric(return_map$위도)
return_map$경도<-as.numeric(return_map$경도)
return_map1<- rename(return_map, lat=위도, lon=경도)

seoul_district <- read.csv('서울시_자치구_중심점.csv', header=T)

# 2021년 전체 대여건수
ggplot() + 
  geom_polygon(data = gu, aes(x=long, y=lat, group=group, fill=Total))+ 
  geom_text(data=seoul_district, aes(x=X, y=Y, label=paste(시군구명)))+ 
  scale_fill_gradient(low = "#ffe5e5", high = "#ff3232", space = "Lab", guide = "colourbar") +
  theme_bw() + labs(title = "2021년 자치구별 대여건수") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5))

# 2021년 전체 반납건수
ggplot() + 
  geom_polygon(data = gu_r, aes(x=long, y=lat, group=group, fill=Total))+ 
  geom_text(data=seoul_district, aes(x=X, y=Y, label=paste(시군구명)))+ 
  scale_fill_gradient(low = "#ccccff", high = "#0000ff", space = "Lab", guide = "colourbar") +
  theme_bw() + labs(title = "2021년 자치구별 반납건수") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5))




#dplyr::

# 1월 대여건수  

rent_map <- read.csv('대여소별전처리_rent_place2.csv')
seoul_id <- read.csv('seoul_id.csv', header=T)

seoul_id <- rename(seoul_id,자치구=시군구명)

gu_cnt1 <- rent_map %>% dplyr::select(c(X1월,자치구)) %>% 
  group_by(자치구) %>% 
  summarise(X1월=sum(X1월))

seoul_gu_cnt1 <- left_join(gu_cnt1,seoul_id,id='자치구')
gu1 <- left_join(seoul_map,seoul_gu_cnt1,id='id')


ggplot() + 
  geom_polygon(data = gu1, aes(x=long, y=lat, group=group, fill=X1월))+ 
  geom_text(data=seoul_district, aes(x=X, y=Y, label=paste(시군구명)))+ 
  scale_fill_gradient(low = "#ffe5e5", high = "#ff3232", space = "Lab", guide = "colourbar") +
  theme_bw() + labs(title = "2021년 1월 자치구별 대여건수") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5))


# 2월 대여건수  

rent_map <- read.csv('대여소별전처리_rent_place2.csv')
seoul_id <- read.csv('seoul_id.csv', header=T)

seoul_id <- rename(seoul_id,자치구=시군구명)

gu_cnt2 <- rent_map %>% dplyr::select(c(X2월,자치구)) %>% 
  group_by(자치구) %>% 
  summarise(X2월=sum(X2월))

seoul_gu_cnt2 <- left_join(gu_cnt2,seoul_id,id='자치구')
gu2 <- left_join(seoul_map,seoul_gu_cnt2,id='id')


ggplot() + 
  geom_polygon(data = gu2, aes(x=long, y=lat, group=group, fill=X2월))+ 
  geom_text(data=seoul_district, aes(x=X, y=Y, label=paste(시군구명)))+ 
  scale_fill_gradient(low = "#ffe5e5", high = "#ff3232", space = "Lab", guide = "colourbar") +
  theme_bw() + labs(title = "2021년 2월 자치구별 대여건수") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5))

# 3월 대여건수  

rent_map <- read.csv('대여소별전처리_rent_place2.csv')
seoul_id <- read.csv('seoul_id.csv', header=T)

seoul_id <- rename(seoul_id,자치구=시군구명)

gu_cnt3 <- rent_map %>% dplyr::select(c(X3월,자치구)) %>% 
  group_by(자치구) %>% 
  summarise(X3월=sum(X3월))

seoul_gu_cnt3 <- left_join(gu_cnt3,seoul_id,id='자치구')
gu3 <- left_join(seoul_map,seoul_gu_cnt3,id='id')


ggplot() + 
  geom_polygon(data = gu3, aes(x=long, y=lat, group=group, fill=X3월))+ 
  geom_text(data=seoul_district, aes(x=X, y=Y, label=paste(시군구명)))+ 
  scale_fill_gradient(low = "#ffe5e5", high = "#ff3232", space = "Lab", guide = "colourbar") +
  theme_bw() + labs(title = "2021년 3월 자치구별 대여건수") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5))

# 4월 대여건수  

rent_map <- read.csv('대여소별전처리_rent_place2.csv')
seoul_id <- read.csv('seoul_id.csv', header=T)

seoul_id <- rename(seoul_id,자치구=시군구명)

gu_cnt4 <- rent_map %>% dplyr::select(c(X4월,자치구)) %>% 
  group_by(자치구) %>% 
  summarise(X4월=sum(X4월))

seoul_gu_cnt4 <- left_join(gu_cnt4,seoul_id,id='자치구')
gu4 <- left_join(seoul_map,seoul_gu_cnt4,id='id')


ggplot() + 
  geom_polygon(data = gu4, aes(x=long, y=lat, group=group, fill=X4월))+ 
  geom_text(data=seoul_district, aes(x=X, y=Y, label=paste(시군구명)))+ 
  scale_fill_gradient(low = "#ffe5e5", high = "#ff3232", space = "Lab", guide = "colourbar") +
  theme_bw() + labs(title = "2021년 4월 자치구별 대여건수") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5))


# 5월 대여건수  

rent_map <- read.csv('대여소별전처리_rent_place2.csv')
seoul_id <- read.csv('seoul_id.csv', header=T)

seoul_id <- rename(seoul_id,자치구=시군구명)

gu_cnt5 <- rent_map %>% dplyr::select(c(X5월,자치구)) %>% 
  group_by(자치구) %>% 
  summarise(X5월=sum(X5월))

seoul_gu_cnt5 <- left_join(gu_cnt5,seoul_id,id='자치구')
gu5 <- left_join(seoul_map,seoul_gu_cnt5,id='id')


ggplot() + 
  geom_polygon(data = gu5, aes(x=long, y=lat, group=group, fill=X5월))+ 
  geom_text(data=seoul_district, aes(x=X, y=Y, label=paste(시군구명)))+ 
  scale_fill_gradient(low = "#ffe5e5", high = "#ff3232", space = "Lab", guide = "colourbar") +
  theme_bw() + labs(title = "2021년 5월 자치구별 대여건수") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5))


# 6월 대여건수  

rent_map <- read.csv('대여소별전처리_rent_place2.csv')
seoul_id <- read.csv('seoul_id.csv', header=T)

seoul_id <- rename(seoul_id,자치구=시군구명)

gu_cnt6 <- rent_map %>% dplyr::select(c(X6월,자치구)) %>% 
  group_by(자치구) %>% 
  summarise(X6월=sum(X6월))

seoul_gu_cnt6 <- left_join(gu_cnt6,seoul_id,id='자치구')
gu6 <- left_join(seoul_map,seoul_gu_cnt6,id='id')


ggplot() + 
  geom_polygon(data = gu6, aes(x=long, y=lat, group=group, fill=X6월))+ 
  geom_text(data=seoul_district, aes(x=X, y=Y, label=paste(시군구명)))+ 
  scale_fill_gradient(low = "#ffe5e5", high = "#ff3232", space = "Lab", guide = "colourbar") +
  theme_bw() + labs(title = "2021년 6월 자치구별 대여건수") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5))


# 7월 대여건수  

rent_map <- read.csv('대여소별전처리_rent_place2.csv')
seoul_id <- read.csv('seoul_id.csv', header=T)

seoul_id <- rename(seoul_id,자치구=시군구명)

gu_cnt7 <- rent_map %>% dplyr::select(c(X7월,자치구)) %>% 
  group_by(자치구) %>% 
  summarise(X7월=sum(X7월))

seoul_gu_cnt7 <- left_join(gu_cnt7,seoul_id,id='자치구')
gu7 <- left_join(seoul_map,seoul_gu_cnt7,id='id')


ggplot() + 
  geom_polygon(data = gu7, aes(x=long, y=lat, group=group, fill=X7월))+ 
  geom_text(data=seoul_district, aes(x=X, y=Y, label=paste(시군구명)))+ 
  scale_fill_gradient(low = "#ffe5e5", high = "#ff3232", space = "Lab", guide = "colourbar") +
  theme_bw() + labs(title = "2021년 7월 자치구별 대여건수") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5))

# 8월 대여건수  

rent_map <- read.csv('대여소별전처리_rent_place2.csv')
seoul_id <- read.csv('seoul_id.csv', header=T)

seoul_id <- rename(seoul_id,자치구=시군구명)

gu_cnt8 <- rent_map %>% dplyr::select(c(X8월,자치구)) %>% 
  group_by(자치구) %>% 
  summarise(X8월=sum(X8월))

seoul_gu_cnt8 <- left_join(gu_cnt8,seoul_id,id='자치구')
gu8 <- left_join(seoul_map,seoul_gu_cnt8,id='id')


ggplot() + 
  geom_polygon(data = gu8, aes(x=long, y=lat, group=group, fill=X8월))+ 
  geom_text(data=seoul_district, aes(x=X, y=Y, label=paste(시군구명)))+ 
  scale_fill_gradient(low = "#ffe5e5", high = "#ff3232", space = "Lab", guide = "colourbar") +
  theme_bw() + labs(title = "2021년 8월 자치구별 대여건수") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5))

# 9월 대여건수  

rent_map <- read.csv('대여소별전처리_rent_place2.csv')
seoul_id <- read.csv('seoul_id.csv', header=T)


seoul_id <- rename(seoul_id,자치구=시군구명)

gu_cnt9 <- rent_map %>% dplyr::select(c(X9월,자치구)) %>% 
  group_by(자치구) %>% 
  summarise(X9월=sum(X9월))

seoul_gu_cnt9 <- left_join(gu_cnt9,seoul_id,id='자치구')
gu9 <- left_join(seoul_map,seoul_gu_cnt9,id='id')


ggplot() + 
  geom_polygon(data = gu9, aes(x=long, y=lat, group=group, fill=X9월))+ 
  geom_text(data=seoul_district, aes(x=X, y=Y, label=paste(시군구명)))+ 
  scale_fill_gradient(low = "#ffe5e5", high = "#ff3232", space = "Lab", guide = "colourbar") +
  theme_bw() + labs(title = "2021년 9월 자치구별 대여건수") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5))


# 10월 대여건수  

rent_map <- read.csv('대여소별전처리_rent_place2.csv')
seoul_id <- read.csv('seoul_id.csv', header=T)

seoul_id <- rename(seoul_id,자치구=시군구명)

gu_cnt10 <- rent_map %>% dplyr::select(c(X10월,자치구)) %>% 
  group_by(자치구) %>% 
  summarise(X10월=sum(X10월))

seoul_gu_cnt10 <- left_join(gu_cnt10,seoul_id,id='자치구')
gu10 <- left_join(seoul_map,seoul_gu_cnt10,id='id')


ggplot() + 
  geom_polygon(data = gu10, aes(x=long, y=lat, group=group, fill=X10월))+ 
  geom_text(data=seoul_district, aes(x=X, y=Y, label=paste(시군구명)))+ 
  scale_fill_gradient(low = "#ffe5e5", high = "#ff3232", space = "Lab", guide = "colourbar") +
  theme_bw() + labs(title = "2021년 10월 자치구별 대여건수") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5))

# 11월 대여건수  

rent_map <- read.csv('대여소별전처리_rent_place2.csv')
seoul_id <- read.csv('seoul_id.csv', header=T)

seoul_id <- rename(seoul_id,자치구=시군구명)

gu_cnt11 <- rent_map %>% dplyr::select(c(X11월,자치구)) %>% 
  group_by(자치구) %>% 
  summarise(X11월=sum(X11월))

seoul_gu_cnt11 <- left_join(gu_cnt11,seoul_id,id='자치구')
gu11 <- left_join(seoul_map,seoul_gu_cnt11,id='id')


ggplot() + 
  geom_polygon(data = gu11, aes(x=long, y=lat, group=group, fill=X11월))+ 
  geom_text(data=seoul_district, aes(x=X, y=Y, label=paste(시군구명)))+ 
  scale_fill_gradient(low = "#ffe5e5", high = "#ff3232", space = "Lab", guide = "colourbar") +
  theme_bw() + labs(title = "2021년 11월 자치구별 대여건수") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5))


# 12월 대여건수  

rent_map <- read.csv('대여소별전처리_rent_place2.csv')
seoul_id <- read.csv('seoul_id.csv', header=T)


seoul_id <- rename(seoul_id,자치구=시군구명)

gu_cnt12 <- rent_map %>% dplyr::select(c(X12월,자치구)) %>% 
  group_by(자치구) %>% 
  summarise(X12월=sum(X12월))

seoul_gu_cnt12 <- left_join(gu_cnt12,seoul_id,id='자치구')
gu12 <- left_join(seoul_map,seoul_gu_cnt12,id='id')


ggplot() + 
  geom_polygon(data = gu12, aes(x=long, y=lat, group=group, fill=X12월))+ 
  geom_text(data=seoul_district, aes(x=X, y=Y, label=paste(시군구명)))+ 
  scale_fill_gradient(low = "#ffe5e5", high = "#ff3232", space = "Lab", guide = "colourbar") +
  theme_bw() + labs(title = "2021년 12월 자치구별 대여건수") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5))

# 1월 반납건수  

return_map <- read.csv('대여소별전처리_return_place2.csv')
seoul_id <- read.csv('seoul_id.csv', header=T)

seoul_id <- rename(seoul_id,자치구=시군구명)


gu_cnt1r <- return_map %>% dplyr::select(c(X1월,자치구)) %>% 
  group_by(자치구) %>% 
  summarise(X1월=sum(X1월))

seoul_gu_cnt1r <- left_join(gu_cnt1r,seoul_id,id='자치구')
gu1r <- left_join(seoul_map,seoul_gu_cnt1r,id='id')


ggplot() + 
  geom_polygon(data = gu1r, aes(x=long, y=lat, group=group, fill=X1월))+ 
  geom_text(data=seoul_district, aes(x=X, y=Y, label=paste(시군구명)))+ 
  scale_fill_gradient(low = "#ccccff", high = "#0000ff", space = "Lab", guide = "colourbar") +
  theme_bw() + labs(title = "2021년 1월 자치구별 반납건수") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5))


# 2월 반납건수  

return_map <- read.csv('대여소별전처리_return_place2.csv')
seoul_id <- read.csv('seoul_id.csv', header=T)

seoul_id <- rename(seoul_id,자치구=시군구명)


gu_cnt2r <- return_map %>% dplyr::select(c(X2월,자치구)) %>% 
  group_by(자치구) %>% 
  summarise(X2월=sum(X2월))

seoul_gu_cnt2r <- left_join(gu_cnt2r,seoul_id,id='자치구')
gu2r <- left_join(seoul_map,seoul_gu_cnt2r,id='id')


ggplot() + 
  geom_polygon(data = gu2r, aes(x=long, y=lat, group=group, fill=X2월))+ 
  geom_text(data=seoul_district, aes(x=X, y=Y, label=paste(시군구명)))+ 
  scale_fill_gradient(low = "#ccccff", high = "#0000ff", space = "Lab", guide = "colourbar") +
  theme_bw() + labs(title = "2021년 2월 자치구별 반납건수") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5))



# 3월 반납건수  

return_map <- read.csv('대여소별전처리_return_place2.csv')
seoul_id <- read.csv('seoul_id.csv', header=T)


seoul_id <- rename(seoul_id,자치구=시군구명)


gu_cnt3r <- return_map %>% dplyr::select(c(X3월,자치구)) %>% 
  group_by(자치구) %>% 
  summarise(X3월=sum(X3월))

seoul_gu_cnt3r <- left_join(gu_cnt3r,seoul_id,id='자치구')
gu3r <- left_join(seoul_map,seoul_gu_cnt3r,id='id')


ggplot() + 
  geom_polygon(data = gu3r, aes(x=long, y=lat, group=group, fill=X3월))+ 
  geom_text(data=seoul_district, aes(x=X, y=Y, label=paste(시군구명)))+ 
  scale_fill_gradient(low = "#ccccff", high = "#0000ff", space = "Lab", guide = "colourbar") +
  theme_bw() + labs(title = "2021년 3월 자치구별 반납건수") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5))



# 4월 반납건수  

return_map <- read.csv('대여소별전처리_return_place2.csv')
seoul_id <- read.csv('seoul_id.csv', header=T)


seoul_id <- rename(seoul_id,자치구=시군구명)


gu_cnt4r <- return_map %>% dplyr::select(c(X4월,자치구)) %>% 
  group_by(자치구) %>% 
  summarise(X4월=sum(X4월))

seoul_gu_cnt4r <- left_join(gu_cnt4r,seoul_id,id='자치구')
gu4r <- left_join(seoul_map,seoul_gu_cnt4r,id='id')


ggplot() + 
  geom_polygon(data = gu4r, aes(x=long, y=lat, group=group, fill=X4월))+ 
  geom_text(data=seoul_district, aes(x=X, y=Y, label=paste(시군구명)))+ 
  scale_fill_gradient(low = "#ccccff", high = "#0000ff", space = "Lab", guide = "colourbar") +
  theme_bw() + labs(title = "2021년 4월 자치구별 반납건수") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5))


# 5월 반납건수  

return_map <- read.csv('대여소별전처리_return_place2.csv')
seoul_id <- read.csv('seoul_id.csv', header=T)


seoul_id <- rename(seoul_id,자치구=시군구명)


gu_cnt5r <- return_map %>% dplyr::select(c(X5월,자치구)) %>% 
  group_by(자치구) %>% 
  summarise(X5월=sum(X5월))

seoul_gu_cnt5r <- left_join(gu_cnt5r,seoul_id,id='자치구')
gu5r <- left_join(seoul_map,seoul_gu_cnt5r,id='id')


ggplot() + 
  geom_polygon(data = gu5r, aes(x=long, y=lat, group=group, fill=X5월))+ 
  geom_text(data=seoul_district, aes(x=X, y=Y, label=paste(시군구명)))+ 
  scale_fill_gradient(low = "#ccccff", high = "#0000ff", space = "Lab", guide = "colourbar") +
  theme_bw() + labs(title = "2021년 5월 자치구별 반납건수") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5))


# 6월 반납건수  

return_map <- read.csv('대여소별전처리_return_place2.csv')
seoul_id <- read.csv('seoul_id.csv', header=T)

seoul_id <- rename(seoul_id,자치구=시군구명)


gu_cnt6r <- return_map %>% dplyr::select(c(X6월,자치구)) %>% 
  group_by(자치구) %>% 
  summarise(X6월=sum(X6월))

seoul_gu_cnt6r <- left_join(gu_cnt6r,seoul_id,id='자치구')
gu6r <- left_join(seoul_map,seoul_gu_cnt6r,id='id')


ggplot() + 
  geom_polygon(data = gu6r, aes(x=long, y=lat, group=group, fill=X6월))+ 
  geom_text(data=seoul_district, aes(x=X, y=Y, label=paste(시군구명)))+ 
  scale_fill_gradient(low = "#ccccff", high = "#0000ff", space = "Lab", guide = "colourbar") +
  theme_bw() + labs(title = "2021년 6월 자치구별 반납건수") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5))


# 7월 반납건수  

return_map <- read.csv('대여소별전처리_return_place2.csv')
seoul_id <- read.csv('seoul_id.csv', header=T)

seoul_id <- rename(seoul_id,자치구=시군구명)


gu_cnt7r <- return_map %>% dplyr::select(c(X7월,자치구)) %>% 
  group_by(자치구) %>% 
  summarise(X7월=sum(X7월))

seoul_gu_cnt7r <- left_join(gu_cnt7r,seoul_id,id='자치구')
gu7r <- left_join(seoul_map,seoul_gu_cnt7r,id='id')


ggplot() + 
  geom_polygon(data = gu7r, aes(x=long, y=lat, group=group, fill=X7월))+ 
  geom_text(data=seoul_district, aes(x=X, y=Y, label=paste(시군구명)))+ 
  scale_fill_gradient(low = "#ccccff", high = "#0000ff", space = "Lab", guide = "colourbar") +
  theme_bw() + labs(title = "2021년 7월 자치구별 반납건수") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5))


# 8월 반납건수  

return_map <- read.csv('대여소별전처리_return_place2.csv')
seoul_id <- read.csv('seoul_id.csv', header=T)

seoul_id <- rename(seoul_id,자치구=시군구명)


gu_cnt8r <- return_map %>% dplyr::select(c(X8월,자치구)) %>% 
  group_by(자치구) %>% 
  summarise(X8월=sum(X8월))

seoul_gu_cnt8r <- left_join(gu_cnt8r,seoul_id,id='자치구')
gu8r <- left_join(seoul_map,seoul_gu_cnt8r,id='id')


ggplot() + 
  geom_polygon(data = gu8r, aes(x=long, y=lat, group=group, fill=X8월))+ 
  geom_text(data=seoul_district, aes(x=X, y=Y, label=paste(시군구명)))+ 
  scale_fill_gradient(low = "#ccccff", high = "#0000ff", space = "Lab", guide = "colourbar") +
  theme_bw() + labs(title = "2021년 8월 자치구별 반납건수") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5))


# 9월 반납건수  

return_map <- read.csv('대여소별전처리_return_place2.csv')
seoul_id <- read.csv('seoul_id.csv', header=T)

seoul_id <- rename(seoul_id,자치구=시군구명)


gu_cnt9r <- return_map %>% dplyr::select(c(X9월,자치구)) %>% 
  group_by(자치구) %>% 
  summarise(X9월=sum(X9월))

seoul_gu_cnt9r <- left_join(gu_cnt9r,seoul_id,id='자치구')
gu9r <- left_join(seoul_map,seoul_gu_cnt9r,id='id')


ggplot() + 
  geom_polygon(data = gu9r, aes(x=long, y=lat, group=group, fill=X9월))+ 
  geom_text(data=seoul_district, aes(x=X, y=Y, label=paste(시군구명)))+ 
  scale_fill_gradient(low = "#ccccff", high = "#0000ff", space = "Lab", guide = "colourbar") +
  theme_bw() + labs(title = "2021년 9월 자치구별 반납건수") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5))


# 10월 반납건수  

return_map <- read.csv('대여소별전처리_return_place2.csv')
seoul_id <- read.csv('seoul_id.csv', header=T)

seoul_id <- rename(seoul_id,자치구=시군구명)


gu_cnt10r <- return_map %>% dplyr::select(c(X10월,자치구)) %>% 
  group_by(자치구) %>% 
  summarise(X10월=sum(X10월))

seoul_gu_cnt10r <- left_join(gu_cnt10r,seoul_id,id='자치구')
gu10r <- left_join(seoul_map,seoul_gu_cnt10r,id='id')


ggplot() + 
  geom_polygon(data = gu10r, aes(x=long, y=lat, group=group, fill=X10월))+ 
  geom_text(data=seoul_district, aes(x=X, y=Y, label=paste(시군구명)))+ 
  scale_fill_gradient(low = "#ccccff", high = "#0000ff", space = "Lab", guide = "colourbar") +
  theme_bw() + labs(title = "2021년 10월 자치구별 반납건수") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5))


# 11월 반납건수  

return_map <- read.csv('대여소별전처리_return_place2.csv')
seoul_id <- read.csv('seoul_id.csv', header=T)

seoul_id <- rename(seoul_id,자치구=시군구명)


gu_cnt11r <- return_map %>% dplyr::select(c(X11월,자치구)) %>% 
  group_by(자치구) %>% 
  summarise(X11월=sum(X11월))

seoul_gu_cnt11r <- left_join(gu_cnt11r,seoul_id,id='자치구')
gu11r <- left_join(seoul_map,seoul_gu_cnt11r,id='id')


ggplot() + 
  geom_polygon(data = gu11r, aes(x=long, y=lat, group=group, fill=X11월))+ 
  geom_text(data=seoul_district, aes(x=X, y=Y, label=paste(시군구명)))+ 
  scale_fill_gradient(low = "#ccccff", high = "#0000ff", space = "Lab", guide = "colourbar") +
  theme_bw() + labs(title = "2021년 11월 자치구별 반납건수") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5))


# 12월 반납건수  

return_map <- read.csv('대여소별전처리_return_place2.csv')
seoul_id <- read.csv('seoul_id.csv', header=T)

seoul_id <- rename(seoul_id,자치구=시군구명)


gu_cnt12r <- return_map %>% dplyr::select(c(X12월,자치구)) %>% 
  group_by(자치구) %>% 
  summarise(X12월=sum(X12월))

seoul_gu_cnt12r <- left_join(gu_cnt12r,seoul_id,id='자치구')
gu12r <- left_join(seoul_map,seoul_gu_cnt12r,id='id')


ggplot() + 
  geom_polygon(data = gu12r, aes(x=long, y=lat, group=group, fill=X12월))+ 
  geom_text(data=seoul_district, aes(x=X, y=Y, label=paste(시군구명)))+ 
  scale_fill_gradient(low = "#ccccff", high = "#0000ff", space = "Lab", guide = "colourbar") +
  theme_bw() + labs(title = "2021년 12월 자치구별 반납건수") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5))












