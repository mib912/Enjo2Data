
library(dplyr)
library(readxl)
library(xlsx)

# =========================================================================
# 대여소 정보 데이터 
# 대여소 정보(위도 경도 포함)
# 2021년 1월 기준
rent <-  read.csv('./data/공공자전거 대여소 정보(21.01.31 기준).csv', header=T)

colnames(rent)
# [1] "대여소.번호"      "보관소.대여소.명" "소재지.위치."     "X"               
# [5] "X.1"              "X.2"              "설치.시기"        "설치형태"        
# [9] "X.3"              "운영.방식" 

colnames(rent) <- c("대여소번호","대여소명","자치구","상세주소","lat","lon" ,"설치시기","거치대수_LCD","거치대수_QR", "운영방식")

# 문자열인 위도 경도를 숫자로 바꿔줌
rent$lat <- as.numeric(rent$lat)
rent$lon <- as.numeric(rent$lon)

# 대여소번호가 NA이나 공백인 행 삭제
rent <- rent %>% filter(!is.na(대여소번호)) %>% filter(!대여소번호=="")

# 공백은 NA로 처리하여 결측치 통일시키기
rent[rent==""] <- NA

rent %>% group_by(대여소번호)%>% 
  summarise(번호별수=n())%>%arrange(desc(번호별수)) %>% head(5)
# 대여소번호 번호별수
#         <int>    <int>
# 1        583        2
# 2        101        1
# 3        102        1
# 4        103        1
# 5        104        1

# 583 대여소에 관한 정보가 하나 더 들어가 있으므로 이 이상치를 제거.
rent <- rent %>% filter(!(대여소번호==583 & is.na(lat) ))

# 위경도 데이터 불러오기
for(i in 1:length(rent$lat)) {
  if(is.na(rent$lat[i])) {  
    g <- geocode(rent$상세주소[i]) # 한개 위경도 얻어오기
    rent$lat[i] <- g$lat
    rent$lon[i] <- g$lon
  }
}

table(is.na(rent$lat)) # NA가 여전히 존재
# FALSE  TRUE 
# 2149     4 

rent[is.na(rent$lat),]$상세주소
# "초동 107-9"    "청파로47길 99" "불광로 41"     "수색로 270-2" 

# 직접 새주소 찾아서 기입
g <- geocode('중구 마른내로 27-1')
which(rent$대여소번호==4761)
rent[166,]$lat <- g$lat
rent[166,]$lon <- g$lon

g <- geocode('청파동 2가 144')
rent[which(rent$대여소번호==4602),]$lat <- g$lat
rent[which(rent$대여소번호==4602),]$lon <- g$lon

g <- geocode('은평구 불광로 41')
rent[which(rent$대여소번호==4653),]$lat <- g$lat
rent[which(rent$대여소번호==4653),]$lon <- g$lon

g <- geocode('은평구 불광로 41') # 위치가 건너편인데 주소상 동일
rent[which(rent$대여소번호==4654),]$lat <- g$lat
rent[which(rent$대여소번호==4654),]$lon <- g$lon



# rent데이터프레임 csv 로 저장.
write.csv(rent,"./pre_data/공공자전거대여소_21년1월기준.csv")
