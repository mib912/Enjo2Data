
library(dplyr)
library(readxl)
library(xlsx)

# =========================================================================
# 대여소 정보 데이터 
# 대여소 정보(위도 경도 포함)
# 2021년 12월 기준
rent <-  read.xlsx(file='./data/공공자전거 대여소 정보(21.12월 기준).xlsx',sheetIndex='대여소현황')

colnames(rent)
# [1] "대여소.번호"      "보관소.대여소.명" "소재지.위치."     "X"               
# [5] "X.1"              "X.2"              "설치.시기"        "설치형태"        
# [9] "X.3"              "운영.방식" 

colnames(rent) <- c("대여소번호","대여소명","자치구","상세주소","lat","lon" ,"설치시기","거치대수_LCD","거치대수_QR", "운영방식")

# 대여소번호가 NA이나 공백인 행 삭제
rent <- rent %>% filter(!is.na(대여소번호)) %>% filter(!대여소번호=="")

# 문자열인 위도 경도를 숫자로 바꿔줌
rent$lat <- as.numeric(rent$lat)
rent$lon <- as.numeric(rent$lon)


# 공백은 NA로 처리하여 결측치 통일시키기
rent[rent==""] <- NA

rent %>% group_by(대여소번호)%>% 
  summarise(번호별수=n())%>%arrange(desc(번호별수)) %>% head(5)
# 대여소번호 번호별수
#         <int>    <int>
# 1        102        1
# 2        103        1
# 3        104        1
# 4        105        1
# 5        106        1

table(is.na(rent$lat)) 
# FALSE 
# 2586

# 위경도 데이터 불러오기
# for(i in 1:length(rent$lat)) {
#   if(is.na(rent$lat[i])) {  
#     g <- geocode(rent$상세주소[i]) # 한개 위경도 얻어오기
#     rent$lat[i] <- g$lat
#     rent$lon[i] <- g$lon
#   }
# }


# rent데이터프레임 csv 로 저장.
write.csv(rent,"./pre_data/공공자전거대여소_21년12월기준.csv")















