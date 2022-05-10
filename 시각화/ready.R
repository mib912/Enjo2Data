getwd()
setwd('C:/RStudy/Project')

library(lubridate)
library(dplyr)

## 일별 데이터를 달마다 다른 변수로 저장

d_jan <- read.csv('./data/서울특별시 공공자전거 일별 대여건수_2020.07.01_2021.01.31.csv', header=T)

# 읽어온 데이터 중 대여일시, 대여건수 열만 추출
d_jan <- subset(d_jan, select=c(대여일시, 대여건수))

# 대여일시가 2021년도인 행 추출
d_jan <- d_jan %>% filter(year(대여일시)== '2021')
d_jan$대여일시 <- as.Date(d_jan$대여일시)
str(d_jan)
View(d_jan)

# 21년 2~6월 데이터 읽어오기
d_data <- read.csv('./data/서울특별시 공공자전거 일별 대여건수_21.02.01_21.06.30.csv', header=T)
d_data <- subset(d_data, select = c(대여일시, 대여건수))
View(d_data)

# 21년 7~12월 데이터 읽어오기
d_data2 <- read.csv('./data/서울특별시 공공자전거 일별 대여건수_21.07-21.12.csv', header=T)
d_data2 <- subset(d_data2, select=c(대여일시, 대여건수))
View(d_data2)


d_feb <- d_data
# 2월 데이터만 따로 나누기 위해 대여일시 중 2월인 것만 추출하여 저장
d_feb$대여일시 <- ymd(d_data$대여일시)
d_feb <- d_feb %>% filter(month(대여일시)=='2')
View(d_feb)

# 위와 마찬가지로 각 월로 나누어 저장
d_mar <- d_data
d_mar$대여일시 <- ymd(d_data$대여일시)
d_mar <- d_mar %>% filter(month(대여일시)=='3')
View(d_mar)

d_apr <- d_data
d_apr$대여일시 <- ymd(d_data$대여일시)
d_apr <- d_apr %>% filter(month(대여일시)=='4')
View(d_apr)

d_may <- d_data
d_may$대여일시 <- ymd(d_data$대여일시)
d_may <- d_may %>% filter(month(대여일시)=='5')
View(d_may)

d_june <- d_data
d_june$대여일시 <- ymd(d_data$대여일시)
d_june <- d_june %>% filter(month(대여일시)=='6')
View(d_june)

d_july <- d_data2
d_july$대여일시 <- ymd(d_data2$대여일시)
d_july <- d_july %>% filter(month(대여일시)=='7')
View(d_july)

d_aug <- d_data2
d_aug$대여일시 <- ymd(d_data2$대여일시)
d_aug <- d_aug %>% filter(month(대여일시)=='8')
View(d_aug)

d_sep <- d_data2
d_sep$대여일시 <- ymd(d_data2$대여일시)
d_sep <- d_sep %>% filter(month(대여일시)=='9')
View(d_sep)

d_oct <- d_data2
d_oct$대여일시 <- ymd(d_data2$대여일시)
d_oct <- d_oct %>% filter(month(대여일시)=='10')
View(d_oct)

d_nov <- d_data2
d_nov$대여일시 <- ymd(d_data2$대여일시)
d_nov <- d_nov %>% filter(month(대여일시)=='11')
View(d_nov)

d_dec <- d_data2
d_dec$대여일시 <- ymd(d_data2$대여일시)
d_dec <- d_dec %>% filter(month(대여일시)=='12')
View(d_dec)

# 1월부터 12월까지 하나의 df로 결합하여 저장
df_day <- rbind(d_jan, d_feb, d_mar, d_apr, d_may, d_june, d_july, d_aug, d_sep, d_oct, d_nov, d_dec)
View(df_day)

#########################################################

## 월별 데이터 받기 (이용건수, 운동량, 탄소량, 이동거리, 이용시간)

m_jan <- read.csv('./data/month/서울특별시 공공자전거 이용정보(월별)_21.01.csv')

# 필요한 열만 추출
m_jan <- subset(m_jan, select=c(이용건수, 운동량, 탄소량, 이동거리.M., 이용시간.분.))
View(m_jan)

m_feb <- read.csv('./data/month/서울특별시 공공자전거 이용정보(월별)_21.02.csv')
m_feb <- subset(m_feb, select=c(이용건수, 운동량, 탄소량, 이동거리.M., 이용시간.분.))
View(m_feb)

m_mar <- read.csv('./data/month/서울특별시 공공자전거 이용정보(월별)_21.03.csv')
m_mar <- subset(m_mar, select=c(이용건수, 운동량, 탄소량, 이동거리.M., 이용시간.분.))
View(m_mar)

m_apr <- read.csv('./data/month/서울특별시 공공자전거 이용정보(월별)_21.04.csv')
m_apr <- subset(m_apr, select=c(이용건수, 운동량, 탄소량, 이동거리.M., 이용시간.분.))
View(m_apr)

m_may <- read.csv('./data/month/서울특별시 공공자전거 이용정보(월별)_21.05.csv')
m_may <- subset(m_may, select=c(이용건수, 운동량, 탄소량, 이동거리.M., 이용시간.분.))
View(m_may)

m_june <- read.csv('./data/month/서울특별시 공공자전거 이용정보(월별)_21.06.csv')
m_june <- subset(m_june, select=c(이용건수, 운동량, 탄소량, 이동거리.M., 이용시간.분.))
View(m_june)

m_july <- read.csv('./data/month/서울특별시 공공자전거 이용정보(월별)_21.07.csv')
m_july <- subset(m_july, select=c(이용건수, 운동량, 탄소량, 이동거리.M., 이용시간.분.))
View(m_july)

m_aug <- read.csv('./data/month/서울특별시 공공자전거 이용정보(월별)_21.08.csv')
m_aug <- subset(m_aug, select=c(이용건수, 운동량, 탄소량, 이동거리.M., 이용시간.분.))
View(m_aug)

m_sep <- read.csv('./data/month/서울특별시 공공자전거 이용정보(월별)_21.09.csv')
m_sep <- subset(m_sep, select=c(이용건수, 운동량, 탄소량, 이동거리.M., 이용시간.분.))
View(m_sep)

m_oct <- read.csv('./data/month/서울특별시 공공자전거 이용정보(월별)_21.10.csv')
m_oct <- subset(m_oct, select=c(이용건수, 운동량, 탄소량, 이동거리.M., 이용시간.분.))
View(m_oct)

m_nov <- read.csv('./data/month/서울특별시 공공자전거 이용정보(월별)_21.11.csv')
m_nov <- subset(m_nov, select=c(이용건수, 운동량, 탄소량, 이동거리.M., 이용시간.분.))
View(m_nov)

m_dec <- read.csv('./data/month/서울특별시 공공자전거 이용정보(월별)_21.12.csv')
m_dec <- subset(m_dec, select=c(이용건수, 운동량, 탄소량, 이동거리.M., 이용시간.분.))
View(m_dec)

#####################################################################################

## 시간대별 데이터 받기 (대여일자, 대여시간, 대여구분코드, 연령대코드, 이용건수, 운동량, 탄소량, 이동거리, 사용시간)

# 1월
t_jan <- read.csv('./data/time/서울특별시 공공자전거 이용정보(시간대별)_21.01.csv')

# 1월데이터에서 사용할 열 추출
t_jan <- subset(t_jan, select=c(대여일자, 대여시간,대여구분코드, 연령대코드, 이용건수, 운동량, 탄소량, 이동거리, 사용시간))
t_jan24 <- t_jan

t_jan$대여시간 <- as.numeric(t_jan$대여시간)

# ~24 데이터는 24시간대별로 나타내기 위한 데이터임
t_jan24$대여시간 <- as.numeric(t_jan24$대여시간)

# 대여시간을 ifelse문을 이용하여 인코딩 함
# 대여시간이 0~6시 이면 1
# 대여시간이 7~10시 이면 2
# 대여시간이 11~13시 이면 3
# 대여시간이 14~17시 이면 4
# 대여시간이 18~20시 이면 5
# 대여시간이 21~23시 이면 6
t_jan$대여시간 <- ifelse((t_jan$대여시간 >= 0 & t_jan$대여시간 <= 6), 1,
                     ifelse((t_jan$대여시간 >= 7 & t_jan$대여시간 <= 10), 2,
                            ifelse((t_jan$대여시간 >= 11 & t_jan$대여시간 <= 13), 3,
                                   ifelse((t_jan$대여시간 >= 14 & t_jan$대여시간 <=17), 4,
                                          ifelse((t_jan$대여시간 >= 18 & t_jan$대여시간 <= 20), 5, 6)))))


# 대여구분코드를 ifelse문을 이용하여 인코딩 함
# 대여구분코드가 정기권이면 1
# 대여구분코드가 일일권이면 2
# 대여구분코드가 단체권이면 3
t_jan$대여구분코드 <- ifelse(t_jan$대여구분코드 == '정기권', 1,
                       ifelse(t_jan$대여구분코드 == '일일권', 2, 3))

# 연령대코드를 ifelse문을 이용하여 인코딩함
# 연령대코드가 ~10대면 1
# 연령대코드가 20대면 2
# 연령대코드가 30대면 3
# 연령대코드가 40대면 4
# 연령대코드가 50대면 5
# 연령대코드가 60대이상이면 6
t_jan$연령대코드 <- ifelse(t_jan$연령대코드 == '~10대', 1,
                      ifelse(t_jan$연령대코드 == '20대', 2,
                             ifelse(t_jan$연령대코드 == '30대', 3,
                                    ifelse(t_jan$연령대코드 == '40대', 4,
                                           ifelse(t_jan$연령대코드 == '50대', 5, 6)))))

View(t_jan24)

# 2월
t_feb <- read.csv('./data/time/서울특별시 공공자전거 이용정보(시간대별)_21.02.csv')
t_feb <- subset(t_feb, select=c(대여일자, 대여시간,대여구분코드, 연령대코드, 이용건수, 운동량, 탄소량, 이동거리, 사용시간))
t_feb24 <- t_feb

t_feb$대여시간 <- as.numeric(t_feb$대여시간)
t_feb24$대여시간 <- as.numeric(t_feb24$대여시간)

t_feb$대여시간 <- ifelse((t_feb$대여시간 >= 0 & t_feb$대여시간 <= 6), 1,
                     ifelse((t_feb$대여시간 >= 7 & t_feb$대여시간 <= 10), 2,
                            ifelse((t_feb$대여시간 >= 11 & t_feb$대여시간 <= 13), 3,
                                   ifelse((t_feb$대여시간 >= 14 & t_feb$대여시간 <=17), 4,
                                          ifelse((t_feb$대여시간 >= 18 & t_feb$대여시간 <= 20), 5, 6)))))

t_feb$대여구분코드 <- ifelse(t_feb$대여구분코드 == '정기권', 1,
                       ifelse(t_feb$대여구분코드 == '일일권', 2, 3))

t_feb$연령대코드 <- ifelse(t_feb$연령대코드 == '~10대', 1,
                      ifelse(t_feb$연령대코드 == '20대', 2,
                             ifelse(t_feb$연령대코드 == '30대', 3,
                                    ifelse(t_feb$연령대코드 == '40대', 4,
                                           ifelse(t_feb$연령대코드 == '50대', 5, 6)))))



# 3월
t_mar <- read.csv('./data/time/서울특별시 공공자전거 이용정보(시간대별)_21.03.csv')
t_mar <- subset(t_mar, select=c(대여일자, 대여시간,대여구분코드, 연령대코드, 이용건수, 운동량, 탄소량, 이동거리, 사용시간))
t_mar24 <- t_mar

# 결측치 처리를 해줌
sum(is.na(t_mar$이동거리)) # 0
na.omit(t_mar$이동거리)


t_mar24 <- t_mar
str(t_mar)

t_mar$대여시간 <- as.numeric(t_mar$대여시간)
t_mar$이동거리 <- as.numeric(t_mar$이동거리)
t_mar24$대여시간 <- as.numeric(t_mar24$대여시간)


t_mar$대여시간 <- ifelse((t_mar$대여시간 >= 0 & t_mar$대여시간 <= 6), 1,
                     ifelse((t_mar$대여시간 >= 7 & t_mar$대여시간 <= 10), 2,
                            ifelse((t_mar$대여시간 >= 11 & t_mar$대여시간 <= 13), 3,
                                   ifelse((t_mar$대여시간 >= 14 & t_mar$대여시간 <=17), 4,
                                          ifelse((t_mar$대여시간 >= 18 & t_mar$대여시간 <= 20), 5, 6)))))

t_mar$대여구분코드 <- ifelse(t_mar$대여구분코드 == '정기권', 1,
                     ifelse(t_mar$대여구분코드 == '일일권', 2, 3))

t_mar$연령대코드 <- ifelse(t_mar$연령대코드 == '~10대', 1,
                    ifelse(t_mar$연령대코드 == '20대', 2,
                           ifelse(t_mar$연령대코드 == '30대', 3,
                                  ifelse(t_mar$연령대코드 == '40대', 4,
                                         ifelse(t_mar$연령대코드 == '50대', 5, 6)))))

View(t_mar)

# 4월
t_apr <- read.csv('./data/time/서울특별시 공공자전거 이용정보(시간대별)_21.04.csv')
View(t_apr)
t_apr <- subset(t_apr, select=c(대여일자, 대여시간,대여구분코드, 연령대코드, 이용건수, 운동량, 탄소량, 이동거리, 사용시간))
t_apr24 <- t_apr

sum(is.na(t_apr)) # 0

t_apr$대여시간 <- as.numeric(t_apr$대여시간)

t_apr$대여시간 <- ifelse((t_apr$대여시간 >= 0 & t_apr$대여시간 <= 6), 1,
                     ifelse((t_apr$대여시간 >= 7 & t_apr$대여시간 <= 10), 2,
                            ifelse((t_apr$대여시간 >= 11 & t_apr$대여시간 <= 13), 3,
                                   ifelse((t_apr$대여시간 >= 14 & t_apr$대여시간 <=17), 4,
                                          ifelse((t_apr$대여시간 >= 18 & t_apr$대여시간 <= 20), 5, 6)))))

t_apr$대여구분코드 <- ifelse(t_apr$대여구분코드 == '정기권', 1,
                       ifelse(t_apr$대여구분코드 == '일일권', 2, 3))

t_apr$연령대코드 <- ifelse(t_apr$연령대코드 == '~10대', 1,
                      ifelse(t_apr$연령대코드 == '20대', 2,
                             ifelse(t_apr$연령대코드 == '30대', 3,
                                    ifelse(t_apr$연령대코드 == '40대', 4,
                                           ifelse(t_apr$연령대코드 == '50대', 5, 6)))))
View(t_apr)


# 5월
t_may <- read.csv('./data/time/서울특별시 공공자전거 이용정보(시간대별)_21.05.csv')
t_may <- subset(t_may, select=c(대여일자, 대여시간,대여구분코드, 연령대코드, 이용건수, 운동량, 탄소량, 이동거리, 사용시간))
t_may24 <- t_may

sum(is.na(t_may)) # 0

t_may$대여시간 <- as.numeric(t_may$대여시간)

t_may$대여시간 <- ifelse((t_may$대여시간 >= 0 & t_may$대여시간 <= 6), 1,
                     ifelse((t_may$대여시간 >= 7 & t_may$대여시간 <= 10), 2,
                            ifelse((t_may$대여시간 >= 11 & t_may$대여시간 <= 13), 3,
                                   ifelse((t_may$대여시간 >= 14 & t_may$대여시간 <=17), 4,
                                          ifelse((t_may$대여시간 >= 18 & t_may$대여시간 <= 20), 5, 6)))))

t_may$대여구분코드 <- ifelse(t_may$대여구분코드 == '정기권', 1,
                       ifelse(t_may$대여구분코드 == '일일권', 2, 3))

t_may$연령대코드 <- ifelse(t_may$연령대코드 == '~10대', 1,
                      ifelse(t_may$연령대코드 == '20대', 2,
                             ifelse(t_may$연령대코드 == '30대', 3,
                                    ifelse(t_may$연령대코드 == '40대', 4,
                                           ifelse(t_may$연령대코드 == '50대', 5, 6)))))
View(t_may)


# 6월
t_june <- read.csv('./data/time/서울특별시 공공자전거 이용정보(시간대별)_21.06.csv')
t_june <- subset(t_june, select=c(대여일자, 대여시간,대여구분코드, 연령대코드, 이용건수, 운동량, 탄소량, 이동거리, 사용시간))
t_june24 <- t_june

sum(is.na(t_june)) # 0

t_june$대여시간 <- as.numeric(t_june$대여시간)

t_june$대여시간 <- ifelse((t_june$대여시간 >= 0 & t_june$대여시간 <= 6), 1,
                     ifelse((t_june$대여시간 >= 7 & t_june$대여시간 <= 10), 2,
                            ifelse((t_june$대여시간 >= 11 & t_june$대여시간 <= 13), 3,
                                   ifelse((t_june$대여시간 >= 14 & t_june$대여시간 <=17), 4,
                                          ifelse((t_june$대여시간 >= 18 & t_june$대여시간 <= 20), 5, 6)))))

t_june$대여구분코드 <- ifelse(t_june$대여구분코드 == '정기권', 1,
                       ifelse(t_june$대여구분코드 == '일일권', 2, 3))

t_june$연령대코드 <- ifelse(t_june$연령대코드 == '~10대', 1,
                      ifelse(t_june$연령대코드 == '20대', 2,
                             ifelse(t_june$연령대코드 == '30대', 3,
                                    ifelse(t_june$연령대코드 == '40대', 4,
                                           ifelse(t_june$연령대코드 == '50대', 5, 6)))))
View(t_june)


# 7월
t_july <- read.csv('./data/time/서울특별시 공공자전거 이용정보(시간대별)_21.07.csv')
t_july <- subset(t_july, select=c(대여일자, 대여시간,대여구분코드, 연령대코드, 이용건수, 운동량, 탄소량, 이동거리, 사용시간))
t_july24 <- t_july

sum(is.na(t_july)) # 0

t_july$대여시간 <- as.numeric(t_july$대여시간)

t_july$대여시간 <- ifelse((t_july$대여시간 >= 0 & t_july$대여시간 <= 6), 1,
                      ifelse((t_july$대여시간 >= 7 & t_july$대여시간 <= 10), 2,
                             ifelse((t_july$대여시간 >= 11 & t_july$대여시간 <= 13), 3,
                                    ifelse((t_july$대여시간 >= 14 & t_july$대여시간 <=17), 4,
                                           ifelse((t_july$대여시간 >= 18 & t_july$대여시간 <= 20), 5, 6)))))

t_july$대여구분코드 <- ifelse(t_july$대여구분코드 == '정기권', 1,
                        ifelse(t_july$대여구분코드 == '일일권', 2, 3))

t_july$연령대코드 <- ifelse(t_july$연령대코드 == '~10대', 1,
                       ifelse(t_july$연령대코드 == '20대', 2,
                              ifelse(t_july$연령대코드 == '30대', 3,
                                     ifelse(t_july$연령대코드 == '40대', 4,
                                            ifelse(t_july$연령대코드 == '50대', 5, 6)))))
View(t_july)

# 8월
t_aug <- read.csv('./data/time/서울특별시 공공자전거 이용정보(시간대별)_21.08.csv')
t_aug <- subset(t_aug, select=c(대여일자, 대여시간,대여구분코드, 연령대코드, 이용건수, 운동량, 탄소량, 이동거리, 사용시간))
t_aug24 <- t_aug

sum(is.na(t_aug)) # 0

t_aug$대여시간 <- as.numeric(t_aug$대여시간)

t_aug$대여시간 <- ifelse((t_aug$대여시간 >= 0 & t_aug$대여시간 <= 6), 1,
                      ifelse((t_aug$대여시간 >= 7 & t_aug$대여시간 <= 10), 2,
                             ifelse((t_aug$대여시간 >= 11 & t_aug$대여시간 <= 13), 3,
                                    ifelse((t_aug$대여시간 >= 14 & t_aug$대여시간 <=17), 4,
                                           ifelse((t_aug$대여시간 >= 18 & t_aug$대여시간 <= 20), 5, 6)))))

t_aug$대여구분코드 <- ifelse(t_aug$대여구분코드 == '정기권', 1,
                        ifelse(t_aug$대여구분코드 == '일일권', 2, 3))

t_aug$연령대코드 <- ifelse(t_aug$연령대코드 == '~10대', 1,
                       ifelse(t_aug$연령대코드 == '20대', 2,
                              ifelse(t_aug$연령대코드 == '30대', 3,
                                     ifelse(t_aug$연령대코드 == '40대', 4,
                                            ifelse(t_aug$연령대코드 == '50대', 5, 6)))))
View(t_aug)

# 9월
t_sep <- read.csv('./data/time/서울특별시 공공자전거 이용정보(시간대별)_21.09.csv')
t_sep <- subset(t_sep, select=c(대여일자, 대여시간,대여구분코드, 연령대코드, 이용건수, 운동량, 탄소량, 이동거리, 사용시간))
t_sep24 <- t_sep

sum(is.na(t_sep)) # 0

t_sep$대여시간 <- as.numeric(t_sep$대여시간)

t_sep$대여시간 <- ifelse((t_sep$대여시간 >= 0 & t_sep$대여시간 <= 6), 1,
                     ifelse((t_sep$대여시간 >= 7 & t_sep$대여시간 <= 10), 2,
                            ifelse((t_sep$대여시간 >= 11 & t_sep$대여시간 <= 13), 3,
                                   ifelse((t_sep$대여시간 >= 14 & t_sep$대여시간 <=17), 4,
                                          ifelse((t_sep$대여시간 >= 18 & t_sep$대여시간 <= 20), 5, 6)))))

t_sep$대여구분코드 <- ifelse(t_sep$대여구분코드 == '정기권', 1,
                       ifelse(t_sep$대여구분코드 == '일일권', 2, 3))

t_sep$연령대코드 <- ifelse(t_sep$연령대코드 == '~10대', 1,
                      ifelse(t_sep$연령대코드 == '20대', 2,
                             ifelse(t_sep$연령대코드 == '30대', 3,
                                    ifelse(t_sep$연령대코드 == '40대', 4,
                                           ifelse(t_sep$연령대코드 == '50대', 5, 6)))))
View(t_sep)

# 10월
t_oct <- read.csv('./data/time/서울특별시 공공자전거 이용정보(시간대별)_21.10.csv')
t_oct <- subset(t_oct, select=c(대여일자, 대여시간,대여구분코드, 연령대코드, 이용건수, 운동량, 탄소량, 이동거리, 사용시간))
t_oct24 <- t_oct

sum(is.na(t_oct)) # 0

t_oct$대여시간 <- as.numeric(t_oct$대여시간)

t_oct$대여시간 <- ifelse((t_oct$대여시간 >= 0 & t_oct$대여시간 <= 6), 1,
                     ifelse((t_oct$대여시간 >= 7 & t_oct$대여시간 <= 10), 2,
                            ifelse((t_oct$대여시간 >= 11 & t_oct$대여시간 <= 13), 3,
                                   ifelse((t_oct$대여시간 >= 14 & t_oct$대여시간 <=17), 4,
                                          ifelse((t_oct$대여시간 >= 18 & t_oct$대여시간 <= 20), 5, 6)))))

t_oct$대여구분코드 <- ifelse(t_oct$대여구분코드 == '정기권', 1,
                       ifelse(t_oct$대여구분코드 == '일일권', 2, 3))

t_oct$연령대코드 <- ifelse(t_oct$연령대코드 == '~10대', 1,
                      ifelse(t_oct$연령대코드 == '20대', 2,
                             ifelse(t_oct$연령대코드 == '30대', 3,
                                    ifelse(t_oct$연령대코드 == '40대', 4,
                                           ifelse(t_oct$연령대코드 == '50대', 5, 6)))))
View(t_oct)

# 11월
t_nov <- read.csv('./data/time/서울특별시 공공자전거 이용정보(시간대별)_21.11.csv')
t_nov <- subset(t_nov, select=c(대여일자, 대여시간,대여구분코드, 연령대코드, 이용건수, 운동량, 탄소량, 이동거리, 사용시간))
t_nov24 <- t_nov

sum(is.na(t_nov)) # 0

t_nov$대여시간 <- as.numeric(t_nov$대여시간)

t_nov$대여시간 <- ifelse((t_nov$대여시간 >= 0 & t_nov$대여시간 <= 6), 1,
                     ifelse((t_nov$대여시간 >= 7 & t_nov$대여시간 <= 10), 2,
                            ifelse((t_nov$대여시간 >= 11 & t_nov$대여시간 <= 13), 3,
                                   ifelse((t_nov$대여시간 >= 14 & t_nov$대여시간 <=17), 4,
                                          ifelse((t_nov$대여시간 >= 18 & t_nov$대여시간 <= 20), 5, 6)))))

t_nov$대여구분코드 <- ifelse(t_nov$대여구분코드 == '정기권', 1,
                       ifelse(t_nov$대여구분코드 == '일일권', 2, 3))

t_nov$연령대코드 <- ifelse(t_nov$연령대코드 == '~10대', 1,
                      ifelse(t_nov$연령대코드 == '20대', 2,
                             ifelse(t_nov$연령대코드 == '30대', 3,
                                    ifelse(t_nov$연령대코드 == '40대', 4,
                                           ifelse(t_nov$연령대코드 == '50대', 5, 6)))))
View(t_nov)

# 12월
t_dec <- read.csv('./data/time/서울특별시 공공자전거 이용정보(시간대별)_21.12.csv')
t_dec <- subset(t_dec, select=c(대여일자, 대여시간,대여구분코드, 연령대코드, 이용건수, 운동량, 탄소량, 이동거리, 사용시간))
t_dec24 <- t_dec

sum(is.na(t_dec)) # 0

t_dec$대여시간 <- as.numeric(t_dec$대여시간)

t_dec$대여시간 <- ifelse((t_dec$대여시간 >= 0 & t_dec$대여시간 <= 6), 1,
                     ifelse((t_dec$대여시간 >= 7 & t_dec$대여시간 <= 10), 2,
                            ifelse((t_dec$대여시간 >= 11 & t_dec$대여시간 <= 13), 3,
                                   ifelse((t_dec$대여시간 >= 14 & t_dec$대여시간 <=17), 4,
                                          ifelse((t_dec$대여시간 >= 18 & t_dec$대여시간 <= 20), 5, 6)))))

t_dec$대여구분코드 <- ifelse(t_dec$대여구분코드 == '정기권', 1,
                       ifelse(t_dec$대여구분코드 == '일일권', 2, 3))

t_dec$연령대코드 <- ifelse(t_dec$연령대코드 == '~10대', 1,
                      ifelse(t_dec$연령대코드 == '20대', 2,
                             ifelse(t_dec$연령대코드 == '30대', 3,
                                    ifelse(t_dec$연령대코드 == '40대', 4,
                                           ifelse(t_dec$연령대코드 == '50대', 5, 6)))))
View(t_dec)




###########################################################################################


# csv 내보내기
write.csv(t_jan, file = './january.csv')
write.csv(t_jan, file = './february.csv')
write.csv(t_jan, file = './march.csv')
write.csv(t_jan, file = './april.csv')
write.csv(t_jan, file = './may.csv')
write.csv(t_jan, file = './june.csv')
write.csv(t_jan, file = './july.csv')
write.csv(t_jan, file = './august.csv')
write.csv(t_jan, file = './september.csv')
write.csv(t_jan, file = './october.csv')
write.csv(t_jan, file = './november.csv')
write.csv(t_jan, file = './december.csv')
