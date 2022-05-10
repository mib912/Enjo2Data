library(dplyr)
library(readxl)
library(xlsx)
# =========================================================================
# 데이터 불러오기
####### 1월 대여 이력 정보 ########
jan <- read.csv('./data/공공자전거 대여이력 정보_2101.csv', header=T)


# 데이터 열 이름 확인
colnames(jan)
# [1] "자전거번호"      "대여일시"        "대여.대여소번호" "대여.대여소명"  
# [5] "대여거치대"      "반납일시"        "반납대여소번호"  "반납대여소명"   
# [9] "반납거치대"      "이용시간"        "이용거리"  

# 열 이름 변경
colnames(jan) <- c("자전거번호" ,"대여일시","대여소번호" ,"대여소명"  ,"대여거치대","반납일시","반납대여소번호","반납대여소명","반납거치대", "이용시간" ,"이용거리")

# 정비소 데이터 제거
jan <- jan %>%
  filter(jan$대여소번호>100 & jan$대여소번호<9000)

# 대여소별 대여 건수 구하기
jan_dae_cnt <- jan %>% 
  subset(select=c(대여일시,대여소번호,대여소명)) %>% 
  group_by(대여소번호, 대여소명) %>% 
  summarise(대여건수_1월=n())



####### 2월 대여 이력 정보 ########
feb <- read.csv('./data/공공자전거 대여이력 정보_2102.csv', header=T)

# 데이터 열 이름 확인
colnames(feb)
# [1] "자전거번호"      "대여일시"        "대여.대여소번호" "대여.대여소명"  
# [5] "대여거치대"      "반납일시"        "반납대여소번호"  "반납대여소명"   
# [9] "반납거치대"      "이용시간"        "이용거리"  

# 열 이름 변경
colnames(feb) <- c("자전거번호" ,"대여일시","대여소번호" ,"대여소명"  ,"대여거치대","반납일시","반납대여소번호","반납대여소명","반납거치대", "이용시간" ,"이용거리")

str(feb)
# 대여소 문자열 > 숫자형
feb$대여소번호 <- as.numeric(feb$대여소번호)
feb$반납대여소번호 <- as.numeric(feb$반납대여소번호)

# 정비소 데이터 제거
feb <- feb %>%
  filter(feb$대여소번호>100 & feb$대여소번호<9000)

# 대여소별 대여 건수 구하기
feb_dae_cnt <- feb %>% 
  subset(select=c(대여일시,대여소번호,대여소명)) %>% 
  group_by(대여소번호, 대여소명) %>% 
  summarise(대여건수_2월=n())

####### 3월 대여 이력 정보 ########
mar <- read.csv('./data/공공자전거 대여이력 정보_2103.csv', header=T)

# 데이터 열 이름 확인
colnames(mar)
# [1] "자전거번호"      "대여일시"        "대여.대여소번호" "대여.대여소명"  
# [5] "대여거치대"      "반납일시"        "반납대여소번호"  "반납대여소명"   
# [9] "반납거치대"      "이용시간"        "이용거리"  

# 열 이름 변경
colnames(mar) <- c("자전거번호" ,"대여일시","대여소번호" ,"대여소명"  ,"대여거치대","반납일시","반납대여소번호","반납대여소명","반납거치대", "이용시간" ,"이용거리")

mar <- mar %>%
  filter(nchar(자전거번호)==9)


str(mar)
# 대여소 문자열 > 숫자형
mar$대여소번호 <- as.numeric(mar$대여소번호)
mar$반납대여소번호 <- as.numeric(mar$반납대여소번호)

# 정비소 데이터 제거
mar <- mar %>%
  filter(mar$대여소번호>100 & mar$대여소번호<9000)

mar_dae_cnt <- mar %>% 
  subset(select=c(대여일시,대여소번호,대여소명))

table(is.na(mar_dae_cnt))

d21 <- mar_dae_cnt %>% filter(대여소번호==210)
d26 <- mar_dae_cnt %>% filter(대여소번호==268)

# 대여소별 대여 건수 구하기
mar_dae_cnt <- mar %>% 
  subset(select=c(대여일시,대여소번호,대여소명)) %>% 
  group_by(대여소번호, 대여소명) %>% 
  summarise(대여건수_3월=n())






##########################################################3
# 함수로 만들기

preprocessing  <- function(df){
  # col name 변경
  colnames(df) <- c("자전거번호" ,"대여일시","대여소번호" ,"대여소명"  ,"대여거치대","반납일시","반납대여소번호","반납대여소명","반납거치대", "이용시간" ,"이용거리")
  
  # 자전거번호의 이상치를 제거하기 위해
  df <- df %>%
    filter(nchar(자전거번호)==9)
  
  # 대여소번호와 반납대여소번호는 숫자형으로 변환
  if(is.numeric(df$대여소번호)==FALSE) {
    df$대여소번호 <- as.numeric(df$대여소번호)
  }

  if(is.numeric(df$반납대여소번호)==FALSE) {
    df$반납대여소번호 <- as.numeric(df$반납대여소번호)
  }  
  
  # 정비소 데이터 제거
  df <- df %>%
    filter(df$대여소번호>100 & df$대여소번호<9000)
  
  # 대여소별 대여 건수 구하기
  df_cnt <- df %>% 
    subset(select=c(대여일시,대여소번호)) %>% 
    group_by(대여소번호) %>% 
    summarise(대여건수=n())
  
  return(df_cnt)
}

jan <- read.csv('./data/공공자전거 대여이력 정보_2101.csv', header=T)
feb <- read.csv('./data/공공자전거 대여이력 정보_2102.csv', header=T)
mar <- read.csv('./data/공공자전거 대여이력 정보_2103.csv', header=T)
apr <- read.csv('./data/공공자전거 대여이력 정보_2104.csv', header=T)
may <- read.csv('./data/공공자전거 대여이력 정보_2105.csv', header=T)
jun <- read.csv('./data/공공자전거 대여이력 정보_2106.csv', header=T)
jul <- read.csv('./data/공공자전거 대여이력 정보_2107.csv', header=T)
aug <- read.csv('./data/공공자전거 대여이력 정보_2108.csv', header=T)
sep <- read.csv('./data/공공자전거 대여이력 정보_2109.csv', header=T)
oct <- read.csv('./data/공공자전거 대여이력 정보_2110.csv', header=T)
nov <- read.csv('./data/공공자전거 대여이력 정보_2111.csv', header=T)
dec <- read.csv('./data/공공자전거 대여이력 정보_2112.csv', header=T)

df_1 <- preprocessing(may)

table(is.na(df_1))




