# 2020.11.02 (월)

# 결측치(Missing Value)
# 1. 비어있는 데이터를 의미
# 2. 결측치가 있는 데이터틑 통계적인 함수가 적용되지 않는다.
# 3. 결측치를 제거하고 데이터 분석을 한다.
# 4. 결측치 데이터는 대문자로 NA로 표기한다.


# 결측치가 있는 데이터 프레임 생성
df <- data.frame(sex = c("M", "F", NA, "M", "F"),
                 score = c(5, 4, 3, 4, NA))
df

# 결측치를 확인하기 : is.na()
is.na(df)                    # 결측치 확인 
#       sex score            # 결측치는 TRUE로 출력됨
# [1,] FALSE FALSE
# [2,] FALSE FALSE
# [3,]  TRUE FALSE
# [4,] FALSE FALSE
# [5,] FALSE  TRUE

# 결측치 데이터 갯수 확인
table(is.na(df))             # 결측치 빈도 출력
# FALSE  TRUE                # 결측치 2개 있음 : TRUE(2)
#   8     2 

# 각 변수별로 결측치의 갯수 확인
# 1) sex 변수의 결측치 빈도 확인
table(is.na(df$sex))
# FALSE  TRUE                # 결측치 1개 있음 : TRUE(1) 
#   4     1 

# 2) score 변수의 결측치 빈도 확인
table(is.na(df$score))      
# FALSE  TRUE                # 결측치 1개 있음 : TRUE(1) 
#   4     1    


# 결측치가 포함된 데이터에 통계적인 함수 적용하기
# : 결측치가 포함된 데이터는 통계적인 함수가 적용되지 않고, NA로 출력되기
#   때문에 결측치를 제거하고 데이터 분석을 해야한다.

sum(df$score)
# [1] NA

mean(df$score)
# [1] NA


#------------------------------------------------------------------------

# 결측치 제거하기

library(dplyr)

# 방법1. filter() 함수
# 1) score변수의 결측치 데이터 추출
df %>% filter(is.na(score))
#    sex  score
# 1   F    NA

# 2) score변수의 결측치가 아닌 데이터 추출
df %>% filter(!is.na(score))
#     sex score
# 1    M     5
# 2    F     4
# 3 <NA>     3
# 4    M     4

# 3) 결측치가 아닌 데이터를 df_nomiss 데이터 프레임에 저장
df_nomiss <- df %>% filter(!is.na(score))
df_nomiss

# 4) df_nomiss 데이터프레임을 이용해서 통계적인 함수 적용하기
#    score 변수에 결측치가 제거되었기 때문에 통계적인 함수가 잘 적용된다.
sum(df_nomiss$score)
# [1] 16

mean(df_nomiss$score)
# [1] 4

# 5) sex, score 변수에 결측치가 없는 데이터 추출
df_nomiss <- df %>% filter(!is.na(sex) & !is.na(score))
df_nomiss
#    sex  score 
# 1   M     5
# 2   F     4
# 3   M     4


# 방법2. omit() 함수
# 1) df 데이터 프레임의 모든 결측치를 제거 : omit()
df_nomiss2 <- na.omit(df)
df_nomiss2
#   sex score
# 1   M     5
# 2   F     4
# 4   M     4

# 2) 통계적인 함수 적용하기
sum(df_nomiss2$score)
# [1] 13
mean(df_nomiss2$score)
# [1] 4.333333


# 방법3. na.rm = T
# 결측치가 있는 데이터를 제외하고 통계적인 함수 적용하기 : na.rm = T
sum(df$score)                 # 결측치가 있는 경우
# [1] NA
sum(df$score, na.rm = T)      # 결측치 데이터를 제외하고 합 구하기     
# [1] 16
mean(df$score, na.rm = T)     # 결측치 데이터를 제외하고 평균 구하기  
# [1] 4

#-----------------------------------------------------------------------
#Q. 결측치를 제거하고 통계적인 함수를 적용하는 예제
# 1) 데이터 불러오기
exam <- read.csv("data/csv_exam.csv")
exam

# 2) exam 데이터프레임의 math변수의  3, 8, 15행 데이터 결측으로 처리
exam[c(3, 8, 15), "math"] <- NA
exam

# 3) math 평균 구하기
#   결측이 포함된 데이터틑 통계적인 함수가 적용되지 않는다.
mean(exam$math)
# [1] NA

exam %>% summarise(mean_math = mean(math))
#        mean_math
# 1        NA

# 결측치를 제거하고 통계적인 함수 적용하기
mean(exam$math, na.rm = T)    # 결측치를 제외하고 평균을 구함
# [1] 55.23529
exam %>% summarise(mean_math = mean(math, na.rm = T))
#    mean_math
# 1  55.23529

# 결측치를 제외한 데이터의 평균, 합, 중앙값 구하기
exam %>% summarise(mean_math = mean(math, na.rm = T),     # 평균
                   sum_math = sum(math, na.rm = T),       # 합
                   median_math = median(math, na.rm = T)) # 중앙값 
#    mean_math sum_math median_math
# 1  55.23529      939          50

#----------------------------------------------------------------------
# 결측치를 대표값(평균, 최빈값)으로 대체하기
# : 결측치가 많은 데이터셋의 경우에는 무조건 결측치를 제거하지 않고,
#   결측치를 대표값(평균, 최빈값)으로 대체해서 처리할 수 있다.

# exma 데이터프레임의 math변수 3, 8, 15행은 결측치
# 1) exam 데이터프레임의 math 변수의 평균 구하기
mean(exam$math, na.rm = T)
# [1] 55.23529

# 2) exam 데이터프레임의 math변수 3, 8, 15행 결측치를 평균값으로 대체하기
#    : math 가 NA이면 평균값 55점을 할당하고 NA가 아니면 기존값을 할당
exam$math <- ifelse(is.na(exam$math), 55, exam$math)
exam                          # NA가 평균값 55점으로 대체되어 있음

table(is.na(exam$math))       # 결측이 없는 데이터만 출력
# FALSE 
# 20 

# 3) math 변수의 통계적인 정보 구하기
#    math 변수는 결측치(NA)가 평균값(55)으로 대체되어서 평균을 구할수 있다. 
mean(exam$math)
# [1] 55.2         

#---------------------------------------------------------------------------
# 이상치(Outlier)
# 1. 정상범위를 크게 벗어나는 데이터를 의미
# 2. 이상치 데이터가 포함되어 있으면 데이터 분석을 왜곡 할 수 있다.
# 3. 이상치 데이터를 결측 데이터로 변환후에 결측치를 제거하는 방법을 
#    이용해서 처리한다.

# 1) 이상치가 포함된 데이터 프레임 생성
#    sex : 1(남자), 2(여자)
#    score : 1 ~ 5
outlier <- data.frame(sex=c(1, 2, 1, 3, 2, 1),
                      score=c(5, 4, 3, 4, 2, 6))
outlier

# 2) 이상치 데이터 확인
table(outlier$sex)          # 3 : 이상치
# 1  2  3 
# 3  2  1

table(outlier$score)        # 6 : 이상치
# 2  3  4  5  6 
# 1  1  2  1  1 

# 3) 이상치 데이터를 결측으로 처리하기
#    sex변수의 3을 결측치(NA)로 처리 
outlier$sex <- ifelse(outlier$sex == 3, NA, outlier$sex)
outlier

#   score변수의 6을 결측치(NA)로 처리
outlier$score <- ifelse(outlier$score == 6, NA, outlier$score )
outlier

# 4) 결측치를 제거하고, 성별(sex) 평균 점수 구하기
outlier %>% 
  filter(!is.na(sex) & !is.na(score)) %>%       # 결측제거
  group_by(sex) %>%                             # 그룹으로 나눠서 처리 
  summarise(mean_score = mean(score))           # 평균 점수
#      sex    mean_score
# 1     1          4
# 2     2          3


outlier %>% 
  group_by(sex) %>%                              # 그룹으로 나눠서 처리 
  summarise(mean_score = mean(score, na.rm = T)) # 결측데이터 제외한 평균 점수         
#     sex     mean_score
# 1     1          4
# 2     2          3
# 3    NA          4


#---------------------------------------------------------------------------

# 이상치(극단치) 데이터
# 극단티 데이터 확인하기

# 1. 데이터 불러오기
library(ggplot2)

mpg <- as.data.frame(ggplot2::mpg)    # 데이터프레임으로 읽어옴
head(mpg)
dim(mpg)              # 234행 11열


# 2. 고속도로연비(hwy) 정보를 상자 그래프
boxplot(mpg$hwy)

# 3. 상자 그래프 통계치 출력
boxplot(mpg$hwy)$stats
#       [,1]
# [1,]   12           # 극단치 하단 경계
# [2,]   18           # 1사분위 
# [3,]   24           # 2사분위 
# [4,]   27           # 3사분위 
# [5,]   37           # 극단치 상단 경계

# 4. 이상치(극단치) 데이터를 결측치로 처리
#    hwy가 12 ~ 37을 벗어나면 극단치 데이터
mpg$hwy <- ifelse(mpg$hwy<12 | mpg$hwy>37, NA, mpg$hwy)


# 5. 이상치(극단치) 데이터가 결측(NA)로 처리되었는지 확인
table(is.na(mpg$hwy))   # 결측으로 처리된 데이터: TRUE 3개
# FALSE  TRUE     
# 231     3 

# 6. 결측치 데이터를 제외하고 분석하기
library(dplyr)

mpg %>% 
  summarise(mean_hwy = mean(hwy, na.rm = T))
#    mean_hwy
# 1  23.18615


# 구동 방식(drv : 4, f, r)에 따른 평균 고속도로 연비 
mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy, na.rm = T))
# drv       mean_hwy
# 1 4         19.2
# 2 f         27.7
# 3 r         21  


