library(ggplot2)

head(mpg)
#1.산점도
# x, y축 데이터를 점으로 표현한 그래프
# 1) 배경 설정하기
ggplot(data = mpg, aes(x=displ, y=hwy))+geom_point()+xlim(3,6)+ylim(10,30)

library(dplyr)
mpg<-as.data.frame(ggplot2::mpg)
head(mpg)

df_mpg<-mpg %>% group_by(drv) %>% summarise(mean_hwy = mean(hwy))
df_mpg

ggplot(data = df_mpg, aes(x=drv, y=mean_hwy))+geom_col()

ggplot(data=df_mpg, aes(x=reorder(drv, mean_hwy), y=mean_hwy))+geom_col

table(mpg$drv)

ggplot(data=mpg, aes(x=drv))+geom_bar()

ggplot(data=mpg, aes(x=hwy))+geom_bar()

ggplot(data=economics, aes(x=date, y=unemploy))+geom_line()

install.packages("foreign")
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

raw_welfare <- read.spss(file="data/koweps_hpc10_2015_beta1.sav", to.data.frame = T)
raw_welfare

head(raw_welfare)
welfare <-raw_welfare
welfare

View(welfare)
dim(welfare)

summary(welfare)

welfare <- rename(welfare,
                  sex = h10_g3,
                  birth = h10_g4,
                  marriage = h10_g10,
                  religion = h10_g11,
                  income = p1002_8aq1,
                  codejob = h10_eco9,
                  code_region = h10_reg7)
View(welfare)

class(welfare$sex)

table(welfare$sex)
welfare$sex <- ifelse(welfare$sex == 9, NA, welfare$sex)
welfare$sex

table(is.na(welfare$sex))

welfare$sex <- ifelse(welfare$sex ==1, "male", "female")
table(welfare$sex)

qplot(welfare$sex)

class(welfare$income)
table(welfare$income)

summary(welfare$income)
qplot(welfare$income)

qplot(welfare$income) + xlim(0, 1000)
table(is.na(welfare$income))

welfare$income<-ifelse(welfare$income ==9999,NA,welfare$income)
welfare$income

sex_income <- welfare %>% filter(!is.na(income)) %>% group_by(sex) %>% summarise(mean_income = mean(income))
sex_income

ggplot(data = sex_income, aes(x=sex, y=mean_income))+geom_col()

welfare
