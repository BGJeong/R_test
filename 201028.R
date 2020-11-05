x<-list("홍길동","2017001", 20, c("it융합","데이터관리"))
x
y<-list(성명="홍길동",학번="2017001",나이=20, 수강과목=c("it융합","데이터관리"))
y

x<- data.frame(성명=c("홍길동","손오공"),
                 나이 = c(20,30),
                 주소 = c("서울","부산"))
x

if(10<=0){
  print("10은 양수")
}else{
  print("x")
}
y<-10
x<-ifelse(y%%2==0, "짝", "홀")
x

for(i in 1:10){
  print(i)
}

sum <- 0
for(i in 1:10){
  sum<-sum+i
}
sum
i<-1

while(i <= 100){
  print(i)
  i<-i+1
}
?round
round(42.195)
factorial(170)

f<-function(a){
  return(a*10)
}
f(10)


a<-5
b<-3
a/b

var <- seq(1,10,by=2)
var

str<-'dd'
str
str<-"hoit"
str<-c("dd", "hoit")
paste(str, collapse = "/")

install.packages("ggplot2")
library(ggplot2)
x<-c("a","b","a","c","d","a","a","b","a","a","b","a","c","d","a","a","b","a")
qplot(x)
qplot(data=mpg, x=hwy)
head(mpg)
mpg


english <- c(90, 80, 60, 70)
english
math<-c(50,60,100,20)
df_midterm <- data.frame(english, math)
df_midterm

class<-c(1,1,2,2)
df_midterm<-data.frame(english,math,class)
df_midterm$english
mean(df_midterm$english)
max(df_midterm$english)
min(df_midterm$english)

df_midterm <- data.frame(english=c(90,80,60,70),
                         math=c(50,60,100,20),
                         class=c(1,1,2,2))
df_midterm

sales <- data.frame(fruit=c("사과", "딸기", "수박"),
                    price=c(1800,1500,3000),
                    sale = c(24, 38, 13))
sales

install.packages("readxl")
library(readxl)
df_exam<-read_excel("data/excel_exam.xlsx")
df_exam

df_exam<-read_excel("C:/Users/user/Documents/data/excel_exam.xlsx")
df_exam

mean(df_exam$english)
df_exam_novar <- read_excel("data/excel_exam_novar.xlsx",
                            col_names = FALSE)
df_exam_novar
df_exam_sheet <- read_excel("data/excel_exam_sheet.xlsx", sheet = 3)
df_exam_sheet
