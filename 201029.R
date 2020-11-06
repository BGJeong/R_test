install.packages("dplyr")
library(dplyr)
df_raw <- data.frame(var1=c(1,2,1),
                      var2 = c(2,3,2))
df_raw

df_new <- df_raw
df_new

df_new <- rename(df_new, v2 = var2)
df_new

df <- data.frame(var1=c(4,3,8),
                 var2=c(2,6,1))
df

df$var_sum <- df$var1 + df$var2
df

df$var_mean <- (df$var1 + df$var2)/2
df

library(ggplot2)
mpg<-as.data.frame(ggplot2::mpg)
mpg
head(mpg)

mpg$total <- (mpg$cty + mpg$hwy)/2
head(mpg)

summary(mpg$total)
hist(mpg$total)

mpg$test <-ifelse(mpg$total >= 20, "pass", "fail")
head(mpg)

table(mpg$test)
qplot(mpg$test)

mpg$grade <-ifelse(mpg$total >=30, "A", ifelse(mpg$total >= 20, "B", "C"))
head(mpg)

table(mpg$grade)
qplot(mpg$grade)

library(dplyr)

exam <- read.csv("data/csv_exam.csv")
exam

exam %>% filter(class == 1)

exam %>% filter(class!=1)
exam %>% filter(english >= 80)

exam %>% filter(class ==1 & math >= 50)

exam %>% filter(english<90 | science <50)

exam %>% select(-math, -english, -science)
exam %>% filter(class ==1) %>% select(english)

exam %>% filter(class==1) %>% select(english)

exam %>% select(id, math) %>% head()

exam %>% arrange(math)

exam %>% arrange(desc(math))

exam %>% arrange(class, desc(math))

exam %>% 
  mutate(total = math + english+science) %>%  head()

exam %>% mutate(total = math + english + science, mean = total/3) %>% head()

exam2 <-exam %>% mutate(test = ifelse(science >= 60, "pass", "fail")) %>% head()
exam2

exam3 <-exam %>% mutate(total = math + english + science) %>% arrange(desc(total)) %>% head()
exam3

exam %>% summarise(mean_math = mean(math))
exam %>% mutate(mean_math = mean(math))

exam %>% filter(class==1) %>% summarise(mean_math = mean(math))

exam %>% group_by(class) %>%  summarise(mean_math = mean(math),
                                        um_math = sum(math),
                                        median_math = median(math),
                                        n=n())


test1 <- data.frame(id=c(1,2,3,4,5),
                    midterm = c(60,80,70,90,85))
test1
test2 <- data.frame(id=c(1,2,3,4,5),
                    final = c(70,83,65,95,80))
test2

total <- left_join(test1, test2, by="id")
total

exam

name <- data.frame(class=c(1,2,3,4,5),
                   teacher = c("kim", "lee","park","choi","jeong"))
name
exam4 <- left_join(exam, name, by="class")
exam4

cla <-data.frame(id=c(21,22,23,24),
                 class = c(6,6,6,6))
exam5 <-bind_rows(exam, cla)
exam5
exam







