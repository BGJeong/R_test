# 의존성 패키지 설치
install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")

# github 버전 설치
install.packages("remotes")

# 64bit 에서만 동작합니다.
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))

library("rJava")
library("memoise")
library("dplyr")
library("KoNLP")

useNIADic()
extractNoun(text)
extractNoun(text)

txt <- readLines("data/hiphop2.txt")
txt
txt2 <-readLines("data/hiphop.txt")
txt2
ttt <-readLines("data/haha.txt")
ttt




