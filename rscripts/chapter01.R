# Chapter 01

#패키지 설치 및 개발환경 확인 

# 실습: R 패키지 보기 
dim(available.packages())

# 실습: R 패키지 목록 보기 
available.packages()

# 실습: R Session 보기 
sessionInfo()

# 실습: stringr 패키지 설치 
install.packages(("stringr"))

# 실습: 설치된 패키지 확인
installed.packages()

# 실습: 패키지 로드 
library(stringr)
search()

library(blm)
require(blm)

# 실습: 패키지 제거
remove.packages("stringr")

# 현재 작업 공간 보기(기본함수)
getwd()

# 작업공간 변경 
setwd("C:/Rwork/Part-I")
data <- read.csv("data/test.csv", header = T, fileEncoding = "euc-kr")
data

