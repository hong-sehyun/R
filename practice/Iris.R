# 1. 데이터 불러오기
data("iris")
iris
View(iris)


## 데이터 확인
head(iris)
tail(iris)
summary(iris)
str(iris)
dim(iris)

set.seed(42)

# 2. 머신러닝

## 데이터 분리
sample <- sample.int(n = nrow(iris),
                     size = floor(.8 * nrow(iris)),
                     replace = F)
train <- iris[sample,]
test <- iris[-sample,]

## 모델 설계
### 랜덤 포레스트 - 중요 변수값 확인
#install.packages("randomForest")
library(randomForest)
names(train)
train_y = train[,'Species']
train_x = train[, names(train) != 'Species']

rf_model = randomForest(train_x, 
                        y = train_y,
                        ntree=100,
                        importance = T)
rf_model$importance

# 중요 변수값: Petal.Length, Petal.Width
