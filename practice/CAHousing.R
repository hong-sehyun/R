library(tidyverse)
library(reshape2)

# 1. 데이터 불러오기
housing = read_csv("data/CAhousing.csv")

## 데이터 확인
head(housing)
tail(housing)
summary(housing)
str(housing)

## EDA를 위한 간단한 시각화

### "가정과 결과"를 정의해서 해당 문제를 빠르게 해결

plot_histo <- ggplot(data = melt(housing), mapping = aes(x = value)) +
  geom_histogram(bins=30) +
  facet_wrap(~variable, scales = 'free_x')
plot_histo

ggplot(data = housing, 
       mapping = aes(x = longitude, y = latitude,
                     color = median_house_value)) +
  geom_point(aes(size = population), alpha=0.4)

# 2. 전처리

## 데이터 결측치
housing$total_bedrooms[is.na(housing$total_bedrooms)] <- median(housing$total_bedrooms, na.rm = T) 
housing$mean_bedrooms <- housing$total_bedrooms / housing$households
housing$mean_rooms <- housing$total_rooms / housing$households
head(housing)
drops <- c('total_bedrooms', 'total_rooms')
housing <- housing[,!(names(housing) %in% drops)]
housing

## 범주형(OneHot Encoding)
categories <- unique(housing$ocean_proximity)
cat_housing <- data.frame(ocean_proximity = housing$ocean_proximity)
head(cat_housing)

for(cat in categories) {
  cat_housing[,cat] = rep(0, times=nrow(cat_housing))
}

for(i in 1:length(cat_housing$ocean_proximity)) {
  cat <- as.character(cat_housing$ocean_proximity[i])
  cat_housing[,cat][i] <- 1
}

head(cat_housing)

cat_names <- names(cat_housing)
keep_columns <- cat_names[cat_names != "ocean_proximity"]
cat_housing <- select(cat_housing, one_of(keep_columns))
tail(cat_housing)

## 결측치 처리(수치형)

colnames(housing)
drops <- c("ocean_proximity", "median_house_value")
housing_num <- housing[, !(names(housing) %in% drops)]
colnames(housing_num)

scaled_housing_num <- scale(housing_num)
head(scaled_housing_num)

## 결합
head(cat_housing)
head(scaled_housing_num)
head(housing$median_house_value)

cleaned_housing <- cbind(cat_housing, 
                         scaled_housing_num, 
                         median_house_value=housing$median_house_value)

head(cleaned_housing)

# 3. 머신러닝
set.seed(42)

## 데이터 분리
sample <- sample.int(n = nrow(cleaned_housing),
                     size = floor(.8 * nrow(cleaned_housing)),
                     replace = F)

train <- cleaned_housing[sample,]
test <- cleaned_housing[-sample,]

nrow(train) + nrow(test) == nrow(cleaned_housing)

## 모델 설계
### 선형 모델
#install.packages("boot")
library(boot)
colnames(cleaned_housing)
glm_house <- glm(median_house_value~median_income+mean_rooms+population, 
                 data=cleaned_housing)
k_fold_cv_error <- cv.glm(cleaned_housing, glm_house, K=5)
k_fold_cv_error$delta

glm_cv_rmse = sqrt(k_fold_cv_error$delta)[1]
glm_cv_rmse

glm_house$coefficients

### 랜덤 포레스트
# install.packages("randomForest")
library(randomForest)

### 중요 변수값 확인
names(train)
train_y = train[,'median_house_value']
train_x = train[, names(train) != 'median_house_value']

rf_model = randomForest(train_x, 
                        y = train_y,
                        ntree=500,
                        importance = T)
rf_model$importance



test_y = test[,'median_house_value']
test_x = test[, names(test) != 'median_house_value']
y_pred = predict(rf_model, test_x)
test_mse = mean((y_pred - test_y)^2)
test_mse
test_rmse = sqrt(test_mse)
test_rmse


#xgboost
library(xgboost)

dtrain = xgb.DMatrix(data = as.matrix(train_x), label = train_y)
dtest = xgb.DMatrix(data = as.matrix(test_x), label = test_y)
watchlist = list(train=dtrain, test=dtest)

bst <- xgb.train(data = dtrain, 
                 max.depth = 8,
                 eta = 0.3,
                 nthresd = 2,
                 nround = 1000, 
                 watchlist = watchlist,
                 objective = "reg:squarederror",
                 early_stopping_rounds = 50,
                 print_every_n = 500)

## 학습

# 4. 결과 확인
#XGBoost를 활용한 RMSE 값이 가장 낮고(48403->4810), 주요 특정값은 '
# inland 가 가장 영향이 큼


























