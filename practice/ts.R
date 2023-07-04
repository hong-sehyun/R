# 미국 나스닥 분석


## 주제 : 확정해야 됨

## 0. 패키지 불러오기
library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)
library(corrr)
library(rstatix)
library(prophet)
library(astsa)
library(forecast)

library(sysfonts)
library(showtext)

## 1. 데이터 프레임 작성
# 파일 목록을 다 들고 와야됨
files <- list.files(path = "data/nasdaq_stock/")

# 들고온 파일 목록을 다 읽어서, 데이터 프레임
stocks <- read_csv(paste0("data/nasdaq_stock/", files), id = "name") %>%
  mutate(name = gsub("data/nasdaq_stock/", "", name), 
         name = gsub("\\.csv", "", name)) %>%
  rename_with(tolower)
## 인덱스가 사라지고 date가 옴. 
## name을 하나 파서 임의로 만듦

# 데이터 프레임을 결합
df <- read_csv("data/nasdaq_stock_names.csv")

stocks <- stocks %>%
  inner_join(df, by = c("name" = "stock_symbol"))

stocks

## 2. 시계열 데이터 시각화
end_labels <- (stocks %>%
  group_by(company) %>%
  filter(date == max(date)) %>%
  arrange(-open) %>%
  select(open, company))[c(1:3,12:14),]

# 좀 더 해봐요!
stocks %>%
  ggplot(aes(date, open)) +
  geom_line(aes(color = company)) +
  scale_y_continuous(sec.axis = sec_axis(~., breaks = end_labels$open, 
                                         labels = end_labels$company)) +
  scale_x_date(expand = c(0,0)) +
                 labs(x = "", y = "Open", color ="", title = "주요 회사의 시장가격") +
                 theme(legend.position = "none")

(stocks %>%
    filter(company %in% end_labels$company[1:3]) %>%
    ggplot(aes(date, open)) +
    geom_line(aes(color = company)) +
    facet_wrap(~company) +
    theme_bw() +
    theme(legend.position = "none") +
    labs(title = "Top 3", x = "")) /
  (stocks %>%
     filter(company %in% end_labels$company[-(1:3)]) %>%
     ggplot(aes(date, open)) +
     geom_line(aes(color = company)) +
     facet_wrap(~company) +
     theme_bw() +
     theme(legend.position = "none") +
     labs(title = "Bottom 3", x = ""))


# 시계열

stocks %>%
  filter(name == "AAPL") %>%
  select(ds = date, y= open)


(aapl %>% 
    mutate(diff = c(NA, diff(y))) %>% 
    ggplot(aes(ds, diff)) + 
    geom_point(color = "steelblue4", alpha = 0.7) +
    labs(y = "Difference", x = "Date",
         title = "One Day Returns")
) /
  (aapl %>% 
     mutate(diff = c(NA, diff(y))) %>% 
     ggplot(aes(diff)) +
     geom_histogram(bins = 50, fill = "steelblue4", color = "black")
  )


## 3. 시계열 데이터 분석

## 4. 증가율 예측