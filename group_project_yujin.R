# Group Project - 

# library
library(readxl)
library(dplyr)
library(ggplot2)
library(jtools)
library(broom)
library(fastDummies)

setwd("C:/Users/이유진/Desktop/Project")
original <- read.csv("이민자체류실태및고용조사.csv")

# 전처리
file1 <- original %>%
  select("응답자국적코드", 
         "배우자혼인상태코드", 
         "배우자한국거주여부", 
         "배우자국적코드", 
         "월평균급여구간코드",
         "연간해외송금액구간코드",
         "월평균지출_생활비비율",
         "월평균지출_주거비비율",
         "월평균지출_송금금액비율",
         "월평균지출_저축금액비율",
         "월평균지출_기타금액비율") %>% 
  rename(country = "응답자국적코드", 
         marriage = "배우자혼인상태코드", 
         partner_kor_living = "배우자한국거주여부", 
         partner_country = "배우자국적코드", 
         income = "월평균급여구간코드",
         remittance = "연간해외송금액구간코드",
         living_rate = "월평균지출_생활비비율",
         housing_rate = "월평균지출_주거비비율",
         remit_rate = "월평균지출_송금금액비율",
         saving_rate = "월평균지출_저축금액비율",
         etc_rate = "월평균지출_기타금액비율")


