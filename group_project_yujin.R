# Group Project - 국내 외국인 노동자의 월 평균 임금이 소비행태에 미치는 영향

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
         earning = "월평균급여구간코드",
         remittance = "연간해외송금액구간코드",
         living_rate = "월평균지출_생활비비율",
         housing_rate = "월평균지출_주거비비율",
         remit_rate = "월평균지출_송금금액비율",
         saving_rate = "월평균지출_저축금액비율",
         etc_rate = "월평균지출_기타금액비율") %>% 
  filter(country == 2)

# 기초통계량 요약 : 분석에 사용된 변수들의 관측치, 평균, 표준편차, 최솟값, 최댓값 등

# 임금구간비중
earning1 <- (511/(511+2462+5981+2309))*100
earning2 <- (2462/(511+2462+5981+2309))*100
earning3 <- (5918/(511+2462+5981+2309))*100
earning4 <- (2309/(511+2462+5981+2309))*100


# table(file1$earning)
freqEarning <- xtabs(~ earning, data = file1); freqEarning
proportions(freqEarning)*100

# 송금액구간비중
remittance1 <- ((271/(271+643+426+539+1085+1192+1093+1373)))*100
remittance2 <- ((643/(271+643+426+539+1085+1192+1093+1373)))*100
remittance3 <- ((426/(271+643+426+539+1085+1192+1093+1373)))*100
remittance4 <- ((539/(271+643+426+539+1085+1192+1093+1373)))*100
remittance5 <- ((1085/(271+643+426+539+1085+1192+1093+1373)))*100
remittance6 <- ((1192/(271+643+426+539+1085+1192+1093+1373)))*100
remittance7 <- ((1093/(271+643+426+539+1085+1192+1093+1373)))*100
remittance8 <- ((1373/(271+643+426+539+1085+1192+1093+1373)))*100

# table(file1$remittance)
freqRemittance <- xtabs(~remittance, data = file1); freqRemittance
proportions(freqRemittance) * 100



# 시각화 관련 : 산점도 1개, 선 그래프 1개, 막대 그래프 1개, 지도 그래프 1개 



# 회귀분석 : 모형 선정의 근거, 결과표(단순선형회귀모형 1개, 다중선형회귀모형 2개)

