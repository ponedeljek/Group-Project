setwd("C:/Users/이유진/Desktop/Project")
library(readxl)
library(dplyr)
library(fastDummies)
library(jtools)
library(broom)
# 원본 데이터 백업
original <- read.csv("이민자체류실태및고용조사.csv", header = T, fileEncoding = "euc-kr")
# 데이터 전처리 #
# 데이터 선택
file1 <- original %>%
  select("응답자국적코드",
         "배우자혼인상태코드",
         "월평균급여구간코드",
         "연간해외송금액구간코드",
         "월평균지출_생활비비율",
         "월평균지출_주거비비율",
         "월평균지출_송금금액비율",
         "월평균지출_저축금액비율",
         "월평균지출_기타금액비율",
         "연령계층코드",
         "성별코드",
         "거주취업학업목적한국입국시기구분코드",
         "X7차직업분류재분류코드",
         "교육정도코드",
         "현재체류자격재분류코드",
         "거주지역권역코드")%>%
  rename(country = "응답자국적코드",
         marriage = "배우자혼인상태코드",
         earning = "월평균급여구간코드",
         remittance = "연간해외송금액구간코드",
         living_rate = "월평균지출_생활비비율",
         housing_rate = "월평균지출_주거비비율",
         remit_rate = "월평균지출_송금금액비율",
         saving_rate = "월평균지출_저축금액비율",
         etc_rate = "월평균지출_기타금액비율",
         age="연령계층코드",
         gender="성별코드",
         entry_time="거주취업학업목적한국입국시기구분코드",
         job="X7차직업분류재분류코드",
         edu="교육정도코드",
         visa="현재체류자격재분류코드",
         area="거주지역권역코드")

# 결측 제거 및 표본 한정
file2<-file1%>%
  filter(country==2&!is.na(country)&!is.na(marriage)&
           !is.na(earning)&earning>1&!is.na(living_rate)&
           !is.na(housing_rate)&!is.na(remit_rate)&!is.na(saving_rate)&
           !is.na(etc_rate)&!is.na(age)&!is.na(gender)&
           !is.na(remittance)&!is.na(area)&
           !is.na(entry_time)& entry_time>=5 & !is.na(job)&!is.na(edu)&
           !is.na(visa)&visa<=3)
# 더미 변수 만들기
file2$earning<-ifelse(file2$earning==2, 1,
                      ifelse(file2$earning==3, 2,3))
dum_file2 <-dummy_cols(file2,
                       select_columns = c('marriage','earning','remittance','age','gender','entry_time','job','edu','visa','area'),
                       remove_selected_columns = T)
# [표3]기초통계량 요약- 월평균 급여구간
round(prop.table(table(file2$earning)),digits = 2)
# [표4] 기초통계량 요약-연간해외송금액구간
round(prop.table(table(file2$remittance)),digits = 2)
# [표5] 기초통계량 요약 – 월평균지출 생활비비율, 주거비비율, 송금금액비율, 저축금액비율, 기타금액비율
summary(file2$remit_rate)
sd(file2$remit_rate)
summary(file2$living_rate)
sd(file2$living_rate)
summary(file2$housing_rate)
sd(file2$housing_rate)
summary(file2$saving_rate)
sd(file2$saving_rate)
summary(file2$etc_rate)
sd(file2$etc_rate)
# [그림 1] 외국인 노동자의 월평균 임금 막대그래프
ggplot(file2, aes(earning))+
  geom_histogram(binwidth=0.5)+
  labs(x="소득구간",title="월평균임금구간별분포")+ scale_x_continuous(breaks = seq(1,3,1))
# [그림 2] 외국인 노동자의 연간 해외 송금액 막대그래프
ggplot(file2,aes(x=remittance))+theme_bw()+geom_bar()+coord_flip()
# [그림3] 2010-2020 입국한 외국인 노동자의 평균 해외 송금액 비율 
file2_line <- file2 %>% 
  group_by(entry_time) %>% 
  summarize(avg_remit_rate= mean(remit_rate))

ggplot(file2_line, aes(x = entry_time, y = avg_remit_rate)) + geom_line() +
  scale_x_continuous(breaks = c(5.0, 10.0, 15.0),
                     labels = c("2010","2015","2020"))
# [그림4] 입국시기별 입국자 수 막대그래프
ggplot(file2, aes(entry_time))+geom_histogram(binwidth = 0.5)+
  scale_x_continuous(breaks=c(5, 10, 15),labels=c("2010", "2015", "2020"))+
  labs(x="입국시기", y="입국자수", title="입국시기별 입국자수 분포포")
#[그림5] 전국 외국인 노동자의 평균 해외 송금액 비율 지도
library(ggmap)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(broom)

map_shp <- shapefile("TL_SCCO_CTPRVN.shp")
file_map<-file2 %>%
  rename(id = "area")
map_shp <- spTransform(map_shp,
                       CRS("+proj=longlat"))
map <- tidy(map_shp)
map<-fortify(map,region="SIG_CD")

file_map<-file_map%>%
  group_by(id)%>%
  summarize(avg_remit_rate= mean(remit_rate))

map$id[map$id==0] <- 11
map$id[map$id==1] <- 21
map$id[map$id==2] <- 23
map$id[map$id==3] <- 12
map$id[map$id==4] <- 24
map$id[map$id==5] <- 22
map$id[map$id==6] <- 21
map$id[map$id==7] <- 22
map$id[map$id==8] <- 13
map$id[map$id==9] <- 25 
map$id[map$id==10] <- 22 
map$id[map$id==11] <- 22
map$id[map$id==12] <- 24
map$id[map$id==13] <- 24
map$id[map$id==14] <- 23
map$id[map$id==15] <- 21
map$id[map$id==16] <- 25

map$id<-as.integer(map$id)

foreign_remitrate_map <- left_join(file_map, map, by = "id")

ggplot() +
  geom_polygon(data = foreign_remitrate_map,
               aes(x = long, y = lat,
                   group = group,              
                   fill =avg_remit_rate)) +
  scale_fill_gradient(low = "#C3D7A4" , high = "#52854C") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())

# 단순선형회귀분석
lm_1<-dum_file2 %>%
  lm(remit_rate~earning_1+earning_3, .)%>%jtools::summ()
lm_2<-dum_file2 %>%
  lm(living_rate~earning_1+earning_3, .)%>%jtools::summ()
lm_3<-dum_file2 %>%
  lm(housing_rate~earning_1+earning_3, .)%>%jtools::summ()
lm_4<-dum_file2 %>%
  lm(saving_rate~earning_1+earning_3, .)%>%jtools::summ()
# 결과표 csv로 정리
lm1_out <- tidy(lm_1)
write.csv(lm1_out, "lm_1.csv")
lm2_out<-tidy(lm_2)
write.csv(lm2_out, "lm_2.csv")
lm3_out<-tidy(lm_3)
write.csv(lm3_out, "lm_3.csv")
lm4_out<-tidy(lm_4)
write.csv(lm4_out, "lm_4.csv")

# 다중회귀분석: 모형 선정의 근거, 결과표
file2$marriage<-as.factor(file2$marriage)
file2$earning <- as.factor(file2$earning)
file2$age <- as.factor(file2$age)
file2$gender <- as.factor(file2$gender)
file2$job <- as.factor(file2$job)
file2$edu <- as.factor(file2$edu)
file2$visa <- as.factor(file2$visa)
file3 <- file2 %>% select(marriage, earning,  gender, job, edu, visa,remit_rate, age)
lm_5 <- lm(remit_rate~.,file3)
lm5_out <- tidy(lm_5)
write.csv(lm5_out, "lm_5.csv")

forw<- lm(remit_rate~ 1, file3)
back<-lm(remit_rate~.,file3)
lm_6<- step(forw, direction = "forward", scope = list(lower=forw, upper=back))
lm6_out <- tidy(lm_6)
write.csv(lm6_out, "lm_6.csv")

#[그림6]교육정도에 따른 송금금액 비율
ggplot(file3,aes(edu, remit_rate,color=edu))+geom_point()+labs(x="교육정도",y="송금금액비율",
                                                     title="교육정도에따른송금금액비율")+geom_boxplot()
