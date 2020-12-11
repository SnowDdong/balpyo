library(ggplot2)
library(dplyr)
library(ggcorrplot)
library(ggthemes)

# 데이터 불러오기
df <-read.csv("cj_schoolzone_Data2.csv", header=T, stringsAsFactors=F)
head(df)

# 필요한 데이터만 추출
data = df[,c('지역구', '대상시설명', '신호등', 'CCTV설치여부', '발생건수', '사망자수')]

# 데이터 변수 이름 변경 -- 한글 너무 길어서
data <- rename(data, gu='지역구', zone='대상시설명', lamp='신호등',cctv='CCTV설치여부', 
               cnt_a='발생건수', cnt_d='사망자수')
head(data)

# 데이터에 수치 부여
data$lamp <- ifelse(data$lamp == "Y", 1, 0)
data$cctv <- ifelse(data$cctv == "Y", 1, 0)
data$zone <- ifelse(data$zone == "", 0, 1)
data <- data[,c('gu','zone','lamp','cctv','cnt_a','cnt_d')]


# 데이터별 합계
data_sum <- data %>% 
  group_by(gu) %>% 
  summarise(zone = sum(zone),
            lamp = sum(lamp),
            cctv = sum(cctv),
            cnt_a = sum(cnt_a),
            cnt_d = sum(cnt_d))
data_sum

# 지역구별 스쿨존 갯수 (bar 그래프)
ggplot(data_sum) +
  aes(x=gu, y=zone, fill=gu) + 
  geom_col(width = 0.7) +
  theme_economist_white() +
  scale_fill_brewer(palette = "Set2") + 
  ggtitle("★청주시★  지역구별 스쿨존 수") + 
  xlab("지역구") + ylab("스쿨존 수") + labs(fill="지역구")

# str(data_sum)
# dput(data_sum)

#지역구별 데이터 (line 그래프)
ggplot(data_sum) +
  geom_line(aes(x=gu, y=zone, color="royalblue"),size=3, group = 1) +
  geom_line(aes(x=gu, y=lamp, color="orange"),size=3, group=2) +
  geom_line(aes(x=gu, y=cctv, color="greenyellow"),size=3, group=3) +
  geom_line(aes(x=gu, y=cnt_a, color="darkcyan"),size=3, group=4) +
  geom_line(aes(x=gu, y=cnt_d, color="violetred"),size=3, group=5) +
  theme_bw() +
  ggtitle("★청주시★  지역구별 데이터") + 
  xlab("지역구") + ylab("각종 데이터 (건)수") +
  scale_color_discrete(name="",labels = c("교통사고(건)", "CCTV 수", "신호등 수", 
                                          "스쿨존 수", "사망자 수", "어린이 수"))
  

# 청주시 어린이 인구 수 추가 (단위 천명)
data_sum$kids = c(21.40,20.14,26.67,32.34)
data_sum

# 어린이 인구 추가된 지역구별 데이터 (line 그래프)
ggplot(data_sum)+
  geom_line(aes(x=gu, y=zone, color="royalblue"),size=3, group = 1) +
  geom_line(aes(x=gu, y=lamp, color="orange"),size=3, group=2) +
  geom_line(aes(x=gu, y=cctv, color="greenyellow"),size=3, group=3) +
  geom_line(aes(x=gu, y=cnt_a, color="darkcyan"),size=3, group=4) +
  geom_line(aes(x=gu, y=cnt_d, color="violetred"),size=3, group=5) +
  geom_line(aes(x=gu, y=kids, color="yellow"), size=3, group=6) +
  theme_bw() +
  ggtitle("★청주시★  지역구별 데이터(어린이 수 추가)") + 
  xlab("지역구") + ylab("각 데이터 건(수)") + 
  scale_color_discrete(name="",labels = c("교통사고(건)", "CCTV 수", "신호등 수", 
                                            "스쿨존 수", "사망자 수", "어린이 수"))


# 상관분석 데이터
data_sum$kids_r = round(data_sum$kids / data_sum$zone,3)
data_sum$lamp_r = round(data_sum$lamp / data_sum$zone,3)
data_sum$cctv_r = round(data_sum$cctv / data_sum$zone,3)
data_sum$cnt_a_r = round(data_sum$cnt_a / data_sum$zone,3)
data_sum$cnt_d_r = round(data_sum$cnt_d / data_sum$zone,3)
data_sum

df <- data_sum[, c("kids_r","lamp_r","cctv_r","cnt_a_r")]
df

# 상관분석 함수 적용
df_cor <- cor(df)
df_cor

# 히트맵
ggcorrplot(df_cor, 
           method="square", 
           type="lower", 
           show.legend=T,
           legend.title="Pearson\nCorrelation",
           show.diag=T,
           outline.color="white",
           lab=T,
           lab_size = 5,
           title="Correlation Heatmap",
           tl.cex=12,
           colors=c(low="yellow",
                    mid="white",
                    high="gold"),
           ggtheme=ggplot2::theme_bw())

