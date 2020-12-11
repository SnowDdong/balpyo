library(ggplot2)
library(dplyr)
library(ggcorrplot)
library(ggthemes)

# 데이터 불러오기
df <-read.csv("kids_accident.csv", header=T, stringsAsFactors=F)

# 데이터 변수 이름 변경 -- 한글 너무 길어서
df <- rename(df, year='년도', kids_a='어린이_교통사고.건.', kids_d='어린이_사망.명.',
             zone_a='스쿨존_교통사고.건.', zone_d='스쿨존_사망.명.', cnt_zone='스쿨존_지정.곳.')
df


# 어린이 사고vs 스쿨존 갯수 -- 정확히 보이는 부적 관계
ggplot(df) +
  geom_line(aes(x=year, y=kids_a, color="pink"), size=3) +
  geom_point(aes(x=year, y=kids_a, color="pink"), size=6) +
  geom_line(aes(x=year, y=cnt_zone, color="skyblue"), size=3) +
  geom_point(aes(x=year, y=cnt_zone, color="skyblue"), size=6) +
  theme_bw() + 
  ggtitle("★전국★ 연도별 어린이 교통사고와 스쿨존 관계") +
  xlab("발생 연도") + ylab("사고(건) & 스쿨존(수)") +
  scale_x_continuous(breaks = seq(2005,2019,by=2)) +
  scale_color_discrete(name="범례",labels = c("어린이 교통사고(건)", "스쿨존(수)"))


# 어린이 사망X3 vs 스쿨존 사고 -- 어린이 사망이 줄어드는데 비해 스쿨존 사고는 줄지X
df$kids_dx3 = df$kids_d * 3
df

ggplot(df) + 
  geom_line(aes(x=year, y=kids_dx3, color="pink"), size=3) +
  geom_point(aes(x=year, y=kids_dx3, color="pink"), size=6) +
  geom_line(aes(x=year, y=zone_a, color="skyblue"), size=3) +
  geom_point(aes(x=year, y=zone_a, color="skyblue"), size=6) +
  theme_bw() +
  ggtitle("★전국★ 연도별 어린이 사망과 스쿨존 교통사고 관계") +
  xlab("발생 연도") + ylab("사망(수) & 스쿨존 사고(건)") +
  scale_x_continuous(breaks = seq(2005,2019,by=2)) +
  scale_color_discrete(name="범례",labels = c("어린이 사망(수) x 3", "스쿨존 사고(건)"))
  

# 상관분석 데이터
df_c <- df[, c("kids_a","kids_d","zone_a","zone_d","cnt_zone")]
df_c

# 상관분석 함수 적용
df_cor <- cor(df_c)
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
           colors=c(low="pink",
                    mid="white",
                    high="aquamarine"),
           ggtheme=ggplot2::theme_bw())

