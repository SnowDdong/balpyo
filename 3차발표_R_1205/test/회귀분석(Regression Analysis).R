mpg$grade2<-ifelse(mpg$total>=30,"A",
                   ifelse(mpg$total>=25,"B",
                          ifelse(mpg$total>=20, "C", "D")))
head(mpg,20)
table(mpg$grade2)
qplot(mpg$grade2)
exam<- read.csv("csv_exam.csv")
exam
class(exam)
exam %>% filter(class ==1)
exam %>% filter(class==2)
exam %>% filter(class!= 1)
exam %>% filter(math > 50 & class ==1)
exam %>% filter(class %in% c(1,3,5))
exam %>% select(-math)
exam %>% select(class, -math, -english)
exam %>% filter(class==1) %>% 
  select(id, math) %>% 
  head(5)
exam %>% arrange(class, math)
exam %>% 
  mutate(total = math+english+science,
         mean = total/3) %>% 
  head

exam %>% 
  mutate(test=ifelse(science>=60, "pass", "fail"))  %>%  
  head


library(ggplot2)
ggplot(data=mpg, aes(x=displ, y=hwy))+
  geom_point()+
  xlim(3,6)+
  ylim(10,30)
head(mpg, 10)

library(dplyr)
df_mpg <- mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy))

df_mpg    
ggplot(data=df_mpg, aes(x=drv, y=mean_hwy)) + geom_col()

ggplot(data = df_mpg, aes(x=reorder(drv, -mean_hwy), y = mean_hwy)) + geom_col()

ggplot(data = mpg, aes(x=hwy))+ geom_bar()

1. 두 변수 rides와 overall의 상관관계를 분석하여 변수들이 서로 얼마나 밀접하게 직선적인 관계를 가지고 있는지 통계적 기법이다.
df<-read.csv("http://goo.gl/HKnl74")
str(df)
colSums(is.na(df))
attach(df)   # 데이터를 변수명으로 바로 접근할 수 있게 한다.
head(weekend)
plot(overall~rides)# 산점도 : 두 변수간의 관계
plot(overall~rides, main="전체만족도~놀이기구 만족도와관계", xlab="놀이기구 만족도", ylab="놀이동산 전체 만족도", cex =1, pch=1, col="red")
# 공분산(Covariance) 과 상관계수(Correlation Coefficient)
많은 data를 공분산 행렬로 표현 할수 있다.

5    4
4    6
공분산은 4이고, x 축보다 y축이 좀더 퍼져있다. 
5    -4
-4    6
공분산은 -4이고, 방향이 내려가는 방향으로 점들이 찍혀있다. 

5    0
0     1
공분산이 0이면 x와 y는 아루런 관계가 없다.

상관계수로우= 두 변수의 공분산(Cov(x,y))을 분산에 루트sqrt(Var(x)*Var(y)를 씌워서나타낸다.

cov(overall, rides) # 두변수가 양적으로 뚜렷한 선형관계가 있다. 
#상관관계는 두 변수 간의 관련성일뿐 원인과 결과의 방향은 알려주지 않는다.

# 공분산은 단위에 영향을 많이 받는다 그래서 표준화 시킨 상관계수
# 공분산은 방향을 알수있고,  상관계수는 얼마나 점들이 잘모여 있나를 알수있다.

# 상관계수 검증

cor.test(overall, rides)
귀무가설 " 상관관계가 없다"에 대한 검겅결과 

install.packages("corrplot")
library(corrplot)
cor(df[,4:8]) #상관계수

X<-cor(df[,4:8])
corrplot(X)


2. 회귀분석(regression analysis) 통계기반 데이터분석 p.26
한 개또는 그 이상의 변수들(독립변수)에 대하여 다른 한 변수(종속변수)사이의 관계를 수학적 모형을 이용하여 설명하고 예측하는 분석기법이다.

상관분석에서는 산점도의 점들의 분포를 통해 일정한 패턴을 확인한 후, 상관계수를 구하여 두 변수 간의 선형관계를 알 수 있었다. 여기서 더 나아가. 이 일정한 패턴을 활용하여 무엇인가 예측하는 분석을 회귀분석이라 한다.

상관분석을 통해 놀이기구에 대한 만족도(rides)와 전체만족도(overall) 간에 일정한 패턴을 확인하였고, 상관계수 또한 0.85로써 양의 선형관계를 이루었습니다.

회귀분석에는 종속변수1, 독립변수1 이면 일원분산분석
종속변수 1개, 독립변수2개 이면 이원분산분석
종속변수 2개이상이면 다변량분산분석으로 구분됩니다. (p.27)

단순회귀분석 y = b0+ b1x+ei
- 회귀식 추정: 두 변수X와 Y의 관계(rides, overall)에 적합한 회귀식을 구하기우해 관측값으로 부터 회귀계수 b0와 b1의 값을 추정하여야 한다. 
일반적으로 많이 사용하는 최소제곱법이라고 한다.

R에서 lm(Y~X)함수로 회귀식을 추정할 수 있다.

attach(df)


상관관계의 연장선에서 회귀분석을 하기위해 같은 데이터를 사용합니다.
df<-read.csv("http://goo.gl/HKnl74")
str(df)
colSums(is.na(df))
attach(df)
lm(overall~rides)
lm(formula = overall ~ rides)

Coefficients:
  (Intercept)        rides  
-94.962        1.703

b0 = -94.962, b1=1.703 으로부터 overall=-94.962+1.703*rides라는 회귀식을 구할 수 있다.

놀이기구에 대한 만족도(rides)가 1증가 할때 마다 전체만족도(overall)이 1.703만큼 증가한다고 볼수 있다.

이렇게 구해진 회귀직선을 산점도(scatter plot)위에 그려보면

m1<-lm(overall~rides)
plot(overall~rides, xlab="놀이기구에 대한 만족도", ylab="놀이동산 전체 만족도")
abline(m1, col='blue')
산점도위에 파란색의 회귀직선이 그려진 것을 확인 할 수 있다.

- 회귀모형의 검정 및 적합도 파악
회귀식이 통계적으로 유의한지, 변수가 유의하게 영향을 미치는지, 그리고 얼마만큼의 영향력을 가지는지 등의 여부를 확인해야 한다.

