library(ggplot2)
library(dplyr)
library(stringr)
library(readxl)
cctv <- read.csv('cctv.csv',fileEncoding = "UCS-2LE")
#cctv csv 불러와 읽음 .fileencoding으로 ucs-2le를써야 한글파일이열림

head(cctv)
#잘불러와졋는지 확인

cctv1<-cctv %>% select(기관명,X2014년,X2015년,X2016년,X2017년,X2018년)
#필요로하는 열만 사용하기위해 select 명령어사용하여 cctv1이라는 새로운변수에담음

head(cctv1)
#cctv1이라는 변수에 필요한열만 들어와져있느지 확인

cctv1<-rename(cctv1,'자치구'=기관명,'2014년'=X2014년,'2015년'=X2015년,'2016년'=X2016년,'2017년'=X2017년,'2018년'=X2018년)
#cctv1 에 새로운 컬럼명을 부여 

head(cctv1)
#cctv1에 새로운컬럼명이 부여되었는지확인

is.na(cctv1)
#null 값 있는지 확인

table(is.na(cctv1))
#null값과 null값이 아닌값을 확인해줌 false true로 나옴.

cctv2 <- na.omit(cctv1)
#cctv2라는 변수에 null값을 제거해주는 na.omit이라는 명령어를사용하여 null값이 제거된데이터를 담음

str(cctv2)
#cctv2에 str명령어를 사용하여 타입 확인 2014년이 문자열로 되어있는것을 확인 나머지는 num 타입(float).

cctv2$'2014년' = str_replace(cctv2$'2014년',',','')
#2014에 , 를 제거해 주고 값을 붙여줌

cctv2$'2014년' <- as.numeric(cctv2$'2014년')
#2014 년 데이터를 num타입으로 변환

is.numeric(cctv2$'2014년')
#2014년 데이터가 num 타입으로 변환되었는지 확인

str(cctv2)
#str명령어를 사용하여 타입 확인 num 으로 변환됨

View(cctv2)
#cctv2데이터를 view명령어를사용하여 확인 

cctv2$'합계'<-(cctv2$'2014년'+cctv2$'2015년'+cctv2$'2016년'+cctv2$'2017년'+cctv2$'2018년')
#cctv2에 합계라는 새로운 컬럼명을 만들어 2014~2018년까지의 cctv데이터를 합침.

head(cctv2)
#ccctv2 합계를추가하고 추가되었는지 확인

View(cctv2)
ggplot(data=cctv2, aes(x = 합계, y = 자치구,fill=자치구))+ggtitle('서울시 구별 cctv')+geom_bar(stat="identity")
#막대그래프를 이용하여 구별 2014~2018년 cctv 합계 를 표현 

report <-read.csv('report.csv',fileEncoding = "UCS-2LE",sep='\t') #encoding ucs-2le사용해야 한글파일이 열림.
head(report)

report2<-report[-1,] #1번째 행 제거 

head(report2)
#1번째행 제거되었는지 확인.
report2<-rename(report2,'발생 합계'=합계,'검거 합계'=합계.1,'살인 발생'=살인, 
                '살인 검거'= 살인.1,'강도 발생'=강도,'강도 검거'=강도.1,'강간강제추행 발생'=강간강제추행,'강간강제추행 검거'=강간강제추행.1,'절도 발생'= 절도,'절도 검거'=절도.1,'폭력 발생'=폭력,'폭력 검거'=폭력.1)
#새로운 컬럼명을 줌.
head(report2)
#새로운 컬럼명이 지정되었는지 확인.
report3<- report2 %>% filter(기간!=2019)# 2019년도 제거 

tail(report3) #2019 제거 되었는지확인

report3$`발생 합계` = str_replace(report3$`발생 합계`,',','')    
report3$`검거 합계` = str_replace(report3$`검거 합계`,',','')
report3$`강간강제추행 발생`= str_replace(report3$`강간강제추행 발생`,',','')
report3$`강간강제추행 검거` = str_replace(report3$`강간강제추행 검거`,',','')
report3$`절도 발생` = str_replace(report3$`절도 발생`,',','')
report3$`절도 검거` = str_replace(report3$`절도 검거`,',','')
report3$`폭력 발생` = str_replace(report3$`폭력 발생`,',','')
report3$`폭력 검거` = str_replace(report3$`폭력 검거`,',','')
report3$`발생 합계`<-as.integer(report3$`발생 합계`)
report3$`검거 합계`<-as.integer(report3$`검거 합계`)
report3$`살인 발생`<-as.integer(report3$`살인 발생`)
report3$`살인 검거`<-as.integer(report3$`살인 검거`)
report3$`강도 발생`<-as.integer(report3$`강도 발생`)
report3$`강도 검거`<-as.integer(report3$`강도 검거`)
report3$`강간강제추행 발생`<-as.integer(report3$`강간강제추행 발생`)
report3$`강간강제추행 검거`<-as.integer(report3$`강간강제추행 검거`)
report3$`절도 발생`<-as.integer(report3$`절도 발생`)
report3$`절도 검거`<-as.integer(report3$`절도 검거`)
report3$`폭력 발생`<-as.integer(report3$`폭력 발생`)
report3$`폭력 검거`<-as.integer(report3$`폭력 검거`)
report3$기간<-as.integer(report3$기간)
#,제거해주고 int 타입으로 변환

str(report3)#변환되었는지확인
tail(report3) # tail 찍어서 자치구에 합계라는 열이 있음.
report3<-report3[!(report3$자치구 == "합계" ),] #합계 필요없어 삭제
View(report3) #삭제되었는지확인
report4<- report3 %>% arrange(report3$자치구) #이름별  오름차순 정렬
View(report4) #오름차순되었는지 확인

ggplot(report4) +ggtitle('서울시 기간별 범죄율 히트맵')+ geom_tile(aes(x = 기간, y = 자치구, fill = `발생 합계`),alpha = 0.6) + scale_fill_gradient2(low = 'white', high = 'red') + theme_classic() #서울시 연도별 범죄율 확인 히트맵.

report5<- report4 %>% group_by(자치구) %>% summarise(발생총합=sum(`발생 합계`),검거총합=sum(`검거 합계`))
#자치구별 발생합계 검거합계 총합구하기.
head(report5)
#값 확인
report5$'검거율' <- report5$검거총합/ report5$발생총합
#검거율이라는 새로운 컬럼 만들고 그값으로 검거총합/발생총합을 해줌

head(report5)
dim(report5)
View(report5)
#값이 들어갔는지확인

ggplot(data=report5, aes(x = 검거율, y =자치구,fill=자치구))+ggtitle('서울시 검거율')+geom_bar(stat="identity")
# 자치구별 검거율 확인하는 막대그래프 
ggplot(data=cctv2, aes(x = 합계, y = 자치구 ,fill=자치구))+ggtitle('서울시 구별 cctv')+geom_bar(stat="identity")
#구별 cctv 설치현황 확인 막대그래프 

cctv2$'자치구'=str_replace_all(cctv2$'자치구',' ',"")
#cctv 자치구는 '강 동 구' 글자마다 띄어쓰기가 되어있고 report 자치구는 강동구 글자가 붙어있어 공통되게만들어줌.
#공통되게 만들어주지않을시 데이터가 불러와지지않음.
seoul <- left_join(cctv2,report5,by="자치구")
#left join 명령어를사용해 cctv2와 report5데이터를 붙이고 자치구를 기준으로했다.
head(seoul)
# 찍어보니 결과가나옴

attach(seoul)
#데이터를 R 검색 경로에 추가하여 변수명으로 바로 접근할 수 있게 한다.
head(검거율)  
#attach가 작동하는지확인
plot(합계~검거율,main="서울시 cctv와 검거율의 관계", xlab="14~18년도 검거율", ylab="14~18년도 구별cctv 합계", cex =1, pch=1, col="red")
#합계~검거율 x합계 y 검거율 로 산점도 표시
cor.test(seoul$합계,seoul$검거율)
#검정을통해 pvalue값이 0.2392로 0.05보다 큼.
# cctv 많다고 검거 잘되는거 아니다. 
plot(seoul)
#전체의 내용을 보여준다 


