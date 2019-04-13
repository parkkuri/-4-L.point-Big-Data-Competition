setwd('C:/r_temp')

#데이터 삽입
b<-read.table('b.txt',sep=',',header=T)
f<-read.csv('f.csv',sep=',',header=T)
#점포코드, 상품소분류코드,소분류명 추출
f_so<-f[c(1,2,3)]
#고객 식별번호, 점포코드, 상품소분류코드, 구매가 발생한 일자 추출
b<-b[,c(1,3,4,6)]
#쇼핑업종상품구매정보에서 대형마트의 정보 추출
b_A02<-subset(b, BIZ_UNIT=='A02')
#쇼핑업종 상품분류정보에서 대형마트의 정보 추출
f_so_A02<-subset(f_so,BIZ_UNIT=='A02')
#대형마트 정보만을 기반으로 쇼핑업종상품구매정보와 쇼핑업종 상품분류정보 합침
mer_A02<-merge(b_A02,f_so_A02,by.x='PD_S_C',by.y='PD_S_C')
#저ㅈ
write.csv(mer_A02,'A02.csv')

library(dplyr)

#ID와 날짜별로 정렬
mer_A02_order<-mer_A02[order(mer_A02$ID,mer_A02$DE_DT),]
#ID와 날짜, 상품소분류코드 추출
mer_A02_order<-mer_A02_order[c('ID','DE_DT','PD_S_NM')]
#상품소분류코드 자료형 캐릭터로 바꾸기
mer_A02_order[,3]<-as.character(mer_A02_order[,3])
#첫줄 넣어주기
ss<-mer_A02_order[1,3]
#매트릭스로 바꾸기
ss<-as.matrix(ss)
#행이 몇개인지 파악
nrow(mer_A02_order)

head(mer_A02_order)
# ID와 날짜가 같으면 콤마로 한줄로 붙이고, 다르면 다음행으로 붙이는 함수 돌리기
for (i in 1:1727091){
  if (mer_A02_order[i,1]==mer_A02_order[i+1,1]&mer_A02_order[i,2]==mer_A02_order[i+1,2]){
    ss[nrow(ss),1]<-paste(ss[nrow(ss),1],mer_A02_order[i+1,3],sep=',')}
      else {
        ss<-rbind(ss,mer_A02_order[i+1,3])
      } 
}

#변수이름 바꾸기
final<-ss


library(dplyr)
library(stringr)


#재사용봉투 없애기
final1<-str_replace_all(final,",재사용봉투","")


#재사용 봉투 있는 채로 콤마로 분리
q<-lapply(final,strsplit,split=",")
#재사용 봉투 없는 채로 콤마로 분리
q_1<-lapply(final1,strsplit,split=",")

#list 풀기
q<-lapply(q,unlist)
#list 풀기
q_1<-lapply(q_1,unlist)

library(arules)

#재사용봉투를 포함한 채로 연관분석
rule<-apriori(q, parameter =list(support=0.003, confidence = 0.2, minlen =2))
#재사용봉투 포함하지 않은 채로 연관분석
rule2<-apriori(q_1, parameter =list(support=0.003, confidence = 0.2, minlen=2))


#연관분석 한 것 보기
inspect(rule)
inspect(rule2)

library(arulesViz)

#연관분석 한 것 데이터프레임으로 변환
rule3<-as.data.frame(inspect(rule2))
rule4<-as.data.frame(inspect(rule))

#연관분석한 것 저장
write.csv(rule3,'rule.csv')
write.csv(rule4,'rule2.csv')
#연관분석 시각화
plot(rule2, method="graph", control=list(type="items"))
