# 두 변수가 어떤 식으로든 관계가 있는지 파악하는 가장 간단한 방법
#  - 공분산(covariance): 두 변수의 분산에 공통점이 있는지 확인

# 분산(s**2): 점수들이 대체로 평균과 얼마나 떨어져 있는지를 나타낸다.
#   s^2 : sum((x[i] - mean(x))^2) / (N - 1)

# 편차를 모두 더할때 발생하는 양의 편차와 음의편차가 상쇄되는 문제는
# 편차를 제곱하여 해결

# 변수가 두 개일 때는 각 편차를 제곱하는 대신 두 변수의 편차를 곱한다.
# 두 변수의 편차를 곱하는 것을 교차곱 편차(cross-product deviation)

# 공분산(convariance): 교차곱 편차들의 합을 관측값의 개수로 나눈다.
#   - cov(x,y): sum((x[i] - mean(x))*(y[i] - mean(y))) / (N - 1)
# 공분산이 양수일 경우 한 변수가 평균에서 이탈하면 다른 변수도 같은 방향으로 이탈한다는 의미
# 음수일 경우 한 변수가 평균에서 이탈하면 다른 변수는 그와 반대 방향으로 이탈한다는 의미

# 문제점으로 공분산은 측정의 축척에 의존한다
# 서로 다른 단위로 측정한 자료 집합들의 공분산들이 주어졌을 때 둘 중 어떤 것이 더 큰지 비교하는 것은 무의미

# 공분산을 어떤 표준 단위로 변화하기 위해 표준화(standardization)을 수행한다.
# 표준화 시 사용하는 측정 단위로 표준편차를 사용
# 표준편차는 분산과 같이 평균과의 차이들을 요약하는 개념
# 표준편차로 나누면 표준편차 단위가 된다.

# 공분산은 두 변수를 사용하기 때문에 각 변수의 표준편차를 곱한 값을 분모로 사용하면 된다.
# 표준화된 공분산을 상관계수(correlation coefficient)라 한다.
# r = cov(x,y) / s(x)*s(y)    - s(): 표준편차
# r은 피어슨 상관계수(Pearson correlation cofficient)라 부른다
# 상관계수는 -1에서 +1까지의 값이 된다

# 상관계수 검증 가설은 상관계수가 0이 아니라는 것이다.
# 검증하는 방법은 z점수를 사용하는 것이다
# 피어슨의 r에는 정규분포가 아닌 표집분포를 따른다는 문제점이 있다
# 이것을 해결하기위해 피셔의 공식을 사용한다
# z.r = ln((1 + r) / (1 - r)) / 2
# SE.z.r(표준오차) = 1 / sqrt(N - 3)
# Z = z.r / SE.z.r

# z 점수로 검증하기 보다는 자유도가 N-2인 t통계량을 이용하여 검증하는 것이 일반적
# t.r = (r*sqrt(N - 2)) / sqrt(1 - r^2)

# 상관의 두 종류
# 1. 이변량 상관(bivariate correlation): 두 변수의 상관관계
#   - 피어슨의 곱적률, 스피어먼의 로, 켄달의 타우
# 2. 편상관(partial correlation): 하나 이상의 다른 변수들의 효과를 '제어'할 때의 두 변수의 상관관계

# 상관 분석을 위한 R 패키지
#install.packages('Hmisc')
#install.packages('ggm')
#install.packages('polycor')
library(boot)
library(ggm)
library(ggplot2)
library(Hmisc)
library(polycor)

# 상관계수 함수: cor(), cor.test(), rcorr()(Hmisc 패키지)
# cor(): 피어슨, 스피어먼, 켄달, 다중상관
# cor.test(): 피어슨, 스피어먼, 켄달, p값, 신뢰구간
# rcor(): 피어슨, 스피어먼, p값, 다중상관(소수점 이하 두 자리까지)

examData <- read.delim('Andy/Chapter6/Exam Anxiety.dat')
cor(examData$Exam, examData$Anxiety, use = 'complete.obs', method = 'pearson')
cor(examData$Exam, examData$Anxiety, use = 'complete.obs', method = 'kendall')

rcorr(examData$Exam, examData$Anxiety, type = 'pearson')

cor.test(examData$Exam, examData$Anxiety, alternative = 'less', method = 'pearson',
         conf.level = 0.99)

# 피어슨 r의 가정들
# 자료가 구간 자료이어야 한다
# 검정통계량이 유효하려면 표집분포가 정규분포이어야 한다.
# 한 변수의 범주가 단 두개뿐인 범주형변수이면 조건을 무시할 수 있다.

examData2 <- examData[, c('Exam', 'Anxiety', 'Revise')]
cor(examData2)

examMatrix <- as.matrix(examData[, c('Exam', 'Anxiety', 'Revise')])
rcorr(examMatrix)

cor.test(examData$Anxiety, examData$Exam)

# R**2: 결정 계수(coefficient of determination)
#  - 상관계수를 제곱한 값, 한 변수의 변이성(variability) 또는 변동(variation)을
#    다른 변수가 어느 정도나 공유하는지 말해주는 측도도

cor(examData2)^2 * 100

# 스피어먼 상관계수(Spearman's correlation coefficient)는 비모수적 통계량
# 정규분포가 아닌 자료 등 모수적 자료의 가정들을 위반하는 자료에 사용가능
# 그리스 글자 p로 표기, 스피어먼의 로라고 부른다
# 스피어먼 상관계수 검정에서는 먼저 자료에 순위를 매기고 그 순위들에 피어슨 상관계수 공식을 적용

liarData <- read.delim('Andy/Chapter6/The Biggest Liar.dat')
cor(liarData$Position, liarData$Creativity, method = 'spearman')
cor.test(liarData$Position, liarData$Creativity, method = 'spearman')

# 켄달의 타우(Kendall's tau): 비모수적 상관계수
#  - 자료 집합의 크기가 작고 동순의 점수들이 많을 때 사용
#  - 점수들에 순위를 매겼을 때 같은 순위의 점수들이 많이 있다면 켄달의 타우를 사용
cor.test(liarData$Position, liarData$Creativity, alternative = 'less', method = 'kendall')

# 부트스트랩
bootTau<-function(liarData, i) {
  cor(liarData$Position[i], liarData$Creativity[i], use = 'complete.obs', method='kendall')
}
boot_kendall <- boot(liarData, bootTau, 2000)
boot_kendall
boot.ci(boot_kendall)

# 이연 상관과 점이연 상관
# 두 변수 중 하나가 이분적(dichotomous, 이항적) 변수일 때, 즉 범주가 단 두 개인 범주형변수일 때 쓰인다.
# 이연 상관, 점이연 상관 사용 여부는 이분적 변수가 이산적이냐 연속적이냐에 따라 결정
# r.pb로 표기하는 점이연 상관계수는 한 변수가 이산적인 이분적 변수(ex: 사망여부)일 때 사용
# r.b로 표기하는 이연 상관계수는 한 변수가 연속적인 이분적 변수(ex: 시험 통과/탈락)일 때 사용
catData <- read.csv('Andy/Chapter6/pbcorr.csv')
cor.test(catData$time, catData$gender)

catFrequencies <- table(catData$gender)
prop.table(catFrequencies)
polyserial(catData$time, catData$gender)

# 편상관(partial correlation)
#  - 다른 변수의 효과를 고정한 상태에서의 두 변수의 상관
# pcor(). pcor.test() 함수 사용(ggm 패키지)
# Exam, Anxiety의 편상관계수, Revise는 제어 변수수
pcor(c('Exam', 'Anxiety', 'Revise'), var(examData2))
pc <- pcor(c('Exam', 'Anxiety', 'Revise'), var(examData2))
pc^2

# pcor, 제어 변수 개수, 표본 크기
pcor.test(pc, 1, 103)

# 준평상관(부분 상관): 제3의 변수가 두 변수 중 한 변수에만 미치는 효과를 제어한 상태에서 두 변수의 관계를 수량화

# 독립적인 r들의 비교
# z.차이 = (z.r1 - z.r2) / sqrt((1/N1-3)+(1/N2-3))