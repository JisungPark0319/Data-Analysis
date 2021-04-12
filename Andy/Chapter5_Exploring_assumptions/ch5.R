#install.packages('car')
#install.packages('ggplot2')
#install.packages('pastecs') # 기술통계학용
#install.packages('psych')

library(car)
library(ggplot2)
library(pastecs)
library(psych)

dlf <- read.delim('Andy/Chapter5/DownloadFestival(No Outlier).dat', header = TRUE)

################ 정규성 검정 ################

################ 1. 시각화 ################
# 1) 밀도 히스토그램에 정규 곡선을 그려 비교
# 2) Q-Q plot
###########################################

# 1) 밀도 히스토그램에 정규 곡선을 그려 비교
# 의생 자료 점수의 히스토그램
hist.day1 <- ggplot(dlf, aes(day1)) + theme(legend.position = 'none') +
  geom_histogram(aes(y = ..density..), colour = 'black', fill = 'white') +      # ..density..: 도수분포가 아닌 밀도그림을 그리라는 의미
  labs(x = 'Higiene score on day 1', y = 'Density')

hist.day1

# 정규분포의 곡선(정규 곡선) 표시
# 비교를 위해 위생 점수 자료의 평균과 표준편차를 입력
# 입력된 평균과 표준편차의 정규분포에서 주어진 값에 해당하는 확률(즉, 밀도)을 출력
hist.day1 + stat_function(fun = dnorm, args = list(mean = mean(dlf$day1, na.rm = TRUE),
                                                   sd = sd(dlf$day1, na.rm = TRUE)),
                          colour = 'black', size = 1)

# 2) Q-Q plot
# 분포가 정규인지 확인시 Q-Q plot(분위수(quantile)-분위수 차트)도 유용
# Quantile: 전체 사례 중 특정 값 미만에서 사례들이 차지하는 비율
# Q-Q plot: 자료의 누적값들을 특정 분포(정규분포)의 누적 확률과 대조해서 출력
#           각 값과 그 값에 해당하는 정규분포의 기댓값을 두 좌표 성분으로 사용해서 점을 찍는다.
ggplot(dlf, aes(sample=day1)) + stat_qq()

################ 2. 정규성 수량화 ################

# psych 패키지의 describe()
describe(dlf$day1)

# pastecs 패키지의 stat.desc()
# basic: 기초 통계량
# norm: 정규분포에 관련된 통계량
stat.desc(dlf$day1, basic=FALSE, norm=TRUE)

stat.desc(dlf[, c('day1', 'day2', 'day3')], basic=FALSE, norm=TRUE)

# 비대칭도(skewness), 첨도(kurtosis)
# 정규분포에서 skewness와 kurtosis는 모두 0이어야 한다.
# skwness가 0보다 크면 분포가 왼쪽에 몰려있다는 의미이고,
# skwness가 0보다 작으면 분포가 오른쪽에 몰려있다는 의미이다.
# kurtosis가 양수이면 분포가 뽀족하고 꼬리가 두꺼운것이고,
# kurtosis가 음수이면 분포가 평평하고 꼬리가 가늘다는 것이다.

# z점수: 실제 점수를 평균이 0, 표준편차가 1인 분포에 맞추어 변환한 값
# z점수가 유용한 이유
#  1) 서로 다른 표본들의 skewness와 kurtosis 비교 가능
#  2) skewness와 kurtosis가 발생할 확률을 파악할 수 있다
# z점수 구하는 법: 분포의 평균을 빼고 분포의 표준편차로 나누면 된다.
# 지금 예에서는 평균은 0, 표준편차는 표준오차(SE)를 사용용

# skew.2SE, kurt.2SE는 표준오차로 나눈 것으로 z점수가 2(1.96)보다 클 때 유의하다
# 이는 skew or kurt을 2 곱하기 표준오차로 나눈 값이 1보다 크면 유의하다고 말하는 것과 같다
# skew.2SE와 kurt.2SE의 값(절대값)이 1보다 크면 해당 비대칭도 또는 첨도가 유의한(p < .05) 것이다.

# normtest.w, normtest.p는 샤피로-월크 정규성 검정과 관련된 수치


################ 3. 자료 그룹들 살펴보기 ###########
rexam <- read.delim('Andy/Chapter5/RExam.dat', header = TRUE)
str(rexam)
# uni 변수를 요인 변환
rexam$uni <- factor(rexam$uni, levels = c(0:1),
                    labels = c('Duncetown University', 'Sussex University'))

ggplot(rexam, aes(x=exam)) + geom_histogram(aes(y=..density..)) + 
  stat_function(fun = dnorm, args = list(mean=mean(rexam$exam, na.rm = TRUE), sd=sd(rexam$exam, na.rm = TRUE)),
                colour = 'black', size = 1)
ggplot(rexam, aes(x=computer)) + 
  geom_histogram(aes(y=..density..), bins = 30) + 
  stat_function(fun = dnorm, args = list(mean=mean(rexam$computer, na.rm = TRUE), sd=sd(rexam$computer, na.rm = TRUE)),
                colour = 'black', size = 1)
ggplot(rexam, aes(x=lectures)) + 
  geom_histogram(aes(y=..density..), bins = 30) + 
  stat_function(fun = dnorm, args = list(mean=mean(rexam$lectures, na.rm = TRUE), sd=sd(rexam$lectures, na.rm = TRUE)),
                colour = 'black', size = 1)
ggplot(rexam, aes(x=numeracy)) + 
  geom_histogram(aes(y=..density..), bins = 30) + 
  stat_function(fun = dnorm, args = list(mean=mean(rexam$numeracy, na.rm = TRUE), sd=sd(rexam$numeracy, na.rm = TRUE)),
                colour = 'black', size = 1)

round(stat.desc(rexam[, c('exam', 'computer', 'lectures', 'numeracy')], 
                norm = TRUE, basic = FALSE), digits = 2)

############ 1) 그룹별 분석 #############
by(data = rexam$exam, INDICES = rexam$uni, FUN = describe)
by(data = rexam$exam, INDICES = rexam$uni, FUN = stat.desc, norm = TRUE, basic = FALSE)

dunceData <- subset(rexam, rexam$uni == 'Duncetown University')
sussexData <- subset(rexam, rexam$uni == 'Sussex University')
ggplot(dunceData, aes(numeracy)) +
  theme(legend.position = 'none') +
  geom_histogram(aes(y = ..density..), fill = 'white', colour = 'black', binwidth = 1) +
  labs(x = 'numeracy score', y = 'Density') +
  stat_function(fun = dnorm, args = list(mean = mean(dunceData$numeracy, na.rm = TRUE),
                                         sd = sd(dunceData$numeracy, na.rm = TRUE)),
                colour = 'black', size = 1)


############## 4. 분포의 정규성 검정 ############
############## 1) 샤피로-월크 검정(Shapiro-Wilk test) #############
# 표본의 점수들을 그 표본과 평균과 표준편차가 같은 정규분포에서 뽑은 점수들과 비교
# 검정 결과가 유의하다면(p < .05), 자료의 분포는 정규분포와 유의하게 다른 것(정규 분포가 아니다)
# 유의하지 않다면 표 분포가 정규분포와 그리 다르지 않다는 것
# 표본이 크면 정규분포를 조금만 벗어나도 유의한 결과가 나오기 쉽다는 점을 고려해야 한다.
shapiro.test(rexam$exam)
shapiro.test(rexam$numeracy)

# W, p-value는 각각 stat.desc()의 normtest.W와 normtest.p에 해당
# 유의한 값이 나왔다는 것은 변수가 정규성에서 이탈
by(rexam$exam, rexam$uni, shapiro.test)
by(rexam$numeracy, rexam$uni, shapiro.test)


############## 5. 분산의 동질성 검정 ############
# 한 변수가가 여러 수준을 거쳐갈 때 다른 변수의 분산이 일정해야 한다는 것
# 여러 그룹의 자료를 수집한 경우: 각 그룹에서 결과변수(들)의 분산이 동일해야 한다는 뜻
# 연속 자료를 수집한 경우: 한 변수의 분산이 다른 변수의 모든 수준에서 일정해야 한다는 뜻

############## 1) 레빈 검정 ###########
# 서로 다른 그룹의 분산이 같을 것이라는 귀무가설을 검정
# car패키지의 leveneTest() 함수 사용
# 레빈 검정을 대문자 F로 표기
# 자유도(degree of freedom, df)가 두 개 있다.
# 결과는 F(df1, df2) = 값, Pr(>F) 형태로 보고
# 표본이 크면 그룹 분산들이 조금만 달라도 유의한 결과가 나올 수 있다
leveneTest(rexam$exam, rexam$uni)
leveneTest(rexam$exam, rexam$uni, center = mean)
leveneTest(rexam$numeracy, rexam$uni)

############# 2) 하틀리의 F.max: 분산비 #############
# 분산비: 서로 다른 그룹들의 분산들의 비인데, 더 큰 분산이 분자에 쓰인다


############# 6. 자료의 문제점 수정 ##############
############# 1) 이상치 처리 ##############
# 1.. 해당 사례를 제거: 이상치를 제공한 레코드를 완전히 제거
# 2.. 자료 변환
# 3.. 데이터 변경: 해당 이상치를 다른 데이터로 대체

