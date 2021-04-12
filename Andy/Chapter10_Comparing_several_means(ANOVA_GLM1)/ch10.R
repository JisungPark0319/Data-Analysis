# 여러 평균의 비교: 분산분석(GLM1)
#   - 조건이 셋 이상인 상황을 분석할 때 사용하는 통계적 모형

# 검사된 그룹들의 모든 조합에 대해 각각 t검정을 수행하면 안되는 이유
#   - 같은실험 자료에 실시한 여러 통계적 검정들의 오류율: 집단별 오류율
#   - 실험 조건을 세 가지에서 다섯 가지로 늘리면, 필요한 t검정의 수는 3에서 10으로 증가
#   - 짐단별 오류 = 1 - (0.95)^n
#   - t검정의 수가 증가할수록 제1종오류를 적어도 한 번은 범할 확률이 증간한다.
#   - 조건이 셋 이상일때는 t검정을 여러번 적용하기보다 분산분석을 사용한다.

# 분산분석의 귀무가설은 주어진 모든 그룹 평균이 같다는 것이다.
# F 통계량 또는 F 비(F-ratio)라는 갑을 산출
# 자료에 존재하는 체계적 변동의 양과 비치계적 변동의 양을 비교한것으로 F는 모형과 오차의 비이다
# 분산분석은 총괄검정에 해당하며, F 비는 실험 조작에 어떤 효과가 존재한다는 점을 알려줄뿐,
# 그것이 구체적으로 어떤 효과인지는 알려주지 않는다.

dummy <- read.delim('./GitHub/Andy/Chapter10_Comparing_several_means(ANOVA_GLM1)/data/Dummy.dat')
head(dummy)
dummy_model <- lm(libido ~ dummy1+dummy2, data = dummy)
summary(dummy_model)
# F(2, 12) = 5.12, p < .05로 유의
# 기저범주와 가변수인 dummy2와는 .28 > .05로 유의하지 않다

# 총제곱합(SS.T)
# 자료의 설명되지 않은 총 변동
# 변동의 총량을 구할 때는 관측된 각 자료점과 총평균의 차이를 계산
# SS.T = sum(x.i - 총x평균)^2
# SS.T = s^2.총(n-1)

# 모형제곱합(SS.M)
# 모형이 설명하는 변동
# 총 변동 중 서로 다른 자료점들이 서로 다른 그룹에서 비롯된 것
# 모형이 예측한 값과 총평균의 차이들로 계산
# SS.M = sum(그룸의 참가자수(그룸 평균 -  총평균)^2)

# 잔차제곱합(residual sum of squares. SS.R)
# 모형이 설명하지 못하는 변동이 어느 정도인지 말해준다.
# SS.R = SS.T - SS.M
# SS.R = sum((각 점수 - 그룹평균)^2)
# SS.R = sum(그룹분산(참가자수 - 1))

# 평균제곱(MS)
# SS.M은 회귀모형이 설명하는 총 변동
# SS.R은 가외 요인들에 의한 총 변동
# 두 값은 계산에 쓰인 점수들이 개수에 영향을 받는다
# 편향을 제거하기 위해 평균적인 제곱합을 계산
# 제곱합을 해당 자유도로 나눈 값
# MS.M = SS.M / df.M
# Ms.R = SS.R / df.R
# MS.M은 모형이 설명하는 변동(체계적 변동)의 평균량
# MS.R은 가외 변수들이 설명하는 변동(비체계적 변동)의 평균량

# F비
# F비는 모형이 설명하는 변동과 비치계적인 요인들이 설명하는 변동의 비
# 모형 평균제곱을 잔차 평균제곱으로 나눈값
# F = MS.M/MS.R
# F비가 1보다 작으면 해당 모형(실험 조작)의 효과는 유의하지 않다는 점

contrast <- read.delim('./GitHub/Andy/Chapter10_Comparing_several_means(ANOVA_GLM1)/data/Contrast.dat')
head(contrast)
contrast_model <- lm(libido ~ dummy1 + dummy2, data = contrast)
summary(contrast_model)
head(contrast)
head(dummy)

library(compute.es)     # 효과크기
library(pastecs)        # 기술통계량
library(ggplot2)        # 시각화
library(car)            # 레빈검정
library(WRS2)           # 강건한 검정들
library(multcomp)       # 사후 검정

# 일원 분산 절차
# 1. 자료 입력
# 2. 자료 탐색
#   - 시각화, 기술 통계량
#   - 분포의 가정들 검정
#   - 등분산성 검정(레빈검정)
# 3. 기본 분산분석 실행
# 4. 계획된 대비 or 사후검정 실행행
libido <- c(3,2,1,1,4,5,2,4,2,3,7,4,5,3,6)
dose <- gl(3,5, labels = c('Placebo', 'Low Dose', 'High Dose'))
viagraData <- data.frame(dose, libido)
viagraData

ggplot(viagraData, aes(dose,libido)) +
  stat_summary(fun = mean, geom='line', size = 1, aes(group=1), colour = '#FF6633') +
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.2, size = 0.75, colour = '#990000') +
  stat_summary(fun = mean, geom = 'point', size = 4, colour = '#990000') +
  stat_summary(fun = mean, geom = 'point', size = 3, colour = '#FF6633')
# 모든 오차 막대의 범위가 겹쳐 있다
# 그룹 간 차이가 존재하지 않다
# 평균들을 이은 선의 형태를 볼 때, 이 자료는 선형추세를 따른다.
# 비아그라 복용량이 증가하면 리비도의 평균 수준도 비례해서 증가한다.

by(viagraData$libido, viagraData$dose, stat.desc)
# CI.mean.0.95는 신뢰수준 95%
# mean에서 CI.mean.95를 빼면 신뢰수준 하계, 더하면 신뢰수준 상계를 뜻한다.
# SE.mean은 표준오차를 말하며 자료의 표집분포의 표준편차이다
# 따라서, 만일 해당 자료의 모집단에서 많은 수의 표본을 수집했다면, 그 표본들의 평균 표준편차는 표준오차이다

leveneTest(viagraData$libido, viagraData$dose, center = median)
# F(2, 12) = 0.118, p = .89
# 등분산검정의 귀무가설을 기각하므로 분산들이 비슷하다고 볼 수 있다.

# 주 분석
# 검정 가정들이 만족될 때
# 분산분석은 일반선형모형의 한 특수 경우일 뿐이다.
# 따라서 선형모형 함수 lm()으로 분산분석을 실행할 수 있다.
# 리비도 = 복용량 + 오차

viagraModel <- lm(libido ~ dose, data=viagraData)
summary(viagraModel)
# 분산분석 함수: aov(), (aov: analysis of variance)
# lm()과 동일한 분석을 수행한다.
# lm()의 분석 결과를 분산분석 접근 방식에 맡는 형태로 변형해서 돌려준다.
# wrapper 함수이다.

viagraModel <- aov(libido ~ dose, data=viagraData)
summary(viagraModel)
# Sum Sq: 모형의 제곱합
# Mean Sq: 모형의 평균제곱
# Residuals: 자료안의 비체계적 변동에 관한 정보
# F비는 그룹 평균들이 같은지에 대한 검사 결과
# Pr(>F): 이러한 값이 순전히 우연하게 나올 확률
#         모집단에 효과가 없는데도 이런 F비가 나올 확률
# 이 검정에서는 실험 효과가 유의하다는 결과가 나온다
# 평균들의 차이가 유의하다고 말할 수 있다.
# 오차 막대 그래프에서는 유의한 차이가 없었다.
# 이러한 모순은 오차 막대그래프를 오직 자료에 대한 대략적인 지침으로만
# 사용하는 것이 바람직하다는 점을 보여준다.

plot(viagraModel)
# 세 그룹의 점들이 같은 간격으로 퍼져 있다.
# 이는 그룹들의 분산이 서로 비슷하다는 뜻이다.
# Q-Q plot
# 모형의 잔차들의 정규성을 점검할 수 있다
# 그래프를 보면 일부 점들이 대각선과 거리가 있는 것이 보인다.

# 그룹들의 분산이 같지 않을 때
# 레빈 검정이 유의하고 자료가 정규분포를 따른다면 웰치의 F(Welch's F)검정을 적용
# 이 검정은 그룹의 분산들의 차이를 바로 잡는다.

oneway.test(libido ~ dose, data=viagraData)
# 검정의 결과가 유의하다면 평균의 차이가 유의하다는 결과가 나온다.

# 강건한 분산분석
# 넓은 형식으로 변환
viagraWide <- unstack(viagraData, libido ~ dose)
viagraWide

# 월콕스가 만든 강건한 분산분석 함수
# 절사평균에 기초한 t1way()
t1way(libido ~ dose, viagraData)
t1way(libido ~ dose, viagraData, tr = .1) # 절사 비율 10%
# F(2, 7.04) = 4.32, p=.54
# 유의한 차이가 없다

# med1way(): 중앙값들을 비교
med1way(libido ~ dose, viagraData)
# F = 4.78, p=.07
# 점수에 유의한 차이가 없다

# 절사평균 방법에 부트스트랩을 적용한 t1waybt()
t1waybt(libido ~ dose, data=viagraData, tr = .05, nboot = 2000)# 절사평규 5% 부트스르랩 표본 2,000개
# F=4.3, p=.06
# 유의한 차이가 없다

# 계획 대비
summary.lm(viagraModel)

contrasts(viagraData$dose) <- contr.helmert(3)

# 각 그룹에 가중치를 배정
contrast1 <- c(-2, 1, 1)
# 둘째 대비에 대비 가중치
contrast2 <- c(0, -1, 1)

contrasts(viagraData$dose) <- cbind(contrast1, contrast2)
viagraData$dose

# 가중치가 양인 그룹과 음인 그룹을 비교
# 대비1: Placebo와 두 실험군(Low Dose, High Dose) 비교
# 대비2: Low Dose와 High Dose 비교
viagraPlanned <- aov(libido ~ dose, data = viagraData)

# Pr(>|t|): 양쪽꼬리 점검으로 한쪽꼬리 점검을 위해 2로 나누어야 한다다
summary.lm(viagraPlanned)

# 추세 분석
# 예측변수의 그룹들을 의미 있는 순서로 부호화해야 한다.
contrasts(viagraData$dose) <- contr.poly(3)
viagraTrend <- aov(libido ~ dose, data=viagraData)
summary.lm(viagraTrend)
# dose.L 행은 실험 효과를 자료에 존재하는 선형(일차) 관계로 설명할 수 있는지에 대한 정보
# dose.Q 행은 실험 효과를 자료에 존재하는 이차관계로 설명할 수 잇는지에 대한 정보보
# 선형 추세부터 살표보기
# 그룹 평균들이 선형으로 증가하는지 검사
# 중요한 항목은 t값과 그 유의확률이다

# 사후 검정
# 본페로니의 방법 및 관련 방법(홀름, 벤야미니-호흐베르크 등)
# 본페로니 사후검정
pairwise.t.test(viagraData$libido, viagraData$dose, p.adjust.method = 'bonferroni')
# 벤야미니-호흐베르크 사후 검정
pairwise.t.test(viagraData$libido, viagraData$dose, p.adjust.method = 'BH')

# 투키의 방법과 더닛의 방법
# multcomp 패키지의 glht()
postHocs <- glht(viagraModel, linfct = mcp(dose = 'Tukey'))
summary(postHocs)
confint(postHocs)
postHocs <- glht(viagraModel, linfct = mcp(dose = 'Dunnett'), base=1)
summary(postHocs)
confint(postHocs)

# 강건한 사후 검정
# 절사평균에 기초한 lincon()
# 그룹 평균들을 절사할 뿐만 아니라 백분위수(percentile) 부트스트랩 기법으로 p값을 계산 mcppb20()
# mcppb20 - 제1종 오류율을 제어하는데 더 효과적
# lincon결과는 신뢰구간들은 검정 횟수를 참작해서 수정되었지만 p값들은 그렇지 않다
# 신뢰구간에 0이 포함되었는지의 여부로 유의성을 판정
lincon(libido ~ dose, viagraData)
mcppb20(libido ~ dose, viagraData)

# 그룹들의 차이에 관한 효과 크기
# compute.es 패키지의 mes()
mes(2.2, 3.2, 1.3038405, 1.3038405, 5, 5)
rcontrast <- function(t,df) {
  r <- sqrt(t^2/(t^2 + df))
  print(paste('r = ', r))
}
rcontrast(2.474, 12)
