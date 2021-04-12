# 두 평균의 비교
# 필요 패키지
#install.packages('ggplot2')
#install.packages('pastecs')
#install.packages('WRS2')
library(ggplot2)
library(pastecs)
library(WRS2)

spiderLong <- read.delim('Andy/Chapter9/SpiderLong.dat', stringsAsFactors = TRUE)
head(spiderLong)
spiderWide <- read.delim('Andy/Chapter9/SpiderWide.dat')
ead(spiderWide)

ggplot(spiderLong, aes(Group, Anxiety)) +
  stat_summary(fun=mean, geom='bar', fill='White', colour='Black') +
  stat_summary(fun.data=mean_cl_normal, geom='pointrange') +
  labs(x='Group', y='Mean Anxiety')

# 반복측정 설계
# 반복측정 설계에서는 여분의 변수들이 제거되어서 자료의 민간성이 좀 더 높아진다.
# 그룹들을 반복해서 측정한 자료에 대해서는 오차 막대그래프를 사용하지 말아야한다
# 사용하기위해서는 그래프를 그리기 전에 자료를 조정할 필요가 있다

# 1단계 평균 계산
spiderWide$pMean <- (spiderWide$picture + spiderWide$real)/2
# 2단계 총평균 계산
grandMean <- mean(c(spiderWide$picture, spiderWide$real))
# 3단계 조정인자 계산
# pMean의 값이 참가자마다 다르다
# 참가자의 평균 불안 점수가 서로 다르다는 사실은 사람들의 개인차가 반영한다
# 본질적인 불안 차이들은 오차 막대그래프를 오염시킨다.
# 반복측정 자료를 조정하지 않고 그래프를 그리면 독립설계 사용했을 때와 같은 그래프가 나오는 이유이다
head(spiderWide)
# 각 참가자의 평균 점수를 총평균에서 뺀다
spiderWide$adj <- grandMean - spiderWide$pMean
# 4단계 조정인자로 각 값을 조정
spiderWide$picture_adj <- spiderWide$picture + spiderWide$adj
spiderWide$real_adj <- spiderWide$real + spiderWide$adj
head(spiderWide)
spiderWide$pMean2 <- (spiderWide$picture_adj+spiderWide$real_adj)/2
head(spiderWide)

rmMeanAdjust <- function(dataframe){
  varNames <- names(dataframe)
  pMean <- (dataframe[,1] + dataframe[,2])/2
  grandmean <- mean(c(dataframe[,1], dataframe[,2]))
  adj <- grandmean - pMean
  varA_adj <- dataframe[,1] + adj
  varB_adj <- dataframe[,2] + adj
  output <- data.frame(varA_adj, varB_adj)
  names(output) <- c(paste(varNames[1], 'Adj', sep='_'),
                     paste(varNames[2], 'Adj', spe='_'))
  return (output)
}
rmMeanAdjust(spiderWide)


# t검정
# 용도
#   - 주어진 상관계수가 0과 다른지 검사
#   - 회귀계수 b가 0과 다른지 검사
#   - 두 그룹 평균이 다른지 검사
# t검정 두 종류
# 1. 평균에 대한 독립 t검정(independent t-test)
#   - 실험 조건이 둘이고 각 조건에 서로 다른 참가자들을 배정한 실험에 사용
# 2. 평균에 대한 종속 t검정(dependent t-test)
#   - 실험 조건이 둘이고 두 조건 모두에 같은 참가자들을 배정한 실험에 사용

spiderLong_model <- lm(Anxiety ~ Group, spiderLong)
summary(spiderLong_model)

# 독립 t검정과 종속 t검정은 모두 정규분포에 기초한 모수적 검정(parametric test)
# 표집 분포가 정규분포
#   -> 독립 t검정에서 이는 점수 자체가 아니라 점수 차이들의 표집 분포가 정규 분포이어야 한다는 뜻
# 자료의 측정 수준이 적어도 구간 수준이어야 한다.
# 독립 t검정 추가적인 가정
#   - 서로 다른 실험 조건의 점수들이 독립적
#   - 이론적으로 분산들이 같아야 한다

# 독립 t검정
# 평균, 표준편차, 표본 크기만으로 t값 계산
x1 <- mean(spiderLong[spiderLong$Group=='Real Spider',]$Anxiety)
x2 <- mean(spiderLong[spiderLong$Group=='Picture',]$Anxiety)
sd1 <- sd(spiderLong[spiderLong$Group=='Real Spider',]$Anxiety)
sd2 <- sd(spiderLong[spiderLong$Group=='Picture',]$Anxiety)
n1 <- length(spiderLong[spiderLong$Group=='Real Spider',]$Anxiety)
n2 <- length(spiderLong[spiderLong$Group=='Picture',]$Anxiety)

ttestfromMeans <- function(x1, x2, sd1, sd2, n1, n2){
  df <- n1 + n2 - 2
  poolvar <- (((n1-1)*sd1^2)+((n2-1)*sd2^2))/df
  t <- (x1-x2)/sqrt(poolvar*((1/n1)+(1/n2)))
  sig <- 2*(1-(pt(abs(t),df)))
  paste('t(df = ', df, ') = ', t, ', p = ', sig, sep = '')
}
ttestfromMeans(x1, x2, sd1, sd2, n1, n2)

by(spiderLong$Anxiety, spiderLong$Group, stat.desc, basic = FALSE, norm = TRUE)
ind.t.test <- t.test(Anxiety ~ Group, data=spiderLong)
ind.t.test

ind.t.test <- t.test(spiderWide$real, spiderWide$picture)
ind.t.test

# 절사평균에 기초한 yuen()
yuen(Anxiety ~ Group, data=spiderLong)
yuen(Anxiety ~ Group, data=spiderLong, tr = .1)

# 부트스트랩 방법을 적용해서 절사평균들을 비교 yuenbt()
yuenbt(formula = Anxiety ~ Group, data = spiderLong, nboot = 2000)

# 부트스트랩과 M추정량으로 검정 수행 pb2gen()
pb2gen(Anxiety ~ Group, data=spiderLong, nboot=2000)

# 종속 t 검정
dep.t.test <- t.test(spiderWide$real, spiderWide$picture, paired=TRUE)
dep.t.test
