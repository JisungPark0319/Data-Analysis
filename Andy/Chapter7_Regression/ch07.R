# 회귀 분석
# - 하나 이상의 독립변수들로부터 종속변수의 값들을 예측하는 것
# 회귀 분석의 종류
#   1. 단순 회귀: 하나의 예측 변수로부터 한 결과변수를 얻는 것
#   2. 다중 회귀: 여러개의 예측변수들로부터 한 결과변수를 예측하는 것

# 회귀분석의 경우 자료에 적합시키는 모형은 선형(linear) 모형
# 자료 집합을 하나의 직선으로 요약
# 요약하는 직선은 하나가 아니라 여러 개며 이중에서 자료를 가장 잘 서술하는 모형을 선택
# 선택 시 사용하는 방법은 최소제곱법(method of least squares)이라는 수학기법을 사용

# 직선은 두가지 요소로 정의
#   1. 기울기(slope): b.1
#   2. 절편(intercept): 선이 그래프의 수직축과 만나는 점, b.0

# Y.i = (b.0 + b.1*X.i)+e.i

# Y.i: 예측하고자 하는 결과 변수
# X.i: i번째 참가자의 예측변수 점수
# e.i: 잔차항, 예측 점수와 실제 값의 차이

# 기울기가 양인 선은 양의 상관
# 기울기가 음인 선은 음의 상관

# 최소제곱법
# 최량적합선(line of best fit): 자료점들에 대해 그릴 수 있는 모든 직선 중 관측된 자료점과 선의차이가 가장 작은 것
# 잔차제곱들의 합은 선이 자료에 얼마나 잘 들어맞는지 나타내는 측도
# SS(sum of squareed differences, 잔차제곱합)가 가장 작은 선이 최량적합선
# 최량적합선을 회귀선(regression line)이라 부른다.

# 총제곱합(total sum of squares)
#   - 평균모형이 예측한 값과 관측값의 차이를 구하고 제곱하여 모두 합한 값
#   - SS.T로 표기
#   - 가장 기본적인 모형 적합도도
# 잔차제곱합(sum of squares)
#   -회귀모형이 예측한 값과 관착값의 차이를 구하고 제곱하여 모두 합한 값
#   - SS.R로 표기
#   - 회귀분석에서 사용
# 모형제곱합(model sum of squares)
#   - 평균모형과 회귀모형의 SS값의 차를 더하고 제곱하여 합한 값
#   - 값이 작으면 평균모형과 회귀모형 차이가 적다것을 표시
# 모형에 따른 향상비율
# 모형제곱합을 총제곱하으로 나눈다
# R^2 = SS.M / SS.T
# 단순회귀에서 상관계수 r은 회귀모형의 전반적인 적합도를 잘 추정한 값
# R^2은 상관관계의 실질적인 크기를 잘 추정한 값

# F = MS.M / MS.R
# MS.M(모형의 평균제곱) = SS.M / SS.M자유도인 모형의 변수 개수
# MS.R(잔차 평균제곱) = SS.R / SS.R자유도인 회귀분석으로 추정하는 매개변수 개수를 관측값 개수에서 뺀 값
# F비(F-ratio)가 기본 모형을 사용했을 때와 비교해서 새모형이 결과의 예측을 얼마나 향상시키는지를 나타낸다

# 사용 패키지
#install.packages('car')
#install.packages('QuantPsyc')

library(boot)
library(car)
library(QuantPsyc)

album1 <- read.delim('Andy/Chapter7/Album Sales 1.dat')

# lm이라는 함수(linear model)를 이용해서 회귀분석을 수행
albumSales.1 <- lm(album1$sales ~ album1$adverts)
albumSales.1 <- lm(sales ~ adverts, data=album1)

summary(albumSales.1)

# albumSales.1 linear model의 Multiple R-squared(0.3346)의 R의 값
sqrt(0.3346)  # 0.5784462
# 피어슨의 상관계수는 .58
# R^2이 .335라는 것은 광고비가 판매량 변동의 33.5%를 설명
# p값은 F비의 유의확률
# F비는 99.59이며 p <.001 수준에서 유의하다
# 귀무가설이 참일 경우이 F비가 나올 확률이 0.1% 미만임을 뜻한다.
# 선형회귀 귀무가설은 b의 값이 0이라는 귀무가설
# 선형모형이 평균모형보다 잘 예측했다는것을 보여준다.

# 예측 변수가 결과의 예측 능력에 유의한 수준의 효과를 가진다면, 이 b값은 0과 달라야한다
# 해당 표준오차에 비해 커야한다.
# b값이 0과 유의한 수준으로 달느지를 t검정으로 확인 가능
# 유의확률이 .05보다 작으면, 효과 반영이 높다고 판단
# 두 값의 확률들이 2e-16보다 작으므로 0일 때 이 t값들이 발생할 확률은.001보다 작다
# 유의하게 기여한다는 결론



# 다중회귀
# Y.i = (b.0 + b.1*X.1i + b.2*X.2i + ... + b.n*X.ni) + e.i
# 다중회귀에 대해 구한 R^2을 다중 R^2(multiple R^2)이라 부른다
# R^2은 Y의 관측값과 다중회구모형이 예측한 Y 값의 차이들 사이 상관계수의 제곱

# 아카이케정보기준(akaike information criterion, AIC)
#   - 적합성의 한 측도
#   - 모형 예측변수가 많을수록 벌점을 준다는 특징
#   - AIC = nln(SSE / n) + 2k
#   - ln: 자연로그, SSE: 오차제곱합, k: 예측변수 개수

# 예측변수가 추가되면 R^2값이 커지고 SSE가 줄어든다
# AIC는 같은 자료에 대한 모형들 사이에서만 비교가능
# AIC 크기는 어떤 모형이 해당 모형보다 더 적합하다 부적합하다로 판별가능
# AIC가 낮을수록 더 나은 모형이다

# 위계적 회귀
#   - 알려진 변수들을 먼저 도입하고 결과예측의 중요도가 높으것을 낮은것보도 먼저 도입
#   - 해당 변수 다 도입한 후 임의의 새 예측변수들을 모형에 추가
# 강제 도입법
#   - 모든 예측변수를 모형에 동시에 도입
#   - 도입할 튼튼한 이론적 이유가 존재할 때 바람직
# 단계적 회귀
# 1. 전진방향
#   - 상수(b.0)하나만 있는 상태로 시작
#   - 결과변수와의 단순 상관이 가장 높은 예측변수를 선택
#   - 결과예측 능력이 향상되면, 그 예측변수를 추가하고 두 번째 예측변수를 검색
#   - 두 번째 예측변수는 준편상관이 가장 큰 것을 선택
#   - AIc값이 이전보다 작아지게 하는 변수가 더 이상 없으면 변수 추가를 중단
#   - 준편상관계수: 결과의 '새 변동'을 남아 있는 예측변수들이 얼마나 많이 설명하는지를 나타낸다.
#     -> 이전 상관비율이 60%경우 남은 40%를 설명하는 변수를 찾기위해 준편상관계수 사용
# 2. 후진방향
#   - 전진방법의 반대
#   - 모든 예측변수를 모형에 추가하고 변수를 하나씩 제거하면서 AIC가 낮아지는 점검
#   - 낮아졋으면 그 변수를 제거하고 남아 있는 예측변수들의 기여도를 재평가
#   - AIC가 증가할 때 까지 반복
# 3. 모두(both)
#   - 전진방법과 같은 방식으로 시작하되, 예측변수를 모형에 도입할 때마다 가장 덜 유용한 예측변수를 제거해서 모형이 향상되는지를 점검
#   - 덜 유용한 예측 변수를 제거해서 회귀 방전식을 평가하는 과정이 끊임없이 반복
# - 전진 방향보다 후진방향이 억제인자 효과때문에 더 좋다
# 단계별 방법의 문제점
#   - 이미 모형에 존재하는 다른 변수들에 기초해서 한 변수의 적합도를 평가한다는 것
# - 탐색적 모형 구축을 제외할 때 단계적 방법들은 가능하면 사용하지 않는 것이 좋다
# - 사용 할 경우 모형에 대해 교차 타당성검사(cross-validation)를 수행하는 것이 바람직하다.

# 이상치가 있으면 모형이 한쪽으로 치우치게 된다.
# 표준화잔차: 보통의 잔차들을 해당 표준편차 추정값으로 나눈 것
# z점수: 실제 점수를 평균이 0이고 표준편차가 1인 정규분포를 따르도록 변환한 값
# 표준화 잔차는 잔차를 z점수로 변환한 값
# z점수는 95%가 -1.96 ~ 1.96
# 99% -2.58 ~ 2.58
# 99.9% -3.29 ~ 3.29 사이의 값

# 수정 예측값(adjusted predicted value): 해당 사례를(이상치) 제외한 자료로 만든 모형이 예측한 값
# 수정 예측값과 원래의 예측값 차이를 흔히 DFFit으로 표기
# 수정 예측값과 원래의 관측값 차이를 제외잔차라고 부르고 그것을 표준오차로 나눈것을 스튜던트화 잔차라 한다
# 스튜던트화 잔차는 하나의 사례가 그 사례에 대한 모형의 예측 능력에 미치는 영향을 평가하는 데 아주 유용하다

# 지렛대값: 결과 변수의 실제 측정값이 예측값에 미치는 영향을 측정
# 평균 지렛대: (k+1)/n, k: 모형의 예측변수 개수, n: 참가자 수
# 값의 범위 0(사례가 예측에 아무런 영향을 주지 않음) ~ 1(사례까 예측에 완전한 영향을 줌)

# DFBeta: 모든 사례를 포함해서 추정한 매개변수와 특정 사례를 제외해서 추정한 매개변수의 차이
# DFFit: 모든 사롈르 포함해서 추정한 모형으로 예측한 특정 사례의 값과 그 사례를 제외하고 추정한 모형으로 예측한 그 사례의 값

# 가정 점검
# 1. 변수의 종류
#   - 모든 예측 변수는 반드시 양적 변수 또는 범주형 변수(범주가 두 개인)이어야 한다
#   - 결과 변수는 반드시 연속이자 비유계인 양적 변수
# 2. 0이 아닌 분산
#   - 예측 변수의 분산이 0이 아니어야 한다.
# 3. 완전 다중공선성의 부재
#   - 예측변수들의 상관계수가 너무 높아서는 안된다.
# 4. '외부 변수'와는 무관한 예측변수
#   - 그 어떤 외부 변수도 회귀모형에 포함된 임의의 예측변수와 상관이 없어야 한다
# 5. 등분산성
#   - 예측변수(들)의 각 수준에서 잔차 항들의 분산이 일정
#   - 등분산성: 잔자들의 분산이 같은 값
#   - 이분산성: 잔차들의 분산이 많이 다른 것
# 6. 오차의 독립성
#   - 임의의 두 관측값의 잔차들이 무관
# 7. 오차의 정규분포
#   - 모형의 잔차들이 평균이 0인 정규분포를 따르는 무작위 값
#   - 예측변수가 정규분포일 필요가 없다
# 8. 독립성
#   - 결과변수의 모든 값이 독립적이어야 한다
# 9. 선형성
#   - 예측변수의 값이 증가함에 따라 결과변수의 평균값들이 하나의 작은 직선을 형성해야 한다

# 타당성 검증
# 수정 R^2(adjusted R^2)
#   - 예측 능력의 손실정도를 나타낸다.
#   - 모형이 Y의 변동을 얼마나 설명할 것인지를 말해준다
#   - 문제점으로 회귀모형이 오나전히 다른 자료 집합을 얼마나 잘 예측할 것인지에 관해서 말해주지 않는다.
# 모형의 교차 타당성이 어느 정도인지 말해주는 다른 수정R^2 공식
# 스타인 공식(Stein's formula)
#   - 수정 R^2 = 1-[((n-1)/(n-k-1)) * ((n-2)/(n-k-2)) * ((n+1)/n)](1-R^2)
#   - k: 모형의 예측변수 개수, n: 참가자 수
# 자료분할
#   - 자료 집하을 무작위로 두 부분으로 나누고 각 회귀모형을 구해서 두 모형을 비교

album2 <- read.delim('Andy/Chapter7/Album Sales 2.dat')
albumSales.2 <- lm(sales ~ adverts, data=album2)
albumSales.3 <- lm(sales ~ adverts+airplay+attract, data=album2)
summary(albumSales.2)
summary(albumSales.3)
# Adjusted R-squared값이 이상적으로 R^2값과 같거나 아주 가까워야 한다.
# R^2과 매우 비슷하다. 따라서 이 모형은 교차 타당성이 아주 좋다고 말할 수 있다.

# R에서 표준화된 베타 추정값을 구하려면 im.beta()함수 필요
# QuantPsyc패키지에 포함
lm.beta(albumSales.3)
# 모형의 한 예측 변수의 '중요도'를 좀 더 잘 나타낸다

# 신뢰구간을 고찰
# confint() 함수 사용
confint(albumSales.3)
# 신뢰구간이 좁다는 것은 해당 표본의 b의 값이 모집단의 b의 참값에 가깝다는 뜻

# 모형의 비교
# R^2이 첫 모형의 R^2보다 유의하게 더 큰지 판단
# F비로 검사
# F = (N-k-1)R^2 / k(1-R^2)
# F.변화 = (N-k-1)R^2.변화 / k.변화(1-R^2)
# R^2.변화: 현재 모형R^2 - 기존 모형R^2
# k.변화: 현재 모형 예측변수 수 - 기존 모형 예측변수 수수

# 모형 비교시 anova() 함수 사용
anova(albumSales.2, albumSales.3)

# 사례별 진단(casewise diagnostics): 각각의 사례마다 하나의 값을 산출
# 이상치
#   - 잔차: resid()
#   - 표준화잔차: rstandard()
#   - 스튜던트화 잔차: rstudent()
# 영향력 큰 사례
#   - 쿡의 거리: cooks.distance()
#   - DFBeta: dfbeta()
#   - DFFit: dffits()
#   - 모자값(지렛대): hatvalues()
#   - 공분산비: covratio()

album2$residuals <- resid(albumSales.3)
album2$standardized.residuals <- rstandard(albumSales.3)
album2$studentized.residuals <- rstudent(albumSales.3)
album2$cooks.distance <- cooks.distance(albumSales.3)
album2$dfbeta <- dfbeta(albumSales.3)
album2$dffit <- dffits(albumSales.3)
album2$leverage <- hatvalues(albumSales.3)
album2$covariance.ratios <- covratio(albumSales.3)
head(album2)

# 표준화 잔차의 절댓값이 2보다 큰 사례는 전체의 5%를 넘어서는 안된다.
# 절댓값이 2.5보다 큰 사례는 전체의 약 1%를 넘어서는 안된다.
# 절댓값이 3보다 큰 사례는 이상치 가능성이 크다
# 쿡의 거리가 1보다 큰 사례는 모형에 영향을 미칠 가능성이 있다.
# 평균 모자 값(예측변수 개수 + 1을 표본 크기로 나눈 것)을 계산하고, 개별 모자 값이 평균값의 두 배 또는 세 배 넘는 사례를 찾아보자
# 공분산비(CVR) 허용 범위의 상한과 하한을 계산
# 상한은 1에 평균 모자 값의 세 배를 더한 값
# 하한은 1에 평균 모자 값의 세 배를 뺀 값
# CVR 범위를 벗어난 사례는 문제가 될 수 있다.

# 독립성 가정의 평가
# 더빈-왓슨 검정통계량은 자기상관 측도와 p값으로 계산
# durbinWatsonTest() or dwt() 함수 사용
# 검정통계량이 1보다 작거나 3보다 크면 주의할 필요가 있다
# 값이 2에 가까울수록 좋다
# 함수를 실행할 때마다 내부 기능으로 인해 p값이 다르게 나올 수 있다.
durbinWatsonTest(albumSales.3)
dwt(albumSales.3)

# 공선성을 평가하는데 유용한 통계량
# VIF(분산팽창인자)와 그것의 역수인 허용(tolerence) 통계량
vif(albumSales.3)
1/vif(albumSales.3)
# 평균 VIF
mean(vif(albumSales.3))
# 가장 큰 VIF가 10보다 크면 문제의 여지가 있는 것
# 평균 VIF가 1보다 확연하게 크면 회귀뫃여이 편향되었을 수 있다
# 허용 통계량이 0.2보다 작으면 모형에 뭔가 문제가 있을 가능성이 있다
# 허용 통곌야이 0.1보다 작으면 모형에 심각한  문제가 있는 것이다


# 잔차 검정
# 시각화를 통해 확인
# 유용한 그래프
# 1. 잔차 대 적합값(fitted value: 모형에 적합된 값, 즉 예측값)
#   - 모든 점이 0을 중심으로 고르게, 무작위로 분산된 모습
#   - 그래프가 깔때기 형태이면 자료에 이분산성이 존재할 가능성
#   - 그래프가 어떤 곡선 형태를 띤다면, 자료가 선형성 가정을 위반했을 가능성
# 2. Q-Q plot
#   - 직선은 정규분포를 점들은 관측된 잔차를 나타낸다.
# 3. 잔차(표준화잔차 or 스튜던트화 잔차)들의 히스토그램
library(ggplot2)
album2$fitted <- albumSales.3$fitted.values
histogram <- ggplot(album2, aes(studentized.residuals)) + theme(legend.position='none') +
  geom_histogram(aes(y=..density..), colour='black', fill='white')  +
  labs(x='Studentized Residual', y='Density')
histogram + stat_function(fun = dnorm, 
                          args = list(mean = mean(album2$studentized.residuals, na.rm=TRUE),
                                      sd=sd(album2$studentized.residuals, na.rm=TRUE)),
                          colour='red', size=1)

qqplot.resid <- qplot(sample = album2$studentized.residuals) + stat_qq() +
  labs(x = 'Theoretical values', y='Observed Values')
qqplot.resid

scatter <- ggplot(album2, aes(fitted, studentized.residuals))
scatter + geom_point() + geom_smooth(method='lm', colour='Blue') +
  labs(x='Fitted Values', y='Studentized Residual')


# 강건한 회귀: 부트스트래핑
# 부트스트랩 이용해서 통계적 유의성과 신뢰구간들을 얻을 수 있고 분산에 관한 가정들을 완화할 수 있다
bootReg <- function(formula, data, indices) {
  d <- data[indices,]
  fit <- lm(formula, data = d)
  return (coef(fit))
}
bootResults <- boot(statistic = bootReg, formula=sales~adverts+airplay+attract, data=album2, R=2000)
# 신뢰구간 boot.ci()
# index - 절편:1, adverts:2, airplay:3, attract:4
# bca: bias corrected accelerated 편향.수정.가속
boot.ci(bootResults, type='bca', index=1)
boot.ci(bootResults, type='bca', index=2)
boot.ci(bootResults, type='bca', index=3)
boot.ci(bootResults, type='bca', index=4)


# 범주형 예측변수
# 범주형 변수는 0,1로 부호화해야한다
# 가변수 부호화를 수행
# 부호화하고자 하는 그룹의 개수에서 1 뺀 것만큼의 변수를 만든다
gfr <- read.delim(file='Andy/Chapter7/GlastonburyFestivalRegression.dat', header=TRUE, stringsAsFactors=TRUE)
head(gfr)

# 가변수 부호화 함수: contrasts
# 변수값들에 대비를 설정하려면 그 변수가 반드시 요인이어야 한다
# contr.treatment()함수는 모든 그룹을 하나의 기저 조건과 비교한 결과에 기초해서 대비를 계산
contrasts(gfr$music)<-contr.treatment(4, base = 4)
gfr$music

glastonburyModel <- lm(change ~ music, data = gfr)
summary(glastonburyModel)     
round(tapply(gfr$change, gfr$music, mean, na.rm=TRUE),3)
