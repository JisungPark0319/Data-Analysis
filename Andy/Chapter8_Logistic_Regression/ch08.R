# 로지스틱 회귀(logistic regression)
#   - 결과 변수가 범주형 변수이고 예측변수들이 연속변수 또는 범주형변수인 다중회귀

# 이항 로지스틱 회귀(binary logistic regression)
#   - 범주가 두 개인 결과변수 예측하는 로지스틱 회귀
# 다항 로지스틱 회귀(polychotomous logistic regression)
#   - 범주가 많은 결과변수에 대한 로지스틱 회귀

# 로지스틱 회귀 방정식
# P(Y) = 1 / (1+exp(-(b.0 + b.1*X.1i + b.2*X.2i + ... + b.n*X.ni)))
# e의 지수는 잔차항이 없다는 점만 빼면 단순회귀의 직선 방정식과 같다
# P(Y)는 Y가 나올 확률

# 선형회귀는 변수들의 관계가 선형이라는 과정이 필요하다
# 결과변수가 범주형인 경우 해당 가정이 깨진다
# 이 문제를 피하기위한 방법 중 로그 변환을 이용해서 자료를 변환하는 것이다
# 다중 선형 관계를 로그 항(로짓(logit))들로 표현함으로써 선형성 가정 위반 문제를 극복한다
# 로지스틱 회귀에서는 최대가능도 추정(maximum-likelihood estimation)이라는 기범을 이용해서 매개변수들을 추정한다

# 로그 가능도(log-likelihood)
# 예측값들과 실제 관측값들에 관한 확률들의 합
# 모형이 적합된 후에도 여전히 설명되지 않는 정보의 양을 나타낸다
# 다중회귀의 잔차제곱하과 비슷
# 값이 클수록 부적합함을 나타낸다.
# ∑[Y.i*ln(P(Y.i)) + (1-Y.i)*ln(1-P(Y.i))]

# 이탈도(deviance)
# 이탈도 = -2 * 로그 가능도
# 로지스틱 회귀모형의 이탈도를 그냥 -2LL이라고도 칭한다
# 비교의 한 가지 용도는 로지스틱 회귀 모형의 상태를 어떤 기저 상태와 비교해 보는 것
# 로그 가능도보다 이탈도 사용이 더 편한데 이유는 카이제곱 분포를 따르기 때문이다
# 기저 모형은 아무 정보도 없는 상황에서 최선의 추측에 해당하는 모형이며,
# 로지스틱 회귀에서는 가장 자주 발생한 결과가 기저 모형이다(빈도수)
# X^2 = (-2LL(기저모형)) - (-2LL(새모형))
#     = 2LL(새모형) - 2LL(기저모형)
# df = K.새모형 - k.기저모형
# 카이 제곱은 새 모형의 이탈도에서 기조 모형의 이탈도를 뺀 값

# 모형 평가
# 호스머-렘쇼측도
#   - R^2.L = (-2LL(기저모형) - (-2LL(새모형))) / -2LL(기저모형)
#   - 로그 가능도의 절댓값이 감소하는 비율에 해당
#   - 0 ~ 1의 범위, 1에 가까울 수록 모형이 결과변수를 완벽하게 예측한다는 뜻
# 콕스-스넬
#   - R^2.CS = 1-exp((-2LL(새모형) - (-2LL(기저모형))) / n)
#   - 이론적으로 측도 값이 1이지만, 실제로 그 값이 나오 는 것이 불가능
# 네이글커크
#   - 콕스-스넬을 수정한 방식
#   - R^2.N = R^2.CS / (1-exp(-((-2LL(기저모형)))/n))

# AIC = -2LL + 2k
# BIC = -2LL + 2k*log(n)

# 개별 예측변수의 기여도
#   - z통계량 사용
#   - z 통계량은 주어진 예측변수의 b계수가 0과 유의하게 다른지의 여부를 나타낸다.
#   - z = b / SE.b
#   - 회귀계수가 클때는 표준오차가 상승해서 z 통계량이 과소평가 될 수 있기 때문에
#   - 가능도비 통계량을 살펴보는 것이 더 정확할 수 있다

# 승산비(odds ratio)
# B변수로 한 지수함수 exp(B)
# 예측변수의 1단위 변화에 따른 승산의 변화 비율
# 어떤 사건이 발생할 승산은  그 사건이 발생할 확률을 그 사건이 발생하지 않을 확률로 나눈 것
# odds = p(사건) / p(사건 없음)
# P(사건 Y) = 1 / (1+exp(-(b.0+b.1*X.1)))
# P(사건 없음Y) = 1-P(사건Y)
# b.0: 상수 계수, b: 예측변수의 계수, X: 예측변수의 값
# 승산비: 예측변수의 1단위 변경 이후의 승산 / 원래의 승산
# 1보다 크다는 것은 예측변수가 증가하면 결과가 발생할 승산도 증가한다는 것

# 로지스틱 회귀 방법
# 1. 강제 도입범
#   - 예측변수들을 모두 한껀번에 회귀뫃여에 포함
# 2. 단계적 방법
#   - 전진, 후진, 둘의 조합 방법
# 선형회귀와 동일

# 가정
# 1. 선형성
#   - 로지스틱 회귀에서 선형성가정은 임의의 연속 예측변수들과 결과변수의 로짓의 관계가 선형
# 2. 오차의 독립성
#   - 기본적으로 이는 자료의 사례들 사이에 관계가 없어야 함을 뜻한다.
# 3. 다중공선성의 부재
#   - 예측변수들 사이의 상관관계가 너무 커서는 안된다.

# 사용 패키지
#install.packages('car')
#install.packages('mlogit')
#install.packages("Rdpack")
library(car)
library(mlogit)

eelData <- read.delim('Andy/Chapter8/eel.dat', stringsAsFactors = TRUE)
head(eelData)

# relevel(): 요인의 기저 범주를 명시적으로 지정
eelData$Cured <- relevel(eelData$Cured, 'Not Cured')
eelData$Intervention <- relevel(eelData$Intervention, 'No Treatment')

# 로지스틱 회귀분석 glm() 함수(generalized linear model)

# 로지스틱 회귀분석은 이항분포에 기초하므로, family = binomial()을 지정
eelModel.1 <- glm(Cured ~ Intervention, data = eelData, family = binomial())
eelModel.2 <- glm(Cured ~ Intervention + Duration, data = eelData, family = binomial())

# 귀무 이탈도(null deviance) : 상수만 잇고 예측변수는 전혀 없는 모형의 이탈도, -2LL(기저모형)
# 잔차 이탈도(residual deviance) : -2LL(새모형)
# 이탈도는 클수록 부적합하다
summary(eelModel.1)
summary(eelModel.2)

# 모형이 결과변수를 얼마나 더 잘 예측하는지는 모형의 카이제곱 통계량으로 평가
# 카이제곱은 현재 의 모형과 상수만 있는 모형의 차이를 측정한 것
modelChi <- eelModel.1$null.deviance - eelModel.1$deviance
modelChi # 9.92로 카이제곱 분포에 따르므로 확률표를 이용해서 유의성을 확인 가능 0.05수준으로 유의하다

# 카이제곰 통계량의 자유도 = 귀무 모형의 자유도 - 모형의 자유도
chidf <- eelModel.1$df.null - eelModel.1$df.residual
chidf

# 카이제곱 통계량과 관련된 확률을 계산할 때 pchisq() 함수 사용
# 하나의 카이제곱 값이고 다른 하나는 자유도이다.
# 우리가 원하는 확률은 1에서 pchisq() 함수의 결과를 뺀 것
chisq.prob <- 1 - pchisq(modelChi, chidf)
chisq.prob
# p값이 0.002로 확률이 .05보다 작으므로 모형이 결과를 더 잘 예측하지 못한다는 귀무가설을 기각할 수 있다.
# 모형의 적합도가 X^2(1) = 9.93, p = .002

# coefficients
# b값은 예측변수의 1단위 변화에 따른 결과의 로짓의 변화량
# 해당 Y값이 나올 승산의 자연로그
# 핵심적인 통계량은 z통계량
# 예측변수의 b 계수가 0과 유의하게 다른지의 여부를 말해준다
# 주의할 점으로 회귀계수(b)가 크면 표준오차가 상승해서 z통곌야이 과소평가되는 경향이 있다는 점

# 로지스틱 회귀의 R
# 호스머-렘쇼측도(R^2.L)
R2.hl <- modelChi/eelModel.1$null.deviance
R2.hl
sqrt(R2.hl)
# 콕스-스넬 통계량
R.cs <- 1 - exp((eelModel.1$deviance - eelModel.1$null.deviance) / 113)
R.cs
# 네이글커크 측도
R.n <- R.cs / (1-(exp(-(eelModel.1$null.deviance/113))))
R.n

# 승산비
# P(치료됨) = 1 / (1 + exp(-(-0.288+(1.229*0)))) = .428
# P(치료되지 않음) = 1 - .428 = .527
# 승산 = .428 / .527 = 0.748
# 1단위 변경
# P(치료됨) = 1 / (1 + exp(-(-0.288+(1.229*1)))) = .719
# P(치료되지 않음) = 1 - .719 = .281
# 승산 = .719 / .281 = 2.559
# 승산비 = 2.56 / 0.75 = 3.41
eelModel.1$coefficients
# 승산비 계산값과 동일
exp(eelModel.1$coefficients)

# 승산비들의 신뢰구간
# confint() 함수 사용
exp(confint(eelModel.1))
# 예측변수 승산비의 신뢰구간에 1이 포함되지 않다는 점이 중요(신뢰구간의 하계와 상계가 모두 1보다 크다)
# 승산비가 1보다 크다는 것은 예측변수가 증가함에 따라 호나자가 치료될 승산이 커짐을 뜻하기 때문

# 모형의 비교
# 이탈도 통계량 차이로 비교하며, 그 차이는 카이제곱 분포를 따른다
# 1. 한 모형의 이탈도에서 다른 모형의 이탈도를 뺀다
# 2. anova()함수 사용
#   - 자유도들까지도 계산해준다.
#   - 유의학률은 계산해 주지 않는다.
# 유의확률 계산
modelChi <- eelModel.1$deviance - eelModel.2$deviance
chidf <- eelModel.1$df.residual - eelModel.2$df.residual
chisq.prob <- 1 - pchisq(modelChi, chidf)
modelChi; chidf; chisq.prob
anova(eelModel.1, eelModel.2)
# 유의확률이 .964인데 .05보다 크다

# R에서 예측된 확률: fitted()함수
eelData$predicted.probabilities <- fitted(eelModel.1)
eelData$standardized.residuals <- rstandard(eelModel.1)
eelData$studentized.residuals <- rstudent(eelModel.1)
eelData$dfbeta <- dfbeta(eelModel.1)
eelData$dffit <- dffits(eelModel.1)
eelData$leverage <- hatvalues(eelModel.1)

head(eelData[, c('Cured', 'Intervention', 'Duration', 'predicted.probabilities')])

# 잔차의 해석
# 잔차를 조사하는 주된 목적
# 1. 모형이 잘 적합하지 않는 자료점들을 격리하는 것
#   - 스튜던트화 잔차, 표준화잔차, 이탈도 통계량
# 2. 모형에 필요 이상으로 큰 영향을 주는 자료점들을 격리하는 것
#   - 쿡의 거리, DFBeta, 모자 값 영향력 통계량
head(eelData[, c('leverage', 'studentized.residuals', 'dfbeta')])
# DFBeta가 1보다 작고, 지렛대 통곌야이 계산된 기댓값인 0.018과 아주 가깝고 스튜던트화 잔차가 +-2이내이므로
# 기본 잔차 통계량들은 아주 좋다


penaltyData <- read.delim('Andy/Chapter8/penalty.dat', stringsAsFactors = TRUE)
penaltyModel.2 <- glm(Scored ~ Previous + PSWQ + Anxious, data = penaltyData, family = binomial())

# 다중 공선성 검정
# car패키지의 vif()함수에 모형을 지정해서 VIF 통계량과 허용 통계량 계산
vif(penaltyModel.2)
1/vif(penaltyModel.2)
# VIF 값이 19을 넘으면 문제로 간주
# Previous(상태 불안 변수)와 이전 페널티킥 성공률 변수 사이에 공선성이 존재
# 공선성 해결 방법
# 1. 변수중 하나를 제거하는 것
#   - 문제점으로 어떤 변수를 제거해야 할지 알 수 없다는 것
# 2. 강한 공선성이 존재하지 않는 다른 예측 변수를 새로 추가하는 것
# 3. 예측변수들에 인자분석을 실행하고, 그 결과로 나온 인자 점수들을 예측변수로 사용하는 것
# 4. 모형의 비신뢰성을 명시하는 것것

# 로짓의 선형성 검사
# 로지스틱 회귀를 실행하되 각 예측변수와 그것의 로그 변환 결과 사이의 상호작용에 해당하는 예측변수들을 포함
# 각 변수와 그 로그 사이의 상호작용 항(interaction term)을 생성, log()함수 이용
penaltyData$logPSWQInt <- log(penaltyData$PSWQ) * penaltyData$PSWQ
penaltyData$logAnxInt <- log(penaltyData$Anxious) * penaltyData$Anxious
penaltyData$logPrevInt <- log(penaltyData$Previous) * penaltyData$Previous
head(penaltyData)

penaltyTest.1 <- glm(Scored ~ Previous + PSWQ + Anxious + 
                       logPSWQInt + logAnxInt + logPrevInt
                      , data = penaltyData, family = binomial())
summary(penaltyTest.1)
# 상호작용 항들이 유의한지만 보면 된다.
# 어떤 상호작용 항이 유의하다는 것은, 그 상호작용 때문에 해당 예측변수와 로짓에 대한 선형성 가정이 깨졌다는 뜻

# 다항 로지스틱 회귀
#   - 다항 로지스틱 회귀분석을 수행하는 방법은 본질적으로 이항 로지스틱 회귀분석과 같다
#   - 결과변수를 두 범주의 비교들로 분해

# 사용 함수: mlogit(), mlogit패키지에 포함
chatData <- read.delim('Andy/Chapter8/Chat-Up Lines.dat', stringsAsFactors = TRUE)
head(chatData)

# 결과변수 Success와 변수 Gender 요인 변환 확인: is.factor()
is.factor(chatData$Success)
is.factor(chatData$Gender)

# Gender의 두 번째 범주를 기저 범주로 설정
chatData$Gender <- relevel(chatData$Gender, ref = 2)

mlChat <- mlogit.data(chatData, choice="Success", shape="wide")
mlChat
chatModel <- mlogit(Success ~ 1 | Good_Mate + Funny + Gender + Sex + Gender:Sex +  Funny:Gender , data = mlChat, reflevel = 'No response/Walk Off')
summary(chatModel)

exp(chatModel$coefficients)
data.frame(exp(chatModel$coefficients))
exp(confint(chatModel))
