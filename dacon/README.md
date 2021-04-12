# dacon


심리 성향 예측 AI 경진대회 - dacon 사이트에서 주최하는 분석대회

사이트 주소: https://www.dacon.io/competitions/official/235647/overview/

진행 사항
 1. lightgbm 모듈을 사용하여 baseline 진행
 2. EDA
 3. feature engineering
      - 마키아벨리니즘 스코어 변수 및 변수들의 파생변수 생성
      - lightgbm 및 randomforest, logistic, ada, gradient boosting등 사용하여 모델 비교
      - lightgbm이 가장 점수가 높게나와 하이퍼파라미터 튜닝 진행
      - validation auc는 baseline보다 1%정도만 높아짐...
      - 제출 후 평가 점수 확인 시 baseline보다 점수나 낮아짐...
 4. AutoML - PyCaret
      - AutoML 사용하여 base data 모델 평가 진행
      - 가장 점수가 높은 3개의 모델을 ensemble하여 예측
      - 각 model의 성능은 기존 baseline에서 lightgbm으로만 측정했을때와 비슷하게 나온다.
      - ensemble하여 측정하여 제출하니 성능 기존에 비해 많이 향상되었다.
      - 각 모델 점수는 비슷하지만 ensemble을 통해 test data를 측정하여 성능이 향상된것으로 보인다.

