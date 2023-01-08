library(dplyr)
library(ggplot2)
library(caret)
library(ModelMetrics)

#
# R caret 패키지 사용.
#  다양한 모델에 대해서
#  파라미터 검색을 통해 최적의 모델을 찾은 후
#  해당 모델의 결과를 플랏하고
#  모델자체는 fit.XXX 라는 변수로 저장한다
#

#
# 데이터 파일을 읽어온다
#
df0 <- read.csv('list.csv')
names(df0) <- c('Util','Type','Diameter','Length','GeoCon')
head(df0)

#
# 사전에 정의된 학습 데이터의 인덱스를 가져와서 학습데이터와 테스트데이터로 나눈다
#  (data.RData 는 prepare_data 에서 만들어진 파일임)
#
load('data.RData')
train_df <- df0[train.idx,]
test_df <- df0[test.idx,]

# 변수들의 분포를 그려본다.
# distributions
ggplot(df0) + geom_histogram(aes(Util))
ggplot(df0) + geom_histogram(aes(Diameter))
ggplot(df0) + geom_histogram(aes(Length))

# table of factors
table(df0$Type)
table(df0$GeoCon)

ggplot(df0) + geom_bar(aes(Type))
ggplot(df0) + geom_bar(aes(GeoCon))

# boxplots
ggplot(df0) + geom_boxplot(aes(x=Type,y=Util,group=Type))
ggplot(df0) + geom_boxplot(aes(x=GeoCon,y=Util,group=Type))

# 변수들의 상관관계를 본다
# correlation
corrplot::corrplot.mixed(cor(df0[,c('Util','Diameter','Length')]), tl.col='black')
GGally::ggpairs(df0)

# 수치형 변수간의 상관계수를 계산해본다
cor(df0[,c('Util','Diameter','Length')])

# 수치형 변수간 선형관계가 있는지 스캐터플랏을 해 본다
ggplot(df0) + geom_point(aes(Diameter,Util,color=Type))
ggplot(df0) + geom_point(aes(Diameter,Util,color=GeoCon))

ggplot(df0) + geom_point(aes(Length,Util,color=Type))
ggplot(df0) + geom_point(aes(Length,Util,color=GeoCon))

# 결과를 보여주는 플랏
show_res_all <- function(fit){
  
  ureal <- df0$Util # 실제값
  upred <- predict(fit, newdata=df0) # 예측값
  mae <- mean(abs(ureal-upred))
  
  colors <- rep('blue', nrow(df0)) # 색깔 - 학습데이터는 파란색, 테스트 데이터는 빨간색
  colors[test.idx] <- 'red'
  plot(ureal, upred, xlim=c(0,70), ylim=c(0,70), main = paste('MAE',round(mae,4)),
       col=colors)
  abline(a=0,b=1)
}


#
# 다양한 변수의 조합으로 회귀분석을 해 본다.
# rlm 은 robust lm 으로, 이상치(outlier) 가 있을때에 영향을 받지않는 회귀분석 방법임
#
# 여러가지 변수조합을 해 보면 Length^2 가 들어갈때 잘 된다는 걸 알 수 있다.
#
#fit.lm1 <- lm(Util ~ Length + I(Length^2) + Diameter + Type + GeoCon, data = df0)

fit.lm <- lm(Util ~ Length + Diameter + GeoCon + Type, data = df0)
pred.lm <- predict(fit.lm, newdata = test_df)

fit.lm1 <- MASS::rlm(Util ~ Length + I(Length^2) + Length*GeoCon + Type*Length , data = df0)
fit.lm1 <- lm(Util ~ Length + I(Length^2) + Length*GeoCon + Type*GeoCon , data = df0)
pred.lm1 <- predict(fit.lm, newdata = test_df)

summary(fit.lm1)
show_res_all(fit.lm1)

df0 %>% ggplot() + geom_point(aes( (Length), Util, color=Type))
df0 %>% ggplot() + geom_point(aes( (Diameter), Util, color=Type))
df0 %>% ggplot() + geom_point(aes( (Length), Util, color=Type))

# LR find best factor
#
# Length^2 이 중요하다는 것을 알게 되어서, 이 변수를 데이터셋에 추가한다.
#
train_df$Length2 <- train_df$Length^2
test_df$Length2 <- test_df$Length^2
df0$Length2 <- df0$Length^2

#
# stepAIC 는 복잡한 모델에서부터 단순한 모델까지, 변수를 하나씩 없애가면서
# 최적의 선형 모델을 찾도록 해 주는 유틸리티임
#
init_mod <- lm(Util ~ 1, data=train_df)
final_mod <- lm(Util ~ Length + Diameter + Length2 + Type + GeoCon + 
                  Length:Type + Length:GeoCon + Length2:Type + Length2:GeoCon + 
                  Diameter:Type + Diameter:GeoCon + 
                  GeoCon:Type -1, data=train_df)

# 모델찾는 함수
step <- MASS::stepAIC(final_mod,k=2,direction='backward')
# 모델찾은 결과.AIC 값이 작을수록 좋은 모델임
step$anova

show_res_all(step)
fit.lm1 <- step  # 최종적으로 가장 좋은 모델을 fit.ml1 에 넣는다

# best model
form.lm1 <- Util ~ Length + Diameter + Length2 + Type + GeoCon + Length:GeoCon + 
  Length2:Type + Length2:GeoCon + Diameter:Type + Type:GeoCon - 1

pred.lm.opt <- predict(step, newdata = test_df)

# lm CV -------------------------------------------------------------------
#
#
# 다른 방법과 동일하게 Cross Validation 계산을 진행. 10-fold CV 진행하여 모델을 정하고,
# 테스트 데이터를 최종모델에 넣어서 예측오차 (MAE) 를 계산하여 플랏한다.
#
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
fit.reg <- train(form.lm1, data = train_df, method = 'lm', trControl = fitControl)

mean(abs(predict(fit.reg, newdata=test_df)-test_df$Util))
show_res_all(fit.reg)

# MARS --------------------------------------------------------------------
#
# Multiple Adaptive Regression Spline.
#  hyper_grid 는 최적 매개변수를 찾기 위한 탐색구간의 정의임
#

hyper_grid <- expand.grid(
  degree = 1:5, 
  nprune = seq(2, 100, length.out = 20) %>% floor()
)

#
# MARS 모델을 정해전 범위내에 탐색해서 최적파라미터를 찾는다.
#

fit.mars <- train(
  x = subset(train_df, select = -Util),
  y = train_df$Util,
  #preProcess = c('scale'),
  method = "earth",
  metric = "MAE",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = hyper_grid
)

# best model
fit.mars$bestTune
#nprune degree
#12     12      2

# plot results 테스트 데이터 결과를 플랏
ggplot(fit.mars)
plot(test_df$Util, predict(fit.mars,newdata=test_df), xlim=c(0,70), ylim=c(0,70))
abline(a=0,b=1)

# 학습 데이터는 어땠는지 확인
plot(train_df$Util, predict(fit.mars,newdata=train_df), xlim=c(0,70), ylim=c(0,70))
abline(a=0,b=1)

# 두가지 같이 플랏(색으로 구분)
mean(abs(predict(fit.mars, newdata=test_df)-test_df$Util))
show_res_all(fit.mars)

pred.mars <- predict(fit.mars, newdata=test_df)

# decision tree -----------------------------------------------------------
#
# Decision Tree 모델
#
#

library(rpart)
library(rpart.plot)

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

df.tree <- rpart( Util ~ ., method="anova", data=df0)
summary(df.tree) # tree Rule 을 표시
prp(df.tree,type=1) # tree 자체를 그려주는 함수

# 파라미터 서치
hyper_grid <- expand.grid(
  cp = seq(0.01,0.15,by=0.01) # 트리의 복잡도(complexity parameter)
)

fit.tree <- train( Util ~ ., data=train_df, method = 'rpart', metric='MAE', 
                   trControl = fitControl, tuneGrid = hyper_grid)
fit.tree
prp(fit.tree$finalModel,type=1)
mean(abs(predict(fit.tree, newdata=test_df)-test_df$Util))

hyper_grid <- expand.grid(
  maxdepth = 2:10
)

fit.tree <- train( Util ~ ., data=train_df, method = 'rpart2', metric='MAE', 
                   trControl = fitControl, tuneGrid = hyper_grid)
fit.tree

pred.tree <- predict(fit.tree, newdata = test_df)

prp(fit.tree$finalModel,type=1)
mean(abs(predict(fit.tree, newdata=test_df)-test_df$Util))
show_res_all(fit.tree)

# forest ------------------------------------------------------------------
#
# random forest 모델
#

library(randomForest)
df.forest <- randomForest( formula = Util ~ ., 
                           data=train_df)

ypred <- predict(df.forest, newdata = test_df)
yval <- test_df[,'Util',drop=T]

plot(yval,ypred,xlim=c(10,50),ylim=c(10,50))
abline(a=0,b=1)

mean(abs(ypred-yval))

hyper_grid <- expand.grid( mtry = 2:5, splitrule = c('extratrees'), min.node.size = c(1,3,5))
fit.rf <- train( Util ~ ., data = train_df, method='ranger',
                 trControl = fitControl, tuneGrid = hyper_grid)

pred.rf <- predict(fit.rf, newdata =test_df)
mean(abs(predict(fit.rf, newdata=test_df)-test_df$Util))
show_res_all(fit.rf)

# nnet --------------------------------------------------------------------

#
# multi-layer neural net
#

# dummies <- dummyVars( Util ~ ., data = df0)
# train_nn <- as.data.frame(predict(dummies, newdata = train_df))
# test_nn <- as.data.frame(predict(dummies, newdata = test_df))
# train_nn$Util <- train_df$Util
# test_nn$Util <- test_df$Util
# 
# # 주의. 이때의 grid 는 뉴럴넷의 구조임임 (layer 3개)
# hyper_grid <- expand.grid( layer1 = 64, layer2 = 64, layer3 = 1)
# fit.mlp <- train( Util ~ ., data = train_nn, method='mlpML',
#                   preProc = c('center','scale'),
#                   trControl = fitControl, tuneGrid = hyper_grid)
# 
# mean(abs(predict(fit.mlp, newdata=test_nn)-test_nn$Util))
# show_res_all(fit.mlp)

# boosting ----------------------------------------------------------------

#
# Gradient Boosting 모델
#

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10, 
                           verboseIter = FALSE, allowParallel = TRUE)

hyper_grid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                           n.trees = (1:10)*50, 
                           shrinkage = c(0.01,0.05),
                           n.minobsinnode = 10)

fit.gbm <- train(Util ~ ., data=train_df, method = 'gbm', 
                 metric = 'MAE',
                 maximize = FALSE,
                 trControl=fitControl, tuneGrid = hyper_grid,
                 verbose=FALSE)

pred.gbm <- predict(fit.gbm, newdata = test_df)
show_res_all(fit.gbm)


# xgboost -----------------------------------------------------------------

library(xgboost)

#
# Gradient Boosting 모델 (xgboost 패키지 사용)
#
#

dummies <- dummyVars( Util ~ ., data = df0)
train_nn <- xgb.DMatrix(predict(dummies, newdata = train_df))
test_nn <- xgb.DMatrix(predict(dummies, newdata = test_df))
y_train <- train_df$Util
y_test <- test_df$Util

xgb_trcontrol = trainControl(
  method = "cv",
  number = 5,  
  allowParallel = TRUE,
  verboseIter = FALSE,
  returnData = FALSE
)

xgbGrid <- expand.grid(nrounds = c(100,200),  # this is n_estimators in the python code above
                       max_depth = c(10, 15, 20, 25),
                       colsample_bytree = seq(0.5, 0.9, length.out = 5),
                       ## The values below are default values in the sklearn-api. 
                       eta = 0.1,
                       gamma=0,
                       min_child_weight = 1,
                       subsample = 1
)

set.seed(0)
fit.xgb = train(
  train_nn, y_train,  
  trControl = xgb_trcontrol,
  tuneGrid = xgbGrid,
  method = "xgbTree"
)

pred.xgb <- predict(fit.xgb, newdata=test_nn)

mean(abs(predict(fit.xgb, newdata=test_nn)-test_df$Util))
plot(test_df$Util, predict(fit.xgb, newdata = test_nn), xlim=c(0,70), ylim=c(0,70))
abline(0,1)

plot(
  c(as.numeric(train_df$Util),as.numeric(test_df$Util)), 
  c(predict(fit.xgb, newdata = train_nn), predict(fit.xgb, newdata=test_nn)),
  xlim=c(0,70), ylim=c(0,70),
  col=c(rep('blue',nrow(train_df)), rep('red',nrow(test_df))))
abline(0,1)


# Ridge -------------------------------------------------------------------

library(glmnet)


fit.ridge = cv.glmnet(train_data, as.matrix(train_labels), alpha=0, lambda=NULL)
coef(fit.ridge)
fit.ridge$lambda.min
pred.ridge <- predict(fit.ridge,newx=test_data)

mae(predict(fit.ridge,newx=test_data), test_df$Util)

# Lasso -------------------------------------------------------------------

fit.lasso = cv.glmnet(train_data, as.matrix(train_labels), alpha=1, lambda=NULL)
fit.lasso$lambda.min
pred.lasso <- predict(fit.lasso,newx=test_data)

mae(predict(fit.lasso,newx=test_data), test_df$Util)

# End ---------------------------------------------------------------------

#
# 결과들 저장
#
save.image(file='fitResult.RData')


# Ensemble Test ----------------------------------------------------------------

load('keras_model.RData')

vars <- ls()
predvars <- vars[grepl('^pred\\.',vars)]
results <- cbind(sapply(predvars,get))
actual <- test_df$Util

resid <- sweep(results,1,actual)

# mean values
fit.result <- data.frame(
  bias = colMeans((resid)),
  mae = colMeans(abs(resid)),
  rmse = sqrt(colMeans(resid^2)),
  mape = colMeans(abs(resid/results)))

# median values
colMeds <- function(x) apply(abs(x),2,median)
fit.result2 <- data.frame(
  bias = colMeds((resid)),
  mae = colMeds(abs(resid)),
  rmse = sqrt(colMeds(resid^2)),
  mape = colMeds(abs(resid/results)))

write.csv(file='Metric.csv',fit.result,row.names = FALSE)
write.csv(file='fitResult.csv',results,row.names = FALSE)

save.image()
