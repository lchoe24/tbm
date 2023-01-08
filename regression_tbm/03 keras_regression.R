library(keras)

#
# keras 인스톨 방법:
#  Anaconda 인스톨(anaconda.org)
#  keras 인스톨 (install.packages)
#
#  library(kerase) 한 후
#  install_keras() 실행
#

#
# 사전에 계산해 놓은 학습/테스트 데이터를 사용한다 (prepare_data.R)
#
load('data.RData')

#
# 2 hidden leyer network 64x64 element, 0.1 dropout 사용
#
build_model <- function() {
  
  model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu",
                input_shape = dim(train_data)[2]) %>%
    layer_dropout(0.1) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop(),
    metrics = list("mean_absolute_error")
  )
  
  model
}

model <- build_model()
model %>% summary()

# 중간에 점 찍기
print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)    

epochs <- 1000

# Fit the model and store training stats
history <- model %>% fit(
  train_data,
  train_labels,
  epochs = epochs,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(print_dot_callback)
)

library(ggplot2)

# 중간학습 결과 그래프 보기
plot(history, metrics = "mean_absolute_error", smooth = FALSE) +
  coord_cartesian()

# 에러 프린트
c(loss, mae) %<-% (model %>% evaluate(test_data, test_labels, verbose = 0))

paste0("Mean absolute error on test set:", sprintf("%.2f", mae ))

# 예측데이터를 이용해서 그림그리기

test_predictions <- model %>% predict(test_data)
train_predictions <- model %>% predict(train_data)
pred.keras <- test_predictions[ , 1]

plot(test_labels, test_predictions[,1], xlim=c(0,70), ylim=c(0,70), col='red',
     main = paste("MAE", 
                  round(
                    mean(abs(test_predictions[,1]-test_labels)),2)))
lines(train_labels, train_predictions[,1], col='blue',type='p')
     
abline(a=0,b=1)

save(model,test_data,train_data,test_labels,train_labels,pred.keras,file='keras_model.RData')
