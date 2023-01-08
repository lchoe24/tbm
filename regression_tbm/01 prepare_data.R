#
# prepare_data.R 
# 데이터를 준비하는 파일
# 러닝 모듈간 성능을 비교하기 위해 사전에 데이터를 학습/테스트 셋으로 나누고 
# 같은 데이터를 계속 사용한다
#

# 파일을 읽어들이고 이름을 정리한다. 
#Type, Geological Condition 이 String 으로 되어있다면 팩터로 고친다.

df0 <- read.csv('list.csv')
names(df0) <- c('Util','Type','Diameter','Length','GeoCon')
df0$Type <- factor(df0$Type)
df0$GeoCon <- factor(df0$GeoCon)

#
# keras neural net 에서 팩터를 사용하기 위해서는 
# 1. 수치 데이터들은 범위를 맞추어주고
# 2. 팩터 데이터의 경우 N-개의 값을 가지는 팩터를 0,1 의 값을 가지는 N-개의 변수로 만들어야 한다
#
# 아래는 팩터를 처리하기 위한 루틴임
#
num_predictors <- c('Diameter','Length')
cat_predictors <- c('Type','GeoCon')

get_cat_mat <- function(data, label){
  mat <- matrix(0, nrow=nrow(data), ncol = nrow(unique(data[,label])))
  for(i in 1:nrow(mat)){
    mat[i, as.numeric(data[i,label])] <- 1
  }
  colnames(mat) <- paste(label,levels(data[,label,drop=T]),sep='_')
  mat
}

# 먼저 숫자데이터는 범위를 맞춘다
df <- cbind(df0[,num_predictors])

response <- c('Util')

#
# 숫자데이터를 8:2 의 학습과 테스트 셋으로 나눈다
#

train.idx <- sample(1:nrow(df), 0.8*nrow(df))
test.idx <- (1:nrow(df0))[-train.idx]

train_data <- as.matrix(df[train.idx, ])
train_labels <- as.matrix(df0[train.idx, response])
test_data <- as.matrix(df[-train.idx, ])
test_labels <- as.matrix(df0[-train.idx, response])

# 학습데이터 값들을 0,1 범위로 스케일 하고 중앙값이 0 이 되도록 바꾼다
train_data <- scale(train_data)
col_means_train <- attr(train_data, "scaled:center")
col_stddevs_train <- attr(train_data, "scaled:scale")
test_data <- scale(test_data, center = col_means_train, scale = col_stddevs_train)

# 수치데이터와 팩터데이터를 합친다.
fmat <- cbind(get_cat_mat(df0,'Type'), get_cat_mat(df0,'GeoCon'))
train_data <- cbind(train_data, fmat[train.idx,])
test_data <- cbind(test_data, fmat[-train.idx,])

save(file='data.RData',
     train_data, train_labels, test_data, test_labels, train.idx, test.idx,
     response, cat_predictors, num_predictors, df0)
