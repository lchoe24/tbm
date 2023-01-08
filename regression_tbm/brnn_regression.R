library(brnn)
library(dplyr)
library(ggplot2)

load('data.RData')

# two layer network (=regression)
# train_labels (y) must be a vector

fit.brnn <- brnn(train_data, as.vector(train_labels), neuros = 64, normalize=TRUE,
                 epochs = 5000, mu = 0.05, mu_dec = 0.1, verbose = TRUE)

plot(as.vector(test_labels), predict(fit.brnn, test_data))
abline(a=0,b=1)

err <- as.vector(test_labels) - predict(fit.brnn, test_data)
summary(err)

# MAE
test_df <- df0[test.idx,]
test_df$err <- err

test_df %>% arrange(desc(abs(err))) %>%
  ggplot() + geom_boxplot(aes(y=err,group=Type))

test_df %>% arrange(desc(abs(err))) %>%
  ggplot() + geom_boxplot(aes(y=err,group=GeoCon))

test_df %>% arrange(desc(abs(err))) %>%
  ggplot() + geom_point(aes(y=err,x=Length, color=Type))

test_df %>% arrange(desc(abs(err))) %>%
  ggplot() + geom_point(aes(y=err,x=Diameter, color=Type))

test_df %>% arrange(desc(abs(err))) %>%
  ggplot() + geom_point(aes(y=err,x=Length, color=GeoCon))

test_df %>% arrange(desc(abs(err))) %>%
  ggplot() + geom_point(aes(y=err,x=Diameter, color=GeoCon))
