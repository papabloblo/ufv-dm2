
rmse <- function(real, pred) sqrt(mean((real - pred)**2))

pred_knn <- function(k, train_x, train_y, test_x){
  return(
    knn.reg(train = train_x, 
            y = train_y, 
            test = test_x, 
            k = k)$pred
  )
} 


plot_knn <- function(test_x, test_y, pred, k){
  test <- cbind(test_x, y = test_y)
  ggplot(
    data = test,
    aes(x = x)
  ) +
    geom_point(aes(y = y), alpha = .3) +
    geom_line(aes(y = pred), color = "firebrick") +
    labs(title = paste0('Predicción para test con k=', k)) +
    theme_minimal() 
}


train_knn <- function(k, train_x, train_y, test_x, test_y){
  
  pred_train <- pred_knn(k, train_x, train_y, train_x)
  pred_test <- pred_knn(k, train_x, train_y, test_x)
  
  rmse_train <- rmse(train_y, pred_train)
  rmse_test <- rmse(test_y, pred_test)
  
  return(
    list(
      rmse_train = rmse_train, 
      rmse_test = rmse_test, 
      pred_test = pred_test, 
      pred_train = pred_train
    )
  )
}


info_knn <- function(k, train_x, train_y, test_x, test_y){
  pred <- train_knn(k, train_x, train_y, test_x, test_y)
  
  cat('RMSE en entrenamiento: ', pred$rmse_train)
  cat('\nRMSE en test: ', pred$rmse_test)
  
  plot_knn(test_x, test_y, pred$pred_test, k)
}

evaluation <- function(K, train_x, train_y, test_x, test_y, echo = FALSE){
  rmse_train <- c()
  rmse_test <- c()
  
  for (k in K){
    res_knn <- train_knn(k, train_x, train_y, test_x, test_y)
    rmse_train <- c(rmse_train, res_knn$rmse_train)
    rmse_test <- c(rmse_test, res_knn$rmse_test)
    if (echo) {
      print(k)  
    }
  }
  
  df <- data.frame(k = K,
                   rmse_train = rmse_train,
                   rmse_test = rmse_test
                   )

  print(ggplot(df, aes(x = k)) +
    geom_line(aes(y = rmse_train, color = 'train')) + 
    geom_line(aes(y = rmse_test,  color = 'test')) + 
    labs(y = "Error", color = '') +
    scale_color_manual(values = c('test' = 'firebrick', 'train' = 'steelblue')) +
    theme_minimal()
  )
  
  return(df)
  
  # return(
  #   data.frame(
  #     k = K, 
  #     rmse_train = rmse_train, 
  #     rmse_test = rmse_test
  #   )
  # )
}


plot_neighbors <- function(k, train_x, train_y, x){
  
  pred <- train_knn(k=k, train_x, train_y, train_x, train_y)$pred_train
  
  ids <- knnx.index(data=train_x, query=data.frame(x = x), k = k)[1,]
  
  train <- cbind(train_x, y = train_y)
  
  train$ids <- 0
  train$ids[ids] <- 1
  
  train$alpha <- 0.1
  train$alpha[ids] <- 0.7
  
  
  ggplot(train, aes(x = x)) +
    geom_point(aes(y = y, color = as.character(ids), alpha = alpha)) +
    geom_line(aes(y = pred), colour = "#23629b", size = 1) +
    geom_point(x = x, y = mean(train_y[ids]), shape = 21, colour = "#23629b", fill = "#de7671", size = 4, stroke = 2) +
    scale_color_manual(values = c("1" = "#de7671", "0" = "#23629b")) +
    theme_minimal() + 
    scale_alpha(range = c(0.1, 0.6)) +
    theme(legend.position = "none")
  
}