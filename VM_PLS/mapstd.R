mapstd <- function(X, mode, Xmean, Xstd){
  
  X = as.matrix(X)

  if (mode == 0){ 
     mu = colMeans(X)
     s = apply(X, 2, sd)
     if(is.na(mu[2]) == FALSE){
       Z_value = (X - matrix(1, nrow(X), ncol(X)) %*% diag(mu)) / (matrix(1,nrow(X),ncol(X)) %*% diag(s))
     }else{
       Z_value = (X - matrix(1, nrow(X), ncol(X)) * mu) / (matrix(1,nrow(X),ncol(X)) * s)
     }
     
     #size_X = dim(X)
     #Z_value = matrix(NaN,size_X[1],size_X[2])
     #for (row in 1: size_X[1]){
     #   for (col in 1: size_X[2]){
     #        Z_value[row, col] = (X[row, col] - mu[col]) / (s[col]) 
     #   }
     #}
  }
  
  if (mode == 1){    ##  'apply'
     mu = c(Xmean)
     s = c(Xstd)
     
     Z_value = (X - matrix(1,nrow(X),ncol(X)) %*% diag(mu)) / (matrix(1,nrow(X),ncol(X)) %*% diag(s))
     if(is.na(mu[2]) == FALSE){
        Z_value = (X - matrix(1, nrow(X), ncol(X)) %*% diag(mu)) / (matrix(1,nrow(X),ncol(X)) %*% diag(s))
     }else{
        Z_value = (X - matrix(1, nrow(X), ncol(X)) * mu) / (matrix(1,nrow(X),ncol(X)) * s)
     }
     
     #size_X = dim(X)
     #Z_value = matrix(NaN,size_X[1],size_X[2])
     #for (row in 1: size_X[1]){
     #   for (col in 1: size_X[2]){
     #       Z_value[row, col] = (X[row, col] - mu[col]) / (s[col]) 
     #   }
     #}
  }
  
  
  if (mode == 2){    ##  'reverse'
    mu = Xmean
    s = Xstd
    
    if(is.na(mu[2]) == FALSE){
       Z_value = (matrix(1,nrow(X),ncol(X)) %*% diag(mu)) + (X %*% diag(s))
    }else{
       Z_value = (matrix(1,nrow(X),ncol(X)) * mu) + (X * s)
    }
    
    #size_X = dim(X)
    #Z_value = matrix(NaN,size_X[1],size_X[2])
    #for (row in 1: size_X[1]){
    #  for (col in 1: size_X[2]){
    #     Z_value[row, col] = (X[row, col] * s[col]) + mu[col] 
    #  }
    #}
  }
  
  save(Z_value, mu, s, file = "mapstd_Z.rda")
  
}