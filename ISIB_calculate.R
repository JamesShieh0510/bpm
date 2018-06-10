ISIB_calculate <- function(X, Refresh_num){
  
  SSX = X
  x_size = dim(SSX)
  
 
  STDSSX_part1 = apply(SSX,2,sd)
  
  
  Choose_Sensor_ID = matrix(1:x_size[2],nrow = 1)
  id_size = dim(Choose_Sensor_ID)
  
  create_X = t(X[,Choose_Sensor_ID])

##
  size_craX = dim(create_X)
  
  ISIFreerun_X_Mean = apply(create_X[,(size_craX[2]-Refresh_num+1):(size_craX[2])],1,mean)
  
  ISIFreerun_X_std = 0.01*ISIFreerun_X_Mean
  
  Z_X_value = (t(create_X)-(matrix(1,ncol(create_X),1) %*% (apply(create_X,1,mean)))) / (matrix(1,ncol(create_X),1) %*% ISIFreerun_X_std)
  
  save(Z_X_value, ISIFreerun_X_Mean, ISIFreerun_X_std, file ="ISIB_calculate.rda")
}
