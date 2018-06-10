PLSRegression <- function(Z_X_value,Z_y_value,PSy,NumPointInx,InModelRoute,ncomp){
  
  ncomp = t(ncomp)
  
  y_mean = t(PSy[1,])
  y_std = t(PSy[2,])
  
  x_combine_z = Z_X_value               # x_combine  i = Lot Num,  j = Sensor Num
  
  xci = nrow(x_combine_z)
  xcj = ncol(x_combine_z)
  
  y_combine_z = Z_y_value[,NumPointInx] # y_combine_z  i = Point Num,  j = Lot Num
  
  one = matrix(1, xci, 1) 
  X_r = cbind(one, x_combine_z)
  Y_r = matrix(y_combine_z)
 
##---- ncomp¨¾§b ----##
  
  if (ncomp[1,NumPointInx] > xcj){
      ncomp[1,NumPointInx] = xcj
  }
  
##---- Modeling -----## 
  
  source('plsregress.R')
  plsregress(x_combine_z, Y_r, ncomp[1,NumPointInx]) # PLS Weight Value
  load("plsregress.rda")
  
##---- Predict ----##
  
  X_z = x_combine_z  # Sensor Modeling Z Value
  
  zxi = nrow(X_z) 
  zxj = ncol(X_z)
  
  ones_r = matrix(1, zxi, 1)
  r_x = cbind(ones_r, X_z)
  r_z = r_x %*% W            # PLS Predict Value (Z Value)
  Predict_Zvalue = r_z
  
  PredictValue = (r_z %*% y_std[1,NumPointInx]) + y_mean[1,NumPointInx] # PLS Predict Value (Actual)
  
  save(W, PredictValue, Predict_Zvalue, file = "PLSRegression.rda")
}