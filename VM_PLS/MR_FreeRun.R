MR_FreeRun <- function(PSx, PSy, newX, MR_Weight){

  Erroresult <- tryCatch({  
  X_mean = t(PSx[1,])
  X_std = t(PSx[2,])
  
  y_mean = t(PSy[1,])
  y_std = t(PSy[2,])
  
  source('mapstd.R')
  mapstd(newX, 1, X_mean, X_std)
  load("mapstd_Z.rda")
  
  new_lot_z = Z_value
  
  nlzi = nrow(new_lot_z) 
  nlzj = ncol(new_lot_z)
  
  NumOfPice = nrow(MR_Weight)
  NumOfPoint = ncol(MR_Weight)
  
  MR_FreeRun_Value = matrix(NaN, 1, NumOfPoint)
  
  for(NumPointInx in 1:NumOfPoint){
     W = matrix(MR_Weight[,NumPointInx])
     one = matrix(1, nlzi, 1)
     r_z_new = cbind(one, new_lot_z)
     r_z = r_z_new %*% W
     Yrhat = (r_z %*% y_std[1,NumPointInx]) +  y_mean[1,NumPointInx]
     MR_FreeRun_Value[1,NumPointInx] = Yrhat;
  }
  
  save(MR_FreeRun_Value, file = "MR_FreeRun.rda")
  
  }, warning = function(war) {
    print(paste("WARNING in MR_FreeRun:  ",war))
    
  }, error = function(err) {
    print(paste("ERROR in MR_FreeRun:  ",err))
    
  })
}