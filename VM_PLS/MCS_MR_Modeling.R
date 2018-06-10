MCS_MR_Modeling <- function(InX,InY,InTarget,NowGroup,InTempRoute,InModelRoute,InSelectAlgorithm){
  
  #InX = read.table("C:/Users/Horace/Desktop/BPM_R/VM_PLS/InX.txt", header = FALSE)
  #InY = read.table("C:/Users/Horace/Desktop/BPM_R/VM_PLS/InY.txt", header = FALSE)
  #InTarget = 10
  #NowGroup = 1
  
  X = InX
  y = InY
  Target = t(InTarget)
  ModelNum = t(NowGroup)
  
  Erroresult <- tryCatch({
  ConjectureTime <- proc.time() 
##---- Standardize ----##
  
  create_y = t(y)
  
  SSX = X
  size_X =dim(X)
  SSX_i = size_X[1]
  SSX_j = size_X[2]
  STDSSX_part1 = apply(SSX,2,sd)
  STDSSX_part1 = as.matrix(STDSSX_part1)
  # STDSSX_part2 = std(SSX(fix(SSX_i/2+1):end,:));
  Choose_Sensor_ID = c(1)
  for (i in 1 : SSX_j){
  # if STDSSX(1,i) ~= 0
     if (STDSSX_part1[i, 1] <= 0.0001){
  
     }else{
        Choose_Sensor_ID =  cbind(Choose_Sensor_ID, i) 
     }
  }
  
  size_Choose_Sensor_ID = dim(Choose_Sensor_ID)
  chsi = size_Choose_Sensor_ID[1]
  chsj = size_Choose_Sensor_ID[2]
  create_X = X[,Choose_Sensor_ID[1,1]]
  
  if (chsj >= 3){
    for (chs in 3 : chsj){
      create_X = cbind(create_X, X[,Choose_Sensor_ID[1,chs]])
    }
  }
  
  source('mapstd.R')
  mapstd(create_X,0)
  load("mapstd_Z.rda")
  
  Z_X_value = Z_value
  X_mean = mu
  X_std = s
  
  mapstd(y,0)
  load("mapstd_Z.rda")
  
  Z_y_value = Z_value
  y_mean = mu
  y_std = s
  
  PSx = rbind(X_mean, X_std)
  PSy = rbind(y_mean, y_std)
  
  #create_X = t(create_X)
  
  # 將全數點的X做標準化
  #[Z_X_value, PSx] = mapstd(create_X )  # [Z_X_value1,X_mean,X_std] = prestd(X);
  
  #Z_X_value = t(Z_X_value)
  
  # 將y做標準化
  #[Z_y_value, PSy] = mapstd(create_y) # [Z_y_value,y_mean,y_std] = prestd(create_y);
  #Z_y_value = t(Z_y_value)

  
##---- PLS ----##
  
  InX = create_X
  InY = t(create_y)
  source('m_PLS_Components.R')
  m_PLS_Components(InX, InY, 1)
  load("m_PLS_Components.rda")
  

##---- Modeling ----##
  
  size_y = dim(y)
  LotNum = size_y[1] 
  PointNum = size_y[2]
  source('PLSRegression.R')
  
  
  for (NowPointInx in 1:PointNum){
    
    
    PLSRegression(Z_X_value, Z_y_value, PSy, NowPointInx, InModelRoute, ncomp)
    load("PLSRegression.rda")
    
    MR_Predict_Zvalue = Predict_Zvalue
    MR_PredictValue = PredictValue
    
    if (NowPointInx ==1){
       MR_Weight = W
       OutMR_Stage_I = MR_PredictValue
    }else{
       MR_Weight = cbind(MR_Weight, W)
       OutMR_Stage_I = cbind(OutMR_Stage_I,MR_PredictValue)
    }
    
    # MR_PredictValue =  MR_Stage_I
    
    # Calculate Precision (Error , MaxError , MAPE)
    if (NowPointInx ==1){
       OutStageI_MR_Error = matrix((abs(OutMR_Stage_I[,NowPointInx] - y[,NowPointInx]) / Target[1,NowPointInx]) * 100)
       OutStageI_MR_MaxError= max(OutStageI_MR_Error[,NowPointInx])
       OutStageI_MR_MAPE = mean(OutStageI_MR_Error[,NowPointInx])
    }else{
      OutStageI_MR_Error = cbind(OutStageI_MR_Error, matrix((abs(OutMR_Stage_I[,NowPointInx] - y[,NowPointInx]) / Target[1,NowPointInx]) * 100))
      OutStageI_MR_MaxError = cbind(OutStageI_MR_MaxError, max(OutStageI_MR_Error[,NowPointInx]))
      OutStageI_MR_MAPE = cbind(OutStageI_MR_MAPE, mean(OutStageI_MR_Error[,NowPointInx]))
    }
  }
  
  proc.time() - ConjectureTime
  
  OutMR_Stage_II = OutMR_Stage_I     
  OutStageII_MR_Error = OutStageI_MR_Error
  OutStageII_MR_MaxError = OutStageI_MR_MaxError  
  OutStageII_MR_MAPE = OutStageI_MR_MAPE
  
  
  #PLS_LeaveOneOut(create_X, y,InSelectAlgorithm,InModelRoute,ncomp)
  #load('PLS_LeaveOneOut.rda')
  
  #OutMR_RT = MR_RT
  RefreshTimes = 0
  IsRefreshOK_Index = 0
  #Refresh_Error = []
  historyRefreshContinuous = 0
  MRRefreshCounter = 0
  
  Phase_I = OutMR_Stage_I
  PhaseI_Error = OutStageI_MR_Error
  PhaseI_MaxError = OutStageI_MR_MaxError
  PhaseI_MAPE = OutStageI_MR_MAPE
  Phase_II = OutMR_Stage_II
  PhaseII_Error = OutStageII_MR_Error
  PhaseII_MaxError = OutStageII_MR_MaxError
  PhaseII_MAPE = OutStageII_MR_MAPE
  
  save(Phase_I ,PhaseI_Error ,PhaseI_MaxError ,PhaseI_MAPE,
       Phase_II,PhaseII_Error,PhaseII_MaxError,PhaseII_MAPE,
       X, y, ncomp, ConjectureTime, file = "MCS_MR_Modeling.rda")
  
  }, warning = function(war) {
    print(paste("WARNING in MCS_MR_Modeling:  ",war))
    
  }, Error = function(err) {
    print(paste("Error in MCS_MR_Modeling:  ",err))
    
  })
  
}