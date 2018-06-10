MCS_MR_OneByOneRun <- function(InX,InY,InTarget,NowGroup,InTempRoute,InModelRoute){
  
  #InX = read.table("C:/Users/Horace/Desktop/BPM_R/VM_PLS/InX.txt", header = FALSE)
  #InY = read.table("C:/Users/Horace/Desktop/BPM_R/VM_PLS/InY.txt", header = FALSE)
  #InTarget = 40
  #NowGroup = 1
  
  TotalnewX = InX
  TotalnewY = InY
  Target = InTarget
  ModelNum = NowGroup
  load("MCS_MR_Modeling.rda")
  
  TotalnewY_i = nrow(TotalnewY)
  TotalnewY_j = ncol(TotalnewY) # TotalnewY_i = Lot Num
  MODEL_X = X
  MODEL_Y = y
  
  source('PLSRegression.R')
  source('mapstd.R')
  source('MR_FreeRun.R')
  source('m_PLS_Components.R')
  
  Sample_time = matrix()
  mPLS_time = matrix(NaN,579,1)
  
  Erroresult <- tryCatch({
    
  ConjectureTime <- proc.time()
  for (TuneIdx in 1:TotalnewY_i){
  Sample <- proc.time()  
##---- Standardize ----##
    
    create_y = t(y)
    
    SSX = X
    size_X =dim(X)
    SSX_i = size_X[1]
    SSX_j = size_X[2]
    STDSSX_part1 = apply(SSX,2,sd)
    STDSSX_part1 = t(STDSSX_part1)
    # STDSSX_part2 = std(SSX(fix(SSX_i/2+1):end,:));
    Choose_Sensor_ID = c(1)
    for (i in 1 : SSX_j){
      # if STDSSX(1,i) ~= 0
      if (STDSSX_part1[1,i] <= 0.0001){
        
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
    
##----------##
    LotNum = nrow(y)
    PointNum  = ncol(y)
    
    for (NowPointInx in 1:PointNum){
      
      PLSRegression(Z_X_value,Z_y_value,PSy,NowPointInx,InModelRoute,ncomp)
      load("PLSRegression.rda")
      
      #MR_Predict_Zvalue = Predict_Zvalue
      #MR_PredictValue = PredictValue
      if ( NowPointInx == 1){
        MR_Weight = W
      }else{
        MR_Weight = cbind(MR_Weight, W)
      }
    
    }
    
    Choose_Sensor_ID = Choose_Sensor_ID[,-1]
    newX = TotalnewX[TuneIdx, Choose_Sensor_ID]
    
    
    MR_FreeRun(PSx,PSy,newX,MR_Weight)
    load("MR_FreeRun.rda")
    
    if (TuneIdx == 1){
       OutMR_Phase_I = matrix(MR_FreeRun_Value)
    }else{
       OutMR_Phase_I = rbind(OutMR_Phase_I, MR_FreeRun_Value)
    }
       
    X = rbind(MODEL_X, TotalnewX[TuneIdx,]) # combine array X & RunX
    y = rbind(MODEL_Y, TotalnewY[TuneIdx,]) # combine array y & Runy
  
## ====================  Phase II ===================== 
  
    create_y = t(y)
  
    SSX = X
    size_X =dim(X)
    SSX_i = size_X[1]
    SSX_j = size_X[2]
    STDSSX_part1 = apply(SSX,2,sd)
    STDSSX_part1 = t(STDSSX_part1)
    # STDSSX_part2 = std(SSX(fix(SSX_i/2+1):end,:));
    Choose_Sensor_ID = c(1)
    for (i in 1 : SSX_j){
      # if STDSSX(1,i) ~= 0
      if (STDSSX_part1[1,i] <= 0.0001){
      
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
    
    
##----------##
  
    InX_II = create_X
    InY_II = t(create_y)
     
    #m_PLS_Components(InX_II, InY_II, 1)
    #load("m_PLS_Components.rda")
    #ncomp = 4
    
    LotNum = nrow(y)
    PointNum  = ncol(y)
    for (NowPointInx in 1:PointNum){
    
      PLSRegression(Z_X_value, Z_y_value, PSy,NowPointInx, InModelRoute, ncomp)
      load("PLSRegression.rda")
      
      #MR_Predict_Zvalue = Predict_Zvalue
      #MR_PredictValue = PredictValue
      
      if ( NowPointInx == 1){
         MR_Weight = W
      }else{
         MR_Weight = cbind(MR_Weight, W)
      }
    }
    
    Choose_Sensor_ID = Choose_Sensor_ID[,-1]
    newX = TotalnewX[TuneIdx, Choose_Sensor_ID]
    
    MR_FreeRun(PSx,PSy,newX,MR_Weight)
    load("MR_FreeRun.rda")
    
    if (TuneIdx == 1){
       OutMR_Phase_II = matrix(MR_FreeRun_Value)
    }else{
       OutMR_Phase_II = rbind(OutMR_Phase_II, MR_FreeRun_Value)
    }
  
    Sample_n = proc.time() - Sample
    Sample_time = rbind(Sample_time, Sample_n[3])
    rm(Sample_n)
  }
  
  
##＝＝＝＝＝＝＝　Calculate Precision (Error , MaxError , MAPE)　＝＝＝＝＝＝＝＝＝##
  
  PointNum = ncol(TotalnewY)
  for (NowPointInx in 1:PointNum){
      if (NowPointInx == 1){
        
         OutMR_PhaseI_Error     = matrix((abs(OutMR_Phase_I[,NowPointInx] - TotalnewY[,NowPointInx]))) #./Target(1,NowPointInx)) * 100;
         OutMR_PhaseI_MaxError  = max(OutMR_PhaseI_Error[,NowPointInx])
         OutMR_PhaseI_MAPE      = mean(OutMR_PhaseI_Error[,NowPointInx])
         OutMR_PhaseII_Error    = matrix((abs(OutMR_Phase_II[,NowPointInx] - TotalnewY[,NowPointInx]))) #./Target(1,NowPointInx)) * 100;
         OutMR_PhaseII_MaxError = max(OutMR_PhaseII_Error[,NowPointInx])
         OutMR_PhaseII_MAPE     = mean(OutMR_PhaseII_Error[,NowPointInx])
         
      }else{
         OutMR_PhaseI_Error     = cbind(OutMR_PhaseI_Error, matrix((abs(OutMR_Phase_I[,NowPointInx] - TotalnewY[,NowPointInx])))) #./Target(1,NowPointInx)) * 100;
         OutMR_PhaseI_MaxError  = cbind(OutMR_PhaseI_MaxError, max(OutMR_PhaseI_Error[,NowPointInx]))
         OutMR_PhaseI_MAPE      = cbind(OutMR_PhaseI_MAPE, mean(OutMR_PhaseI_Error[,NowPointInx]))
         OutMR_PhaseII_Error    = cbind(OutMR_PhaseII_Error, matrix((abs(OutMR_Phase_II[,NowPointInx] - TotalnewY[,NowPointInx])))) #./Target(1,NowPointInx)) * 100;
         OutMR_PhaseII_MaxError = cbind(OutMR_PhaseII_MaxError, max(OutMR_PhaseII_Error[,NowPointInx]))
         OutMR_PhaseII_MAPE     = cbind(OutMR_PhaseII_MAPE, mean(OutMR_PhaseII_Error[,NowPointInx]))
      }
  }
  
  Phase_I = OutMR_Phase_I
  PhaseI_Error = OutMR_PhaseI_Error
  PhaseI_MaxError = OutMR_PhaseI_MaxError
  PhaseI_MAPE = OutMR_PhaseI_MAPE  
  Phase_II = OutMR_Phase_II
  PhaseII_Error = OutMR_PhaseII_Error
  PhaseII_MaxError = OutMR_PhaseII_MaxError
  PhaseII_MAPE = OutMR_PhaseII_MAPE
  
  proc.time() - ConjectureTime
  
  Sample_time = Sample_time[-1,]
  
  save(Phase_I ,PhaseI_Error ,PhaseI_MaxError ,PhaseI_MAPE, Sample_time,
       Phase_II ,PhaseII_Error,PhaseII_MaxError,PhaseII_MAPE, ConjectureTime, mPLS_time,
       file = "MCS_MR_OneByOneRun.rda")
  
  }, warning = function(war) {
    print(paste("WARNING in MCS_MR_OneByOneRun:  ",war))
    
  }, Error = function(err) {
    print(paste("Error in MCS_MR_OneByOneRun:  ",err))
    
  })
}