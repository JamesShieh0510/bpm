Main_AVM_ControlKernel <- function(InTempRoute, InModelRoute){
  
  All_ConjectureID = 1
  TempConjectureTime = 0
  load("../DataTransfer_OneByOneData.rda")
  
  GroupSize = GroupInfo.list$GroupSize
  
  source('MCS_MR_OneByOneRun.R')
  
  Erroresult <- tryCatch({
  for (NowGroup in 1:GroupSize){
    
    temp = paste0("InX = Group",NowGroup,".list$X")
    eval(parse(text = temp))
    
    temp = paste0("InY = Group",NowGroup,".list$Y")
    eval(parse(text = temp))
    
    temp = paste0("InTarget = Group",NowGroup,".list$Target")
    eval(parse(text = temp))
    
    temp = paste0("PointID = Group",NowGroup,".list$PointID")
    eval(parse(text = temp))
    
    temp = paste0("PointSiteIdx = Group",NowGroup,".list$PointSiteIdx")
    eval(parse(text = temp))
    
    TempPointSiteIdx  =  PointSiteIdx
    
    size_Y = dim(InY)
    LotNum = size_Y[1]
    PointNum = size_Y[2]
    
    MCS_MR_OneByOneRun(InX,InY,InTarget,NowGroup,InTempRoute,InModelRoute)
    load("MCS_MR_OneByOneRun.rda")
    
    TempContextID      = GroupInfo.list$ContextID
    TempPhaseID        = rbind(4* matrix(1, LotNum, 1), 5* matrix(1, LotNum, 1))  #PhaseID, ID: 4: PhaseI & 5: PhaseI
    TempPointID        = PointID
    TempPredictValue   = Phase_II
    TempErrorValue     = PhaseII_Error
    TempMaxError       = PhaseII_MaxError
    TempMAPE           = PhaseII_MAPE
    #TempRT             = RT_Value
    TempConjectureTime = TempConjectureTime + ConjectureTime 
  }
  
  All_ContextID      = TempContextID
  All_PhaseID        = TempPhaseID
  All_PointID        = TempPointID
  All_PredictValue   = TempPredictValue
  All_ErrorValue     = TempErrorValue
  All_MaxError       = TempMaxError
  All_MAPE           = TempMAPE
  #All_RT             = TempRT
  All_ConjectureTime = TempConjectureTime
  
##---- ¡iMend Matrix¡j ----##
  
  size_PredictValue = dim(All_PredictValue)
  LotNum = size_PredictValue[1]
  PointNum = size_PredictValue[2]
  
  TempContextID = All_ContextID[,1]   
  All_ContextID = TempContextID
  TempPhaseID   = All_PhaseID[,1]     
  All_PhaseID   = TempPhaseID
  TempPointID   = All_PointID[1,]        
  All_PointID   = TempPointID
  #TempRT        = All_RT[1,] 
  #All_RT        = TempRT
  
  if (PointNum >= 2){
      for (NowMendMatrix in 2:PointNum){
          All_ContextID = cbind(All_ContextID, TempContextID)
          All_PhaseID   = cbind(All_PhaseID, TempPhaseID)
      }
  }
  
  if (LotNum >= 2){
     for (NowMendMatrix in 2:LotNum){
          All_PointID = rbind(All_PointID, TempPointID)
          #All_RT      = rbind(All_RT, TempRT)
     }
  }
  
##---- ¡iRearrange Value¡j ----##
  
  All_ContextID = t(All_ContextID)
  All_MaxError = t(All_MaxError)
  All_MAPE = t(All_MAPE)
  All_PhaseID = t(All_PhaseID)
  All_PointID = as.matrix(All_PointID)
  for (NowRearrange in 1:PointNum){
      if (NowRearrange == 1){
         OutAll_ContextID    = All_ContextID[,NowRearrange]
         OutAll_PhaseID      = All_PhaseID[,NowRearrange]
         OutAll_PointID      = All_PointID[,NowRearrange]
         OutAll_PredictValue = All_PredictValue[,NowRearrange]
         OutAll_ErrorValue   = All_ErrorValue[,NowRearrange]
         OutAll_MaxError     = All_MaxError[,NowRearrange]
         OutAll_MAPE         = All_MAPE[,NowRearrange]
      }else{
         OutAll_ContextID[,TempPointSiteIdx[1,NowRearrange]]    = All_ContextID[,NowRearrange]
         OutAll_PhaseID[,TempPointSiteIdx[1,NowRearrange]]      = All_PhaseID[,NowRearrange]
         OutAll_PointID[,TempPointSiteIdx[1,NowRearrange]]      = All_PointID[,NowRearrange]
         OutAll_PredictValue[,TempPointSiteIdx[1,NowRearrange]] = All_PredictValue[,NowRearrange]
         OutAll_ErrorValue[,TempPointSiteIdx[1,NowRearrange]]   = All_ErrorValue[,NowRearrange]
         OutAll_MaxError[,TempPointSiteIdx[1,NowRearrange]]     = All_MaxError[,NowRearrange]
         OutAll_MAPE[,TempPointSiteIdx[1,NowRearrange]]         = All_MAPE[,NowRearrange]
         #OutAll_RT[,TempPointSiteIdx[1,NowRearrange]]           = All_RT[,NowRearrange]
      }
  }
  
  save(OutAll_PredictValue, OutAll_ErrorValue, file = "AVM_ControlKernel.rda")
  
  }, warning = function(war) {
    print(paste("WARNING in Main_AVM_ControlKernel:  ",war))
    
  }, error = function(err) {
    print(paste("ERROR in Main_AVM_ControlKernel:  ",err))
    
     finally = {
     print(paste("End Try&Catch"))
     }
  })
}