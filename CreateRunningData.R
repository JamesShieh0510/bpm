CreateRunningData <- function(Running_X, Running_Y, InTempRoute, InModelRoute){
  
  USL = 50
  LSL = 20
  UCL = 45
  LCL = 25
  Target = 40
  
  ContextID           = matrix(1:nrow(Running_Y),ncol = 1)
  PieceID             = matrix(1:nrow(Running_Y),ncol = 1)
  IsAbnormal_X        = 0
  IsAbnormal_Y        = 0
  IsAbnormal_Y_IsOK_X = 0
  GroupSize           = 1
  IndicatorSize       = ncol(Running_X)
  PointSize           = ncol(Running_Y)
  IndicatorID         = matrix(1:ncol(Running_X),nrow = 1)
  PointID             = matrix(1:ncol(Running_Y),nrow = 1)
  ModelRoute          = InModelRoute
  TempRoute           = InTempRoute
  
  GroupInfo.list <- list(GroupSize=GroupSize, IndicatorSize=IndicatorSize, IndicatorID=IndicatorID, ContextID=ContextID, 
                         PieceID=PieceID, PointSize=PointSize, PointID=PointID, ModelRoute=ModelRoute, TempRoute=TempRoute,
                         IsAbnormal_X=IsAbnormal_X, IsAbnormal_Y=IsAbnormal_Y, IsAbnormal_Y_IsOK_X=IsAbnormal_Y_IsOK_X)
  
  IndicatorSize_1        = ncol(Running_X)
  PointSize_1            = ncol(Running_Y)
  IndicatorID_1          = matrix(1:ncol(Running_X),nrow = 1)
  PointID_1              = matrix(1:ncol(Running_Y),nrow = 1)
  PointSiteIdx           = matrix(1:ncol(Running_Y),nrow = 1)
  USL_1                  = matrix(1,1,ncol(Running_Y))*USL
  LSL_1                  = matrix(1,1,ncol(Running_Y))*LSL
  UCL_1                  = matrix(1,1,ncol(Running_Y))*UCL
  LCL_1                  = matrix(1,1,ncol(Running_Y))*LCL
  Target_1               = matrix(1,1,ncol(Running_Y))*Target
  IndicatorUSL           = matrix(1,1,ncol(Running_X))*10000
  IndicatorLSL           = matrix(1,1,ncol(Running_X))*-10000
  IndicatorUCL           = matrix(1,1,ncol(Running_X))*10000
  IndicatorLCL           = matrix(1,1,ncol(Running_X))*-10000
  X                      = Running_X
  Y                      = Running_Y
  
  Group1.list <- list(IndicatorSize=IndicatorSize_1, IndicatorID=IndicatorID_1, PointSize=PointSize_1, PointID=PointID_1,
                      PointSiteIdx=PointSiteIdx, USL=USL_1, LSL=LSL_1, UCL=UCL_1, LCL=LCL_1, Target=Target_1, X=X, Y=Y,
                      IndicatorUSL=IndicatorUSL, IndicatorLSL=IndicatorLSL, IndicatorUCL=IndicatorUCL, IndicatorLCL=IndicatorLCL)
  
  save(GroupInfo.list, Group1.list, file ="DataTransfer_RunData.rda")
  save(GroupInfo.list, Group1.list, file ="DataTransfer_OneByOneData.rda")
  rm( list = ls ( all = TRUE))
}
