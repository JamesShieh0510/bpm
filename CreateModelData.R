CreateModelData <- function(Create_X, Create_Y, InTempRoute, InModelRoute){
  
  USL = 50
  LSL = 20
  UCL = 45
  LCL = 25
  Target = 10
  
  GroupSize           = 1
  IndicatorSize       = ncol(Create_X)       #籹祘把计(X)计
  IndicatorID         = matrix(1:ncol(Create_X),nrow = 1)   #籹祘把计絪腹
  
  ContextID           = matrix(1:nrow(Create_Y),ncol = 1)  #秖代(Y)絪腹
  PieceID             = matrix(1:nrow(Create_Y),ncol = 1)  #秖代(Y)絪腹
  PointSize           = ncol(Create_Y)       #翴计
  PointID             = matrix(1:ncol(Create_Y),nrow = 1)   #翴絪腹
  
  IsAbnormal_X        = 0                      #﹚竡籹祘戈Τぶ钵盽(Default=0)
  IsAbnormal_Y        = 0                      #﹚竡秖代戈Τぶ钵盽(Default=0)
  IsAbnormal_Y_IsOK_X = 0
  
  ModelRoute          = InModelRoute
  TempRoute           = InTempRoute
  
  GroupInfo.list <- list(GroupSize=GroupSize, IndicatorSize=IndicatorSize, IndicatorID=IndicatorID, ContextID=ContextID, 
                         PieceID=PieceID, PointSize=PointSize, PointID=PointID, ModelRoute=ModelRoute, TempRoute=TempRoute,
                         IsAbnormal_X=IsAbnormal_X, IsAbnormal_Y=IsAbnormal_Y, IsAbnormal_Y_IsOK_X=IsAbnormal_Y_IsOK_X)
  
  
  PointSiteIdx           = matrix(1:ncol(Create_Y), nrow = 1)
  USL_1                  = matrix(1, 1, ncol(Create_Y))*USL
  LSL_1                  = matrix(1, 1, ncol(Create_Y))*LSL
  UCL_1                  = matrix(1, 1, ncol(Create_Y))*UCL
  LCL_1                  = matrix(1, 1, ncol(Create_Y))*LCL
  Target_1               = matrix(1, 1, ncol(Create_Y))*Target
  IndicatorUSL           = matrix(1, 1, ncol(Create_X))*10000
  IndicatorLSL           = matrix(1, 1, ncol(Create_X))*-10000
  IndicatorUCL           = matrix(1, 1, ncol(Create_X))*10000
  IndicatorLCL           = matrix(1, 1, ncol(Create_X))*-10000
  X                      = Create_X
  Y                      = Create_Y
  
  Group1.list <- list(IndicatorSize=IndicatorSize, IndicatorID=IndicatorID, PointSize=PointSize, PointID=PointID,
                      PointSiteIdx=PointSiteIdx, USL=USL_1, LSL=LSL_1, UCL=UCL_1, LCL=LCL_1, Target=Target_1, X=X, Y=Y,
                      IndicatorUSL=IndicatorUSL, IndicatorLSL=IndicatorLSL, IndicatorUCL=IndicatorUCL, IndicatorLCL=IndicatorLCL)
  
  save(GroupInfo.list, Group1.list, file ="DataTransfer_TrainData.rda")
  rm( list = ls ( all = TRUE))
}
  