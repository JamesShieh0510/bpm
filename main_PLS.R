main_PLS <- function(InTempRoute,InModelRoute){
  
  setwd("../BPM_R/VM_PLS")
  source('Main_MCS_ControlKernel.R')
  Main_MCS_ControlKernel(InTempRoute,InModelRoute)
  load("MCS_ControlKernel.rda")
  
  
  MR_ModelValue = matrix(OutAll_PredictValue)
  MR_ModelError = matrix(OutAll_ErrorValue)
  
  source('Main_AVM_ControlKernel.R')
  Main_AVM_ControlKernel(InTempRoute,InModelRoute)
  load("AVM_ControlKernel.rda")
  
  
  MR_AVMValue = matrix(OutAll_PredictValue)
  MR_AVMError = matrix(OutAll_ErrorValue)
  
  save(MR_AVMError, MR_AVMValue, file = "../main_PLS.rda")
  setwd("..")
}