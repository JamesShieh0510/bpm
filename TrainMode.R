TrainMode <- function(X, Route, spec, Hotspec, spec_Mode)
{
  
  Sample = nrow(X)
  parameter_num = ncol(X)
  Matrix_CL = matrix(NaN, 2, parameter_num)
  ##---- Load ----##
  
  load("Condition_train.rda")
  
  ##---- 定義參數 ----##
    
  Baseline_mean = apply(X, 2, mean)
  Baseline_std = apply(X, 2, sd)
  Baseline_mean_std = Baseline_std[1]
  
  ##---- 定義蝴蝶閥Spec & 其他參數管制上下界 ----##
  if (spec_Mode == 1){
    
      LeakRateCL = 100
  
      for (i in choose_parameter_spec){
    
         temp = paste0("X_UCL_",i," = (1+ parameter_spec)*Baseline_mean[",i,"]")
         eval(parse(text = temp))
    
         temp = paste0("X_LCL_",i," = (1- parameter_spec)*Baseline_mean[",i,"]")
         eval(parse(text = temp))    
      }

      for (i in choose_parameter_spec2){
    
         temp = paste0("X_UCL_",i," = (1+ parameter_spec2)*Baseline_mean[",i,"]")
         eval(parse(text = temp))
    
         temp = paste0("X_LCL_",i," = (1- parameter_spec2)*Baseline_mean[",i,"]")
         eval(parse(text = temp))    
      }
  
      for (i in choose_parameter_CL){
    
         temp = paste0("X_UCL_",i," = UCL + Baseline_mean[",i,"]")
         eval(parse(text = temp))
    
         temp = paste0("X_LCL_",i," = LCL - Baseline_mean[",i,"]")
         eval(parse(text = temp))    
      } 
  
      for (i in choose_parameter[1] : parameter_num){
    
         temp = paste0("Matrix_CL[1,",i," ] = X_UCL_",i)
         eval(parse(text = temp))
    
         temp = paste0("Matrix_CL[2,",i," ] = X_LCL_",i)
         eval(parse(text = temp))    
      }
      
  }else{
    
      LeakRateCL = 100
      for (i in choose_parameter){
        
          temp = paste0("X_UCL_",i," = Baseline_std[",i,"]*Std + Baseline_mean[",i,"]")
          eval(parse(text = temp))
        
          temp = paste0("X_LCL_",i," = (-1)*Baseline_std[",i,"]*Std + Baseline_mean[",i,"]")
          eval(parse(text = temp))
        
        
          temp = paste0("Matrix_CL[1,",i," ] = X_UCL_",i)
          eval(parse(text = temp))
        
          temp = paste0("Matrix_CL[2,",i," ] = X_LCL_",i)
          eval(parse(text = temp)) 
      }
    
  }  
  
  
  
  SoftSpecBFLCL =  Baseline_mean[1] - spec
  if (SoftSpecBFLCL < 0){
  SoftSpecBFLCL = 0
  }
  
  SoftSpecBFUCL =  Baseline_mean[1] + spec
  HardSpecBF = Hotspec
  BF3Q = Baseline_mean[1] + 3* Baseline_mean_std
  
  ErrorT = Hotspec - Baseline_mean[1]
  TrendindicatorTHD = 10
  
  
  save(LeakRateCL, TrendindicatorTHD, parameter_num, Matrix_CL, choose_parameter, Baseline_mean, Baseline_mean_std, SoftSpecBFLCL, SoftSpecBFUCL, spec, HardSpecBF, file = "CBIvalue.rda")
  save(Matrix_CL, choose_parameter, Baseline_mean, file = "ModelPdM.rda")
  save(Matrix_CL, BF3Q, ErrorT, Sample, file = "Train_Output.rda")
  
}