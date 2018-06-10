RUL <- function (Error, Terror, ModelRoute){

# 計算Target剩餘壽命  
    
# [BottomNum, Coefficient] = PreMainKernalExponent(Error);
  
  Error = t(Error)
  RunNumX  = nrow(Error)
  
  base = mean(Error[, 1])
  
  for (i in 1: RunNumX){
    if (Error[i,1] <= 0)
        Error[i,1] = base*0.000001
  }
  
  LogError = log(Error)
  RunNumX  = nrow(Error)
  one = matrix(1,RunNumX, 1)
  X = matrix(1:RunNumX, RunNumX, 1)
  RunX = cbind(one, X)
  #W = LogError / RunX
  #Py = RunX * W
  W = rbind(LogError,0)
  Py = LogError
  PredictiveValue = exp(Py)
  BottomNum = exp(W[2])
  Coefficient = exp(W[1])
  fitEquationValue = 0
  
  for (i in 1: RunNumX){
     fitEquation = Coefficient*(BottomNum^X[i]);
     fitEquationValue = cbind(fitEquationValue, fitEquation)
  }
  
  
# [yExponet, deadpoint] = fitplot(BottomNum,Coefficient, Terror, Error, ModelRoute);
  
  yExponet = matrix()
  
  load("CBIvalue.rda")
  load("ModelPdM.rda")
  load("sickNo.rda")
  
  Errornomber = nrow(Error)
  x = sickNo
  
  deadrun = log(Terror/Coefficient) / log(BottomNum)
  deadpoint = deadrun - x
  
  while (deadpoint < 0){
    
    BottomNum = BottomNum*0.999;
    if (BottomNum < 1){ 
       BottomNum = 1.009
    }
    deadrun = log(Terror/Coefficient) / log(BottomNum) 
    deadpoint = ceiling(deadrun - x)     
    
    while (BottomNum == 1.009 && deadpoint < 0){
       Coefficient = Coefficient*0.6
       deadrun = log(Terror/Coefficient) / log(BottomNum)
       deadpoint = ceiling(deadrun - x)            
    }
  }
  
  while  (deadpoint < 20 && Errornomber < 50){
    #  Coefficient = Coefficient*0.65;原來的
     BottomNum = BottomNum*0.999
     deadrun = log(Terror/Coefficient) / log(BottomNum)
     deadpoint = ceiling(deadrun - x)    
  }
  
  while  (deadpoint < 35  && Errornomber > 30){
   # Coefficient = Coefficient*0.9;原來的
     BottomNum = BottomNum*0.999;
     deadrun = log(Terror/Coefficient) / log(BottomNum)
     deadpoint = ceiling(deadrun - x)   
  }
  
  
  while  (deadpoint > 70 && Errornomber > 150){
      # Coefficient = Coefficient*1.2; 原來的
     BottomNum = BottomNum*1.005
     deadrun = log(Terror/Coefficient) / log(BottomNum)
     deadpoint = ceiling(deadrun - x)
      # process70 = 1;
  }
  
  while  (deadpoint > 60){
        # Coefficient = Coefficient*1.2; 原來的
     BottomNum = BottomNum*1.005
     deadrun = log(Terror/Coefficient) / log(BottomNum)
     deadpoint = ceiling(deadrun - x) 
       # process70 = 1;
  }
  
  # x =1:size(Error);
  # x2 = 1:deadrun;
  # yRegress = Interceptb + SlopeM*x2;
  
  for  (i in startNo: deadrun){
     y = Coefficient*BottomNum^i
     yExponet = matrix(c(yExponet, y),1)
  }
  
  
}