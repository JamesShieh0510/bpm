CBIcalculate <- function (Sample, Route, ErrorMR, spec, Hotspec, RunX){

# 計算BEI值並記錄哪一筆資料開始sick  
    
##---- Load Data ----##
  x1 = 0
  x2 = 0
  x3 = 0 
  # ResultCBINN = [];
  ResultCBIMR = matrix()
  CBIvalue = matrix()
  # temp = ['load ' ModelRoute 'Config.txt'];eval(temp);
  #load C:\PAM\MHIModule\Model\Config.txt;
  # temp = ['load ' ModelRoute '\CBIvalue.mat'];eval(temp);
  
  load("BEI_value.rda")
  load("ModelPdM.rda")
  
##------------------## 
  
  WarmSpec = spec
  HardSpecBF = Hotspec
  AlarmSpec = HardSpecBF - Baseline_mean[1]
  Zero = 90 - Baseline_mean[1];  # By case 
  
##---- 計算CBI(BEI) ----##
  if (count_sick == 0){
    sickpointVector = ErrorMR
    sickpoint = 0
    
  }else if (count_sick == 1){
     sickpointVector = rbind(sickpointVector, ErrorMR)
     sickpoint = 0
  }else{
     sickpointVector = as.matrix(sickpointVector,ncol=1)
     sickpointVector = rbind(sickpointVector, ErrorMR)
     
     x1 = sickpointVector[2, 1] - sickpointVector[1, 1]
     x2 = sickpointVector[2, 1] - 5    
     x3 = sickpointVector[3, 1] - 5
     
     if (nrow(sickpointVector) >= 3){
        sickpointVector = sickpointVector[-1, ]
     }
     
     if (((x2 > 0 )&& (x2 < 2)) && ((x3 > 0) && (x3 < 2))  && ((x1 < 2) && (x1 > 0))&& (RunX[1,1] > Baseline_mean[1])){
       sickpoint = 1
       sickNo = Sample
       save(sickNo, sickpointVector, file = "sickNo.rda")
     }else{
       sickpoint = 0
     }
     #sickpointVector = sickpointVector[-1, ]
  }
  count_sick = count_sick + 1
  
##----------------------##  
  
  if (ErrorMR <= WarmSpec){
  
     CBIMR = 1- ( ErrorMR/ WarmSpec)*0.3
     
  }else if (ErrorMR > WarmSpec & ErrorMR <= AlarmSpec){
    
     CBIMR = 0.7 - ( ErrorMR/ (AlarmSpec - WarmSpec))*0.4
     
  }else{
    
     CBIMR = 0.3 - ( ErrorMR/ (Zero - AlarmSpec))*0.3
  }
  
  ResultCBIMR = cbind(Sample, CBIMR)
  
  BEI = ResultCBIMR[1, 2]
  
  if (bind_BEI == 0){
     BEI_value = ResultCBIMR
     bind_BEI =bind_BEI + 1
  }else{
    ResultCBIMR = as.matrix(ResultCBIMR)
    ResultCBIMR[1,1] =  nrow(BEI_value)+1
    BEI_value = rbind(BEI_value, ResultCBIMR)
  }
  
  #clear ErrorMR;
  
  save(BEI, BEI_value, sickpointVector, bind_BEI, sickpoint, count_sick, file = "BEI_value.rda")
  
}