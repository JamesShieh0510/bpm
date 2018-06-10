Reasoning <- function (status, patternType, MHI, CBIvalue, ISI_Xrun, ISIT, Route){

  ## x1為AVM或IPM所送出之message   1：AVM   2：IPM
  ## x2為目前狀態     0：正常 1：Warning  2：Alarm
  ## x3為 pattern type
  ## x4為MHI 指標是否異常   0：正常 1：大於softspec 2：大於hardspec
  ## x5為CBI 指標是否異常   0：正常  1：大於softspec  2：大於hardspec  3：尚未有值。由於需要30筆建模，故在30筆前僅有PdM之結果。
  ## ISI_Alarm 值是否異常    0：正常 1：異常 2：尚未有值。
  ## sign 判斷燈號
  # temp = ['load ' ModelRoute 'ModelPdM.mat'];eval(temp);
  # load ModelPdM.mat
  
  x1 = 2 
  x2 = status 
  x3 = patternType
  
  parameter_num = ncol(ISI_Xrun)
  
  ISI_Alarm = matrix(0,1,parameter_num)
  
  if (MHI <= 0.3){ # 判斷MHI(模組健康狀態指標)是否大於門檻值
     x4 = 2
  }else if (MHI <= 0.7 & MHI > 0.3){
     x4 = 1
  }else{
     x4 = 0
  }
  
  
  if  (CBIvalue <= 0.3){ # 判斷CBI是否大於門檻值
      x5 = 2
  }else if (CBIvalue <= 0.7 & CBIvalue > 0.3){
      x5 = 1
  }else{
      x5 = 0
  }
  
  
  for (ISIT_test in 1: parameter_num){
    
     if (ISI_Xrun[1, ISIT_test] > ISIT){  # 判斷ISIB是否大於門檻值
       
        ISI_Alarm[1, ISIT_test] = 1
    
     }else{
        ISI_Alarm[1, ISIT_test] = 0
     }
  }
  
##---- 判斷燈號，當MHI及CBI皆有值時，進行判斷 ----##
  
  if ( MHI < 0.7 && sum(ISI_Alarm) == 0 || ( MHI < 0.7 && CBIvalue < 0.7 ) && sum(ISI_Alarm) == 0 ){
      sign = 4 # 紅燈
  }else if ((MHI < 0.7) && (sum(ISI_Alarm) >= 1 || CBIvalue < 0.7 )){
      sign = 3 #黃燈
  }else if ((MHI > 0.7) && sum(ISI_Alarm) >= 1){
      sign = 2 # 紫燈
  #    elseif (LeakRateAlarm == 1 || TempWarning1 ==1 || TempWarning2 ==1 || TempWarning3 ==1 || TempWarning4 ==1 || TempWarning5 ==1)
  #        sign = 1;%藍燈 
  }else{
      sign = 0 #綠燈
  }
  
  alarmCode = cbind(x1, x2, x3, x4, x5, ISI_Alarm, sign)
  save(alarmCode, file = "alarmCode.rda")
}