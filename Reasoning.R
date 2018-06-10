Reasoning <- function (status, patternType, MHI, CBIvalue, ISI_Xrun, ISIT, Route){

  ## x1��AVM��IPM�Ұe�X��message   1�GAVM   2�GIPM
  ## x2���ثe���A     0�G���` 1�GWarning  2�GAlarm
  ## x3�� pattern type
  ## x4��MHI ���ЬO�_���`   0�G���` 1�G�j��softspec 2�G�j��hardspec
  ## x5��CBI ���ЬO�_���`   0�G���`  1�G�j��softspec  2�G�j��hardspec  3�G�|�����ȡC�ѩ�ݭn30���ؼҡA�G�b30���e�Ȧ�PdM�����G�C
  ## ISI_Alarm �ȬO�_���`    0�G���` 1�G���` 2�G�|�����ȡC
  ## sign �P�_�O��
  # temp = ['load ' ModelRoute 'ModelPdM.mat'];eval(temp);
  # load ModelPdM.mat
  
  x1 = 2 
  x2 = status 
  x3 = patternType
  
  parameter_num = ncol(ISI_Xrun)
  
  ISI_Alarm = matrix(0,1,parameter_num)
  
  if (MHI <= 0.3){ # �P�_MHI(�Ҳհ��d���A����)�O�_�j����e��
     x4 = 2
  }else if (MHI <= 0.7 & MHI > 0.3){
     x4 = 1
  }else{
     x4 = 0
  }
  
  
  if  (CBIvalue <= 0.3){ # �P�_CBI�O�_�j����e��
      x5 = 2
  }else if (CBIvalue <= 0.7 & CBIvalue > 0.3){
      x5 = 1
  }else{
      x5 = 0
  }
  
  
  for (ISIT_test in 1: parameter_num){
    
     if (ISI_Xrun[1, ISIT_test] > ISIT){  # �P�_ISIB�O�_�j����e��
       
        ISI_Alarm[1, ISIT_test] = 1
    
     }else{
        ISI_Alarm[1, ISIT_test] = 0
     }
  }
  
##---- �P�_�O���A��MHI��CBI�Ҧ��ȮɡA�i��P�_ ----##
  
  if ( MHI < 0.7 && sum(ISI_Alarm) == 0 || ( MHI < 0.7 && CBIvalue < 0.7 ) && sum(ISI_Alarm) == 0 ){
      sign = 4 # ���O
  }else if ((MHI < 0.7) && (sum(ISI_Alarm) >= 1 || CBIvalue < 0.7 )){
      sign = 3 #���O
  }else if ((MHI > 0.7) && sum(ISI_Alarm) >= 1){
      sign = 2 # ���O
  #    elseif (LeakRateAlarm == 1 || TempWarning1 ==1 || TempWarning2 ==1 || TempWarning3 ==1 || TempWarning4 ==1 || TempWarning5 ==1)
  #        sign = 1;%�ſO 
  }else{
      sign = 0 #��O
  }
  
  alarmCode = cbind(x1, x2, x3, x4, x5, ISI_Alarm, sign)
  save(alarmCode, file = "alarmCode.rda")
}