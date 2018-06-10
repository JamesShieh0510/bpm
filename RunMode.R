RunMode <- function(RunX, index, ModelRoute, BF3Q, bind_matrix, bind_start){

# 5掸ず璸衡DHI&魁Τ礚禬砏穓5掸秨﹍耞5翴ずΤ礚3翴禬砏璝Τ砫祇Alarm
      
##---- ﹍て ----##  
  newRunX = RunX
  bind_senario = 0
  #output1 = matrix()
  RunMetrix = matrix()        
  #alarmRecord = matrix()
  #result = matrix() 
  #result1 = matrix() 
  startpointVector = matrix()
  r1 = matrix() 
  #LeakRatearray = matrix()
  senario = matrix()           # 魁把计琌禬砏
  #senario_new = matrix()
  
  #MHI = 0
  #PatternType = 0
  #status = 0
##---- Load Data ----##
  
  load("Condition_run.rda")
  load("ModelPdM.rda")
  load("CBIvalue.rda")
  
##---- 魁 5 翴ずΤ礚禬筁 3 翴禬砏 ----##
  
  alarmRecord = matrix(0, 1, ncol(newRunX))
  
if (nrow(RunMetrix) < alarmwindow){
  
##---- 耞Throttle Valve琌禬筁50禬筁50玥耞Alarm ----##
  
    if  (newRunX[1, 1] >= HardSpecBF){   
    
    senario_1 = 2               # 0.タ盽, 1.warning, 2.Alarm             
    PatternType = 1
    
##---- 璸衡MHI ----##
    
    MHI = 0.3 - ((newRunX[1, 1] - HardSpecBF)/(90 - HardSpecBF))*0.3
    
##-------------------##
    
       for (i in choose_parameter[1] : parameter_num){
      
           if (newRunX[1, i] < Matrix_CL[2, i] || newRunX[1, i] > Matrix_CL[1, i]){
          
             temp = paste0("senario_",i,"= 1")
             eval(parse(text = temp))
          
           }else{
          
             temp = paste0("senario_",i,"= 0")
             eval(parse(text = temp))
           }
         
           if (bind_senario == 0){
             temp = paste0("senario = senario_",i)
             eval(parse(text = temp))
             bind_senario = bind_senario + 1
           }else{
              temp = paste0("senario = cbind(senario, senario_",i,")")
              eval(parse(text = temp))
           }
      
       }
    
       status = 2                 ## 耞把计琌タ盽 0.タ盽,1.warning, 2.Alarm
       
       if(bind_matrix == 0){
         senario_new = matrix(c(senario_1, senario),nrow = 1)
         x = matrix(c(1, newRunX, PatternType, status, MHI),1)
         RunMetrix = matrix(c(x, senario, alarmRecord),1)
         result =  matrix(c(x, senario, alarmRecord),1)
         bind_matrix = bind_matrix+1
       }else{
         senario_new = rbind(senario_new, matrix(c(senario_1, senario),1))
         x = matrix(c(nrow(RunMetrix)+1, newRunX, PatternType, status, MHI),1)
         RunMetrix = rbind(RunMetrix, matrix(c(x, senario, alarmRecord),1))
         result = rbind(result, matrix(c(x, senario, alarmRecord),1))
       }
    
##---- 耞ㄤ把计琌硈尿5翴钵盽 ----## 
       
       if (nrow(senario_new) == alarmwindow){ ##璝峨Ч单硈尿Alarm耞掸计杠玥秈︽耞                         
    
          senario_sum = apply(senario_new, 2, sum)
          for (i in 1: ncol(senario_sum)){
      
            if (senario_sum[i] >= AlarmRule)
               alarmRecord[i] = 1
            else
               alarmRecord[i] = 0 
            
          }
    
          if ((sum(alarmRecord)) >= 1){  ##璶ヴ把计硈尿5翴禬筁specAlarm
             status = 2
             RunMetrix [5, 16] = status
             result [5, 16] = status
          } 
       }
    
##---- Throttle Valve 禬筁spec ----##
    
    }else if(newRunX[1, 1] < SoftSpecBFLCL || newRunX[1, 1] > SoftSpecBFUCL){
      
        senario_1 = 1  # 0.タ盽, 1.warning, 2.Alarm
        PatternType =0
       
##---- 衡MHI秨﹍ ----##
       
        if (newRunX[1, 1] > Baseline_mean[1]){
           MHI = 0.7 - ((newRunX[1, 1] - SoftSpecBFUCL)/(HardSpecBF - SoftSpecBFUCL))*0.4
        }else{
           MHI = 0.7 - ((SoftSpecBFLCL - newRunX[1, 1])/(SoftSpecBFLCL - 5))*0.4
        }
       
##---- 耞ㄤ把计Τ礚钵盽 ----##
       
        for (i in choose_parameter[1] : parameter_num){
            if (newRunX[1, i] < Matrix_CL[2, i] || newRunX[1, i] > Matrix_CL[1, i]){
            
               temp = paste0("senario_",i,"= 1")
               eval(parse(text = temp))
            
            }else{
            
               temp = paste0("senario_",i,"= 0")
               eval(parse(text = temp)) 
            }
          
            if (bind_senario == 0){
               temp = paste0("senario = senario_",i)
               eval(parse(text = temp))
               bind_senario = bind_senario + 1
            }else{
              temp = paste0("senario = cbind(senario, senario_",i,")")
              eval(parse(text = temp))
            }
       }
       
       status = 1   # 0タ盽, 1warning, 2Alarm
       
       if(bind_matrix == 0){
         senario_new = matrix(c(senario_1, senario),nrow = 1)
         x = matrix(c(1, newRunX, PatternType, status, MHI),1)
         RunMetrix = matrix(c(x, senario, alarmRecord),1)
         result =  matrix(c(x, senario, alarmRecord),1)
         bind_matrix = bind_matrix+1
       }else{
         senario_new = rbind(senario_new, matrix(c(senario_1, senario),1))
         x = matrix(c(nrow(RunMetrix)+1, newRunX, PatternType, status, MHI),1)
         RunMetrix = rbind(RunMetrix, matrix(c(x, senario, alarmRecord),1))
         result = rbind(result, matrix(c(x, senario, alarmRecord),1))
       }
       
       
##---- 耞ㄤ把计琌硈尿5翴钵盽 ----##
       
       if (nrow(senario_new) == alarmwindow){  # 璝峨Ч单硈尿Alarm耞掸计杠玥秈︽耞
       
          senario_sum = apply(senario_new, 2, sum)
          for (i in 1:ncol(senario_sum)){
             if (senario_sum[i] >= AlarmRule){
                alarmRecord[i] = 1
             }else{
                alarmRecord[i] = 0
             }
          }
       
          if (sum(alarmRecord) >= 1){   # 璶ヴ把计硈尿5翴禬筁specAlarm
             status = 2
             RunMetrix [5, 16] = status
             result [index, 16] = status
          } 
       }
    

##---- Throttle Valveタ盽 ----##    %% X1 タ盽
   }else{
         senario_1 = 0    # 0.タ盽, 1.warning, 2.Alarm
         PatternType = 0
       
##---- 衡MHI ----##
         
       if (newRunX[1, 1] > Baseline_mean[1]){
          MHI = 1 - ((newRunX[1, 1] - Baseline_mean[1])/spec)*0.3
       }else{
          MHI = 1 - ((Baseline_mean[1] - newRunX[1,1])/spec)*0.3
       }
         
##---- 耞ㄤ把计Τ礚钵盽 ----##
         
       for (i in choose_parameter[1] : parameter_num){
           
            if (newRunX[1, i] < Matrix_CL[2, i] || newRunX[1, i] > Matrix_CL[1, i]){
              
               temp = paste0("senario_",i,"= 1")
               eval(parse(text = temp))
              
            }else{
              
               temp = paste0("senario_",i,"= 0")
               eval(parse(text = temp))
            }
         
           if (bind_senario == 0){
              temp = paste0("senario = senario_",i)
              eval(parse(text = temp))
              bind_senario = bind_senario + 1
           }else{
              temp = paste0("senario = cbind(senario, senario_",i,")")
              eval(parse(text = temp))
           }
       }
         
         if(bind_matrix == 0){
           senario_new = matrix(c(senario_1, senario),nrow = 1)
         }else{   
           senario_new = rbind(senario_new, matrix(c(senario_1, senario),1))
         }
         
# Status -- 0.タ盽, 1.warning  2.Alarm
         
         if (sum(senario) >= 1){
            status =1;
         }else{
            status =0;
         } 
         
        if(bind_matrix == 0){
          x = matrix(c(1, newRunX, PatternType, status, MHI),1)
          RunMetrix = matrix(c(x, senario, alarmRecord),1)
          result =  matrix(c(x, senario, alarmRecord),1)
          bind_matrix = bind_matrix+1
        }else{
          x = matrix(c(nrow(RunMetrix)+1, newRunX, PatternType, status, MHI),1)
          RunMetrix = rbind(RunMetrix, matrix(c(x, senario, alarmRecord),1))
          result = rbind(result, matrix(c(x, senario, alarmRecord),1))
        }
         
##---- 耞ㄤ把计琌硈尿5翴钵盽 ----## 
         
         if (nrow(senario_new) == alarmwindow){  # 璝峨Ч单硈尿Alarm耞掸计杠玥秈︽耞
         
            senario_sum = apply(senario_new, 2, sum)
            for (i in 1: ncol(senario_sum)){
               if (senario_sum[i] >= AlarmRule){
                  alarmRecord[i] = 1
               }else{
                  alarmRecord[i] = 0 
               }
            }                   
         
         if (sum(alarmRecord) >= 1){  # 璶ヴ把计硈尿5翴禬筁specAlarm
            status = 2
            RunMetrix [5, 16] = status
            result [index, 16] = status
         }
         }                      
    }
}else{
##---- 戈禬筁Alamrwindow猵矪瞶 (Alamrwindow ヘ玡璹5) ----##
##---- 耞Throttle Valve琌禬筁50禬筁50玥耞Alarm ----##   
    if  (newRunX[1, 1] >= HardSpecBF){ 
         
       senario_1 = 2  # 0タ盽, 1.warning, 2.Alarm
       PatternType = 1
         
##---- 璸衡MHI----##
         
       MHI = 0.3 - ((newRunX[1, 1] - HardSpecBF)/(90 - HardSpecBF))*0.3
         
##---- 耞ㄤ把计Τ礚钵盽----##
         
       for (i in choose_parameter[1] : parameter_num){
          if (newRunX[1, i] < Matrix_CL[2, i] || newRunX[1, i] > Matrix_CL[1, i]){
              
              temp = paste0("senario_",i,"= 1")
              eval(parse(text = temp))
              
          }else{
              
              temp = paste0("senario_",i,"= 0")
              eval(parse(text = temp))
          }
         
          if (bind_senario == 0){
             temp = paste0("senario = senario_",i)
             eval(parse(text = temp))
             bind_senario = bind_senario + 1
          }else{
             temp = paste0("senario = cbind(senario, senario_",i,")")
             eval(parse(text = temp))
          }
       }
         
         status = 2  # 0.タ盽, 1.warning, 2.Alarm              
         
##--- 耞ㄤ把计琌硈尿5翴钵盽---##
         
         senario_new = senario_new[-1,]
         senario_new = rbind(senario_new, cbind(senario_1, senario))
         
         senario_sum = apply(senario_new, 2, sum)
         for (i in 1: ncol(senario_sum)){
            if (senario_sum[i] >= AlarmRule){
               alarmRecord(i) = 1
            }else{
               alarmRecord(i) = 0
            }
         }
         
         RunMetrix = RunMetrix[-1,]
         x = matrix(c(nrow(RunMetrix)+1, newRunX, PatternType, status, MHI),1)                   
         #RunMetrix[, 1] = [1:1:alarmwindow - 1];
         RunMetrix = rbind(RunMetrix, matrix(c(x, senario, alarmRecord),1))
         result = rbind(result, matrix(c(x, senario, alarmRecord),1))  
         
         if (sum(alarmRecord) >= 1){  # 璶ヴ把计硈尿5翴禬筁specAlarm
            status = 2
            RunMetrix [5, 16] = status;
            result [index, 16] = status;
         }
    
##---- Throttle Valve禬筁spec ----## 
    }else if  (newRunX[1, 1] < SoftSpecBFLCL || newRunX[1, 1] > SoftSpecBFUCL){
         
         senario_1 = 1  # 0タ盽, 1warning, 2Alarm
         PatternType = 1
         
##---- 衡MHI秨﹍ ----##
         
         if (newRunX[1, 1] > Baseline_mean[1]){
            MHI = 0.7 - ((newRunX[1, 1] - SoftSpecBFUCL)/(HardSpecBF - SoftSpecBFUCL))*0.4
         }else{
            MHI = 0.7 - ((SoftSpecBFLCL - newRunX[1, 1])/(SoftSpecBFLCL - 5))*0.4
         }
         
##---- 耞ㄤ把计Τ礚钵盽 ----##
         
         for (i in choose_parameter[1] : parameter_num){
            if (newRunX[1, i] < Matrix_CL[2, i] || newRunX[1, i] > Matrix_CL[1, i]){
              
              temp = paste0("senario_",i,"= 1")
              eval(parse(text = temp))
              
            }else{
              
              temp = paste0("senario_",i,"= 0")
              eval(parse(text = temp))
            }
           
            if (bind_senario == 0){
               temp = paste0("senario = senario_",i)
               eval(parse(text = temp))
               bind_senario = bind_senario + 1
            }else{
               temp = paste0("senario = cbind(senario, senario_",i,")")
               eval(parse(text = temp))
            }
         }
         
         senario_new = senario_new[-1,]
         senario_new = rbind(senario_new, matrix(c(senario_1, senario),1))
         status = 1 # 0.タ盽, 1.warning, 2.Alarm
         
##---- 耞ㄤ把计琌硈尿5翴钵盽 ----##
         
         senario_sum = apply(senario_new, 2, sum)
         for (i in 1: ncol(senario_sum)){
            if (senario_sum[i] >= AlarmRule){
               alarmRecord[i] = 1
            }else{
               alarmRecord[i] = 0 
            }
         }
         
         RunMetrix = RunMetrix[-1,]
         x = matrix(c(nrow(RunMetrix)+1,newRunX, PatternType, status, MHI),1)
         #RunMetrix(:, 1) = [1:1:alarmwindow - 1];
         RunMetrix = rbind(RunMetrix, matrix(c(x, senario, alarmRecord),1))
         ##x(1,1) = result(end,1)+1;
         result = rbind(result, matrix(c(x, senario, alarmRecord),1))  
         
         if (sum(alarmRecord) >=1){  # 璶ヴ把计硈尿5翴禬筁specAlarm
            status = 2
            RunMetrix [5, 16] = status
            result [index, 16] = status
         }

##---- Throttle Valveタ盽 ----##                                    
    }else{
##---- 衡MHI秨﹍ ----##
    
         if (newRunX[1, 1] > Baseline_mean[1]){
            MHI = 1 - ((newRunX[1, 1] - Baseline_mean[1])/spec)*0.3
         }else{
            MHI = 1 - ((Baseline_mean[1] - newRunX[1,1])/spec)*0.3
         }
         
         senario_1 = 0   # 0.タ盽, 1.warning, 2.Alarm
         PatternType = 0
         
##---- 耞ㄤ把计Τ礚钵盽 ----##
         
         for (i in choose_parameter[1] : parameter_num){
           
            if (newRunX[1, i] < Matrix_CL[2, i] || newRunX[1, i] > Matrix_CL[1, i]){
              
              temp = paste0("senario_",i,"= 1")
              eval(parse(text = temp))
               
            }else{
              
              temp = paste0("senario_",i,"= 0")
              eval(parse(text = temp))
            }
           
            if (bind_senario == 0){
               temp = paste0("senario = senario_",i)
               eval(parse(text = temp))
               bind_senario = bind_senario + 1
            }else{
               temp = paste0("senario = cbind(senario, senario_",i,")")
               eval(parse(text = temp))
            } 
           
         }
         
         senario_new = senario_new[-1,]
         senario_new = rbind(senario_new, matrix(c(senario_1, senario),1))
         
         # Status  0.タ盽, 1.warning, 2.Alarm
         
         if (sum(senario[1, 2:ncol(senario)]) >=1){ 
             status = 1 
         }else{
             status = 0
         }                
         
##---- 耞ㄤ把计琌硈尿5翴钵盽 ----##
         
         senario_sum = apply(senario_new, 2, sum)
         for (i in 1: ncol(senario_sum)){
            if (senario_sum[i] >= AlarmRule){
               alarmRecord[i] = 1
            }else{
               alarmRecord[i] = 0 
            }
         }
         
         RunMetrix = RunMetrix[-1, ]
         x = matrix(c(nrow(RunMetrix)+1, newRunX, PatternType, status, MHI),1)                   
         #RunMetrix(:, 1) = [1:1:alarmwindow - 1];
         RunMetrix = rbind(RunMetrix, matrix(c(x, senario, alarmRecord),1))
         result = rbind(result, matrix(c(x, senario, alarmRecord),1)) 
         
         if (sum(alarmRecord) >= 1){   # 璶ヴ把计硈尿5翴禬筁specAlarm
            status = 2
            RunMetrix [5, 16] = status
            result [index, 16] = status
         }                                                             
    }
}   

    result[nrow(result), 1] = nrow(result)
  
    
  if (nrow(startpointVector) < 7){
    if (bind_start == 0){
       startpointVector = newRunX[1, 1]
       bigthenBF3Qpoint = 0
       bind_start = bind_start +1
    }else{
     startpointVector = rbind(startpointVector, newRunX[1, 1])
     bigthenBF3Qpoint = 0
    }
  }else{
    startpointVector = rbind(startpointVector, newRunX[1, 1])
  
  
##---- 耞赣BF琌3std  start ----##
  
    if (startpointVector[7, 1] > BF3Q && startpointVector[8, 1] > BF3Q){
       bigthenBF3Qpoint = 1
  
       if (startNo <= 6){              
          startNo = count1
             
       }else{
          bigthenBF3Qpoint = 0
       }
    }
    
    startpointVector = startpointVector[-1, ]
  }
    r1 = bigthenBF3Qpoint

  # clear r1;
  count = count + 1
  count1 = count1 + 1
  
  if (MHI < 0){
    MHI = 0 
  }
  
  output1 = matrix(c(PatternType, status, MHI, senario, alarmRecord),1)
  
  save(parameter_num, Matrix_CL, choose_parameter, Baseline_mean, Baseline_mean_std,
  SoftSpecBFLCL, SoftSpecBFUCL, TrendindicatorTHD, spec, HardSpecBF, LeakRateCL,
  RunMetrix, senario_new, alarmRecord, result, startNo, file = "ModelPdM.rda")
  
  save(output1, bigthenBF3Qpoint, PatternType, status, MHI, file = "RunMode_Output.rda")
  
}