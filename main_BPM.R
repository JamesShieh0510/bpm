
process_time <- proc.time()

Status_IPM = read.table("Status_IPM.txt", header = FALSE)

##---- Load Functions ----##

source('TrainMode.R')
source('RunMode.R')
source('CBIcalculate.R')
source('Reasoning.R')
source('RUL.R')
source('CreateModelData.R')
source('CreateRunningData.R')
source('ISIB_calculate.R')
source('main_PLS.R')
source('main_PLS_2.R')

##---- Definition Spec & Condition setting ----##
if (Status_IPM == 0){
spec = 5            
Hotspec = 50         
#windowSize = 5
Refresh_num = 10    # 家妓セ (ヘ玡妓セ计:Refresh_num + DMW = 40)
ISIT = 6            # ISIB 耬 -- ヘ玡﹚6

spec_Mode = 1  # 砞﹚ spec 家Α: 0.箇砞(8夹非畉)  1.﹚竡


##---- TrainMode & RunMode 兵ン砞﹚ ----##

Route = paste0(getwd(),"/")              # load data          
X = read.table("Base_X.txt", header = FALSE)
Y = read.table("Base_Y.txt", header = FALSE)  
DMW_X = read.table("DMW_X.txt", header = FALSE)
DMW_Y = read.table("DMW_Y.txt", header = FALSE)


if (spec_Mode == 1){   # 璹把计Spec (斗皌TrainMode.R)

    parameter_spec = 0.05
    choose_parameter_spec = c(3, 4)

    parameter_spec2 = 0.1
    choose_parameter_spec2 = c(2)

    UCL = 30
    LCL = -30
    choose_parameter_CL = (5)
    
    choose_parameter = sort(c(choose_parameter_spec, choose_parameter_spec2, choose_parameter_CL))
    save(UCL, LCL, parameter_spec, choose_parameter_spec, parameter_spec2, choose_parameter_spec2, 
         choose_parameter_CL, choose_parameter, file = "Condition_train.rda")
    
}else{
   parameter_num = ncol(X)
   choose_parameter = c(2: (parameter_num+1))
   Std = 8    # 箇砞夹非畉瞯
   save(choose_parameter, Std, file = "Condition_train.rda")
}

alarmwindow = 5          # 硈尿5翴 璝Τ3翴禬砏 玥祇 alarm
AlarmRule = 3

count = 6
startNo = 6
count1 = 6

save(alarmwindow, AlarmRule, count, startNo, count1, file = "Condition_run.rda")


##---- Data Processing ----##

if (ncol(X) < 2){
  print("Error : Does not support Parameter < 2")     # ň:ヘ玡ぃや穿把计计 = 1
  break
}

##---- For PLS ----##


train_X = X[1:Refresh_num,]  # 玡 n 掸ㄓtarin, n = Refresh_num
train_Y = Y[1:Refresh_num,]

train_X = as.matrix(train_X)
DMW_X = as.matrix(DMW_X)

train_Y = as.matrix(train_Y)
DMW_Y = as.matrix(DMW_Y)

Running_X = X[(Refresh_num+1): nrow(X),]
Running_Y = Y[(Refresh_num+1): nrow(Y), 1]

if(ncol(X) == 1){
   Running_X = as.matrix(Running_X, ncol = 1)
}
Running_Y = as.matrix(Running_Y, ncol = 1)

##---- For BPM ----##


BPM_train = cbind(train_Y, train_X)    
BPM_Run = cbind(Running_Y, Running_X)
bind_BEI = 0
count_sick = 0
save(bind_BEI, count_sick, file = "BEI_value.rda")
bind_alarm = 0
DHI = matrix()

sick_num = matrix()
output = matrix()
bind_matrix = 0
bind_start = 0


## TD Baseline Model ( PLS ) ----##

#for (Sample in 1:nrow(BPM_Run_0)){
#  BPM_Run = BPM_Run_0[Sample,]
train_X1 = rbind(train_X, DMW_X)
train_Y1 = rbind(train_Y, DMW_Y)
rootFile = getwd()

InModelRoute = paste0(rootFile,"/VM_PLS/Model/")
InTempRoute = paste0(rootFile,"/VM_PLS/Temp/")
CreateModelData(train_X1, train_Y1, InTempRoute, InModelRoute)
CreateRunningData(Running_X, Running_Y, InTempRoute, InModelRoute)     

setdata_time = proc.time() - process_time

main_PLS(InTempRoute, InModelRoute)
load("main_PLS.rda")
PLS_time = proc.time() - process_time

#MR_AVMError = read.table("C:/Users/Horace/Desktop/BPM_R/MR_AVMError.txt", header = FALSE)
ErrorMR = MR_AVMError


##---- 璸衡 ISIB ----##


ISIB_calculate(train_X, Refresh_num)
load("ISIB_calculate.rda")

Z_BPM_Run = (Running_X - matrix(1,nrow(Running_X), 1) %*% ISIFreerun_X_Mean) / (matrix(1,nrow(Running_X), 1) %*% ISIFreerun_X_std)
ISIB_X = abs(Z_BPM_Run)
ISIB_X_all = ISIB_X

##---- Train Data ----##


TrainMode(BPM_train, Route, spec, Hotspec, spec_Mode)
load("Train_Output.rda")


##---- BPM Runing ----##

for (Sample in 1:nrow(BPM_Run)){
  RunX = BPM_Run[Sample,]
  
  RunMode(RunX, Sample, Route, BF3Q, bind_matrix, bind_start)
  load("RunMode_Output.rda")
  #load("ModelPdM.rda")
  
  DHI = rbind(DHI, MHI)
  ErrorMR_run = ErrorMR[Sample, 1]  
  CBIcalculate(Sample, Route, ErrorMR_run, spec, Hotspec, RunX)
  
  load("BEI_value.rda")
  
  
  if (sickpoint == 1){
    
     load("sickNo.rda")
     sick_num = rbind(sick_num, Sample)
  }
  
  ISI_Xrun = ISIB_X[Sample,]
  Reasoning(status, PatternType, MHI, BEI, ISI_Xrun, ISIT, Route)
  load("alarmCode.rda")
  
  if (bind_alarm == 0){
     alarmCodeMetrix = alarmCode
     output = output1
     bind_alarm = bind_alarm + 1
  }else{
     alarmCodeMetrix = rbind(alarmCodeMetrix, alarmCode)
     output = rbind(output, output1)
  }
  
  alarmCode = matrix()
  
  if (sickpoint == 1){
     
     RUL(ErrorMR, ErrorT, Route)
    
  } 
  
}

Status_IPM = 1
write.table(Status_IPM, file = "Status_IPM.txt", row.name = FALSE, col.name = FALSE)
#write.table(DHI, file = "DHI.txt", row.name = FALSE)
save.image(file = "Run_Data.rda")

##-------------------------------------------------------------------------------------------

}else{
  load("Run_Data.rda")
  Running_X = read.table("X.txt", header = FALSE)
  Running_Y = read.table("Y.txt", header = FALSE)
  
  if(ncol(X) == 1){
    Running_X = as.matrix(Running_X, ncol = 1)
  }
  Running_Y = as.matrix(Running_Y, ncol = 1)
  
  BPM_Run = cbind(Running_Y, Running_X)
  CreateRunningData(Running_X, Running_Y, InTempRoute, InModelRoute)
  
##----------------------------------------------------------------------- 
  
  main_PLS_2(InTempRoute, InModelRoute)
  load("main_PLS.rda")
  ErrorMR = MR_AVMError
  
  Z_BPM_Run = (Running_X - matrix(1,nrow(Running_X), 1) %*% ISIFreerun_X_Mean) / (matrix(1,nrow(Running_X), 1) %*% ISIFreerun_X_std)
  ISIB_X = abs(Z_BPM_Run)
  ISIB_X_all = rbind(ISIB_X_all, ISIB_X)
  
  for (Sample in 1:nrow(BPM_Run)){
    RunX = BPM_Run[Sample,]
    
    RunMode(RunX, Sample, Route, BF3Q, bind_matrix, bind_start)
    load("RunMode_Output.rda")
    #load("ModelPdM.rda")
    
    DHI = rbind(DHI, MHI)
    ErrorMR_run = ErrorMR[Sample, 1]  
    CBIcalculate(Sample, Route, ErrorMR_run, spec, Hotspec, RunX)
    
    load("BEI_value.rda")
    
    
    if (sickpoint == 1){
      
      load("sickNo.rda")
      sick_num = rbind(sick_num, Sample)
    }
    
    ISI_Xrun = ISIB_X[Sample,]
    Reasoning(status, PatternType, MHI, BEI, ISI_Xrun, ISIT, Route)
    load("alarmCode.rda")
    
    if (bind_alarm == 0){
      alarmCodeMetrix = alarmCode
      output = output1
      bind_alarm = bind_alarm + 1
    }else{
      alarmCodeMetrix = rbind(alarmCodeMetrix, alarmCode)
      output = rbind(output, output1)
    }
    
    alarmCode = matrix()
    
    if (sickpoint == 1){
      
      RUL(ErrorMR, ErrorT, Route)
      
    } 
    
  }
  save.image(file = "Run_Data.rda")
  write.table(MHI, file = "DHI.txt", row.name = FALSE , col.names = FALSE)
}

##---- BPM_end ----##

total_time = proc.time() - process_time
