m_PLS_Components <- function(InX, InY, InQ_sqr){
  
  iPCRate = 0.5; # 設定最低為參數之幾倍的PC數
  
  #ncomp  = [] ## the PLS components by max Qsq
  #Other_ncomp = []
  #sse = []
  #sse_model = []
  
  ## 關閉 InQ_sqr強制 = 1的狀態。
  ## InQ_sqr = 1; %使用q2 , 等於1為會進行判斷是否要進Q_sqr , 非1皆為進行以lamda值計算解釋力做為門檻取PC個數
  ## 【1】: 進入 Q_Sqr ,【0】: 進入 R_Sqr %% Q_SqrFlag 決定 PLS component 個數方選擇法
  InSelectPCRule = 0.9 # 解釋力門檻         %%%%%%%%%%%%%%%
  iLot = nrow(InY)
  iPoint = ncol(InY)
  All_InY = InY
  
  source('mapstd.R')
  source('plsregress.R')
  bind = 0
  bind1 = 0
  
  for (iPointIdx in 1:iPoint){
    
    InY = All_InY[,iPointIdx]
    InY = as.matrix(InY,ncol=1)
    
    #========= 選PC ========= #
    
    FolderNo = 9           # 資料夾設定
    CrossValidationGroup = 3
    FoldUnit =  ceiling(nrow(InX)/FolderNo)  # 1個folder的最大資料量
    FoldUnitMin =  trunc(nrow(InX)/FolderNo) # 1個folder的最小資料量
    MaxFolder =  nrow(InX)%%FolderNo
    
    if (InQ_sqr == 1 & nrow(InX)-(FolderNo / CrossValidationGroup)* FoldUnit > ncol(InX)){
      
      PCstart = floor(ncol(InX)*iPCRate)
      if (PCstart==0){ PCstart=1 }
      
      
      for (Folder in 1 :FolderNo){
         if (Folder <= MaxFolder){
           
            temp = paste0("FolderX_",Folder,"= InX[(FoldUnit*(",Folder,"-1)+1)   : (FoldUnit*",Folder,"),]") 
            eval(parse(text = temp))
            
            temp = paste0("FolderY_",Folder," = matrix(InY[(FoldUnit*(",Folder,"-1)+1)   : (FoldUnit*",Folder,"),])")
            eval(parse(text = temp))
            
         }else{
           
            temp = paste0("FolderX_",Folder," = InX[(FoldUnitMin*(",Folder,"-1)+1+MaxFolder)  :   (FoldUnitMin*",Folder,"+MaxFolder),]")
            eval(parse(text = temp))
            
            temp = paste0("FolderY_",Folder," = matrix(InY[(FoldUnitMin*(",Folder,"-1)+1+MaxFolder)  :   (FoldUnitMin*",Folder,"+MaxFolder),])")
            eval(parse(text = temp))
         }
      }   
      
      
      
      for (PC_descide in PCstart:ncol(InX)){
         for (CV_GroupNum in 1: CrossValidationGroup){
          
           ## 將歸類到各自的Group, 分成建模端&測試端
              cat_test = c(CV_GroupNum, CV_GroupNum + CrossValidationGroup, CV_GroupNum + 2*CrossValidationGroup)
              trainFold = setdiff(1:FolderNo, cat_test)
              
              count = 0
              for (i in trainFold){
                  if (count == 0){
                     
                     temp = paste0("TrainX = FolderX_",i)
                     eval(parse(text = temp))
                     
                     temp = paste0("TrainY =  FolderY_",i)
                     eval(parse(text = temp))
                     count = count + 1
                  }else{
                    
                    temp = paste0("TrainX = rbind(TrainX, FolderX_",i,")")
                    eval(parse(text = temp))
                    temp = paste0("TrainY = rbind(TrainY, FolderY_",i,")")
                    eval(parse(text = temp))
                  }
              }
              
              count1 = 0
              for (i in cat_test){
                if (count1 == 0){
                  
                  temp = paste0("TestX = FolderX_",i)
                  eval(parse(text = temp))
                  
                  temp = paste0("TestY =  FolderY_",i)
                  eval(parse(text = temp))
                  count1 = count1 + 1
                }else{
                  
                  temp = paste0("TestX = rbind(TestX, FolderX_",i,")")
                  eval(parse(text = temp))
                  temp = paste0("TestY = rbind(TestY, FolderY_",i,")")
                  eval(parse(text = temp))
                }
              }
              
            ## 標準化 ##
              
              mapstd(TrainX,0)
              load("mapstd_Z.rda")
              trainZ_XF = Z_value
              MAP_trainXF_mean = mu
              MAP_trainXF_std = s
              
              mapstd(TrainY,0)
              load("mapstd_Z.rda")
              Z_trainYF = Z_value
              MAP_YF_mean = mu
              MAP_YF_std = s
              
          ## Modeling ##
              
              plsregress(trainZ_XF,  Z_trainYF , PC_descide)
              load("plsregress.rda")
              
          ## Testing (Free Run) ##
              
              mapstd(TestX, 1, MAP_trainXF_mean, MAP_trainXF_std)
              load("mapstd_Z.rda")
              TestX_Z = Z_value
              
              Test_yPLS_z = cbind(matrix(1,nrow(TestX_Z),1),TestX_Z) %*% W
              
              mapstd(Test_yPLS_z, 2, MAP_YF_mean, MAP_YF_std)
              load("mapstd_Z.rda")
              Test_yPLS = Z_value
              
          ##  StageII ##
              
              Train_yPLS_z = cbind(matrix(1,nrow(trainZ_XF),1), trainZ_XF) %*% W
              mapstd(Train_yPLS_z, 2, MAP_YF_mean, MAP_YF_std)
              load("mapstd_Z.rda")
              Train_yPLS =  Z_value
              
         ## Total SSE ##  
              
              if (bind == 0){
                 sse_model = (TrainY - Train_yPLS)^2
                 sse = (TestY - Test_yPLS)^2
                 bind = bind + 1
              }else{
                 sse_model = rbind(sse_model, (TrainY - Train_yPLS)^2)
                 sse = rbind(sse , (TestY - Test_yPLS)^2)
              }
         }
        
         train_mean_square = var(InY)
         Qsq  = (train_mean_square - mean(sse)) / train_mean_square
         
         if (bind1 ==0 ){
           Qsq_set = Qsq
           sse_set = mean((TestY - Test_yPLS))
           bind1 = bind1 + 1
         }else{
           Qsq_set = rbind(Qsq_set, Qsq)
           sse_set = rbind(sse_set, mean((TestY - Test_yPLS)))
         }
      }
      
      ## find ncomp ##
      
      Temp_ncomp = which.max(Qsq_set)
      Temp_ncomp = Temp_ncomp+PCstart-1
     }else{  # R-sqr
      
       if (InSelectPCRule == 1){ # 防止出現1.0000000
          InSelectPCRule = 0.99999999
       }
       
       mapstd(InX,0)
       load("mapstd_Z.rda")
       newZ_X = Z_value
       MAP_newX_mean = mu
       MAP_newX_std = s
       
       mapstd(InY,0)
       load("mapstd_Z.rda")
       Z_Y = Z_value
       MAP_Y_mean = mu
       MAP_Y_std = s
       
       o=matrix(1, ncol(InY), 1)
       x=cbind(o, newZ_X)
       y=Z_Y
       corrXz = cor(newZ_X)
       
#--------------------------------------------------------- 
# 判斷矩陣是否產生NaN狀態，當有NaN就強制給定0.0001。       
      
       for (i in 1:nrow(corrXz)){
         for (j in 1:ncol(corrXz)){
           if(i == j){
             if(is.nan(corrXz[i,j]) == TRUE){
               corrXz[i,j] = 1
             }
           }else{
             if(is.nan(corrXz[i,j]) == TRUE){
                corrXz[i,j] = 0.0001
             }
           }
         }
       }
       
#---------------------------------------------------------
       
       invCovX = solve(corrXz)
       eig = eigen(invCovX)
       eVector = abs(eig$values)
       eVector = sort(eVector, decreasing=T)
       
    # ========= 特徵值解釋力 ========= #
    #  特徵值為負防呆 
       
       eVector = as.matrix(evector)
       for (eVectorNo in 1:nrow(eVector)){
           if (eVectorNo == 1){
              explain_PLS = abs(eVector(eVectorNo)) / sum(abs(eVector))
           }else{
              explain_PLS = rbind(explain_PLS,  explain_PLS(eVectorNo-1) + (abs((eVector(eVectorNo))) /sum(abs(eVector))))      
           }
       }
       
    #  Kenny新增參數>資料筆數防呆
       
       Temp_ncomp =which(explain_PLS >= InSelectPCRule, arr.ind = T)
       Temp_ncomp = nrow(Temp_ncomp) 
       if (Temp_ncomp > ncol(InX)){
          Temp_ncomp = ncol(InX)-1 
       }else{
          Temp_ncomp = Temp_ncomp
       }
       
    #  Kenny新增參數>資料筆數防呆 end
       
    #  卡最低ncomp門檻為參數個數的20%開始 (與Q2之 PCstart 相同)
       
       if (Temp_ncomp < floor(ncol(InX)*iPCRate)){
          Temp_ncomp = floor(ncol(InX)*iPCRate)
       }
       
       InQ_sqr =0 
       nouse = 0
       
    }
  }
  ncomp = Temp_ncomp
  save(ncomp, file = "m_PLS_Components.rda")
}