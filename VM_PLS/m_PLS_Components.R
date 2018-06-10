m_PLS_Components <- function(InX, InY, InQ_sqr){
  
  iPCRate = 0.5; # �]�w�̧C���ѼƤ��X����PC��
  
  #ncomp  = [] ## the PLS components by max Qsq
  #Other_ncomp = []
  #sse = []
  #sse_model = []
  
  ## ���� InQ_sqr�j�� = 1�����A�C
  ## InQ_sqr = 1; %�ϥ�q2 , ����1���|�i��P�_�O�_�n�iQ_sqr , �D1�Ҭ��i��Hlamda�ȭp������O�������e��PC�Ӽ�
  ## �i1�j: �i�J Q_Sqr ,�i0�j: �i�J R_Sqr %% Q_SqrFlag �M�w PLS component �ӼƤ��ܪk
  InSelectPCRule = 0.9 # �����O���e         %%%%%%%%%%%%%%%
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
    
    #========= ��PC ========= #
    
    FolderNo = 9           # ��Ƨ��]�w
    CrossValidationGroup = 3
    FoldUnit =  ceiling(nrow(InX)/FolderNo)  # 1��folder���̤j��ƶq
    FoldUnitMin =  trunc(nrow(InX)/FolderNo) # 1��folder���̤p��ƶq
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
          
           ## �N�k����U�۪�Group, �����ؼҺ�&���պ�
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
              
            ## �зǤ� ##
              
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
      
       if (InSelectPCRule == 1){ # ����X�{1.0000000
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
# �P�_�x�}�O�_����NaN���A�A����NaN�N�j��w0.0001�C       
      
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
       
    # ========= �S�x�ȸ����O ========= #
    #  �S�x�Ȭ��t���b 
       
       eVector = as.matrix(evector)
       for (eVectorNo in 1:nrow(eVector)){
           if (eVectorNo == 1){
              explain_PLS = abs(eVector(eVectorNo)) / sum(abs(eVector))
           }else{
              explain_PLS = rbind(explain_PLS,  explain_PLS(eVectorNo-1) + (abs((eVector(eVectorNo))) /sum(abs(eVector))))      
           }
       }
       
    #  Kenny�s�W�Ѽ�>��Ƶ��ƨ��b
       
       Temp_ncomp =which(explain_PLS >= InSelectPCRule, arr.ind = T)
       Temp_ncomp = nrow(Temp_ncomp) 
       if (Temp_ncomp > ncol(InX)){
          Temp_ncomp = ncol(InX)-1 
       }else{
          Temp_ncomp = Temp_ncomp
       }
       
    #  Kenny�s�W�Ѽ�>��Ƶ��ƨ��b end
       
    #  �d�̧Cncomp���e���ѼƭӼƪ�20%�}�l (�PQ2�� PCstart �ۦP)
       
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