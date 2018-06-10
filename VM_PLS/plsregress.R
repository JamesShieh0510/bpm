plsregress <- function(X, Y, ncomp){

   #X = read.table("C:/Users/Horace/Desktop/BPM_R/VM_PLS/XF.txt", header = FALSE)
   #Y = read.table("C:/Users/Horace/Desktop/BPM_R/VM_PLS/YF.txt", header = FALSE)
   #ncomp = 3
   
   n = nrow(X)
   dx = ncol(X)
   
   maxncomp = min(n-1,dx)
   
   if (ncomp > maxncomp){
      ncomp = maxncomp
   }
   
   meanX = colMeans(X)
   meanY = colMeans(Y)
   
   X0 = matrix(NaN, n ,dx)
   Y0 = matrix(NaN, nrow(Y), ncol(Y))
   
   for (i in 1:n){
     for (j in 1:dx){
       X0[i,j] = X[i,j] - meanX[j]
     }
   }
   
   for (i in 1:nrow(Y)){
     for (j in 1:ncol(Y)){
       Y0[i,j] = Y[i,j] - meanY[j]
     }
   }
   
   dy = ncol(Y0)
   
   Xloadings = matrix(0, dx, ncomp)
   Yloadings = matrix(0, dy, ncomp)
   Weights = matrix(0, dx, ncomp)
   
   V = matrix(0, dx, ncomp)
   
   Cov = t(X0) %*% Y0
   
   for (i in 1:ncomp){
     
       svd = svd(Cov)
       si = svd$d[1]
       ri = svd$u[,1]
       ci = svd$v[,1]
       
       ri = as.matrix(ri, ncol=1)
       ci = as.matrix(ci, ncol=1)
       
       ti = X0 %*% ri
       
       normti = norm(ti, '2') 
       ti = ti / normti  # ti'*ti == 1
       
       Xloadings[,i] = t(X0) %*% ti
       
       qi = (si * ci) / normti # = Y0'*ti
       Yloadings[,i] = qi
       
       Weights[,i] = ri / normti
       
       vi = matrix(Xloadings[,i])
       
       if (i >= 2){
          for (k in 1:2){
            for (j in 1:(i-1)){
                vj = matrix(V[,j])
                vi = vi - as.matrix(outer((t(vj) %*% vi) , vj, '*'))
            }
          }
       }
       
       vi = vi / norm(vi,'2')
       V[,i] = vi
       
       Cov = Cov - vi %*% (t(vi) %*% Cov)
       Vi = V[,(1:i)]
       Cov = Cov - Vi %*% (t(Vi) %*% Cov)
                  
   }
   
   beta = Weights %*% t(Yloadings)
   beta = rbind(meanY - (meanX %*% beta), beta)
   
   W = beta
   
   save(W, file = "plsregress.rda")
}