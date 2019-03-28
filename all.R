library(MASS)
library(stats)
library(MplusAutomation)
library(texreg)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~定义生成数据的函数
#nreps = 100,rela=0.3,nobs = 100, CL = 0.05,TL = 0.55, gamma1 =0.14,gamma2=0.14, seed = 1234,p_mean=0.05,p_variance=0.01
data_gen<-function(Type=1,nreps=100,rela=0.3,nobs=100,
                  TL=0.55,CL=0,gamma1 =0.14,gamma2=0.14, seed = 1234){
PA=3
PB=3
PC=3
coef_AB=rela
N=nobs
coef_AC=gamma1
coef_BC=gamma2 
#数据分布
# 1 is normal distribution
# 2 is abnormal distribution
NI<-PA+PB+PC
#用来建模的数据矩阵
Y<-array(0, dim=c(NI, N))
set.seed(seed)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~模型设定
var_A<-1.0     # variance of factor A
var_B<-1.0     # variance of factor B
var_C<-0.2    # residual variance of factor C
#coef_AB<-0.3    # 相关 of A -> B
#coef_AC<-0.14     # regression coefficient of A -> C
#coef_BC<-0.14     # regression coefficient of B -> C

TL_A<-matrix(c(TL, TL, TL), PA, 1, byrow = T)  # 属于因子A的三个指标在A因子的factor loadings
CL_A<-matrix(c(CL,CL,CL), PA, 1, byrow = T)   # 属于因子B的三个指标在A因子的cross loadings,PA行，一列

TL_B<-matrix(c(TL, TL, TL), PB, 1, byrow=T)
CL_B<-matrix(c(CL,CL,CL), PB, 1, byrow=T)

L_C<-matrix(c(0.70, 0.70, 0.70), PC, 1, byrow=T)

v1=1-TL_A[1,1]^2-CL_A[1,1]^2-2*coef_AB*TL_A[1,1]*CL_B[1,1]
v2=1-TL_B[1,1]^2-CL_B[1,1]^2-2*coef_AB*TL_B[1,1]*CL_A[1,1]
v3=1-L_C[1,1]^2

PSX_A<-diag(c(v1, v1, v1))
PSX_B<-diag(c(v2, v2, v2))
PSX_C<-diag(c(v3, v3, v3))                    # variace of measruement error


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~生成数据
#data_gen<-function(NR = 10, indicators = 4, N = 100,Seed = 1222)

setwd("C:/Users/apc/Desktop/data")
if(Type == 1){dir.create(paste('Type',Type,'_', nobs,'_',TL,'_',CL,'_',gamma1,'_',gamma2, sep = ''))
  setwd(paste('Type',Type,'_', nobs,'_',TL,'_',CL,'_',gamma1,'_',gamma2, sep = ''))}
#非正态数据哇
if(Type == 2){dir.create(paste('Type',Type,'_',nobs,'_',TL,'_',CL,'_',gamma1,'_',gamma2,sep = ''))
  setwd(paste('Type',Type,'_', nobs,'_',TL,'_',CL,'_',gamma1,'_',gamma2, sep = ''))}
#setwd(paste('Data_','_N', N, '_NI', PA,PB,PC,'_CL', CL_A[1,1], '_TL', TL_A[1,1], sep = ''))

for(rep in 1:nreps){ 
  #~~~~~~~~~~~~生成潜变量随机数据,方差均为1
  #A<-rnorm(N, 0, sqrt(var_A)) 
  #B<-coef_AB*A+rnorm(N, 0, sqrt(1-coef_AB^2))
  H<-matrix(c(1,coef_AB,coef_AB, 1), 2, 2, byrow=T)       #AB变量间的协方差矩阵
  AB<- array(0, dim=c(2,N))
  theta<- array(0, dim=c(2,N))                      #设定AB变量的均值
  for(i in 1:N) 
    AB[1:2,i]<-mvrnorm(1, mu=theta[,i], Sigma=H)
  A<-AB[1,]
  B<-AB[2,]
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
  C<-coef_AC*A + coef_BC*B + rnorm(N, 0, sqrt(1-coef_AC^2-coef_BC^2-2*coef_AC*coef_BC*coef_AB))
  #theta<-MU_A+TL_A%*%A+CL_A%*%B
  theta<-TL_A%*%A+CL_A%*%B
  if(Type == 1)
    for(i in 1:N) 
      Y[1:PA,i]<-mvrnorm(1, mu=theta[,i], Sigma=PSX_A)
  if(Type == 2)
    for(i in 1:N){
      error <- runif(PA, 0, 1)
      Y[1:PA,i]<-theta[,i]+error}
  if(Type == 3)
    for(i in 1:N){
      error <- rt(PA, 5)/sqrt(rchisq(PA, 5)/3)
      Y[1:PA,i]<-theta[,i]+error}
  if(Type == 4)
    for(i in 1:N){
      error <- rt(PA, 5)/(rchisq(PA, 5)/3)
      Y[1:PA,i]<-theta[,i]+error}
  if(Type == 5)
    for(i in 1:N){
      error <- rchisq(PA, 1)
      Y[1:PA,i]<-theta[,i]+error}
  
  #for(i in 1:N) 
  #Y[1,i]<-Y[1,i]/TL_A[1,1]
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  #theta<-MU_B+TL_B%*%B+CL_B%*%A
  theta<-TL_B%*%B+CL_B%*%A
  if(Type == 1)
    for(i in 1:N) 
      Y[(PA+1):(PA+PB),i]<-mvrnorm(1, mu=theta[,i], Sigma=PSX_B)
  if(Type == 2)
    for(i in 1:N){
      error <- runif(PB, 0, 1)
      Y[(PA+1):(PA+PB),i]<-theta[,i]+error}
  if(Type == 3)
    for(i in 1:N){
      error <- rt(PB, 5)/sqrt(rchisq(PB, 5)/3)
      Y[(PA+1):(PA+PB),i]<-theta[,i]+error}
  if(Type == 4)
    for(i in 1:N){
      error <- rt(PB, 5)/(rchisq(PB, 5)/3)
      Y[(PA+1):(PA+PB),i]<-theta[,i]+error}
  if(Type == 5)
    for(i in 1:N){
      error <- rchisq(PB, 1)
      Y[(PA+1):(PA+PB),i]<-theta[,i]+error}
  
  #for(i in 1:N) 
  # Y[1+PA,i]<-Y[1+PA,i]/TL_B[1,1]
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~变量C
  #theta<-MU_C+L_C%*%C
  theta<-L_C%*%C
  if(Type == 1)
    for(i in 1:N) 
      Y[(PA+PB+1):(PA+PB+PC),i]<-mvrnorm(1, mu=theta[,i], Sigma=PSX_C)
  if(Type == 2)
    for(i in 1:N){
      error <- runif(PC, 0, 1)
      Y[(PA+PB+1):(PA+PB+PC),i]<-theta[,i]+error}
  if(Type == 3)
    for(i in 1:N){
      error <- rt(PC, 5)/sqrt(rchisq(PC, 5)/3)
      Y[(PA+PB+1):(PA+PB+PC),i]<-theta[,i]+error}
  if(Type == 4)
    for(i in 1:N){
      error <- rt(PC, 5)/(rchisq(PC, 5)/3)
      Y[(PA+PB+1):(PA+PB+PC),i]<-theta[,i]+error}
  if(Type == 5)
    for(i in 1:N){
      error <- rchisq(PC, 1)
      Y[(PA+PB+1):(PA+PB+PB),i]<-theta[,i]+error}
  
  #for(i in 1:N) 
  #Y[1,i]<-Y[1,i]/TL_A[1,1]
  #Y[1+PA,i]<-Y[1+PA,i]/TL_B[1,1]
  #Y[1+PA+PB,i]<-Y[1+PA+PB,i]/L_C[1,1]
  
  #(t(Y),file= paste('Sample_for_Mplus_', rep, '.dat', sep = ''), ncol=NI,sep="\t", append=T) #   the data save will be t(Y), N*NY
  write.table(t(Y), file = paste('Type',Type, '_',nobs,'_',TL,'_',CL,'_',gamma1,'_',gamma2,'_',rep,'.dat', sep = ''), 
              sep = '\t', row.names = FALSE, col.names = FALSE)
  print(rep)
}}


data_gen(Type=1,nreps=100,rela=0.3,nobs=100,TL=0.7,CL=0,gamma1 =0.14,gamma2=0.14, seed = 1234) #生成数据需要的参数


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~产生MPLUS inp文件产生不同类型的模型inp
#createModels("runmplus.txt")
SEM_gen<-function(Type=1,nreps=100,rela=0.3,nobs=100,
                  TL=0.7,CL=0,gamma1 =0.14,gamma2=0.14, seed = 1234)
{pre="sem"
rela=0.3
#setwd(paste('Type',Type,'_', nobs,'_',TL,'_',CL,'_',gamma1,'_',gamma2, sep = ''))
model=paste('f1 by y1-y3*',TL,';','\n',
            'f2 by y4-y6*',TL,';','\n',
            'f3 by y7-y9*0.7;','\n',
            'y1-y9*1;','\n',
            '[y1-y9*0];','\n',
            'f1-f2@1;f3@1;','\n',
            'f1 with f2*',rela,';','\n',
            'f3 on f1*',gamma1,'\t','f2*',gamma2,';')
dat=paste('Type',Type, '_',nobs,'_',TL,'_',CL,'_',gamma1,'_',gamma2,'_',nreps,'.dat',';',sep = '')
sem.body <- mplusObject(
  TITLE = "Monte Carlo simulation/SEM;",
  DATA=dat,
  VARIABLE=paste('NAMES ARE y1-y9;'),
  ANALYSIS = "ESTIMATOR = ML;",
  MODEL = model ,
  OUTPUT = "TECH9;")

out<- mplusModeler(sem.body,
                   dataout = paste0(pre,Type,'_',nobs,'_',TL,'_',CL,'_',gamma1,'_',gamma2,'_',nreps,".dat"),
                   modelout =paste0(pre,Type,'_',nobs,'_',TL,'_',CL,'_',gamma1,'_',gamma2,'_',nreps,".inp"), run=TRUE)
}


ESEM_gen<-function(nreps = 100, rela=0.3,nobs,
                   TL = 0.55,CL = 0.05,  gamma1 =0.14,gamma2=0.14, seed = 1234)
{ pre="esem"
DATA=paste('file IS Sample_for_Mplus_',nreps,'.dat',';')
VARIABLE=paste('NAMES ARE y1-y9;')
model=paste('f1 by y1-y3*',TL,'\t','y4-y6*',CL,'(*1)',';','\n',
            'f2 by y1-y3*',CL,'\t','y4-y6*',TL,'(*1)',';','\n',
            'f3 by y7-y9*0.7;','\n',
            'y1-y9*1;','\n',
            '[y1-y9*0];','\n',
            'f3@1;','\n',
            'f1 with f2*',rela,';','\n',
            'f3 on f1*',gamma1,'\t','f2*',gamma2,';')             #ESEM不能设定f1和f2的方差
esem.body <- mplusObject(
  TITLE = "Monte Carlo simulation/ESEM;",
  MONTECARLO= monte,
  MODELPOPULATION = population,
  ANALYSIS = "ESTIMATOR = ML;",
  MODEL = model,
  OUTPUT = "TECH9;")

out <- mplusModeler(esem.body,
                    dataout = paste0(pre,'_',nobs,'_',TL,'_',CL,'_',gamma1,'_',gamma2,'_',nreps,".dat"),
                    modelout =paste0(pre,'_',nobs,'_',TL,'_',CL,'_',gamma1,'_',gamma2,'_',nreps,".inp"), run=TRUE)
}


BSEM_gen<-function(nreps = 100, rela=0.3,nobs = 200, CL = 0.05, TL = 0.55, 
                   gamma1 =0.14,gamma2=0.14, seed = 1234,p_mean=0,p_variance=0.01)
{ pre="esem"
monte=paste('names = y1-y9;','\n', 'nobs =', nobs,';','\n', 'nreps=',nreps,';',
            '\n','seed=',seed,';','\n',sep = '')
population=paste('f1 by y1-y3*',TL,'\t','y4-y6*',CL,';','\n',
                 'f2 by y1-y3*',CL,'\t','y4-y6*',TL,';','\n',
                 'f3 by y7-y9*0.7;','\n',
                 'y1-y9*1;','\n',
                 '[y1-y9*0];','\n',
                 'f1-f2@1;f3@1;','\n',
                 'f1 with f2*',rela,';','\n',
                 'f3 on f1*',gamma1,'\t','f2*',gamma2,';')
model=paste('f1 by y1-y3*',TL,'\t','y4-y6*',CL,'(b11-b16)',';','\n',
            'f2 by y1-y3*',CL,'\t','y4-y6*',TL,'(b21-b26)',';','\n',
            'f3 by y7-y9*0.7;','\n',
            'y1-y9*1;','\n',
            '[y1-y9*0];','\n',
            'f1-f2@1;f3@1;','\n',
            'f1 with f2*',rela,';','\n',
            'f3 on f1*',gamma1,'\t','f2*',gamma2,';','\n',
            'model prior:','\n','b14-b16~N(',p_mean,',',p_variance,');','\n',
            'b21-b23~N(',p_mean,',',p_variance,');')            #BSEM要设定交叉负载先验
bsem.body <- mplusObject(
  TITLE = "Monte Carlo simulation/BSEM;",
  MONTECARLO= monte,
  MODELPOPULATION = population,
  ANALYSIS = "ESTIMATOR = BAYES; biter=(500);",
  MODEL = model,
  OUTPUT = "TECH9;")

out<- mplusModeler(bsem.body,
                   dataout = paste0(pre,'_',nobs,'_',TL,'_',CL,'_',gamma1,'_',gamma2,'_',nreps,".dat"),
                   modelout =paste0(pre,'_',nobs,'_',TL,'_',CL,'_',gamma1,'_',gamma2,'_',nreps,".inp"), run=TRUE)
}

N <- c(100,200,500)
ATL<- c(0.55,0.70,0.84,0.95)
ACL<- c(0.00,0.05,0.10,0.15,0.20,0.25)
GAMMA<- matrix(c(0.14,0.14,0.14,0.36,0.14,0.51,0.36,0.36,0.36,0.51,0.51,0.51),nrow =2)          #GAMMA[1,]表示一组路径系数组合

reps=10

for(i in 1:length(N))
  {nobs=N[i];
  for(j in 1:length(ATL))
    {TL=ATL[j]
   for(k in 1:length(ACL))
     {CL=ACL[k]
    for(l in 1:ncol(GAMMA))
    { if(CL^2+TL^2+2*CL*TL*0.3<=1){
          gamma1=GAMMA[1,l]
          gamma2=GAMMA[2,l]
          #data_gen(nreps=100,0.3,nobs,TL,CL, 
           #gamma1,gamma2,seed = 1234)
          for(t in 1:reps){
          SEM_gen(nreps=1,0.3,100,0.70,0.00, 
                   0.14,0.14,seed = 1234)
          
          }
      }
    }
   }
  }
}

ESEM_gen(nreps=1.3,nobs,TL,CL, 
                   gamma1,gamma2,seed = 1234)
          BSEM_gen(nreps=1,0.3,nobs,TL,CL, 
                   gamma1,gamma2,seed = 1234,p_mean=CL,p_variance=0.01)
SEM_gen(nreps=1,0.3,100,0.7,0,0.14,0.14,seed = 1234)
