library(MplusAutomation)
library(texreg)
library(xlsx)

base <- tempdir() #Create Names for Temporary Files
 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~生成模拟inp文件的函数
SEM_gen<-function(nreps=100, rela=0.3,nobs=100,
                  TL=0.55,CL=0, gamma1=0.14,gamma2=0.14, seed = 1234)
  {pre="sem"
  monte=paste('names = y1-y9;','\n', 'nobs =', nobs,';','\n', 'nreps=',nreps,';',
            '\n','seed=',seed,';')
  population=paste('f1 by y1-y3*',TL,'\t','y4-y6*',CL,';','\n',
                 'f2 by y1-y3*',CL,'\t','y4-y6*',TL,';','\n',
                 'f3 by y7-y9*0.7;','\n',
                 'y1-y9*1;','\n',
                 '[y1-y9*0];','\n',
                 'f1-f2@1;f3@1;','\n',
                 'f1 with f2*',rela,';','\n',
                 'f3 on f1*',gamma1,'\t','f2*',gamma2,';')
  model=paste('f1 by y1-y3*',TL,';','\n',
                   'f2 by y4-y6*',TL,';','\n',
                   'f3 by y7-y9*0.7;','\n',
                   'y1-y9*1;','\n',
                   '[y1-y9*0];','\n',
                   'f1-f2@1;f3@1;','\n',
                   'f1 with f2*',rela,';','\n',
                   'f3 on f1*',gamma1,'\t','f2*',gamma2,';')
  sem.body <- mplusObject(
  TITLE = "Monte Carlo simulation/SEM;",
  MONTECARLO= monte,
  MODELPOPULATION = population,
  ANALYSIS = "ESTIMATOR = ML;",
  MODEL = model ,
  OUTPUT = "TECH9;")
  setwd("C:/Users/apc/Desktop/MplusAutomation/type I/sem")
out<- mplusModeler(sem.body,
                      dataout = paste0(pre,'_',nobs,'_',TL,'_',CL,'_',gamma1,'_',gamma2,".dat"),
                      modelout =paste0(pre,'_',nobs,'_',TL,'_',CL,'_',gamma1,'_',gamma2,".inp"), run=TRUE)
}


ESEM_gen<-function(nreps = 100, rela=0.3,nobs,
                   TL = 0.55,CL = 0.05,  gamma1 =0.14,gamma2=0.14, seed = 1234)
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
   setwd("C:/Users/apc/Desktop/MplusAutomation/type I/esem")
  out <- mplusModeler(esem.body,
                        dataout = paste0(pre,'_',nobs,'_',TL,'_',CL,'_',gamma1,'_',gamma2,".dat"),
                        modelout =paste0(pre,'_',nobs,'_',TL,'_',CL,'_',gamma1,'_',gamma2,".inp"), run=TRUE)
}

# biter=(500);
BSEM_gen<-function(nreps = 100, rela=0.3,nobs = 200, TL = 0.55,  CL = 0.05,
                   gamma1 =0.14,gamma2=0.14, seed = 1234,p_mean=0,p_variance=0.01,wz='bsem_1')
{ pre="bsem"
  monte=paste('names = y1-y9;','\n', 'nobs =', nobs,';','\n', 'nreps=',nreps,';',
              '\n','seed=',seed,';','\n',sep = '')
  wd=paste('C:/Users/apc/Desktop/MplusAutomation/type I/',wz,sep = '')
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
    ANALYSIS = "ESTIMATOR = BAYES;",
    MODEL = model,
    OUTPUT = "TECH9;")
  setwd(wd)
  out<- mplusModeler(bsem.body,
                        dataout = paste0(pre,'_',nobs,'_',TL,'_',CL,'_',gamma1,'_',gamma2,".dat"),
                        modelout =paste0(pre,'_',nobs,'_',TL,'_',CL,'_',gamma1,'_',gamma2,".inp"), run=TRUE)
}

#SEM_gen(nreps = 100,rela=0.3,nobs = 100, CL = 0.05, 
                  #TL = 0.55, gamma1 =0.14,gamma2=0.14, seed = 1234)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~生成各种类型文件的循环
#100,200,0,0.05,!!!!!
N <- c(100,200,500)
ATL<- c(0.55,0.7,0.84,0.95)    #0.55,0.7,0.84,0.95
ACL<- c(0,0.05,0.10,0.15,0.20,0.25)    #0,0.05,0.10,0.15,0.20,0.25
GAMMA<-matrix(c(0,0),nrow =2) 
GAMMA<- matrix(c(0.14,0.14,0.14,0.36,0.14,0.51,0.36,0.36,0.36,0.51),nrow =2)          #GAMMA[1,]表示一组路径系数组合0.14,0.14,0.14,0.36,0.14,0.51,0.36,0.36,


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
          SEM_gen(nreps=500,0.3,nobs, TL,CL, 
                  gamma1,gamma2,seed = 1234)
          }
        }
    }
  }
}

SEM_gen(nreps=500,0.3,100, 0.84,0, 
        0.36,0.36,p_mean=0,p_variance=0.01,wz='bsem-0.01',seed = 1234)
BSEM_gen(nreps = 500, rela=0.3,100, 0.84,0, 
         0.36,0.36,seed = 123,p_mean=0,p_variance=100,wz='bsem-100')
r<-readModels(target = "C:/Users/apc/Desktop/MplusAutomation", recursive = FALSE)
s9 <- sapply(r, function(m) m$summaries)                        #能提取一个。。很多个表格出来了
p9 <- sapply(r, function(n) n$parameters)
#BSEM_gen(nreps = 100,rela=0.3,nobs = 100, CL = 0.05,TL = 0.55, gamma1 =0.14,gamma2=0.14, seed = 1234,p_mean=0.05,p_variance=0.01)
#setwd('"C:/Users/apc/Desktop/MplusAutomation') 
for(j in 1:1)
{
  b<-matrix()
  for(i in 1:39){
    b<-cbind(b,r$parameters$unstandardized[i,])
  }
  setwd("C:/Users/apc/Desktop/MplusAutomation/")
  if (j==1)
    write.table(r$summaries,file="bsem-100.csv",append = TRUE,sep = ", ",na = "NA",col.names = TRUE,row.names = FALSE) 
  if (j!=1)
    write.table(b,file="bsem-10.csv",append = TRUE,sep = ", ",na = "NA",col.names = TRUE,row.names = FALSE) 
}


res1<-readModels(target = "C:/Users/apc/Desktop/MplusAutomation/result500/esem500", recursive = FALSE) 
s1 <- sapply(res1, function(m) m$summaries)                        #能提取一个。。很多个表格出来了
p1 <- sapply(res1, function(n) n$parameters)
setwd("C:/Users/apc/Desktop/MplusAutomation/result500")
#write.xlsx(s1,"result.xls",sep = ",",sheetName = "sheet1",append = TRUE) 
#,sep = ","
write.table(s1,file="summaries.csv",append = FALSE,sep = ", ",na = "NA")           #,是csv文件中的分隔符
write.table(p1,file="parameters.csv",append = FALSE,sep = ", ",na = "NA")



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
        ESEM_gen(nreps=500,0.3,nobs, TL,CL, 
           gamma1,gamma2,seed = 1234)
       }
      }
    }
  }
}

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
  BSEM_gen(nreps = 500, rela=0.3,nobs, TL,  CL,
           gamma1,gamma2, seed = 1234,p_mean=0,p_variance=0.01,wz='bsem-0.01')
}
}
}
}
}

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
  BSEM_gen(nreps = 500, rela=0.3,nobs, TL,  CL,
           gamma1,gamma2, seed = 1234,p_mean=0,p_variance=1,wz='bsem-1')
}
}
}
}
}
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
  BSEM_gen(nreps = 500, rela=0.3,nobs, TL,  CL,
           gamma1,gamma2, seed = 1234,p_mean=CL,p_variance=1,wz='bsem-CL')
}
}
}
}
}


res2<-readModels(target = "C:/Users/apc/Desktop/MplusAutomation/esem500", recursive = FALSE)
s2 <- sapply(res2, function(m) m$summaries)                        #能提取一个。。很多个表格出来了
p2 <- sapply(res2, function(n) n$parameters)
setwd("C:/Users/apc/Desktop/MplusAutomation/result500")
write.table(s2,file="Esummaries.csv",append = FALSE,sep = ", ",na = "NA")           #,是csv文件中的分隔符
write.table(p2,file="Eparameters.csv",append = FALSE,sep = ", ",na = "NA") 

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
     BSEM_gen(nreps = 500, rela=0.3,nobs, TL,  CL,
           gamma1,gamma2, seed = 1234,p_mean=0,p_variance=0.01,wz='bsem-0.01')
    }
   }
  }
 }
}
]

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
  BSEM_gen(nreps = 500, rela=0.3,nobs, TL,  CL,
           gamma1,gamma2, seed = 1234,p_mean=0,p_variance=1,wz='bsem_1')
}
}
}
}
}


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
  BSEM_gen(nreps = 500, rela=0.3,nobs, TL,  CL,
           gamma1,gamma2, seed = 1234,p_mean=0,p_variance=1,wz='bsem-0.01')
}
}
}
}
}
BSEM_gen(nreps = 500, rela=0.3,500, 0.55, 0.1,
         0.51,0.51, seed = 1234,p_mean=0,p_variance=1,wz='bsem-1')

p4<-list()
res4<-readModels(target = "C:/Users/apc/Desktop/MplusAutomation/bsem-1", recursive = FALSE)
s4 <- sapply(res4, function(m) m$summaries)                        #能提取一个。。很多个表格出来了
p4 <- sapply(res4, function(n) n$parameters)
setwd("C:/Users/apc/Desktop/MplusAutomation/result500")
write.table(s4,file="Bsummaries-1.csv",append = FALSE,sep = ", ",na = "NA")           #,是csv文件中的分隔符
write.table(p4,file="Bparameters-1.csv",append = FALSE,sep = ", ",na = "NA")  
unlist(s4)

res5<-readModels(target = "C:/Users/apc/Desktop/MplusAutomation/bsem-0.01", recursive = FALSE)
s5 <- sapply(res5, function(m) m$summaries)                        #能提取一个。。很多个表格出来了
p5 <- sapply(res5, function(n) n$parameters)
setwd("C:/Users/apc/Desktop/MplusAutomation/result500")
write.table(s5,file="Bsummaries-0.01.csv",append = FALSE,sep = ", ",na = "NA")           #,是csv文件中的分隔符
write.table(p5,file="Bparameters-0.01.csv",append = FALSE,sep = ", ",na = "NA")  

res6<-readModels(target = "C:/Users/apc/Desktop/MplusAutomation/result500/bsem500", recursive = FALSE)
s6 <- sapply(res6, function(m) m$summaries)                        #能提取一个。。很多个表格出来了
p6 <- sapply(res6, function(n) n$parameters)
setwd("C:/Users/apc/Desktop/MplusAutomation/result500")
write.table(s6,file="Bsummaries-cl0.01.csv",append = FALSE,sep = ", ",na = "NA")           #,是csv文件中的分隔符
write.table(p6,file="Bparameters-cl0.01.csv",append = FALSE,sep = ", ",na = "NA") 

res7<-readModels(target = "C:/Users/apc/Desktop/MplusAutomation/bsem-100", recursive = FALSE)
s7<- sapply(res7, function(m) m$summaries)   #能提取一个。。很多个表格出来了
p7<- sapply(res7, function(n) n$parameters)
setwd("C:/Users/apc/Desktop/MplusAutomation/result500")
write.table(s7,file="Bsummaries-100.csv",append = FALSE,sep =", ",na = "NA")           #,是csv文件中的分隔符
write.table(p7,file="Bparameters-100.csv",append = FALSE,sep = ",",na = "NA")  

write.table(p6[[1]]$unstandardized[1,],file="1.csv",append = TRUE,sep = ",",na = "NA") 
write.table(p6[[1]]$unstandardized[2,],file="1.csv",append =TRUE,sep = ",",na = "NA") 

#_________________________按规则输出嘛

for(j in 1:378)
{
b<-matrix()
for(i in 1:39){
b<-cbind(b,p6[[j]][i,])
}
setwd("C:/Users/apc/Desktop/MplusAutomation/result500")
  if (j==1)
  write.table(b,file="bsem-cl0.01.csv",append = TRUE,sep = ", ",na = "NA",col.names = TRUE,row.names = FALSE) 
  if (j!=1)
  write.table(b,file="bsem-cl0.01.csv",append = TRUE,sep = ", ",na = "NA",col.names = FALSE,row.names = FALSE) 
}

for(j in 1:378)
{
  b<-matrix()
  for(i in 1:39){
    b<-cbind(b,p1[[j]][i,])
  }
  setwd("C:/Users/apc/Desktop/MplusAutomation/result500")
  if (j==1)
    write.table(b,file="esem.csv",append = TRUE,sep = ", ",na = "NA",col.names = TRUE,row.names = FALSE) 
  if (j!=1)
    write.table(b,file="esem.csv",append = TRUE,sep = ", ",na = "NA",col.names = FALSE,row.names = FALSE) 
}
#________________________________________________________
for(j in 1:378)
{
  b<-matrix()
  for(i in 1:39){
    b<-cbind(b,p5[[j]][i,])
  }
  setwd("C:/Users/apc/Desktop/MplusAutomation/result500")
  if (j==1)
    write.table(b,file="bsem-0.01.csv",append = TRUE,sep = ", ",na = "NA",col.names = TRUE,row.names = FALSE) 
  if (j!=1)
    write.table(b,file="bsem-0.01.csv",append = TRUE,sep = ", ",na = "NA",col.names = FALSE,row.names = FALSE) 
}
#__________________________输出summaries
for(j in 1:378)
{
  b<-matrix()
  for(i in 1:21){
    b<-cbind(b,s4[[j]][i])
  }
  setwd("C:/Users/apc/Desktop/MplusAutomation/result500")
  if (j==1)
    write.table(b,file="bsummary-1.csv",append = TRUE,sep = ", ",na = "NA",col.names = TRUE,row.names = FALSE) 
  if (j!=1)
    write.table(b,file="bsummary-1.csv",append = TRUE,sep = ", ",na = "NA",col.names = FALSE,row.names = FALSE) 
}

