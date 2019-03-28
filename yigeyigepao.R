library(MplusAutomation)
library(texreg)
library(xlsx)


Type=1
nreps=100
rela=0.3
nobs=100
TL=0.7
CL=0
gamma1 =0.14
gamma2=0.14
seed = 1234
createModels("runmplus.txt")

#Set the folder containing input files as working directory
setwd('C:/Users/apc/Desktop/MplusAutomation/data')      #����·��Ϊ���Ǹո����inp�ļ����ļ���
runModels() 

t=paste('[[init]]','\n','iterators = dataset;','\n',
        'dataset = 1:10;','\n',
        'filename = Sample_for_Mplus_[[dataset]].inp;','\n',
        'outputDirectory = C:/Users/apc/Desktop/MplusAutomation/data;','\n',    #ע��һ��inp�ļ�Ҫ��data�ļ�����һ��
        '[[/init]]','\n',
        'TITLE:','\n',
          'one dataset at the time.','\n',
        'DATA:','\n',
         'file = Sample_for_Mplus_[[dataset]].dat;','\n',
        'VARIABLE:','\n',
          'NAMES = y1-y9;','\n',
        'ANALYSIS:','\n',
          'ESTIMATOR=ML;','\n',
        'MODEL: ','\n',
        '[y1-y9@0];','\n',
        'y1-y6;','\n',
        'y7-y9;','\n',
        'f1-f2@1;','\n',
        'f3;','\n',
        'f1 BY y1-y3;','\n',
        'f2 BY y4-y6;','\n',
        'f3 BY y7-y9;','\n',
        'f1 WITH f2*0.3;','\n',
        'f3 ON f1*',gamma1,' f2*',gamma2,';','\n',
        'OUTPUT: standardized;', '\n')
cat(t,file ="runmplus3.txt")

write.table(t, file = paste('Type',Type, '_',nobs,'_',TL,'_',CL,'_',gamma1,'_',gamma2,'_',rep,'.txt', sep = ''))
createModels("runmplus3.txt")  

h<-readModels(target = "C:/Users/apc/Desktop/MplusAutomation/data/1", recursive = FALSE)
s3 <- sapply(h, function(m) m$$summaries)                        #����ȡһ�������ܶ�����������
p3 <- sapply(h, function(n) n$parameters)
write.table(h,file="suibian2.csv",append = TRUE,sep = ", ",na = "NA",col.names = TRUE,row.names=FALSE,eol="\r")           #,��csv�ļ��еķָ���