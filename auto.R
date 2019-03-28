library(MplusAutomation)
library(texreg)
#study5 <- read.spss("reanalysis-study-5-mt-fall-08.sav", to.data.frame=TRUE)
#ASData5 <- subset(study5, select=c("ppnum", paste("as", 1:33, sep="")))
#my_data=read.table('C:/Users/apc/Desktop/MplusAutomation/data/Sample_for_Mplus_1.dat')
#prepareMplusData(my_data,'C:/Users/apc/Desktop/MplusAutomation/data/Sample_for_Mplus_1.dat')
#��data�ļ�����ʺ���Mplus���������ݸ�ʽ
#N=10
#for (i in 1:N)
#filename=paste('Sample_for_Mplus_',N,sep = '')
#mydata=read.table(paste(filename,'.dat',sep = ''))
#prepareMplusData(mydata,'C:/Users/apc/Desktop/MplusAutomation/data/Mplus_1.dat')

createModels("runmplus.txt")

#Set the folder containing input files as working directory
setwd('"C:/Users/apc/Desktop/MplusAutomation/input/sem')      #����·��Ϊ���Ǹո����inp�ļ����ļ���
runModels() 

#Set the folder containing output files as working directory??????????
result<-readModels(target = getwd(), recursive = FALSE)
res <- sapply(result, function(m) m$results$summaries)

write.table(res,file="auto.csv",append = FALSE,sep = ", ",na = "NA")   

screenreg(result,single.row = TRUE,summaries=c('CFI','BIC','SRMR'))  #file = result.xlsx,
SummaryTable(result)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cfa <- list(t3cfa, t4cfa, t5cfa, t6cfa)
res <- sapply(cfa, function(m) m$results$summaries)
out <- res[c("ChiSqM_Value", "ChiSqM_DF",
             "ChiSqM_PValue", "CFI", "RMSEA_Estimate", "SRMR"), ]

rownames(out) <- c("Chi square", "df", "p value",
                   "CFI", "RMSEA", "SRMR")
colnames(out) <- paste("Time", 3:6)