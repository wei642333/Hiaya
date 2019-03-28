library(MplusAutomation)
#Set the folder containing mplus.txt as working
createModels("mplus.txt")

#Set the folder containing input files as working directory
runModels() 


#Set the folder containing output files as working directory
outputs<-extractModelParameters()



ar<-numeric(202)  #vector of length #outputs
#i=list (one per output)
a=1 #1=unstandardized, 2=standardized (if available)
b=3 #which column: param name,point estimate, posterior sd,..,
c=1 #which parameter: AR-coff, intercept, resid.var.

for(i in 1:202){
  
  ar[i]<-as.numeric((outputs[[i]][[a]][b])[c,])
}
summary(ar)

