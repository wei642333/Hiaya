library(MplusAutomation)
library(texreg)

str(wdat, list.len = 3)  # see the dataset
prcvars  # vector of variable stubs
# setup a temporary directory to write Mplus files in
base <- tempdir()              #Create Names for Temporary Files
pre <- "cfa"


t3cfa.body <- mplusObject(
  TITLE = "CFA for PRC Control and Threat at T3;",
  ANALYSIS = "ESTIMATOR = ML;",
  MODEL = "Threat BY prc1T3* prc2T3 prc4T3 prc7T3;
  Control BY prc7T3* prc8T3 prc9T3;
  Threat@1 Control@1;
  Threat WITH Control;",
  OUTPUT = "STDYX;
  MODINDICES (ALL 10);",
  usevariables = paste0(prcvars, "T3"),
  rdata = wdat)

str(t3cfa.body, max.level = 1, nchar.max = 30)

cd(base, pre, num <- "prct3")           #改变工作目录
#~~~~~~~~~~~run这个模型
t3cfa <- mplusModeler(t3cfa.body,
                      dataout = paste0(pre, num, ".dat"),
                      modelout = paste0(pre, num, ".inp"), run=TRUE)

summary(t3cfa)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~输出结果
cfa <- list(t3cfa, t4cfa, t5cfa, t6cfa)
res <- sapply(cfa, function(m) m$results$summaries)

out <- res[c("ChiSqM_Value", "ChiSqM_DF",
             "ChiSqM_PValue", "CFI", "RMSEA_Estimate", "SRMR"), ]

rownames(out) <- c("Chi square", "df", "p value",
                   "CFI", "RMSEA", "SRMR")
colnames(out) <- paste("Time", 3:6)