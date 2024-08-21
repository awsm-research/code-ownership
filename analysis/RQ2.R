source("~/code-ownership/analysis/modelMisc.R")
library(Rnalytica)
library(rms)
library(ScottKnottESD)
library(reshape2)
library(plyr)

setwd("~/mining-ownership/defect_data_file/")


releases = list.files("./metrics/")

ranks = data.frame()
AUC_results = data.frame()
for(rel in releases){
print(rel)
  # if(rel %in% c("activemq_activemq-5.1.0.csv","activemq_activemq-5.2.0.csv","activemq_activemq-5.3.0.csv","activemq_activemq-5.8.0.csv")){
  #   next
  # }
data = read.csv(paste0("metrics/",rel), stringsAsFactors = F)


#Check BugRatio
bratio = nrow(data[data$HasBug > 0,])/nrow(data)
print(bratio)

print("BUILD MODEL")

#Set independent vars
indep = colnames(data)
indep = indep[!(indep %in% c('File','Ratio_code_comment','NumFiles','HasBug','BugCount'))]
#Remove constant
for(i in indep){
  if(var(data[,i]) == 0 ){
    indep = indep[indep != i]
  } 
}
print(indep)


dep = "HasBug"

print(summary(data[,indep]))

plotVarClus(data, indep, correlation = "spearman",correlation.threshold = 0.7)
indep = AutoSpearman(dataset = data, metrics = indep)
print(indep)

##Check degrees of freedom
print(table(data[,dep]))
budgetted_DF = floor(min(nrow(data[data[,dep] == T,]), nrow(data[data[,dep] == F,]) )/15)
print(budgetted_DF)
print(length(indep))

#Assign non-linear
gen_formula = GetNonlinearFormula(data, indep, dep, threshold = 0.2)
print(gen_formula)
dd <- datadist(data[,c(dep,indep)])
options(datadist = "dd")
fit <- lrm(gen_formula, data=data, x=T, y=T)
val = validate(fit, B=100)
AUC = 0.5 + val[1,1]/2
AUC_optimism_reduced = (0.5 + val[1,5]/2) 
AUC_optimism = AUC - AUC_optimism_reduced

proj = gsub("_.*","",rel)
AUC_results = rbind(AUC_results, data.frame(project=proj, release = rel, auc = AUC, auc_optimism_reduced=AUC_optimism_reduced, optimism = AUC_optimism))

#Bootstrap ANOVA
all_var_imp = data.frame()
seed.cnt = 0
success_boot = 0
while(success_boot < 100 & seed.cnt < 1000){
  seed.cnt = seed.cnt + 1
  set.seed(seed.cnt+1234)
  subd = sample(nrow(data),replace = TRUE)
  tryCatch({
    
    # gen_f = GetNonlinearFormula(data[subd,],indep, dep, threshold = 0.25)
    # sub_fit = lrm(gen_f, data=data[subd,], x=T, y=T) 
    sub_fit = update(fit,data=data, subset=subd)
    
    var_imp = data.frame(anova(sub_fit, type=2))
    var_imp$indep = row.names(var_imp)
    var_imp$Chi.Square = var_imp$Chi.Square*100/var_imp$Chi.Square[var_imp$indep == "TOTAL"]
    all_var_imp = rbind(all_var_imp, data.frame(boot=success_boot, variable = var_imp[var_imp$indep %in% indep,]$indep, value = var_imp[var_imp$indep %in% indep,]$Chi.Square ))
    
    success_boot = success_boot + 1
  }
    , error = function(e) {print(e)
    })
  print(seed.cnt)
}




all_var_imp = dcast(all_var_imp, boot ~ variable, drop = F,fill = 0)
all_var_imp$boot = NULL
rank = sk_esd(all_var_imp)


ranks = rbind(ranks, data.frame(project = proj, release = rel, var=names(rank$group),rank=rank$groups))
}

write.csv(ranks, file="../analysis_results/RQ4-var_imp.csv",row.names = F)
write.csv(AUC_results, file = "../analysis_results/RQ4-AUC.csv",row.names = F)
num_releases = length(unique(ranks$release))
probs = ddply(ranks, .(var), .fun = function(x){
  return(data.frame(Top1Prob = round(nrow(x[x$rank == 1,])/num_releases,digits = 2),
         Top2Prob = round(nrow(x[x$rank <= 2,])/num_releases, digits = 2),
         Top3Prob = round(nrow(x[x$rank <= 3,])/num_releases, digits = 2)
  ))
})
