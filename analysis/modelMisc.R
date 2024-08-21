library(rms)
GetNonlinearFormula = function(data, indep, dep, threshold = 0.1){
  sp <- spearman2(formula(paste(dep ," ~ ",paste0(indep, collapse=" + "))), data= data, p=2)
  sp = data.frame(rho2 = sp[,1])
  formula_fit = dep
  for(i in 1:nrow(sp))
  {
    if(i == 1){
      formula_fit = paste0(formula_fit," ~ ")
    }else{
      formula_fit = paste0(formula_fit," + ")
    }
    
    var = row.names(sp)[i]
    if(round(sp$rho2[i],digits = 2) > threshold){
      formula_fit = paste0(formula_fit, "rcs(",var,",3)")
    }else{
      formula_fit = paste0(formula_fit,var)
    }
  }
  return(formula(formula_fit))
  
}