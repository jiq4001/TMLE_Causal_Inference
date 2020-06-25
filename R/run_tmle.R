#'
#'@description inherit parma from main wrapper, do fine tune tmleMSM() --line 50
#'default variance.methods = "tmle", working.msm="Y ~ (time + cum.A)" 
#'@depends ltmle,SuperLearner
#'@parma int_tp: int, interval between nodes (months) 
#'@parma max_censor: costum censoring time (month), use large number for administritive censoring
#'@parma med_to_check: a string, treatment (which medication) to evaluate
#'@parma cwd inherit work directory passed by warraper function
#'@return obj of tmleMSM() result
#'
run_tmle <- function(int_tp, #month/ between nodes
                     max_censor,
                     med_to_check,
                     cwd = cwd){

  if(is.null(int_tp) | is.null(max_censor)){
    stop("needs nodes interval and max_censoring frame.\n")
  }
  #----------load data---------- 
  finaldf <- HMIII_data_wrag(int_tp = int_tp,
                             max_censor = max_censor,
                             med_to_check = med_to_check,
                             cwd = cwd)
  for (i in grep("C_*_", names(finaldf))) {
    finaldf[, i] <- factor(finaldf[, i], levels = c(0,1), labels = c("uncensored", "censored"))
  }
  #----------treatment regimes----------  
  num.nodes <- max_censor/int_tp
  # empirical regimes
  Regimes.design <- as.matrix(finaldf[, grep("A_*", colnames(finaldf))])
  colnames(Regimes.design) <- paste("TimeNode", 0: (ncol(Regimes.design)-1), sep = "")
  
  num.of.Regimes <- nrow(Regimes.design)
  num.pt <- 128 
  regimesList <- array(NA, dim = c(num.pt, num.nodes, num.of.Regimes))
  for (i in 1 : num.pt) {
    regimesList[i, , ] <- Regimes.design
  }
  #----------measurment summary----------   
  num.ynodes <- max_censor/int_tp 
  Summary.Measures <- array(NA, dim = c(num.of.Regimes, 2, num.ynodes))
  Summary.Measures[, , 1] <- cbind(Regimes.design[, 1], rep(1, num.of.Regimes))
  for (i in 2 : num.ynodes) {
    time <- c(1:num.ynodes)[i]
    temp.A <- rowSums(Regimes.design[, 1:i])
    Summary.Measures[, , i] <- cbind(temp.A, rep(time, num.of.Regimes))
  }
  dimnames(Summary.Measures)[[2]] <- c("cum.A", "time")
  #----------library---------- 
  load_sl_library()
  #----------TMLE function----------   
  result.regList <- ltmleMSM(finaldf, 
                             Anodes= grep("A_*_", names(finaldf)), 
                             Cnodes= grep("C_*_", names(finaldf)), 
                             Ynodes= grep("Y_*_", names(finaldf)),
                             Lnodes= grep("L_.*", names(finaldf)),
                             survivalOutcome=TRUE, 
                             regimes=regimesList,
                             summary.measures=Summary.Measures, 
                             variance.method = "tmle", 
                             final.Ynodes= grep("Y_*", names(finaldf)),
                             SL.library = list("SL.mlp", "SL.glm", "SL.caretRF", "SL.caretXGB", "SL.glmboost", "SL.knn"),
                             SL.cvControl = list(V = 2, stratifyCV = F),
                             working.msm="Y ~ (time + cum.A)",
                             iptw.only = F, 
                             gcomp = F,
                             msm.weights = NULL, 
                             estimate.time=FALSE)
  return(result.regList)
}
