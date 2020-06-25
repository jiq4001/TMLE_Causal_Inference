#'
#'@description wrapper function calls for daat_wrag.R, load_sl_library.R, run_tmle.R
#'fine tune the tmleMSM() if needed in run_tmle().R for change of variance.methods, working.msm, SL.ctrl,......
#'@depends tidyverse, magrittr, janitor, mice, ltmle,SuperLearner
#'@parma int_tp: int, interval between nodes (months) 
#'@parma max_censor: costum censoring time (month), use large number for administritive censoring
#'@parma med_to_check: a string, treatment (which medication) to evaluate
#'@parma cwd inherit work directory passed by warraper function
#'

source(paste(cwd, "R/run_tmle.R", sep = "/"))
source(paste(cwd, "R/data_wrag.R", sep = "/"))
source(paste(cwd, "R/load_sl_library.R", sep = "/"))

library(tidyverse)
library(magrittr)
library(janitor)
library(mice)
library(ltmle)
library(SuperLearner)


cwd <- paste(getwd(), "pack", sep = "/")
result <- run_tmle(int_tp = 3, #month/ between nodes
                  max_censor = 12,
                  med_to_check = "i_ace_arb",
                  cwd = cwd)

summary(result)