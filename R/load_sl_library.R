#'
#'
#'@description load superlearner libraries
#'pass to main wrapper function 
#'
SL.caretRF <- function(Y, X, newX, family, obsWeights, ...) {
  SL.caret(Y, X, newX, family, obsWeights, method = "rf",  tuneLength = 10,
           trControl =  caret::trainControl(method = "cv", number = 3, search = "random",
                                            verboseIter = TRUE), ...)
}
SL.caretXGB <- function(Y, X, newX, family, obsWeights, ...) {
  SL.caret(Y, X, newX, family, obsWeights, method = "xgbTree", tuneLength = 10,
           trControl =  caret::trainControl(method = "cv", number = 3, search = "random",
                                            verboseIter = TRUE), ...)
}


SL.glmboost <- function(Y, X, newX, family, obsWeights, ...) {
  SL.caret(Y, X, newX, family, obsWeights, method = "glmboost", tuneLength = 10, 
           trControl =  caret::trainControl(method = "cv", number = 3, search = "random",
                                            verboseIter = TRUE), ...)
}

SL.mlp <- function(Y, X, newX, family, obsWeights, ...) {
  SL.caret(Y, X, newX, family, obsWeights, method = "mlp", tuneLength = 5, 
           trControl =  caret::trainControl(method = "cv", number = 3, search = "random",
                                            verboseIter = TRUE), ...)
}

SL.knn <- function(Y, X, newX, family, obsWeights, ...) {
  SL.caret(Y, X, newX, family, obsWeights, method = "kknn", tuneLength = 10, 
           preProc = c("center", "scale"),
           trControl =  caret::trainControl(method = "cv", number = 3, search = "random",
                                            verboseIter = TRUE), ...)
}