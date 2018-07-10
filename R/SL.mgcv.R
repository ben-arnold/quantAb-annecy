

#----------------------------------
# SL.mgcv.R
# SuperLearner wrapper for a cubic
# spline GAM model in the mgcv 
# package, adapted from the SL.gam
# wrapper that comes with the package
#----------------------------------
SL.mgcv <- function (Y, X, newX, family, obsWeights, k = -1, cts.num = 4, 
          ...) 
{
  require("mgcv")
  if ("gam" %in% loadedNamespaces())
    warning("mgcv and gam packages are both in use. You might see an error because both packages use the same function names.")
  cts.x <- apply(X, 2, function(x) (length(unique(x)) > cts.num))
  if (sum(!cts.x) > 0) {
    gam.model <- as.formula(paste("Y~", paste(paste("s(", 
                                                    colnames(X[, cts.x, drop = FALSE]), ", k=", k, 
                                                    ")", sep = ""), collapse = "+"), "+", paste(colnames(X[, 
                                                                                                           !cts.x, drop = FALSE]), collapse = "+")))
  }
  else {
    gam.model <- as.formula(paste("Y~", paste(paste("s(", 
                                                    colnames(X[, cts.x, drop = FALSE]), ",k=", k, 
                                                    ")", sep = ""), collapse = "+")))
  }
  if (sum(!cts.x) == length(cts.x)) {
    gam.model <- as.formula(paste("Y~", paste(colnames(X), 
                                              collapse = "+"), sep = ""))
  }
  
  fit.gam <- mgcv::gam(gam.model, data = X, family = family, 
                      control = mgcv::gam.control(maxit = 50), 
                      weights = obsWeights)
  pred <- mgcv::predict.gam(fit.gam, newdata = newX, type = "response")
  fit <- list(object = fit.gam)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.mgcv")
  return(out)
}

#----------------------------------
# create a set of learners 
# varying parameter k from 2 to 20
#----------------------------------
k<-2:20
create.SL.mgcv <- function(tune = list(k=k)) {
  for (mm in seq(length(tune$k))) {
    eval(parse(file = "", text = paste("SL.mgcv.k", tune$k[mm], 
                                       "<- function(...,k = ", tune$k[mm], ") SL.mgcv(..., k = k)", 
                                       sep = "")), envir = .GlobalEnv)
  }
  invisible(TRUE)
}
create.SL.mgcv()



