#'
#' @name get_logodds_from_prob
#'
#' @title Conversion probabilities to log(oods).
#' @description Function calculating log(oods) based on probabilities.
#'
#'
#' @param prob    vector of probabilities values
#'
#'
#' @return vector of log(oods)
#' @export
get_logodds_from_prob <- function(prob) {
  log((prob)/(1-prob))
}

#'
#' @name get_probs_from_logodds
#'
#' @title Conversion log(oods) to probabilities.
#' @description Function calculating probabilities based on log(oods).
#'
#'
#' @param logodds    vector of log(odds) values
#'
#'
#' @return vector of probabilities
#' @export
get_probs_from_logodds <- function(logodds) {
  return(exp(logodds)/(1 + exp(logodds)))
}

#'
#' @name jacobian_reg
#'
#' @title Jacobian calculation for regresion problem.
#' @description Function calculating Jacobians for Mean Squared Error function.
#'
#'
#' @param y_pred    vector of predicted values
#' @param y_true    vector of true values
#'
#'
#' @return vector of Jacobians.
#' @export
jacobian_reg <- function(y_pred, y_true){
  jac = -2*(y_true - y_pred)
  return(jac)
}

#'
#' @name hessian_reg
#'
#' @title Hessian calculation for regresion problem.
#' @description Function calculating Hessians for Mean Squared Error function.
#'
#'
#' @param y_pred    vector of predicted values
#' @param y_true    vector of true values
#'
#'
#' @return vector of Hessians
#' @export
hessian_reg <- function(y_pred, y_true){
  len = length(y_pred)
  hess = rep(2, len)
  return(hess)
}

#'
#' @name jacobian_cls
#'
#' @title Jacobian calculation for classifiaction problem.
#' @description Function calculating Jacobians for Binary Cross Entrophy loss function.
#'
#'
#' @param y_pred    vector of predicted values in the log(odds) form
#' @param y_true    vector of true values in the binary form
#'
#'
#' @return vector of Jacobians.
#' @export
jacobian_cls <- function(y_pred, y_true){
  y_pred_prob = get_probs_from_logodds(y_pred)
  jac = y_pred_prob - y_true
  return(jac)
}

#'
#' @name hessian_cls
#'
#' @title Hessian calculation for classifiaction problem.
#' @description Function calculating Hessians for Binary Cross Entrophy loss function.
#'
#'
#' @param y_pred    vector of predicted values in the log(odds) form
#' @param y_true    vector of true values in the binary form
#'
#'
#' @return vector of Hessians
#' @export
hessian_cls <- function(y_pred, y_true){
  y_pred_prob = get_probs_from_logodds(y_pred)
  hess = y_pred_prob* (1 - y_pred_prob)
  return(hess)
}