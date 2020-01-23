library(rpart)
library(rpart.plot)
source('helper_funcs.R')

lb_init <- function(y, offset, parms, wt) {

  if (is.matrix(y) && ncol(y) > 1)
    stop("Matrix response not allowed")
  sfun <- function(yval, dev, wt, ylevel, digits ) {
    paste("  mean=", format(signif(yval, digits)),", MSE=" , format(signif(dev/wt, digits)),sep ='')
  }
  environment(sfun) <- .GlobalEnv
  list(y = c(y), parms = parms, numresp = 1, numy = 1, summary = sfun)
}

lb_eval <- function(indexes, wt, parms) {
  tree_number <- parms$tree_number
  lookup_table <- parms$lookup_table

  y_real = lookup_table$Y[indexes]
  rss <- sum(wt*(y_real-sum(y_real*wt)/sum(wt))^2)

  probs = lookup_table[[paste('P_', tree_number-1, sep='')]][indexes]

  probs_sum = sum(probs)
  y_sum = sum(y_real)

  y_pred = (y_sum - probs_sum) / sum(probs * (1 - probs))

  list(label = y_pred, deviance = rss)
}

lb_split <- function(indexes, wt, x, parms, continuous) {
  # Center y
  lookup_table <- parms$lookup_table
  tree_number <- parms$tree_number
  
  y = lookup_table[[paste('resid_', tree_number-1, sep='')]][indexes]
  n <- length(y)
  y <- y- sum(y*wt)/sum(wt)
  if (continuous) {
    # continuous x variable
    temp <- cumsum(y*wt)[-n]
    left.wt  <- cumsum(wt)[-n]
    right.wt <- sum(wt) - left.wt
    lmean <- temp/left.wt
    rmean <- -temp/right.wt
    goodness <- (left.wt*lmean^2 + right.wt*rmean^2)/sum(wt*y^2)
    list(goodness = goodness, direction = sign(lmean))
  } else {
      # Categorical X variable
    ux <- sort(unique(x))
    wtsum <- tapply(wt, x, sum)
    ysum  <- tapply(y*wt, x, sum)
    means <- ysum/wtsum
    
    ord <- order(means)
    n <- length(ord)
    temp <- cumsum(ysum[ord])[-n]
    left.wt  <- cumsum(wtsum[ord])[-n]
    right.wt <- sum(wt) - left.wt
    lmean <- temp/left.wt
    rmean <- -temp/right.wt
    list(goodness= (left.wt*lmean^2 + right.wt*rmean^2)/sum(wt*y^2),direction = ux[ord])
  }
}

#'
#' @name logitboost
#'
#' @title Custom LogitBoost implementation.
#' @description Function constructing custom LogitBoost
#' It grows mutliple Decision Trees storing them in a single object of 'LogitBoost' class.
#'
#'
#' @param input_formula   name of the column of classes (values) in a formula form
#' @param dataset         dataframe consising of attributes & classes used for training
#' @param hiperparms      LogitBoost hiperparameters in a form of named list, containing the following parameters:
#' \describe{
#'   \item{eta}{step size (0-1) - it describes how consecutive trees affect prediction}
#'   \item{n_trees}{Number of trees to build}
#' }
#'
#'
#'
#' @return multiple trees representing model encapsulated inside 'LogitBoost' class.
#' @export
logitboost <- function(input_formula,
                    dataset,
                    hiperparms) {

  eta <-hiperparms$eta
  n_trees <- hiperparms$n_trees

  lb_methods <- list(eval = lb_eval,
                      split = lb_split,
                      init = lb_init)

  all_variables = all.vars(input_formula)
  target_name = input_formula[[2]]
  feature_names = input_formula[[3]]

  rownames(dataset) <- 1:nrow(dataset)
  source_table <- data.frame(dataset[all_variables])
  source_table$Y <- source_table[[target_name]]
  source_table$idx <- as.numeric(rownames(source_table))
 
  source_table[paste('P_', 0, sep = '')] <- mean(source_table$Y)
  source_table[paste('F_', 0, sep = '')] <- get_logodds_from_prob(source_table$P_0)
  source_table[paste('resid_', 0, sep = '')] <- source_table$Y - source_table[paste('P_', 0, sep = '')]

  boost_trees <- list()
  for (tree_number in c(1:n_trees)){

    current_parms <- list(
      tree_number = tree_number,
      lookup_table = source_table)

    fit <- rpart(paste('idx',
                       paste(feature_names,
                             collapse = " + "),
                       sep = " ~ "),
                 data = source_table,
                 method = lb_methods,
                 parms = current_parms)

    source_table[paste("F_", tree_number, sep='')] <- source_table[paste("F_", tree_number-1, sep='')] + eta * predict(fit, source_table)
    source_table[paste("P_", tree_number, sep='')] <- get_probs_from_logodds(source_table[paste("F_", tree_number, sep='')])
    source_table[paste("resid_", tree_number, sep='')] <- source_table["Y"] - source_table[paste("P_", tree_number, sep='')]

    boost_trees[[tree_number]] <- fit

    # Plot tree structure
    # rpart.plot(fit, box.palette="Blues", shadow.col="grey", nn=TRUE)
  }

  lb <- list(tree_struct = boost_trees,
             hiperparms = hiperparms,
             first_prediction = mean(source_table$Y))
  class(lb) <- "LogitBoost"
  return(lb)
}


#'
#' @name predict.LogitBoost
#'
#' @title Prediction for LogitBoost class.
#' @description Function constructs prediction based on fitted trees in LogitBoost instance.
#'
#'
#' @param fit             fitted LogitBoost instance
#' @param dataset         dataframe consising of attributes & classes used for training
#'
#'
#' @return vector of prediction as a result of additive training
#' @export
 predict.LogitBoost <- function(fit,
                                dataset) {
   eta <- fit$hiperparms$eta
   lb_trees <- fit$tree_struct

   rownames(dataset) <- 1:nrow(dataset)
   prediction = get_logodds_from_prob(rep(fit$first_prediction, nrow(dataset)))
   for (tree in lb_trees) {
     prediction <- prediction + eta * predict(tree, dataset)
   }
   prediction <- get_probs_from_logodds(prediction)
   return (prediction)
 }
