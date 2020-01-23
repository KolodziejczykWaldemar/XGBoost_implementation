library(rpart)
source('helper_funcs.R')

xgb_init <- function(y, offset, parms, wt) {

  if (is.matrix(y) && ncol(y) > 1)
    stop("Matrix response not allowed")
  sfun <- function(yval, dev, wt, ylevel, digits ) {
    paste("  mean=", format(signif(yval, digits)),", MSE=" , format(signif(dev/wt, digits)),sep ='')
  }
  environment(sfun) <- .GlobalEnv
  list(y = c(y), parms = parms, numresp = 1, numy = 1, summary = sfun)
}

xgb_eval <- function(indexes, wt, parms) {

  lambda <- parms$lambda
  tree_number <- parms$tree_number
  lookup_table <- parms$lookup_table

  y_real = lookup_table$Y[indexes]
  rss <- sum(wt*(y_real-sum(y_real*wt)/sum(wt))^2)

  jacobians = lookup_table[[paste('G_', tree_number-1, sep='')]][indexes]
  hessians = lookup_table[[paste('H_', tree_number-1, sep='')]][indexes]

  jac_sum = sum(jacobians)
  hess_sum = sum(hessians)

  y_pred = - jac_sum / (hess_sum + lambda)

  list(label = y_pred, deviance = rss)
}

xgb_split <- function(indexes, wt, x, parms, continuous){

  lambda = parms$lambda
  gamma = parms$gamma
  tree_number <- parms$tree_number
  lookup_table <- parms$lookup_table

  y_real = lookup_table$Y[indexes]
  jacobians = lookup_table[[paste('G_', tree_number-1, sep='')]][indexes]
  hessians = lookup_table[[paste('H_', tree_number-1, sep='')]][indexes]

  if (!(continuous)) {
    # Categorical X variable
    sorted = sort(x, index.return=TRUE)
    sorted_idxes = sorted$ix
    y_real <- y_real[sorted_idxes]
    indexes <- indexes[sorted_idxes]
    jacobians <- jacobians[sorted_idxes]
    hessians <- hessians[sorted_idxes]
  }

  gain_list = c()
  direction_list = c()
  for (split_idx in c(2: length(y_real))) {
    y_left = y_real[0: (split_idx-1)]
    y_right = y_real[split_idx:length(y_real)]

    jac_left_sum = sum(jacobians[0: (split_idx-1)])
    jac_right_sum = sum(jacobians[split_idx:length(jacobians)])

    hess_left_sum = sum(hessians[0: (split_idx-1)])
    hess_right_sum = sum(hessians[split_idx:length(hessians)])

    left_formula = jac_left_sum^2 / (hess_left_sum + lambda)
    right_formula = jac_right_sum^2 / (hess_right_sum + lambda)
    all_formula = ((jac_left_sum + jac_right_sum)^2)/(hess_left_sum + hess_right_sum + lambda)

    gain = 0.5*(right_formula + left_formula - all_formula) - gamma
    gain_list <- c(gain_list, gain)

    if (mean(y_left)>mean(y_right)) {
      direction_list <- c(direction_list, 1)
    }
    else {
      direction_list <- c(direction_list, -1)
    }
  }
  list(goodness = gain_list, direction = direction_list)
}

#'
#' @name xgboost
#'
#' @title Custom XGboost implementation.
#' @description Function constructing custom XGboost.
#' It grows mutliple Decision Trees storing them in a single object of 'XGBoost' class.
#'
#'
#' @param input_formula   name of the column of classes (values) in a formula form
#' @param dataset         dataframe consising of attributes & classes used for training
#' @param task_type       task type, either 'regression' or 'classification'
#' @param hiperparms      XGBoost hiperparameters in a form of named list, containing the following parameters:
#' \describe{
#'   \item{eta}{step size (0-1) - it describes how consecutive trees affect prediction}
#'   \item{gamma}{Tree complexity penalty proportional to the numer of leafs}
#'   \item{lambda}{L2 regularization parameter}
#'   \item{n_trees}{Number of trees to build}
#' }
#'
#'
#'
#' @return multiple trees representing model encapsulated inside 'XGBoost' class.
#' @export
xgboost <- function(input_formula,
                    dataset,
                    task_type,
                    hiperparms) {

  eta <-hiperparms$eta
  lambda <- hiperparms$lambda
  gamma <- hiperparms$gamma
  n_trees <- hiperparms$n_trees

  xgb_methods <- list(eval = xgb_eval,
                      split = xgb_split,
                      init = xgb_init)

  all_variables = all.vars(input_formula)
  target_name = input_formula[[2]]
  feature_names = input_formula[[3]]

  rownames(dataset) <- 1:nrow(dataset)
  source_table <- data.frame(dataset[all_variables])
  source_table$Y <- source_table[[target_name]]
  source_table$idx <- as.numeric(rownames(source_table))
  source_table$Y_0 <- 0.5


  if (task_type == 'classification') {
    source_table['G_0'] <- jacobian_cls(y_pred=source_table['Y_0'],
                                        y_true=source_table$Y)
    source_table['H_0'] <- hessian_cls(y_pred=source_table['Y_0'],
                                       y_true=source_table$Y)
  } else if (task_type == 'regression') {
    source_table['G_0'] <- jacobian_reg(y_pred=source_table['Y_0'],
                                        y_true=source_table$Y)
    source_table['H_0'] <- hessian_reg(y_pred=source_table['Y_0'],
                                       y_true=source_table$Y)
  } else {
    stop('Wrong task type!')
  }


  xgb_trees <- list()
  for (tree_number in c(1:n_trees)){

    current_parms <- list(
      lambda = lambda,
      gamma = gamma,
      tree_number = tree_number,
      lookup_table = source_table)

    fit <- rpart(paste('idx',
                       paste(feature_names,
                             collapse = " + "),
                       sep = " ~ "),
                 data = source_table,
                 method = xgb_methods,
                 parms = current_parms)

    source_table[paste('Y_', tree_number, sep='')] <- source_table[paste('Y_', tree_number - 1, sep='')] + eta * predict(fit, source_table)

    if (task_type == 'classification') {
      source_table[paste('G_', tree_number, sep='')] <- jacobian_cls(y_pred=source_table[paste('Y_', tree_number, sep='')],
                                                                     y_true=source_table$Y)
      source_table[paste('H_', tree_number, sep='')] <- hessian_cls(y_pred=source_table[paste('Y_', tree_number, sep='')],
                                                                    y_true=source_table$Y)
    } else if (task_type == 'regression') {
      source_table[paste('G_', tree_number, sep='')] <- jacobian_reg(y_pred=source_table[paste('Y_', tree_number, sep='')],
                                                                     y_true=source_table$Y)
      source_table[paste('H_', tree_number, sep='')] <- hessian_reg(y_pred=source_table[paste('Y_', tree_number, sep='')],
                                                                    y_true=source_table$Y)
    } else {
      stop('Wrong task type!')
    }

    xgb_trees[[tree_number]] <- fit

    # Print useful visualizations
    # jpeg(paste('Y_', tree_number-1, '.jpg', sep=''), width = 800, height = 620)
    # plot(x1,
    #      source_table$Y,
    #      col='green',
    #      main=paste("tree: ", tree_number-1,
    #                 '  lambda: ', lambda,
    #                 '  gamma: ', gamma,
    #                 '  step_size: ', alpha,
    #                 sep=''))
    # lines(x1, source_table[[paste('Y_', tree_number-1, sep='')]])
    # dev.off()
    # rpart.plot(fit, box.palette="RdBu", shadow.col="grey", nn=TRUE)
  }

  xgb <- list(tree_struct = xgb_trees,
              hiperparms = hiperparms,
              task_type = task_type)
  class(xgb) <- "XGBoost"
  return(xgb)
}


#'
#' @name predict.XGBoost
#'
#' @title Prediction for XGBoost class.
#' @description Function constructs prediction based on fitted trees in XGBoost instance.
#'
#'
#' @param fit             fitted XGBoost instance
#' @param dataset         dataframe consising of attributes & classes used for training
#'
#'
#' @return vector of prediction as a result of additive training
#' @export
 predict.XGBoost <- function(fit,
                             dataset) {
   eta <- fit$hiperparms$eta
   task_type <- fit$task_type
   xgb_trees <- fit$tree_struct

   rownames(dataset) <- 1:nrow(dataset)
   prediction = rep(0.5, nrow(dataset))
   for (tree in xgb_trees) {
     prediction <- prediction + eta * predict(tree, dataset)
   }

   if (task_type == 'classification') {
     prediction <- get_probs_from_logodds(prediction)
   }
   return (prediction)
 }

