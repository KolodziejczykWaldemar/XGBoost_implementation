library(rpart)

#'
#' @name gradientboost
#'
#' @title Custom GradientBoost implementation.
#' @description Function constructing custom GradientBoost
#' It grows mutliple Decision Trees storing them in a single object of 'GradientBoost' class.
#'
#'
#' @param input_formula   name of the column of classes (values) in a formula form
#' @param dataset         dataframe consising of attributes & classes used for training
#' @param hiperparms      GradientBoost hiperparameters in a form of named list, containing the following parameters:
#' \describe{
#'   \item{eta}{step size (0-1) - it describes how consecutive trees affect prediction}
#'   \item{n_trees}{Number of trees to build}
#' }
#'
#'
#'
#' @return multiple trees representing model encapsulated inside 'GradientBoost' class.
#' @export
gradientboost <- function(input_formula,
                          dataset,
                          hiperparms) {
  
  eta <-hiperparms$eta
  n_trees <- hiperparms$n_trees
  
  all_variables = all.vars(input_formula)
  target_name = input_formula[[2]]
  feature_names = input_formula[[3]]
  
  source_table <- data.frame(dataset[all_variables])
  source_table$Y <- source_table[[target_name]]
  
  source_table[paste('F_', 0, sep = '')] <- mean(source_table$Y)
  source_table[paste('resid_', 0, sep = '')] <- source_table$Y - source_table[paste('F_', 0, sep = '')]
  
  boost_trees <- list()
  for (tree_number in c(1:n_trees)){
    
    fit <- rpart(paste(paste('resid_',
                             tree_number-1, 
                             sep = ''),
                       paste(feature_names,
                             collapse = " + "),
                       sep = " ~ "),
                 data = source_table)
    
    source_table[paste("F_", tree_number, sep='')] <- source_table[paste("F_", tree_number-1, sep='')] + eta * predict(fit, source_table)
    source_table[paste("resid_", tree_number, sep='')] <- source_table["Y"] - source_table[paste("F_", tree_number, sep='')]
    
    boost_trees[[tree_number]] <- fit
    
    # Plot useful visualizations
    # plot(source_table$X1,
    #      source_table$Y,
    #      col='green',
    #      main=paste("tree: ", tree_number-1,
    #                 '  step_size: ', alpha,
    #                 sep=''))
    # lines(source_table$X1, source_table[[paste('F_', tree_number-1, sep='')]])
    # rpart.plot(fit, box.palette="RdBu", shadow.col="grey", nn=TRUE)
  }
  
  gb <- list(tree_struct = boost_trees,
             hiperparms = hiperparms,
             first_prediction = mean(source_table$Y))
  class(gb) <- "GradientBoost"
  return(gb)
}


#'
#' @name predict.GradientBoost
#'
#' @title Prediction for GradientBoost class.
#' @description Function constructs prediction based on fitted trees in GradientBoost instance.
#'
#'
#' @param fit             fitted GradientBoost instance
#' @param dataset         dataframe consising of attributes & classes used for training
#'
#'
#' @return vector of prediction as a result of additive training
#' @export
predict.GradientBoost <- function(fit,
                                  dataset) {
  eta <- fit$hiperparms$eta
  boost_trees <- fit$tree_struct
  
  prediction = rep(fit$first_prediction, nrow(dataset))
  for (tree in boost_trees) {
    prediction <- prediction + eta * predict(tree, dataset)
  }
  return (prediction)
}
