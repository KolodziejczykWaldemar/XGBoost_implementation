library(ggplot2)
library(rpart)
library(partykit)
library(rpart.utils)
library(rpart.plot)
library(caret)

titanic <- read.csv('C:/Users/Waldemar/Documents/R/titanic.csv', header=TRUE, sep=',')
dataset <- subset(titanic, select=c(y, x1, x2, x3, x4, x5))

get_logodds_from_prob <- function(prob) {
  log((prob)/(1-prob))
}

get_probs_from_logodds <- function(logodds) {
  exp(logodds)/(1 + exp(logodds))
}

get_train_predictions <- function(tree, gradient_results, m) {
  leafs = as.numeric(unlist(row.names(tree$frame)[tree$where]))
  id_leaf_mapping <- data.frame("leaf" = leafs, "sample_number" = as.numeric(rownames(gradient_results)))
  
  unique_leaf_numbers <- unique(id_leaf_mapping[, "leaf"])
  
  gammas <- list()
  i <- 1
  for (leaf in unique_leaf_numbers) {
    # wybor id przykladow treningowych dla kazdego liscia
    # print(leaf)
    # print(id_leaf_mapping[id_leaf_mapping$leaf == leaf,]$sample_number)
    # dla wybranych id zobacz podzbior danych
    # print(gradient_results[id_leaf_mapping[id_leaf_mapping$leaf == leaf,]$sample_number,])
    
    # wylicz predykcje
    last_probs_in_leaf <- gradient_results[id_leaf_mapping[id_leaf_mapping$leaf == leaf,]$sample_number,][[paste('P_', m-1, sep='')]]
    numerator <- sum(gradient_results[id_leaf_mapping[id_leaf_mapping$leaf == leaf,]$sample_number,][['y']]) - sum(last_probs_in_leaf)
    denominator <- sum(last_probs_in_leaf * (1 - last_probs_in_leaf))
    prediction_in_leaf <- numerator / denominator
    
    # print(paste('predykcja:', prediction_in_leaf))
    
    gammas[i] <- prediction_in_leaf
    i <- i + 1
    # TODO na podstawie wartoœci gamma wylicz nowe predykcje F_1
  }
  leaf_prediction_maping <- data.frame("leaf" = unique_leaf_numbers, 
                                       "gamma" = unlist(gammas, use.names=FALSE))
  merged_leaf_prediction = merge(id_leaf_mapping, leaf_prediction_maping, by='leaf')
  # TODO coœ jest nie tak  z tym mapowaniem na liscie w partykit
  pred_leafs <- predict(as.party(tree), dataset[, 2:6], type="node")
  new_leaf_mapping <- data.frame("LEAF" = pred_leafs, "sample_number" = as.numeric(rownames(gradient_results)))
  merged = merge(merged_leaf_prediction, new_leaf_mapping, by='sample_number')
  return(merged)
  
}


ggplot(data = dataset, mapping = aes(x = x1, y = x3, color = as.factor(y))) + geom_point()


# Hiperparametry
alpha= 0.2
n_trees = 15

# Trenowanie modelu
tree_list <- list()
gradient_results = cbind(dataset)
# oblicz pierwsz¹ estymacjê jako œredni¹ z kolumny y oraz rezidua
gradient_results[paste('P_', 0, sep = '')] <- mean(gradient_results$y)
gradient_results[paste('F_', 0, sep = '')] <- get_logodds_from_prob(gradient_results$P_0)
gradient_results[paste('resid_', 0, sep = '')] <- gradient_results['y'] - gradient_results[paste('P_', 0, sep = '')]

print(confusionMatrix(factor(as.integer(gradient_results[, paste("P_", 0, sep='')] > 0.5)), factor(gradient_results$y)))

for (m in 1:n_trees) 
{
  tree <- rpart(paste(paste('resid_', m-1, sep = ''), 
                      paste(c('x1', 'x2', 'x3', 'x4', 'x5'), collapse = " + "), 
                      sep = " ~ "), 
                data = gradient_results)
  rpart.plot(tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)
  
  tree_list <- append(tree_list, list(tree))
  
  tree_results <- get_train_predictions(tree, gradient_results, m)
  gradient_results[, paste("gamma_", m, sep='')] <- tree_results$gamma
  gradient_results[, paste("leaf_", m, sep='')] <- tree_results$leaf
  gradient_results[, paste("LEAF_", m, sep='')] <- tree_results$LEAF
  
  gradient_results[, paste("F_", m, sep='')] <- gradient_results[, paste("F_", m-1, sep='')] + alpha*gradient_results[, paste("gamma_", m, sep='')] 
  gradient_results[, paste("P_", m, sep='')] <- get_probs_from_logodds(gradient_results[, paste("F_", m, sep='')])
  gradient_results[, paste("resid_", m, sep='')] <- gradient_results[, "y"] - gradient_results[, paste("P_", m, sep='')]
  
  print(confusionMatrix(factor(as.integer(gradient_results[, paste("P_", m, sep='')] > 0.5)), factor(gradient_results$y)))
  m <- m + 1
  
}



# ------------------------------------- test-----------------------------------------------------------------
titanic <- read.csv('C:/Users/Waldemar/Documents/R/titanic.csv', header=TRUE, sep=',')
dataset <- subset(titanic, select=c(y, x1, x2, x3, x4, x5))

alpha= 0.2
n_trees = 15

# Trenowanie modelu
tree_list <- list()
gradient_results = cbind(dataset)
# oblicz pierwsz¹ estymacjê jako œredni¹ z kolumny y oraz rezidua
gradient_results[paste('P_', 0, sep = '')] <- mean(gradient_results$y)
gradient_results[paste('F_', 0, sep = '')] <- get_logodds_from_prob(gradient_results$P_0)
gradient_results[paste('resid_', 0, sep = '')] <- gradient_results['y'] - gradient_results[paste('P_', 0, sep = '')]

gradient_results[, 'pred'] = as.integer(gradient_results$P_0 > 0.5)
confusionMatrix(factor(gradient_results$pred), factor(gradient_results$y))

m = 1
tree <- rpart(paste(paste('resid_', m-1, sep = ''), 
                    paste(c('x1', 'x2', 'x3', 'x4', 'x5'), collapse = " + "), 
                    sep = " ~ "), 
              data = gradient_results)
rpart.plot(tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)



#----------------------------
leafs = as.numeric(unlist(row.names(tree$frame)[tree$where]))
id_leaf_mapping <- data.frame("leaf" = leafs, "sample_number" = as.numeric(rownames(gradient_results)))
unique_leaf_numbers <- unique(id_leaf_mapping[, "leaf"])
gammas <- list()
i <- 1
for (leaf in unique_leaf_numbers) {
  # wybor id przykladow treningowych dla kazdego liscia
  print(leaf)
  print(id_leaf_mapping[id_leaf_mapping$leaf == leaf,]$sample_number)
  # dla wybranych id zobacz podzbior danych
  print(gradient_results[id_leaf_mapping[id_leaf_mapping$leaf == leaf,]$sample_number,])
  
  # wylicz predykcje
  last_probs_in_leaf <- gradient_results[id_leaf_mapping[id_leaf_mapping$leaf == leaf,]$sample_number,][[paste('P_', m-1, sep='')]]
  numerator <- sum(gradient_results[id_leaf_mapping[id_leaf_mapping$leaf == leaf,]$sample_number,][['y']]) - sum(last_probs_in_leaf)
  denominator <- sum(last_probs_in_leaf * (1 - last_probs_in_leaf))
  prediction_in_leaf <- numerator / denominator
  
  # print(paste('predykcja:', prediction_in_leaf))
  
  gammas[i] <- prediction_in_leaf
  i <- i + 1
  # TODO na podstawie wartoœci gamma wylicz nowe predykcje F_1
}
leaf_prediction_maping <- data.frame("leaf" = unique_leaf_numbers, 
                                     "gamma" = unlist(gammas, use.names=FALSE))
merged_leaf_prediction = merge(id_leaf_mapping, leaf_prediction_maping, by='leaf')

pred_leafs <- predict(as.party(tree), dataset[, 2:6], type="node")
new_leaf_mapping <- data.frame("LEAF" = pred_leafs, "sample_number" = as.numeric(rownames(gradient_results)))
merged = merge(merged_leaf_prediction, new_leaf_mapping, by='sample_number')


#-------------------------------------
# tree_results <- get_train_predictions(tree, gradient_results, m)
tree_results <- merged
gradient_results[, paste("gamma_", m, sep='')] <- tree_results$gamma
gradient_results[, paste("leaf_", m, sep='')] <- tree_results$leaf
gradient_results[, paste("LEAF_", m, sep='')] <- tree_results$LEAF

gradient_results[, paste("F_", m, sep='')] <- gradient_results[, paste("F_", m-1, sep='')] + alpha*gradient_results[, paste("gamma_", m, sep='')] 
gradient_results[, paste("P_", m, sep='')] <- get_probs_from_logodds(gradient_results[, paste("F_", m, sep='')])
gradient_results[, paste("resid_", m, sep='')] <- gradient_results[, "y"] - gradient_results[, paste("P_", m, sep='')]

print(confusionMatrix(factor(as.integer(gradient_results$P_1 > 0.5)), factor(gradient_results$y)))

