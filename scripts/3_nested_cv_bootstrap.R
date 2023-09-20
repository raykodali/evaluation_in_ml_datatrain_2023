###
#   Bootstrap inference for nested CV
###
#   Author:
#   Max Westphal (max.westphal@mevis.fraunhofer.de)
###
#   Description:
#   This script performs different bootstrap procedures for nested CV results.
###
#   Remark:
#   This needs only be executed for a reproduction of (pre-computation) results.
###

library(dplyr)
library(fabricatr)
library(ggplot2)

# Functions -----------------------------------------------------------------------------------
calc_auc_delta <- function(data){
  roc_ranger <- pROC::roc(data, "truth", "prob.suspect_ranger", levels=c("normal", "suspect"))
  roc_glmnet <- pROC::roc(data, "truth", "prob.suspect_glmnet", levels=c("normal", "suspect"))
  
  delta <- pROC::roc.test(roc_ranger, roc_glmnet)
  
  c(delta=as.numeric(delta$estimate[1] - delta$estimate[2]),
    lower=delta$conf.int[1],
    upper=delta$conf.int[2])
}


calc_auc_delta_cv <- function(data, aggr="mean"){
  
  lapply(unique(data$fold), function(id_fold){
    data %>% filter(fold == id_fold)
  }) %>% 
    lapply(calc_auc_delta) %>% 
    do.call(rbind, .) %>% 
    apply(2, aggr) 

}


calc_auc_delta_resample <- function(data, nboot=12, aggr=mean, cv=TRUE, nested = TRUE){
  lapply(1:nboot, function(i){
    
    n_fold <- length(unique(data$fold))
    n_obs_per_fold <- nrow(data)/n_fold
    
    if(nested){
      data_re <- resample_data(data, 
                               N=c(n_fold, n_obs_per_fold),
                               ID_labels = c("fold", "row_ids"),
                               unique_labels = TRUE) %>% 
        select(-fold, -row_ids) %>% 
        rename(fold = fold_unique, row_ids=row_ids_unique)
    }else{
      data_re <- resample_data(data)
    }
    
    if(cv){
      results <- calc_auc_delta_cv(data_re, aggr="mean")
    }else{
      results <- calc_auc_delta(data_re)
    }
    
    results
  }) %>% 
    do.call(rbind, .) %>% 
    as.data.frame() %>% 
    return()
}





# Execution -----------------------------------------------------------------------------------

nboot <- 5000

set.seed(123)
resampled_simple <- calc_auc_delta_resample(data_eval_ncv_2, nboot=nboot, cv=FALSE, nested=FALSE)
resampled_nested <- calc_auc_delta_resample(data_eval_ncv_2, nboot=nboot, cv=FALSE, nested=TRUE)

set.seed(123)
resampled_cv_simple <- calc_auc_delta_resample(data_eval_ncv_2, nboot=nboot, cv=TRUE, nested=FALSE)
resampled_cv_nested <- calc_auc_delta_resample(data_eval_ncv_2, nboot=nboot, cv=TRUE, nested=TRUE)



# Comparison ----------------------------------------------------------------------------------
e <- calc_auc_delta(data)
ec <- calc_auc_delta_cv(data)

qs <- quantile(resampled_simple$delta, c(0.025, 0.975))
qn <- quantile(resampled_nested$delta, c(0.025, 0.975))

qcs <- quantile(resampled_cv_simple$delta, c(0.025, 0.975))
qcn <- quantile(resampled_cv_nested$delta, c(0.025, 0.975))


qdf <- data.frame(factor(c("naive_simple", "naive_nested",
                           "folds_simple", "folds_nested")),
                  do.call(rbind, list(qs, qn, qcs, qcn)))
names(qdf) <- c("method", "q025", "q975") 


qdf %>% 
  tidyr::pivot_longer(cols=starts_with("q"), names_to="quantile", values_to="value") %>% 
ggplot() +
  geom_point(aes(method, value, col=quantile)) 




# Save results --------------------------------------------------------------------------------

saveRDS(resampled_simple, "data/resampled_auc_delta_simple_ncv_2.rds")
saveRDS(resampled_nested, "data/resampled_auc_delta_nested_ncv_2.rds")

saveRDS(resampled_cv_simple, "data/resampled_auc_delta_cv_simple_ncv_2.rds")
saveRDS(resampled_cv_nested, "data/resampled_auc_delta_cv_nested_ncv_2.rds")


## clean environment:
rm(list = ls())