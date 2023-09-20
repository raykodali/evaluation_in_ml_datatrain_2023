###
#   RWD CTG example: model development
###
#   Author:
#   Max Westphal (max.westphal@mevis.fraunhofer.de)
###
#   Description:
#   This script covers the model development.
###
#   Remark:
#   This needs only be executed for a reproduction of (pre-computation) results.
###

# Preparation ---------------------------------------------------------------------------------
library(dplyr)
library(mlr3verse)

rm(list = ls())

set.seed(123)

# Data import ---------------------------------------------------------------------------------

data_ctg <- readRDS("data/data_ctg.rds")



# Task definition -----------------------------------------------------------------------------

task_ctg <- as_task_classif(data_ctg %>% select(-Year),
                            target = "status",
                            positive = "suspect",
                            id = "task_ctg")

task_ctg$set_col_roles("status", add_to = "stratum")

task_ctg

# Data description ----------------------------------------------------------------------------


dim(data_ctg)
head(data_ctg)

summary(data_ctg$status)
summary(data_ctg$Year)


# Settings ------------------------------------------------------------------------------------

n_hp <- 50 # hyperparams per learning algorithm


# Design definitions --------------------------------------------------------------------------


## train-tune-test:
# -> https://mlr3.mlr-org.com/reference/partition.html

set.seed(123)
r <- rsmp("cv", folds = 10)$instantiate(task_ctg)

split_ttt <- list(
  train = filter(r$instance, fold %in% 1:6) %>% "[["("row_id"),
  tune = filter(r$instance, fold %in% 7:8) %>% "[["("row_id"),
  test = filter(r$instance, fold %in% 9:10) %>% "[["("row_id")
)

sapply(split_ttt, length)


train_sets_ttt <- list(split_ttt$train, # tuning
                       c(split_ttt$train, split_ttt$tune)) # evaluation

test_sets_ttt <- list(split_ttt$tune, # tuning
                      split_ttt$test) # evaluation


design_ttt <- rsmp("custom")
design_ttt$instantiate(task_ctg,
                       train_sets = train_sets_ttt,
                       test_sets = test_sets_ttt)
design_ttt$instance %>% str()


## nested cv (outer: 5fold CV, inner: 75/25 holdout):
# https://mlr.mlr-org.com/articles/tutorial/nested_resampling.html
designs_ncv <- list(
  outer = rsmp("cv", folds=5),
  inner = rsmp("holdout", ratio=0.75)
) 


## temporal transferability:
train_sets_tmp <- list(
  which(data_ctg$Year == "1995"),
  which(data_ctg$Year == "1996"),
  which(data_ctg$Year == "1997")
)
test_sets_tmp <- list(
  which(data_ctg$Year == "1996"),
  which(data_ctg$Year == "1997"),
  which(data_ctg$Year == "1998")
) 

design_tmp <- rsmp("custom")
design_tmp$instantiate(task_ctg,
                        train_sets = train_sets_tmp,
                        test_sets = test_sets_tmp)
design_tmp$instance %>% str()





# Algorithms & hyperparameters setup ----------------------------------------------------------------

## Prep multisession:
future::availableCores()
n_threads <- round(0.8*future::availableCores())
future::plan("multisession")



## Prep learner glmnet:
ts_glmnet <- mlr3tuningspaces::lts("classif.glmnet.default")
learner_glmnet <- ts_glmnet$get_learner()
learner_glmnet$predict_type <- "prob"
set_threads(learner_glmnet, n = n_threads)
learner_glmnet

set.seed(123)
space_glmnet <- learner_glmnet$param_set$search_space()
hp_glmnet <- generate_design_lhs(space_glmnet, n_hp)
hp_glmnet


## Prep learner ranger:
ts_ranger <- mlr3tuningspaces::lts("classif.ranger.default")
learner_ranger <- ts_ranger$get_learner()
learner_ranger$predict_type <- "prob"
set_threads(learner_ranger, n = n_threads)
learner_ranger

set.seed(123)
space_ranger <- learner_ranger$param_set$search_space()
hp_ranger <- generate_design_lhs(space_ranger, n_hp)
hp_ranger




save_rds <- function(object){
  saveRDS(eval(parse(text=object)), file.path("data", paste0(object, ".rds")))
}


# Model development (train-tune-test) ---------------------------------------------------------
set.seed(123)
tuned_ttt_glmnet <-   
  mlr3tuning::tune(
    tuner = tnr("design_points", design = hp_glmnet$data, batch_size = n_threads),
    task = task_ctg,
    learner = learner_glmnet,
    resampling = design_ttt,
    measure = msr("classif.auc")
)
save_rds("tuned_ttt_glmnet")

tuned_ttt_ranger <-   
  mlr3tuning::tune(
    tuner = tnr("design_points", design = hp_ranger$data, batch_size = n_threads),
    task = task_ctg,
    learner = learner_ranger,
    resampling = design_ttt,
    measure = msr("classif.auc")
)
save_rds("tuned_ttt_ranger")


# Model development (nested cv) ---------------------------------------------------------------
tuned_ncv_glmnet <-   
  mlr3tuning::tune_nested(
    tuner = tnr("design_points", design = hp_glmnet$data, batch_size = n_threads),
    task = task_ctg,
    learner = learner_glmnet,
    outer_resampling = designs_ncv$outer,
    inner_resampling = designs_ncv$inner,
    measure = msr("classif.auc")
  )
save_rds("tuned_ncv_glmnet")

tuned_ncv_ranger <-   
  mlr3tuning::tune_nested(
    tuner = tnr("design_points", design = hp_ranger$data, batch_size = n_threads),
    task = task_ctg,
    learner = learner_ranger,
    outer_resampling = designs_ncv$outer,
    inner_resampling = designs_ncv$inner,
    measure = msr("classif.auc")
  )
save_rds("tuned_ncv_ranger")


# Model development (temporal split) ----------------------------------------------------------
tuned_tmp_glmnet <-   
  mlr3tuning::tune(
    tuner = tnr("design_points", design = hp_glmnet$data, batch_size = n_threads),
    task = task_ctg,
    learner = learner_glmnet,
    resampling = design_tmp,
    measure = msr("classif.auc")
  )
save_rds("tuned_tmp_glmnet")

tuned_tmp_ranger <-   
  mlr3tuning::tune(
    tuner = tnr("design_points", design = hp_ranger$data, batch_size = n_threads),
    task = task_ctg,
    learner = learner_ranger,
    resampling = design_tmp,
    measure = msr("classif.auc")
  )
save_rds("tuned_tmp_ranger")




# Construct evaluation data -------------------------------------------------------------------

tuned_ttt_glmnet <- readRDS("data/tuned_ttt_glmnet.rds")
tuned_ttt_ranger <- readRDS("data/tuned_ttt_ranger.rds")

tuned_ncv_glmnet <- readRDS("data/tuned_ncv_glmnet.rds")
tuned_ncv_ranger <- readRDS("data/tuned_ncv_ranger.rds")

tuned_tmp_glmnet <- readRDS("data/tuned_tmp_glmnet.rds")
tuned_tmp_ranger <- readRDS("data/tuned_tmp_ranger.rds")

## for ttt design:
construct_data_tune <- function(tuned, algo){
  
  # tuned <- tuned_ttt_glmnet; algo="glmnet"
  
  a <- tuned$archive

  idx_hp <- 1:a$n_evals
  
  lapply(idx_hp, function(i){
    data.frame(algo = algo,
               idx_hp = i, 
               uhash = a$data$uhash[i],
               a$predictions(i)[[1]] %>% as.data.table()) # [[1]] because this is the tune fold
   }) %>% 
    do.call(rbind, .) %>% 
    group_by(algo, idx_hp, uhash) %>% 
    summarize(auc_tune = pROC::auc(response= as.numeric(truth=="suspect"),
                                   predictor = prob.suspect) %>% as.numeric()) %>% 
    ungroup()
  
}


## best tuning models
data_tune_ttt_glmnet <- construct_data_tune(tuned_ttt_glmnet, "glmnet") 
data_tune_ttt_ranger <- construct_data_tune(tuned_ttt_ranger, "ranger") 


set.seed(123)
data_tune_ttt_glmnet %>%
  arrange(-auc_tune)  %>% 
  mutate(auc_rank = rank(-auc_tune, ties.method="random")) %>% 
  filter(auc_rank <= 5.5)

data_tune_ttt_ranger %>% 
  arrange(-auc_tune) %>% 
  mutate(auc_rank = rank(-auc_tune, ties.method="random")) %>% 
  filter(auc_rank <= 5)

rbind(data_tune_ttt_glmnet, data_tune_ttt_ranger) %>% 
  arrange(-auc_tune) %>% 
  mutate(auc_rank = rank(-auc_tune, ties.method="random")) %>% 
  filter(auc_rank <= 10)




data_eval_ttt_1 <- tuned_ttt_ranger$archive$predictions(15)[[2]] %>% 
  as.data.table() 
  

ttt_part1 <- tuned_ttt_glmnet$archive$predictions(20)[[2]] %>% as.data.table() 
names(ttt_part1)[3:5] <- paste0(names(ttt_part1)[3:5], "_glmnet")

ttt_part2 <- tuned_ttt_ranger$archive$predictions(15)[[2]] %>% as.data.table() 
names(ttt_part2)[3:5] <- paste0(names(ttt_part2)[3:5], "_ranger")


data_eval_ttt_2 <- left_join(ttt_part1, ttt_part2)

save_rds("data_eval_ttt_1")
save_rds("data_eval_ttt_2")


## for ncv design:
ncv_part1_list <- tuned_ncv_glmnet$predictions("test") 
ncv_part2_list <- tuned_ncv_ranger$predictions("test")

ncv_part1 <- lapply(1:5, function(i){
  cbind(fold=i, ncv_part1_list[[i]] %>% as.data.table())
}) %>% 
  do.call(rbind, .)
names(ncv_part1)[4:6] <- paste0(names(ncv_part1)[4:6], "_glmnet")


ncv_part2 <- lapply(1:5, function(i){
  cbind(fold=i, ncv_part2_list[[i]] %>% as.data.table())
}) %>% 
  do.call(rbind, .)
names(ncv_part2)[4:6] <- paste0(names(ncv_part2)[4:6], "_ranger")


data_eval_ncv_2 <- left_join(ncv_part1, 
                             ncv_part2 %>% select(-truth, -fold),
                             by="row_ids" )
save_rds("data_eval_ncv_2")



# Model development (temporal split) ----------------------------------------------------------

# sensitvity analysis for earlier (ttt) winners:
tmp_part1_list <- tuned_tmp_glmnet$archive$predictions(20) 
tmp_part2_list <- tuned_tmp_ranger$archive$predictions(15)


tmp_part1 <- lapply(1:3, function(i){
  cbind(fold=i, tmp_part1_list[[i]] %>% as.data.table())
}) %>% 
  do.call(rbind, .)
names(tmp_part1)[4:6] <- paste0(names(tmp_part1)[4:6], "_glmnet")


tmp_part2 <- lapply(1:3, function(i){
  cbind(fold=i, tmp_part2_list[[i]] %>% as.data.table())
}) %>% 
  do.call(rbind, .)
names(tmp_part2)[4:6] <- paste0(names(tmp_part2)[4:6], "_ranger")


data_eval_tmp_2 <- left_join(tmp_part1, 
                             tmp_part2 %>% select(-truth, -fold),
                             by="row_ids" )
save_rds("data_eval_tmp_2")





## clean environment:
rm(list = ls())







