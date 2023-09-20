###
#   RWD CTG example: data pre-processing
###
#   Author:
#   Max Westphal (max.westphal@mevis.fraunhofer.de)
###
#   Description:
#   This script preprocesses the data used for the "support" example.
#   The result will be a simplified, i.e. binary classification task.
#   Original data/credit: https://hbiostat.org/data/.
### 
#   Documentation:
#   https://hbiostat.org/data/repo/supportdesc
###
#   Reference: Knaus WA, Harrell FE, Lynn J et al. (1995): The SUPPORT prognostic model: 
#   Objective estimates of survival for seriously ill hospitalized adults. 
#   Annals of Internal Medicine 122:191-203.
###
#   Variables:
#   https://hbiostat.org/data/repo/csupport2
###
#   Remark:
#   This needs only be executed for a reproduction of (pre-computation) results.
###

# TODO: first col = id, remove directly
# TODO: data vs. objects?!?


library(dplyr)


## load original data:
data <- readr::read_csv("data_raw/support2.csv")

## From documentation:
# "To develop models without using findings from previous models, be sure not to use aps, sps, surv2m,
# surv6m as predictors. You also will probably not want to use prg2m, prg6m, dnr, dnrday."


## exclusion of features based on description:
data_support <- data %>%
  select(-id, -aps, -sps, -surv2m, -surv6m, -prg2m, -prg6m, -dnr, -dnrday) %>% 
  select(-slos, avtisst, -sfdm2)

head(data_support)

## for survival task:
# survival targets: death (binary) / d.time (time)
data_support_surv <- data_support %>% 
  select(-totcst, -totmcst, -hospdead) 

## for regression task:
# outcome: totcst
data_support_regr <- data_support %>% 
  select(-death, -d.time, -hospdead, -totmcst)

## Serialize:
saveRDS(data_support_surv, "data/data_support_surv.rds")
saveRDS(data_support_regr, "data/data_support_regr.rds")

## clean environment:
rm(list = ls())
