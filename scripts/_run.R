###
#   Run all scripts
###
#   Author:
#   Max Westphal (max.westphal@mevis.fraunhofer.de)
###
#   Description:
#   This script runs all remaining scripts in correct order 
#   to (re)produce the contents of the 'data' folder.
###
#   Remark:
#   This needs only be executed for a reproduction of (pre-computation) results.
###

dir.create("data", showWarnings = FALSE)

source("scripts/1_rwd_ctg_data_proc")
source("scripts/2_rwd_ctg_model_dev")
source("scripts/3_nested_cv_bootstrap")

## clean environment:
rm(list = ls())