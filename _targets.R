library(targets)
library(tarchetypes)
#library(upulR) # personal R package for creating Table-1

library(tidyverse)
#install.packages("xgboost")

# Define custom functions and other global objects -----------------------------
source("R/functions.R")


# shift functions-------------------------------------

#1st scenario (observed)
d0 <- NULL

# What if edentulous people retained 1-4 teeth

d1 <- function(data,trt){

  out <- list()

  a <- data[[trt]]

  for (i in 1:length(a)) {
    if (as.character(a[i]) %in% "1") {
      out[[i]] <- 2 # 2= 1-4 teeth category
    } else {
      out[[i]] <- as.numeric(a[i])
    }
  }
  factor(unlist(out), levels = 1:5, ordered = TRUE)

}



# What if edentulousness and 1-4 teeth was was 5-9

d2 <- function(data,trt){

  out <- list()

  a <- data[[trt]]

  for (i in 1:length(a)) {
    if (as.character(a[i]) %in% c("1","2")) {
      out[[i]] <- 3 # 3= 5-9 teeth category
    } else {
      out[[i]] <- as.numeric(a[i])
    }
  }
  factor(unlist(out), levels = 1:5, ordered = TRUE)

}


# What if edentulousness, 1-4 teeth, and 5-9 teeth was 10-19

d3 <- function(data,trt){

  out <- list()

  a <- data[[trt]]

  for (i in 1:length(a)) {
    if (as.character(a[i]) %in% c("1","2","3")) {
      out[[i]] <- 4 # 4= 10-19 teeth category
    } else {
      out[[i]] <- as.numeric(a[i])
    }
  }
  factor(unlist(out), levels = 1:5, ordered = TRUE)

}


# What if everyone retained at least minimal functional dentition

d4 <- function(data,trt){

  out <- list()

  a <- data[[trt]]

  for (i in 1:length(a)) {

      out[[i]] <- 5 # 5= >20 teeth category

  }
  factor(unlist(out), levels = 1:5, ordered = TRUE)

}



expo <- c("a0_teeth3","a1_teeth3")
cens <- c("c1","c2")


# Set target-specific options such as packages-----------------------------------
tar_option_set(packages = c("tidyverse", "haven",
                            "lmtp","mice", "janitor",
                            "fastDummies"))

# Starting the list of targets--------------------------------------------------
list(
  tar_target(df_file,
             "~/Desktop/stata_df2_0207.csv",
             format = "file")
  ,
  # Working data -------------------------------------------------------------
  tar_target(working_df,
             read_csv(file=df_file))
  ,


  # create a dataset for descriptive analysis---------------------------------


  tar_target(analytic_data,
             get_analytic_data(working_df))

 ,

  #perform imputation-----------------------------------------------------


  tar_target(imp_data, get_mice_data(analytic_data,m=10,method="rf"))

 ,

 tar_target(tmle_data, get_tmle_data(imp_data) )


 ,

 tar_target(w0, tmle_data %>% select(contains("w_")) %>%
              # select(-w_spm_corr) %>%
              colnames()),
 tar_target(l0, tmle_data %>% select(contains("l0")) %>% colnames()),
 tar_target(l1, tmle_data %>% select(contains("l1")) %>% colnames()),
 tar_target(tv, list(l0, l1)),

 tar_target(tv1,
            lapply(tv, function(x) setdiff(x,c('l0_spm_corr','l1_spm_corr')))
            ), # without outcome adjustment

 tar_target( params, list(trt= expo,
                          outcome="y_c",
                          baseline = w0,
                          outcome_type = "continuous",
                          cens=cens,
                          time_vary=tv))

  ,
 tar_target( params1, list(trt= expo,
                          outcome="y_c",  # without outcome adjustment
                          baseline = w0,
                          outcome_type = "continuous",
                          cens=cens,
                          time_vary=tv1))

  ,

  tar_target(sl_lib,

             c("SL.xgboost","SL.nnet","SL.gam"))


  ,

   tar_target( params_sl,

               list(trt= expo,
                    outcome="y_c",
                    baseline = w0,
                    cens=cens,
                    time_vary=tv,
                    outcome_type = "continuous",
                    learners_outcome = sl_lib,
                    learners_trt = sl_lib,
                    folds=5))
   ,

   tar_target( params_sl1,

               list(trt= expo,
                    outcome="y_c",
                    baseline = w0,
                    cens=cens,
                    time_vary=tv1,  # without outcome adjustment
                    outcome_type = "continuous",
                    learners_outcome = sl_lib,
                    learners_trt = sl_lib,
                    folds=5))
   ,



  # Run TMLE --------------------------------------------------


 tar_group_by(grouped_data,
              tmle_data,
              .imp
 )
 ,

 tar_target(branched_data,
            grouped_data,
            map(grouped_data))
 ,

 tar_target(tmle_d0,
            tibble(
              imp=branched_data$.imp %>% unique(),
              shift="d0",
              mod= rlang::exec(lmtp_tmle,
                               rlang::splice(params),
                               data=branched_data,
                               shift=d0) %>% list()
            ),
            map(branched_data)
 )
 ,

 tar_target(tmle_d1,
            tibble(
              imp=branched_data$.imp %>% unique(),
              shift="d1",
              mod= rlang::exec(lmtp_tmle,
                               rlang::splice(params),
                               data=branched_data,
                               shift=d1,
                               intervention_type = "dynamic") %>% list()
            ),
            map(branched_data)
 )
 ,

 tar_target(tmle_d2,
            tibble(
              imp=branched_data$.imp %>% unique(),
              shift="d2",
              mod= rlang::exec(lmtp_tmle,
                               rlang::splice(params),
                               data=branched_data,
                               shift=d2,
                               intervention_type = "dynamic") %>% list()
            ),
            map(branched_data)
 )
 ,

 tar_target(tmle_d3,
            tibble(
              imp=branched_data$.imp %>% unique(),
              shift="d3",
              mod= rlang::exec(lmtp_tmle,
                               rlang::splice(params),
                               data=branched_data,
                               shift=d3,
                               intervention_type = "dynamic") %>% list()
            ),
            map(branched_data)
 )

 ,

 tar_target(tmle_d4,
            tibble(
              imp=branched_data$.imp %>% unique(),
              shift="d4",
              mod= rlang::exec(lmtp_tmle,
                               rlang::splice(params),
                               data=branched_data,
                               shift=d4,
                               intervention_type = "static") %>% list()
            ),
            map(branched_data)
 )
 ,

 tar_target(tmle_d0_1,
            tibble(
              imp=branched_data$.imp %>% unique(),
              shift="d0",
              mod= rlang::exec(lmtp_tmle,
                               rlang::splice(params1),
                               data=branched_data,
                               shift=d0) %>% list()
            ),
            map(branched_data)
 )
 ,

 tar_target(tmle_d1_1,
            tibble(
              imp=branched_data$.imp %>% unique(),
              shift="d1",
              mod= rlang::exec(lmtp_tmle,
                               rlang::splice(params1),
                               data=branched_data,
                               shift=d1,
                               intervention_type = "dynamic") %>% list()
            ),
            map(branched_data)
 )
 ,

 tar_target(tmle_d2_1,
            tibble(
              imp=branched_data$.imp %>% unique(),
              shift="d2",
              mod= rlang::exec(lmtp_tmle,
                               rlang::splice(params1),
                               data=branched_data,
                               shift=d2,
                               intervention_type = "dynamic") %>% list()
            ),
            map(branched_data)
 )
 ,

 tar_target(tmle_d3_1,
            tibble(
              imp=branched_data$.imp %>% unique(),
              shift="d3",
              mod= rlang::exec(lmtp_tmle,
                               rlang::splice(params1),
                               data=branched_data,
                               shift=d3,
                               intervention_type = "dynamic") %>% list()
            ),
            map(branched_data)
 )

 ,

 tar_target(tmle_d4_1,
            tibble(
              imp=branched_data$.imp %>% unique(),
              shift="d4",
              mod= rlang::exec(lmtp_tmle,
                               rlang::splice(params1),
                               data=branched_data,
                               shift=d4,
                               intervention_type = "static") %>% list()
            ),
            map(branched_data)
 )

 ,
 tar_target(tmle_d0_sl,
            tibble(
              imp=branched_data$.imp %>% unique(),
              shift="d0",
              mod= rlang::exec(lmtp_tmle,
                               rlang::splice(params_sl),
                               data=branched_data,
                               shift=d0) %>% list()
            ),
            map(branched_data)
 )
 ,

 tar_target(tmle_d1_sl,
            tibble(
              imp=branched_data$.imp %>% unique(),
              shift="d1",
              mod= rlang::exec(lmtp_tmle,
                               rlang::splice(params_sl),
                               data=branched_data,
                               shift=d1,
                               intervention_type = "dynamic") %>% list()
            ),
            map(branched_data)
 )
 ,

 tar_target(tmle_d2_sl,
            tibble(
              imp=branched_data$.imp %>% unique(),
              shift="d2",
              mod= rlang::exec(lmtp_tmle,
                               rlang::splice(params_sl),
                               data=branched_data,
                               shift=d2,
                               intervention_type = "dynamic") %>% list()
            ),
            map(branched_data)
 )
 ,

 tar_target(tmle_d3_sl,
            tibble(
              imp=branched_data$.imp %>% unique(),
              shift="d3",
              mod= rlang::exec(lmtp_tmle,
                               rlang::splice(params_sl),
                               data=branched_data,
                               shift=d3,
                               intervention_type = "dynamic") %>% list()
            ),
            map(branched_data)
 )

 ,

 tar_target(tmle_d4_sl,
            tibble(
              imp=branched_data$.imp %>% unique(),
              shift="d4",
              mod= rlang::exec(lmtp_tmle,
                               rlang::splice(params_sl),
                               data=branched_data,
                               shift=d4,
                               intervention_type = "static") %>% list()
            ),
            map(branched_data)
 )

 ,

 tar_target(tmle_d0_sl1,
            tibble(
              imp=branched_data$.imp %>% unique(),
              shift="d0",
              mod= rlang::exec(lmtp_tmle,
                               rlang::splice(params_sl1),
                               data=branched_data,
                               shift=d0,
                               intervention_type = "static") %>% list()
            ),
            map(branched_data)
 )

 ,

 tar_target(tmle_d1_sl1,
            tibble(
              imp=branched_data$.imp %>% unique(),
              shift="d1",
              mod= rlang::exec(lmtp_tmle,
                               rlang::splice(params_sl1),
                               data=branched_data,
                               shift=d1,
                               intervention_type = "static") %>% list()
            ),
            map(branched_data)
 )

 ,

 tar_target(tmle_d2_sl1,
            tibble(
              imp=branched_data$.imp %>% unique(),
              shift="d2",
              mod= rlang::exec(lmtp_tmle,
                               rlang::splice(params_sl1),
                               data=branched_data,
                               shift=d2,
                               intervention_type = "static") %>% list()
            ),
            map(branched_data)
 )

 ,

 tar_target(tmle_d3_sl1,
            tibble(
              imp=branched_data$.imp %>% unique(),
              shift="d3",
              mod= rlang::exec(lmtp_tmle,
                               rlang::splice(params_sl1),
                               data=branched_data,
                               shift=d3,
                               intervention_type = "static") %>% list()
            ),
            map(branched_data)
 )

 ,

 tar_target(tmle_d4_sl1,
            tibble(
              imp=branched_data$.imp %>% unique(),
              shift="d4",
              mod= rlang::exec(lmtp_tmle,
                               rlang::splice(params_sl1),
                               data=branched_data,
                               shift=d4,
                               intervention_type = "static") %>% list()
            ),
            map(branched_data)
 )

 ,



 tar_target(res_all_nosl,

            bind_rows(!!!syms(paste0("tmle_d",0:4)))
            )
,

 tar_target(res_all_nosl1,

            bind_rows(!!!syms(paste0("tmle_d",0:4,"_1")))
            )
,

 tar_target(res_all_sl,

            bind_rows(!!!syms(paste0("tmle_d",0:4,"_sl")))
            )
,

 tar_target(res_all_sl1,

            bind_rows(!!!syms(paste0("tmle_d",0:4,"_sl1")))
            )
,


tar_target(res_nosl,

           res_all_nosl %>%
             pivot_wider(names_from = shift,
                         values_from = mod)
           %>%

             mutate(
               d0_vs_d1= map2(.x=d0,.y=d1, ~lmtp_contrast(.y, ref = .x,type = "additive")$vals),
               d0_vs_d2= map2(.x=d0,.y=d2, ~lmtp_contrast(.y, ref = .x,type = "additive")$vals),
               d0_vs_d3= map2(.x=d0,.y=d3, ~lmtp_contrast(.y, ref = .x,type = "additive")$vals),
               d0_vs_d4= map2(.x=d0,.y=d4, ~lmtp_contrast(.y, ref = .x,type = "additive")$vals)) %>%
             select(imp,d0_vs_d1, d0_vs_d2, d0_vs_d3,d0_vs_d4) %>%
             pivot_longer(cols = -imp,names_to = "contrast") %>%
             unnest(value) %>%
             pool_estimates(mi=10)

)
,

tar_target(res_nosl1,

           res_all_nosl1 %>%
             pivot_wider(names_from = shift,
                         values_from = mod)
           %>%

             mutate(
               d0_vs_d1= map2(.x=d0,.y=d1, ~lmtp_contrast(.y, ref = .x,type = "additive")$vals),
               d0_vs_d2= map2(.x=d0,.y=d2, ~lmtp_contrast(.y, ref = .x,type = "additive")$vals),
               d0_vs_d3= map2(.x=d0,.y=d3, ~lmtp_contrast(.y, ref = .x,type = "additive")$vals),
               d0_vs_d4= map2(.x=d0,.y=d4, ~lmtp_contrast(.y, ref = .x,type = "additive")$vals)) %>%
             select(imp,d0_vs_d1, d0_vs_d2, d0_vs_d3,d0_vs_d4) %>%
             pivot_longer(cols = -imp,names_to = "contrast") %>%
             unnest(value) %>%
             pool_estimates(mi=10)

)
,

tar_target(res_sl,

           res_all_sl %>%
             pivot_wider(names_from = shift,
                         values_from = mod)
           %>%

             mutate(
               d0_vs_d1= map2(.x=d0,.y=d1, ~lmtp_contrast(.y, ref = .x,type = "additive")$vals),
               d0_vs_d2= map2(.x=d0,.y=d2, ~lmtp_contrast(.y, ref = .x,type = "additive")$vals),
               d0_vs_d3= map2(.x=d0,.y=d3, ~lmtp_contrast(.y, ref = .x,type = "additive")$vals),
               d0_vs_d4= map2(.x=d0,.y=d4, ~lmtp_contrast(.y, ref = .x,type = "additive")$vals)) %>%
             select(imp,d0_vs_d1, d0_vs_d2, d0_vs_d3,d0_vs_d4) %>%
             pivot_longer(cols = -imp,names_to = "contrast") %>%
             unnest(value) %>%
             pool_estimates(mi=10)

)
,

tar_target(res_sl1,

           res_all_sl1 %>%
             pivot_wider(names_from = shift,
                         values_from = mod)
           %>%

             mutate(
               d0_vs_d1= map2(.x=d0,.y=d1, ~lmtp_contrast(.y, ref = .x,type = "additive")$vals),
               d0_vs_d2= map2(.x=d0,.y=d2, ~lmtp_contrast(.y, ref = .x,type = "additive")$vals),
               d0_vs_d3= map2(.x=d0,.y=d3, ~lmtp_contrast(.y, ref = .x,type = "additive")$vals),
               d0_vs_d4= map2(.x=d0,.y=d4, ~lmtp_contrast(.y, ref = .x,type = "additive")$vals)) %>%
             select(imp,d0_vs_d1, d0_vs_d2, d0_vs_d3,d0_vs_d4) %>%
             pivot_longer(cols = -imp,names_to = "contrast") %>%
             unnest(value) %>%
             pool_estimates(mi=10)

)


)

