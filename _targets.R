
library(targets)
#library(upulR) # personal R package for creating Table-1

library(future)
library(tidyverse)
#install.packages("xgboost")
library(xgboost)
# Define custom functions and other global objects -----------------------------
source("R/functions.R")


# shift functions-------------------------------------

#1st scenario (observed)
d0 <- NULL

#2nd scenario (move eden to 1-19)
d1 <- function(data,trt){
  
  out <- list()
  
  a <- data[[trt]]
  
  for (i in 1:length(a)) {
    if (as.character(a[i]) %in% c("2", "3")) {
      out[[i]] <- as.character(a[i])
    } else {
      out[[i]] <- as.numeric(as.character(a[i])) + 1
    }
  }
  factor(unlist(out), levels = 1:3, ordered = TRUE)
  
}

#3rd scenario (move 1-19 to >20)
d2 <- function(data,trt){
  
  out <- list()
  
  a <- data[[trt]]
  
  for (i in 1:length(a)) {
    if (as.character(a[i]) %in% c("1", "3")) {
      out[[i]] <- as.character(a[i])
    } else {
      out[[i]] <- as.numeric(as.character(a[i])) + 1
    }
  }
  factor(unlist(out), levels = 1:3, ordered = TRUE)
  
}

#3rd scenario (move eden to 1-19, 1-19 to >20)

d3 <- function(data,trt){
  
  out <- list()
  
  a <- data[[trt]]
  
  for (i in 1:length(a)) {
    if (as.character(a[i]) %in% c( "3")) {
      out[[i]] <- as.character(a[i])
    } else {
      out[[i]] <- as.numeric(as.character(a[i])) + 1
    }
  }
  factor(unlist(out), levels = 1:3, ordered = TRUE)
  
}

#4th scenario (move 0, 1-19 to >20)
d4 <- function(data,trt){
  
  out <- list()
  
  a <- data[[trt]]
  
  for (i in 1:length(a)) {
    if (as.character(a[i]) %in% c( "3")) {
      out[[i]] <- as.character(a[i])
    }
    else if (as.character(a[i]) %in% c( "2")) {
      out[[i]] <- as.numeric(as.character(a[i])) + 1
    } else {
      out[[i]] <- as.numeric(as.character(a[i])) + 2
    }
  }
  factor(unlist(out), levels = 1:3,  ordered = TRUE)
  
  
}

#5th scenario (move to 0)
d5 <- function(data,trt){
  
  out <- list()
  
  a <- data[[trt]]
  
  for (i in 1:length(a)) {
    if (as.character(a[i]) %in% c( "1")) {
      out[[i]] <- as.character(a[i])
    }
    else if (as.character(a[i]) %in% c( "2")) {
      out[[i]] <- as.numeric(as.character(a[i])) - 1
    } else {
      out[[i]] <- as.numeric(as.character(a[i])) - 2
    }
  }
  factor(unlist(out), levels = 1:3,  ordered = TRUE)
  
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
             "data/stata_df2_0913.csv",
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

  
  tar_target(imp_data, get_mice_data(analytic_data,m=20,method="rf"))
 
 ,
  
  tar_target(tmle_data, get_tmle_data(imp_data) )
 
 
 ,

  tar_target(w0, tmle_data %>% select(contains("w_")) %>% 
               # select(-w_spm_corr) %>% 
               colnames()),
  tar_target(l0, tmle_data %>% select(contains("l0")) %>% colnames()),
  tar_target(l1, tmle_data %>% select(contains("l1")) %>% colnames()),
  tar_target(tv, list(l0, l1))

  ,

  tar_target( params, list(trt= expo,
                             outcome="y_c",
                             baseline = w0,
                             intervention_type = "mtp",
                             cens=cens,
                             time_vary=tv,
                             outcome_type = "continuous"))

  ,

  tar_target(sl_lib,

             c("SL.xgboost","SL.nnet","SL.gam"))

 
  ,

   tar_target( params_sl,

               list(trt= expo,
                    outcome="y_c",
                    baseline = w0,
                    intervention_type = "mtp",
                    cens=cens,
                    time_vary=tv,
                    outcome_type = "continuous",
                    learners_outcome = sl_lib,
                    learners_trt = sl_lib))
   ,



  # Run TMLE --------------------------------------------------

  tar_target(m ,
             c(1:20))  # range of imputed set

  ,

  tar_target(imps,
            cbind(imp=m) %>% as.data.frame())

  ,


# without sl estimates-------------------------

  tar_target(tmle_d0,
             imps %>%
               mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_data ,
                                                           m= .x,
                                                           d= "d0",
                                                           params = params))))
  ,

  tar_target(tmle_d1,
             imps %>%
               mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_data ,
                                                           m= .x,
                                                           d= "d1",
                                                           params = params))))
  ,

  tar_target(tmle_d2,
             imps %>%
               mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_data ,
                                                           m= .x,
                                                           d= "d2",
                                                           params = params))))
  ,

  tar_target(tmle_d3,
             imps %>%
               mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_data ,
                                                           m= .x,
                                                           d= "d3",
                                                           params = params))))
  ,

  tar_target(tmle_d4,
             imps %>%
               mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_data ,
                                                           m= .x,
                                                           d= "d4",
                                                           params = params))))
,

# with sl estimates-------------------------

tar_target(tmle_d0_sl,
           imps %>%
             mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_data ,
                                                         m= .x,
                                                         d= "d0",
                                                         params = params_sl))))
,

tar_target(tmle_d1_sl,
           imps %>%
             mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_data ,
                                                         m= .x,
                                                         d= "d1",
                                                         params = params_sl))))

,

tar_target(tmle_d2_sl,
           imps %>%
             mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_data ,
                                                         m= .x,
                                                         d= "d2",
                                                         params = params_sl))))
,

tar_target(tmle_d3_sl,
           imps %>%
             mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_data ,
                                                         m= .x,
                                                         d= "d3",
                                                         params = params_sl))))
,

tar_target(tmle_d4_sl,
           imps %>%
             mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_data ,
                                                         m= .x,
                                                         d= "d4",
                                                         params = params_sl))))
,


tar_target(res_nosl,
           cbind(imp=c(1:20),
                 d0=tmle_d0$tmle,
                 d1=tmle_d1$tmle,
                 d2=tmle_d2$tmle,
                 d3=tmle_d3$tmle,
                 d4=tmle_d4$tmle) %>% 
  as_tibble() %>%
  unnest(imp) )
,

tar_target(res_sl,
           cbind(imp=c(1:20),
                 d0=tmle_d0_sl$tmle,
                 d1=tmle_d1_sl$tmle,
                 d2=tmle_d2_sl$tmle,
                 d3=tmle_d3_sl$tmle,
                 d4=tmle_d4_sl$tmle) %>%
  as_tibble() %>%
  unnest(imp) )
,

tar_target(res_contrast_sl,
           res_sl %>%
           mutate(
             d0_vs_d1= map2(.x=d0, .y=d1, ~lmtp::lmtp_contrast(.y,ref = .x, type = "additive")),
             d0_vs_d2= map2(.x=d0, .y=d2, ~lmtp::lmtp_contrast(.y,ref = .x, type = "additive")),
             d0_vs_d3= map2(.x=d0, .y=d3, ~lmtp::lmtp_contrast(.y,ref = .x, type = "additive")),
             d0_vs_d4= map2(.x=d0, .y=d4, ~lmtp::lmtp_contrast(.y,ref = .x, type = "additive"))

                      ) %>%

                      select(imp, contains("vs")) %>%
                      pivot_longer(!imp, names_to = "contrast", values_to = "results") %>%
                      mutate(results=map(results, ~.$vals)) %>%
                      unnest(cols = results) %>%
                      pool_estimates(m=20)%>%
                      round_uc())

,

tar_target(res_marginal_sl,
           res_sl %>%
           pool_marginal() )

 ,

tar_target(res_contrast_nosl, 
           res_nosl %>% 
           mutate(
             d0_vs_d1= map2(.x=d0, .y=d1, ~lmtp::lmtp_contrast(.y,ref = .x, type = "additive")),
             d0_vs_d2= map2(.x=d0, .y=d2, ~lmtp::lmtp_contrast(.y,ref = .x, type = "additive")),
             d0_vs_d3= map2(.x=d0, .y=d3, ~lmtp::lmtp_contrast(.y,ref = .x, type = "additive")),
             d0_vs_d4= map2(.x=d0, .y=d4, ~lmtp::lmtp_contrast(.y,ref = .x, type = "additive"))

                      ) %>%

                      select(imp, contains("vs")) %>%
                      pivot_longer(!imp, names_to = "contrast", values_to = "results") %>%
                      mutate(results=map(results, ~.$vals)) %>%
                      unnest(cols = results) %>%
                      pool_estimates(m=20)%>%
                      round_uc())

,

tar_target(res_marginal_nosl,
           res_nosl %>%
           pool_marginal() )



)

