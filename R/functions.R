

is_binary <- function(x) {
  x0 <- na.omit(x) %>% as.character()
  length(unique(x0)) %in% 1:2 && all(x0 %in% 0:2)
}

is_cat <- function(x) {
  x0 <- na.omit(x) 
  (length(unique(x0)) > 2 & length(unique(x0)) < 7)
}



get_analytic_data <- function(data){
  
  
  l1_vars <- data %>% select(contains("l1_")) %>% colnames()
  
  l0_vars <-  data %>% select(contains("l0_")) %>% colnames()
  
  base_vars <- data %>% select(contains(c("w_", "l0_")))%>% colnames()
  
  
  
  
  # creating analytic data-----------------------------------------------

  
  analytic<- data %>%
    mutate(across(c(a0_teeth3,a1_teeth3) ,
                  ~factor(.x,
                          levels = c(1:3),
                          labels = c("eden",
                                     "1_19",
                                     "20_more"),ordered = T))) %>% 
    mutate(across(where(is_binary), factor)) %>%
    mutate(across(where(is_cat), factor)) %>%
    
    select(base_vars,
           a0_teeth3,
           a0_den_2c,
           c1,
           l1_vars,
           a1_teeth3,
           c2,
           y_bi= y_cognition,
           y_c= y_spm_corr)

  
}




get_mice_data <- function(df, ...){
  
  data<- df %>% 
    mutate_at(vars(contains(c("l1","a1_","y_"))), ~ifelse(c1==0 , NA,.)) %>%
    mutate(y_bi= ifelse(c2==0 ,NA,y_bi),
           y_c= ifelse(c2==0 ,NA,y_c)) 
  
  #impute all missing values (not missing due to censoring)----
  to_imp<- data %>% 
    mutate_at(vars(contains(c("l1","a1_"))),
                    ~ifelse(c1==0 , -99,.)) %>%
    mutate(y_bi= ifelse(c2==0 ,-999,y_bi),
           y_c= ifelse(c2==0 ,-999,y_c)) %>%
    mice::make.where("missing")
  
  # Use only base line vars for mice
  mice_vars<- df %>%
    select(starts_with(c("w_","l0_","a0_"))) %>% colnames()
  
  # variables that get imputed only (not contributing to mice)
  imp_only_vars<- df %>%
    select(starts_with(c("l1","a1_")), y_bi, y_c) %>% colnames()
  
  # get predictor matrix----
  allVars <- names(df)
  
  ## names of variables with missingness
  missVars <- names(df)[colSums(is.na(df)) > 0]
  
  ## mice predictorMatrix
  predictorMatrix <- matrix(0, ncol = length(allVars), nrow = length(allVars))
  rownames(predictorMatrix) <- allVars
  colnames(predictorMatrix) <- allVars
  imputerMatrix <- predictorMatrix
  imputerMatrix[,mice_vars] <- 1
  imputedMatrix <- predictorMatrix
  imputedMatrix[,unique(c(mice_vars,imp_only_vars))] <- 1
  
  ## Keep correct imputer-imputed pairs only
  predictorMatrix <- imputerMatrix * imputedMatrix
  
  ## Diagonals must be zeros (a variable cannot impute itself)
  diag(predictorMatrix) <- 0
  set.seed(19851111)
  m <- mice::mice(data = data,
                  predictorMatrix = predictorMatrix,
                  where = to_imp,
                  ...)
  imp_df <- mice::complete(m,"long")
  return(imp_df)
  
}



get_tmle_data <- function(imp_data){
  
  cat_cols <- imp_data %>%
    select(-contains(c("teeth","imp"))) %>%
    select_if(is_cat) %>%
    colnames()
  
  binary <- imp_data %>%
    select_if(is_binary) %>%
    colnames()
  
  
tmle_data<- imp_data %>%
  mutate(across(contains("teeth"), 
                ~factor(.x,labels = 1:3,ordered = T))) %>% 
  mutate(across(binary, as.numeric)) %>%  
  mutate(across(binary, function(x) x-1)) %>% 
  
  dummy_cols(select_columns = cat_cols,
             remove_selected_columns = T,
             remove_first_dummy = T,
             ignore_na = T ) %>% 
  mutate(across(where(is.integer), as.numeric)) 

tmle_data %>% select(-l0_cognition ,-a0_den_2c )
  
}


get_tmle_data_den <- function(imp_data){
  

  cat_cols <- imp_data %>%
    select(-contains(c("teeth","imp","a0_den_2c"))) %>%
    select_if(is_cat) %>%
    colnames()


  tmle_data2<- imp_data %>%
    mutate_all(as.numeric) %>%
    mutate(across(where(is_binary), function(x) x-1)) %>%
    mutate(across(where(is_cat),factor)) %>%
    mutate(y_c= as.numeric(y_c)) %>%

    dummy_cols(select_columns = cat_cols,
               remove_selected_columns = T,
               remove_first_dummy = T,
               ignore_na = T ) %>%
    mutate(across(where(is_binary), as.numeric))

}



run_lmtp <- function(data,
                     shift=NULL,
                     svy=FALSE,
                     wt_only=FALSE,
                     wt_var="",
                     ...){

  if (svy==TRUE){

    svy <- survey::svydesign(~psu, weights = data[[wt_var]], data = data)
    wt <- svy$prob
    psu <- svy$psu

    progressr::with_progress(
      m<-lmtp::lmtp_tmle(...,
                         data=data,
                         shift=shift,
                         weights = wt,
                         id = psu
      ))
  }

  else if (wt_only==TRUE){

    svy <- survey::svydesign(~1, weights = data[[wt_var]], data = data)
    wt <- svy$prob

    progressr::with_progress(
      m<-lmtp::lmtp_tmle(...,
                         data=data,
                         shift=shift,
                         weights = wt
      ))

  }

  else {

    progressr::with_progress(
      m<-lmtp::lmtp_tmle(...,
                         data=data,
                         shift=shift))

  }

  return(m)
}




pool_estimates <- function(df,mi=5){

  # from https://rdrr.io/cran/mice/src/R/barnard.rubin.R
  # barnard.rubin <- function(m, b, t, dfcom = Inf) {
  #   lambda <- (1 + 1 / m) * b / t
  #   lambda[lambda < 1e-04] <- 1e-04
  #   dfold <- (m - 1) / lambda^2
  #   dfobs <- (dfcom + 1) / (dfcom + 3) * dfcom * (1 - lambda)
  #   ifelse(is.infinite(dfcom), dfold, dfold * dfobs / (dfold + dfobs))
  # }

  df %>%
    group_by(contrast) %>%
    dplyr::mutate(variance= std.error^2,
                  p.z = qnorm(p.value)) %>%
    dplyr::summarise(
      p.z.mean= mean(p.z),
      p.combined= median(p.value),
      # p.den= sqrt(1 + var(p.z)),
      # p.combined= pnorm( p.z.mean / p.den),
      theta.combined = mean(theta),
      Vw = sum(variance)/mi, # Within imputation variance
      Vb = sum((theta - mean(theta))^2)/(mi-1), # Between imputation variance
      Vt = Vw + Vb/mi, # Total variance
      SE.combined = sqrt(Vt),
      vm = (mi-1)*(1 + (Vw/((1+1/mi)*Vb)))^2, #df correction
      conf.low = theta.combined - qt(0.975,vm)*SE.combined,
      conf.high = theta.combined + qt(0.975,vm)*SE.combined) %>% 
    dplyr::select(contrast, theta= theta.combined,conf.low,
                  conf.high, p.value= p.combined) %>%
    mutate(p.value= format.pval(p.value, digits = 3, eps = 0.001)) %>% 
    ungroup()
}




run_lmtp_imp_data <- function(data, m, d, params){
  
  data %>%
    filter(.imp== m) %>%
    
    purrr::lift(run_lmtp)(data=.,params, shift= eval(as.symbol(d)))
  
  
}




round_uc <- function(data){
  data %>%
    mutate_at(vars(theta,conf.low, conf.high), ~format(round(.,2),nsmall= 2)) %>%
    mutate(
      `P value`= p.value,
           est_ci = glue::glue("{theta} [{conf.low},{conf.high}]")) %>%
    dplyr::select(contrast,
                  `Additive TE [95% CI]`= est_ci,
                  `P value`)
}




pool_marginal <- function(res,mi=20){
  
  res %>% 
    pivot_longer(d0:d4,names_to = "d") %>% 
    mutate(
      est= map_dbl(.x=value, ~.x$theta),
      se= map_dbl(.x=value, ~.x$standard_error)
    ) %>% 
    mutate(variance=se^2) %>%
    group_by(d) %>% 
    summarise(
      theta.combined = mean(est),
      Vw = sum(variance)/mi, # Within imputation variance
      Vb = sum((est - mean(est))^2)/(mi-1), # Between imputation variance
      Vt = Vw + Vb/mi, # Total variance
      SE.combined = sqrt(Vt),
      vm = (mi-1)*(1 + (Vw/((1+1/mi)*Vb)))^2, #df correction
      conf.low = theta.combined - qt(0.975,vm)*SE.combined,
      conf.high = theta.combined + qt(0.975,vm)*SE.combined) %>% 
    dplyr::select(d, tmle_estimate= theta.combined,conf.low,
                  conf.high) 
  
}