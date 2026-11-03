EQR_CR <- function(CR, uselog=F){
  
  #' wrappper function for the 'real'
  #' EQR_CR calculation to take care
  #' of vectors of CR values
  EQR <- CR %>%
    lapply(.EQR_CR_single, uselog=uselog) %>%
    unlist()
  return(EQR)
}

.EQR_CR_single <- function(CR, uselog){
  #' function to normalise Contamination Ratio (CR)
  #' value to 0.0 - 1,0 scale
  require(dplyr)
  
  # boundaries for CR and their corresponding EQR values
  bnds <- c(0.1, 0.5, 1.0, 5,  10,  50)
  eqrs <- c(1.0, 0.8, 0.6, 0.4, 0.2, 0)

  # log-transform CR and boundaries
  if(uselog){
    # it could cause confusion if we use log-linear interpolation
    # all other EQR tools use linear interpolation
    
    CR_log <- log10(CR)
    bnds_log <- log10(bnds)
  }else{
    CR_log <- CR
    bnds_log <- bnds
  }
  
  # get the number of boundaries less than CR
  i <- bnds_log[CR_log>=bnds_log] %>% length()
  
  if(i<1){
    # CR_log is less than  the lowest boundary value
    EQR <- 1
  }else if(i==length(bnds_log)){
    # CR_log is greater than  the highest boundary value
    EQR <- 0
  }else{
    bnds0 <- bnds_log[i]
    bnds1 <- bnds_log[i+1]
    eqr0 <- eqrs[i]
    eqr1 <- eqrs[i+1]
    EQR <- eqr0 + (eqr1-eqr0) * ((CR_log - bnds0)/(bnds1- bnds0))
  }
  return(EQR)
}


# ----------- tests ------------------- 

if(sys.nframe() == 0L) {
  #' this if() checks if the script 
  #' is called from source()
  #' in that case, the test code is not run
  
  # calling the function with a vector
  CR=c(0.12, 0.67, 1.1, 7.3, 34.2)
  EQR_CR(CR)
  
  # using the function in mutate() with a data frame
  df_test <- data.frame(ID=c(1,2,3,4,5), 
                        CR=c(0.12, 0.67, 1.1, 7.3, 34.2))
  
  df_test <- df_test %>%
    mutate(EQR = EQR_CR(CR)) 
  
  print(df_test)
  
}  
