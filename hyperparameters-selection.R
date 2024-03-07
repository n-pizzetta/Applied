# AIC #
find_aic_params <- function(ts_object, PQmax = 2, Imax = 1, exogenous_var=NULL){
  
  # Creation of new objects to keep track
  min_AIC <- Inf
  min_AIC_params <- NULL
  error_params <- list() # Store params that cause errors
  
  # Loop to test different parameters 
  for (ar in 0:PQmax) {
    for (ma in 0:PQmax) {
      for (AR in 0:PQmax) {
        for (MA in 0:PQmax) {
          for (i in 0:Imax) {
            for (I in 0:Imax) {
              # Try-Catch block
              tryCatch({
                # Fitting the model
                fit <- Arima(ts_object, order=c(ar,i,ma), seasonal=list(order=c(AR,I,MA), period=12), xreg=exogenous_var)
                # Check if the current AIC is the minimum
                current_AIC <- AIC(fit)
                if (current_AIC < min_AIC) {
                  min_AIC <- current_AIC # Minimum AIC value at this stage
                  min_AIC_params <- c(ar, i, ma, AR, I, MA) # Best parameters at this stage
                }
              }, error=function(e){
                # Here, instead of adjusting params, we log them
                error_params[[length(error_params) + 1]] <- list(ar=ar, ma=ma, AR=AR, MA=MA, i=i, I=I)
              })
            }
          }
        }
      }
    }
  }
  
  return(list(min_AIC_params=min_AIC_params, min_AIC = min_AIC, error_params=error_params))
}




# BIC #
find_bic_params <- function(ts_object, PQmax = 2, Imax = 1, exogenous_var=NULL){
  
  # Creation of new objects to keep track
  min_BIC <- Inf
  min_BIC_params <- NULL
  error_params <- list() # Store params that cause errors
  
  # Loop to test different parameters 
  for (ar in 0:PQmax) {
    for (ma in 0:PQmax) {
      for (AR in 0:PQmax) {
        for (MA in 0:PQmax) {
          for (i in 0:Imax) {
            for (I in 0:Imax) {
              # Try-Catch block
              tryCatch({
                # Fitting the model
                fit <- Arima(ts_object, order=c(ar,i,ma), seasonal=list(order=c(AR,I,MA), period=12), xreg=exogenous_var)
                # Check if the current BIC is the minimum
                current_BIC <- BIC(fit)
                if (current_BIC < min_BIC) {
                  min_BIC <- current_BIC # Minimum BIC value at this stage
                  min_BIC_params <- c(ar, i, ma, AR, I, MA) # Best parameters at this stage
                }
              }, error=function(e){
                # Here, instead of adjusting params, we log them
                error_params[[length(error_params) + 1]] <- list(ar=ar, ma=ma, AR=AR, MA=MA, i=i, I=I)
              })
            }
          }
        }
      }
    }
  }
  
  return(list(min_BIC_params=min_BIC_params, min_BIC=min_BIC, error_params=error_params))
}