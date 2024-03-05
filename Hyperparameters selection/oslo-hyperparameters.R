# AIC
PQmax = 2
Imax = 1

min_AIC <- Inf
min_AIC_params_oslo <- NULL

results_AIC <- data.frame(ar = numeric(),
                          ma = numeric(),
                          AR = numeric(),
                          MA = numeric(),
                          i = numeric(),
                          I = numeric(),
                          AIC = numeric())


for (ar in 0:PQmax) {
  for (ma in 0:PQmax) {
    for (AR in 0:PQmax) {
      for (MA in 0:PQmax) {
        for (i in 0:Imax) {
          for (I in 0:Imax) {
            fit <- Arima(oslo_ts19, order=c(ar,i,ma), seasonal=list(order=c(0,I,MA), period=12))
            results_AIC <- rbind(results_AIC, c(ar, i, ma, AR, I, MA, AIC(fit)))
            
            # Check if the current AIC is the minimum
            if (AIC(fit) < min_AIC) {
              min_AIC <- AIC(fit)
              min_AIC_params_oslo <- c(ar, i, ma, AR, I, MA)
            }
          }
        }
      }
    }
  }
}
print(min_AIC_params_oslo)


# BIC
PQmax = 2
Imax = 1

min_BIC <- Inf
min_BIC_params_oslo <- NULL

results_BIC <- data.frame(ar = numeric(),
                          ma = numeric(),
                          AR = numeric(),
                          MA = numeric(),
                          i = numeric(),
                          I = numeric(),
                          BIC = numeric())


for (ar in 0:PQmax) {
  for (ma in 0:PQmax) {
    for (AR in 0:PQmax) {
      for (MA in 0:PQmax) {
        for (i in 0:Imax) {
          for (I in 0:Imax) {
            fit <- Arima(oslo_ts19, order=c(ar,i,ma), seasonal=list(order=c(0,I,MA), period=12))
            results_BIC <- rbind(results_BIC, c(ar, i, ma, AR, I, MA, BIC(fit)))
            
            # Check if the current BIC is the minimum
            if (BIC(fit) < min_BIC) {
              min_BIC <- BIC(fit)
              min_BIC_params_oslo <- c(ar, i, ma, AR, I, MA)
            }
          }
        }
      }
    }
  }
}
print(min_BIC_params_oslo)
