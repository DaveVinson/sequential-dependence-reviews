############################################################################
########## SEQUENTIAL DEPENDENCY IN function list  #########################
############################################################################
################## Written by David W. Vinson ##############################
##################### DO NOT DISTRIBUTE ####################################
############################################################################
############################################################################

####FUN for mean_se for plot ####
mean_se <- function(x, mult = 1) { 
  x <- na.omit(x)
  se <- sqrt(var(x) / length(x))
  mean <- mean(x)
  data.frame(y = mean, ymin = mean - se, ymax = mean + se)
}

####Lag function for data.table #####
panel_lag <- function(var, k) { 
  if (k > 0) {
    #bring past values forward k times
    return(c(rep(NA, k), head(var, -k)))
  } else {
    #bring future values backward
    return(c(tail(var, k), rep(NA, -k)))
  }  
}

##### find the mode ##### 
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}