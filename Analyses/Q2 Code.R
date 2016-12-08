###Question 2###
library(tseries)

AGIOdata <- get.hist.quote('AGIO',quote="Close")

AGIOret <- log(lag(AGIOdata)) - log(AGIOdata)

AGIOvol <- sd(AGIOret) * sqrt(250) * 100

## volatility
getVol <- function(d, logrets)
{
  
  var = 0
  
  lam = 0
  
  varlist <- c()
  
  for (r in logrets) {
    
    lam = lam*(1 - 1/d) + 1
    
    var = (1 - 1/lam)*var + (1/lam)*r^2
    
    varlist <- c(varlist, var)
    
  }
  
  sqrt(varlist)
}


# Recreate Figure 6.12 in the text on page 155

volest <- getVol(10,AGIOret)

volest2 <- getVol(30,AGIOret)

volest3 <- getVol(100,AGIOret)

plot(volest,type="l")

lines(volest2,type="l",col="red")

lines(volest3, type = "l", col="blue")
