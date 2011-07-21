"tlmrwei" <-
function(trim=NULL, leftrim=NULL, rightrim=NULL,
         dbeg=-10, dend=10, by=.1) {
  ds <- seq(dbeg, dend, by=by)
  n <- length(ds)
  T3 <- T4 <- vector(mode="numeric", length=n)
  i <- 0
  for(d in ds) {
    tmp.para <- vec2par(c(0,1,d), type="wei", paracheck=FALSE)
    tmp.lmr  <- theoTLmoms(tmp.para, nmom=4,
                  trim=trim, leftrim=leftrim, rightrim=rightrim)
    i <- i + 1
    T3[i] <- tmp.lmr$ratios[3]
    T4[i] <- tmp.lmr$ratios[4]
  }
  z <- list(tau3=T3, tau4=T4)
  return(z)
}

