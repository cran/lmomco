"pwmRC" <-
function(x,threshold=NULL,nmom=5,sort=TRUE,checkbetas=FALSE) {
  if(sort) x <- sort(x)

  if(is.null(threshold)) {
    warning("threshold is NULL")
    return(NULL)
  }
  T <- threshold
  x <- sapply(x,function(v) { if(v >= T) return(T); return(v)})

  n <- length(x)
  m <- n - length(x[x == T])
  
  observed.sample <- x[x < T]
  #  print(length(observed.sample))
  z <- pwm(observed.sample,nmom=5,sort=FALSE)
  #  print(z)
  Abetas <- z$betas

  Bbetas <- vector(mode="numeric",length=nmom)
  Bbetas <- rep(NA,length(Bbetas))

  #cat(c("DEBUG m=",m,"  n=",n,"\n"))

  for(r in seq(0,nmom-1)) {
    i <- r+1
    sumA <- 0
    sumB <- 0
    for(j in seq(1,m)) {
      sumA <- sumA + (choose(j-1,r))*x[j]
    }
    if(m < n) { # seq can return reversed elements, avoid loop if no censored values
      for(j in seq(m+1,n)) {
        sumB <- sumB + (choose(j-1,r))*T
      }
    }
    Bbetas[i] <- (sumA+sumB)/(n*choose(n-1,r))
  }

  if(checkbetas) { # see eqs. 29.3.3 and 29.3.4 in Hosking (1995, p. 548)
    # The Abetas and Bbetas have an intrinsic interconnection.
    # See the above reference "The use of L-moments in the Analysis of Censored Data"
    checkBbetas <- vector(mode="numeric")
    for(r in seq(0,nmom-1)) {
      bigZ <- (m/n)*choose(m-1,r)/choose(n-1,r)
      checkBbetas[r+1] <- bigZ*Abetas[r+1] + ((1-bigZ)/(r+1))*T
    }
    cat(c("Betas:",Bbetas,"\n"))
    cat(c("checkBbetas:",checkBbetas,"\n"))
  }
  
  zeta <- m/n # see section 29.7, p. 552 of Hosking(1995)

  z <- list(Abetas=Abetas,
            Bbetas=Bbetas,
            source="pwmRC",
            threshold=T,
            zeta=zeta,
            numabovethreshold=(n-m),
            observedsize=m,
            samplesize=n)
  return(z)

} 
