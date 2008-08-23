"quawak" <-
function(f,wakpara) {
    #
    #   UFL SHOULD BE CHOSEN SO THAT EXP(UFL) JUST DOES NOT CAUSE
    #    UNDERFLOW 
    #
    UFL <- log(.Machine$double.xmin);
    if(! check.fs(f)) return()
    if(! are.parwak.valid(wakpara)) return()

    XI <- wakpara$para[1]
    A <- wakpara$para[2]
    B <- wakpara$para[3]
    C <- wakpara$para[4]
    D <- wakpara$para[5]
    n <- length(f)
    x <- vector(mode="numeric",length=n)
    for(i in seq(1,n)) {
      if(f[i] == 0) { x[i] <- XI; next }
      if(f[i] == 1) {
        if(D < 0) { x[i] <- XI+A/B-C/D; next }
        if(D == 0 & C == 0 & B > 0) { x[i] <- XI+A/B; next }
        if(D < 0 | B == 0) { x[i] <- Inf }
        warning("argument of function is invalid")
        return()
      }
      Z <- -log(1-f[i])
      Y1 <- Z
      if(B == 0) {
        Y2 <- Z
        if(D != 0) Y2 <- (1-exp(D*Y2))/(-D)
        x[i] <- XI+A*Y1+C*Y2
      }
      else {
        TEMP <- -B*Z
        if(TEMP <  UFL) Y1 <- 1/B
        if(TEMP >= UFL) Y1 <- (1-exp(TEMP))/B
        Y2 <- Z
        if(D != 0) Y2 <- (1-exp(D*Y2))/(-D)
        x[i] <- XI+A*Y1+C*Y2
      }
    }
    return(x)
}

