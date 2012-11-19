"quawak" <-
function(f,wakpara,paracheck=TRUE) {
    #    #   UFL SHOULD BE CHOSEN SO THAT EXP(UFL) JUST DOES NOT CAUSE
    #    UNDERFLOW 
    #
    UFL <- log(.Machine$double.xmin);
    if(! check.fs(f)) return()
    if(paracheck == TRUE) {
      if(! are.parwak.valid(wakpara)) return()
    }
    XI <- wakpara$para[1]
    A <- wakpara$para[2] # alpha
    B <- wakpara$para[3] # beta
    C <- wakpara$para[4] # gamma
    D <- wakpara$para[5] # delta
    n <- length(f)
    x <- vector(mode="numeric",length=n)
    for(i in seq(1,n)) {
      if(f[i] == 0) { x[i] <- XI; next }
      if(f[i] == 1) {
        x[i] <- XI+A/B*(1-0^B) - C/D*(1 - 0^(-D))
        next
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

#   30 IF(D.GT.ZERO)GOTO 1010 
#      IF(D.LT.ZERO)QUAWAK=XI+A/B-C/D 
#      IF(D.EQ.ZERO.AND.C.GT.ZERO)GOTO 1010 
#      IF(D.EQ.ZERO.AND.C.EQ.ZERO.AND.B.EQ.ZERO)GOTO 1010 
#      IF(D.EQ.ZERO.AND.C.EQ.ZERO.AND.B.GT.ZERO)QUAWAK=XI+A/B 
#      RETURN 
#C 
# 1000 WRITE(6,7000) 
#      QUAWAK=ZERO 
#      RETURN 
# 1010 WRITE(6,7010) 
#      QUAWAK=ZERO 
#      RETURN 
