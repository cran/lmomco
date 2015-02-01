"quatri" <-
function(f,para, paracheck = TRUE) {
    if(! check.fs(f)) return()
    if(paracheck == TRUE) {
       if(! are.partri.valid(para)) return()
    }

    MIN  <- para$para[1]
    MODE <- para$para[2]
    MAX  <- para$para[3]
    A <- (MAX  - MIN)
    B <- (MODE - MIN)
    C <- (MAX  - MODE)
    AB <- A*B
    AC <- A*C

    P <- B/A # The F value at the mode

    x <- vector(mode = "numeric", length=length(f))
    for(i in seq(1,length(f))) {
       F <- f[i]
       if(F > P) {
          x[i] <- MAX - sqrt((1-F)*AC)
       } else if(F < P) {
          x[i] <- MIN + sqrt(F*AB)
       } else { # F == P
          x[i] <- MODE
       }
    }
    return(x)
}

