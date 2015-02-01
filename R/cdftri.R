"cdftri" <-
function(x,para) {

    if(! are.partri.valid(para)) return()

    MIN  <- para$para[1]
    MODE <- para$para[2]
    MAX  <- para$para[3]
    A <- (MAX  - MIN)
    B <- (MODE - MIN)
    C <- (MAX  - MODE)
    AB <- A*B
    AC <- A*C

    f <- vector(mode = "numeric", length=length(x))
    for(i in seq(1,length(x))) {
       X <- x[i]
       if(X > MODE) {
          f[i] <- 1 - (MAX-X)^2/(AC)
       } else if(X < MODE) {
          f[i] <-     (X-MIN)^2/(AB)
       } else { # X == MODE
          f[i] <- B/A
       }
    }
    return(f)
}

