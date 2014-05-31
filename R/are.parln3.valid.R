"are.parln3.valid" <-
function(para,nowarn=FALSE) {
    if(! is.ln3(para)) return(FALSE)

    ZETA  <- para$para[1]
    MU    <- para$para[2]
    SIG   <- para$para[3]
    ETA   <- exp(MU);
    # Now the conventional location, scale, shape 
    XI <- ZETA + ETA # get the parameters
    ALPHA <- ETA*SIG # into the GNO domain, we can do
    K     <- -SIG # a check using the first two L-moments

    LAM1 <- XI + (ALPHA/K)*(1-exp(K^2/2))
    LAM2 <- (ALPHA/K) * (exp(K^2/2)) * (1-2*pnorm(-K/sqrt(2)))
    
    op <- options()
    GO <- TRUE
    if(nowarn == TRUE) options(warn=-1)
    if(SIG <= 0) {
       warning("Parameters are invalid, Slog <= 0")
       GO <- FALSE
    }
    if(ZETA >= (LAM1 + LAM2)) {
       warning("Parameters are invalid, zeta >= L1 + L2")
       GO <- FALSE
    }
    options(op)
    if(GO) return(TRUE)
    return(FALSE)
}

