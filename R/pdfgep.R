"pdfgep" <-
function(x, para) {
    if(! are.pargep.valid(para)) return()
    attributes(para$para) <- NULL
    B <- 1/para$para[1]
    K <-   para$para[2]
    H <-   para$para[3]
    x[x < 0] <- NA
    hex <- H*exp(-B*x)
    e1 <- exp(-H + hex)
    e2 <- exp(-H - B*x + hex)
    C  <- (K*B*H)/(1-exp(-H))^K
    fx <- C*(1 - e1)^(K - 1)*e2
    return(fx)
}

