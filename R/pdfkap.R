"pdfkap" <-
function(x,para) {
    if(! are.parkap.valid(para)) return()

    # Function based on written communication of FORTRAN source
    # from J.R.M. Hosking in late October 2007.


    # SMALL IS USED TO TEST WHETHER X IS EFFECTIVELY AT 
    # THE ENDPOINT OF THE DISTRIBUTION 
    SMALL <- 1e-15 

    XI <- para$para[1] 
    A  <- para$para[2] 
    K  <- para$para[3] 
    H  <- para$para[4]

    Fs <- cdfkap(x,para)
 
    f <- vector(mode = "numeric")
    for(i in seq(1,length(x))) {
      Y  <- (x[i] - XI)/A
      if(K != 0) {
        Y <- 1 - K*Y
        if(Y <= SMALL) {
           f[i] <- 0
           next
        }
        else {
           Y <- (1-1/K)*log(Y)
        }
      }
      Y <- exp(-Y)
      f[i] <- Y/A * Fs[i]^(1-H)
    }
    return(f)
}

