"lmomgld" <-
function(gldpara) {
    z <- list(L1   = NULL,
              L2   = NULL,
              TAU3 = NULL,
              TAU4 = NULL,
              TAU5 = NULL,
              LCV  = NULL,
              L3   = NULL,
              L4   = NULL,
              L5   = NULL,
              source = "lmomgld"
             )

    if(! are.pargld.valid(gldpara)) return()

    L1  <- gldpara$para[1]
    L2  <- gldpara$para[2]
    L3  <- gldpara$para[3]
    L4  <- gldpara$para[4]

    # This first section was the historical first attempt (successful)
    # to compute the L-moments in long notation.
    # LAMBDA 1
    #z$L1 <- L1 + (L2)*(1/(L3+1) - 1/(L4+1))

    # LAMBDA 2    
    #Y <- L1 + 2/(L2*(L3+2))
    #z$L2 <- Y - 2*L2 * (1/(L4+1) - 1/(L4+2)) - z$L1

    # LAMBDA 3
    #Y <- 2*L1 + 6*L2/(L3+3) - 3*(z$L2+z$L1) + z$L1
    #X <- 2/(L4+2) - 1/(L4+3) - 1/(L4+1)
    #z$L3 <- Y + (6/L2)*X

    # LAMBDA 4
    # The 20F^3 * x(F) term
    #T  <- -3/(L4+4) * (2/(L4+2) - 1/(L4+1) - 1/(L4+3))
    #F3 <- (20/4)*L1 + 20*L2/(L3+4) - 20*T*L2
    # The 30F^2 * x(F) term
    #F2 <- 5*(z$L3 + 3*(z$L2+z$L1) - z$L1)    
    # The +12F * x(F) term
    #F1 <- 6*(z$L2+z$L1)
    # The x(F) term
    #F0 <- z$L1
    #z$L4   <- F3 - F2 + F1 - F0
    
    # THE RATIOS
    #z$LCV  <- z$L2/z$L1
    #z$TAU3 <- z$L3/z$L2
    #z$TAU4 <- z$L4/z$L2

    # Uncomment for numerical double check on analytical solutions
    #l1 <- integrate(function(f) (1)                   *(L1+L2*(f^L3 - (1-f)^L4)),0,1,subdivisions=10000)
    #l2 <- integrate(function(f) (2*f-1)               *(L1+L2*(f^L3 - (1-f)^L4)),0,1,subdivisions=10000)
    #l3 <- integrate(function(f) (6*f^2-6*f+1)         *(L1+L2*(f^L3 - (1-f)^L4)),0,1,subdivisions=10000)
    #l4 <- integrate(function(f) (20*f^3-30*f^2+12*f-1)*(L1+L2*(f^L3 - (1-f)^L4)),0,1,subdivisions=10000)
    #print(c(l1$value,l2$value,l3$value,l4$value))

    # Using more compact formulations in the documentation
    LAM1 <- L1 + (L2)*(1/(L3+1) - 1/(L4+1))
    LAM2 <- (L2)*(L3/((L3+1)*(L3+2)) + L4/((L4+1)*(L4+2)))
    
    LCV <- LAM2/LAM1
    
    N1 <- L3*(L3-1)*(L4+3)*(L4+2)*(L4+1)
    N2 <- L4*(L4-1)*(L3+3)*(L3+2)*(L3+1)
    D1 <- (L3+3)*(L4+3)
    D2 <- L3*(L4+1)*(L4+2) + L4*(L3+1)*(L3+2)
    TAU3 <- (N1 - N2)/(D1*D2)

    LAM3 <- TAU3*LAM2

    
    N1 <- L3*(L3-2)*(L3-1)*(L4+4)*(L4+3)*(L4+2)*(L4+1)
    N2 <- L4*(L4-2)*(L4-1)*(L3+4)*(L3+3)*(L3+2)*(L3+1)
    D1 <- (L3+4)*(L4+4)*(L3+3)*(L4+3)
    D2 <- L3*(L4+1)*(L4+2) + L4*(L3+1)*(L3+2)
    TAU4 <- (N1 + N2)/(D1*D2)    

    LAM4 <- TAU4*LAM2

    N1 <- L3*(L3-3)*(L3-2)*(L3-1)*(L4+5)*(L4+4)*(L4+3)*(L4+2)*(L4+1)
    N2 <- L4*(L4-3)*(L4-2)*(L4-1)*(L3+5)*(L3+4)*(L3+3)*(L3+2)*(L3+1)
    D1 <- (L3+5)*(L4+5)*(L3+4)*(L4+4)*(L3+3)*(L4+3)
    D2 <- L3*(L4+1)*(L4+2) + L4*(L3+1)*(L3+2)
    TAU5 <- (N1 - N2)/(D1*D2)    

    LAM5 <- TAU5*LAM2
    
    z$L1 <- LAM1
    z$L2 <- LAM2
    z$L3 <- LAM3
    z$L4 <- LAM4
    z$L5 <- LAM5
    z$LCV <- LCV
    z$TAU3 <- TAU3
    z$TAU4 <- TAU4
    z$TAU5 <- TAU5
    
    return(z)
}

