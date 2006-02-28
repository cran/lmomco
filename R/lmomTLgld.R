"lmomTLgld" <-
function(gldpara) {
    L <- seq(1,4)
    R <- seq(1,4)

    if(! are.pargld.valid(gldpara)) return()

    E  <- gldpara$para[1]
    A  <- gldpara$para[2]
    K  <- gldpara$para[3]
    H  <- gldpara$para[4]

    # Uncomment for numerical double check on analytical solutions
    #l1 <- integrate(function(f) (6)    *f*(1-f)*(E+A*(f^K - (1-f)^H))                            ,0,1,subdivisions=10000)
    #l2 <- integrate(function(f) (6)    *f*(1-f)*(E+A*(f^K - (1-f)^H))*(                  2*f - 1),0,1,subdivisions=10000)
    #l3 <- integrate(function(f) (20/3) *f*(1-f)*(E+A*(f^K - (1-f)^H))*(          5*f^2 - 5*f + 1),0,1,subdivisions=10000)
    #l4 <- integrate(function(f) (15/2) *f*(1-f)*(E+A*(f^K - (1-f)^H))*(14*f^3 - 21*f^2 + 9*f - 1),0,1,subdivisions=10000)
    #print(c(l1$value,l2$value,l3$value,l4$value))


    L1 <- E + 6*A*(1/((K+2)*(K+3)) - 1/((H+2)*(H+3)))
    L2 <-     6*A*(K/((K+2)*(K+3)*(K+4)) + H/((H+2)*(H+3)*(H+4)))
    
    D1 <- (K+5)*(K+4)*(K+3)*(K+2)
    D2 <- (H+5)*(H+4)*(H+3)*(H+2)
    L3 <- (20/3)*A*( (K*(K-1)/D1) - (H*(H-1)*(1)/D2) )
    G  <- K*(H+4)*(H+3)*(H+2)+H*(K+4)*(K+3)*(K+2)
    T3 <- (10/9)*(K*(K-1)*D2 - H*(H-1)*D1)/((K+5)*(H+5)*G)


    D1 <- (K+6)*(K+5)*(K+4)*(K+3)*(K+2)
    D2 <- (H+6)*(H+5)*(H+4)*(H+3)*(H+2)
    L4 <- (15/2)*A*( (K*(K-1)*(K-2)/D1) + (H*(H-1)*(H-2)/D2) )
    T4 <- (5/4)*(K*(K-1)*(K-2)*D2 + H*(H-1)*(H-2)*D1)/((K+6)*(H+6)*(K+5)*(H+5)*G)

    D1 <- (K+7)*(K+6)*(K+5)*(K+4)*(K+3)*(K+2)
    D2 <- (H+7)*(H+6)*(H+5)*(H+4)*(H+3)*(H+2)
    L5 <- (42/5)*A*( (K*(K-1)*(K-2)*(K-3)/D1) - (H*(H-1)*(H-2)*(H-3)/D2) )
    T5 <- (7/5)*(K*(K-1)*(K-2)*(K-3)*D2 - H*(H-1)*(H-2)*(H-3)*D1)/((K+7)*(H+7)*(K+6)*(H+6)*(K+5)*(H+5)*G)

    T2 <- L2/L1

    #print(c(L1,L2,L3,L4,L5))
    #print(c(0,T2,T3,T4,T5))

    L[1] <- L1
    L[2] <- L2
    L[3] <- L3
    L[4] <- L4
    L[5] <- L5

    R[1] <- 0
    R[2] <- T2
    R[3] <- T3
    R[4] <- T4
    R[5] <- T5

    z <- list(lambdas = L, ratios = R, source = "lmomTLgld--trimmed (t=1)")
    return(z)
}

