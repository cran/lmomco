"cdfwak" <-
function(x,wakpara) {

    # CONVERT Z TO PROBABILITY
    z2f <- function(Z,UFL) {
      if(-Z < UFL) return(1)
      return(1-exp(-Z))
    }


    #  METHOD: THE EQUATION X=G(Z), WHERE G(Z) IS THE WAKEBY QUANTILE
    #  EXPRESSED AS A FUNCTION OF Z=-LOG(1-F), IS SOLVED USING HALLEY'S
    #  METHOD (THE 2ND-ORDER ANALOGUE OF NEWTON-RAPHSON ITERATION).
    #

    #
    #         EPS,MAXIT CONTROL THE TEST FOR CONVERGENCE OF THE ITERATION
    #         ZINCMX IS THE LARGEST PERMITTED ITERATIVE STEP
    #         ZMULT CONTROLS WHAT HAPPENS WHEN THE ITERATION STEPS BELOW ZERO
    #         UFL SHOULD BE CHOSEN SO THAT DEXP(UFL) JUST DOES NOT CAUSE
    #           UNDERFLOW

    EPS    <- 1e-8;
    MAXIT  <- 20;
    ZINCMX <- 3;
    ZMULT  <- 0.2;
    UFL    <- log(.Machine$double.xmin);

    if(! are.parwak.valid(wakpara)) return()

    XI <- wakpara$para[1]
    A  <- wakpara$para[2]
    B  <- wakpara$para[3]
    C  <- wakpara$para[4]
    D  <- wakpara$para[5]

    if(x <= XI) return(0)

    #
    #         TEST FOR SPECIAL CASES
    #
    if(B == 0 & C == 0 & D == 0) {
      #  SPECIAL CASE B=C=D=0: WAKEBY IS EXPONENTIAL
      Z <- (x-XI)/A
      return(z2f(Z,UFL))
    }
    if(C == 0) {
      #  SPECIAL CASE C=0: WAKEBY IS GENERALIZED PARETO, BOUNDED ABOVE
      CDFWAK <- 1
      if(x >= XI+A/B) return(1)
      Z <- -log(1-(x-XI)*B/A)/B
      return(z2f(Z,UFL))
    }
    if(A == 0) {
      #  SPECIAL CASE A=0: WAKEBY IS GENERALIZED PARETO, NO UPPER BOUND
      Z <- log(1+(x-XI)*D/C)/D
      return(z2f(Z,UFL))
    }


    #         GENERAL CASE
    #
    if(D < 0 & x >= XI+A/B-C/D) return(1)

    # INITIAL VALUES FOR ITERATION:
    #   IF X IS IN THE LOWEST DECILE OF THE DISTRIBUTION,
    #     START AT Z = 0 (F = 0);
    #   IF X IS IN THE HIGHEST PERCENTILE OF THE DISTRIBUTION,
    #   STARTING VALUE IS OBTAINED FROM ASYMPTOTIC FORM OF THE
    #   DISTRIBUTION FOR LARGE Z (F NEAR 1);
    #   OTHERWISE START AT Z <- 0.7 (CLOSE TO F <- 0.5).
    #
    Z <- 0.7
    if(x < quawak(0.1,wakpara)) Z <- 0
    if(x >= quawak(0.99,wakpara)) {
      if(D <  0) Z <- log((x-XI-A/B)*D/C+1)/D
      if(D == 0) Z <- (x-XI-A/B)/C
      if(D >  0) Z <- log((x-XI)*D/C+1)/D
    }
    #
    #  HALLEY'S METHOD, WITH MODIFICATIONS:
    #  IF HALLEY ITERATION WOULD MOVE IN WRONG DIRECTION
    #   (TEMP <= ZERO), USE ORDINARY NEWTON-RAPHSON INSTEAD;
    #   IF STEP GOES TOO FAR (ZINC > ZINCMX | ZNEW <= 0),
    #   LIMIT ITS LENGTH.
    #

    LOOPEND <- FALSE

    for(IT in seq(1,MAXIT)) {
      EB <- 0
      BZ <- -B*Z
      if(BZ >= UFL) EB <- exp(BZ)
      GB <- Z
      if(abs(B) > EPS) GB <- (1-EB)/B
      ED <- exp(D*Z)
      GD <- -Z
      if(abs(D) > EPS) GD <- (1-ED)/D
      XEST <- XI+A*GB-C*GD
      FUNC <- x-XEST
      DERIV1 <- A*EB+C*ED
      DERIV2 <- -A*B*EB+C*D*ED
      TEMP <- DERIV1+0.5*FUNC*DERIV2/DERIV1
      if(TEMP <= 0) TEMP <- DERIV1
      ZINC <- FUNC/TEMP
      if(ZINC > ZINCMX) ZINC <- ZINCMX
      ZNEW <- Z+ZINC
      if(ZNEW <= 0) { 
        Z <- Z*ZMULT
        next
      }
      Z <- ZNEW
      if(abs(ZINC) <= EPS) break
      if(IT == MAXIT) LOOPEND <- TRUE
    }
    if(LOOPEND == TRUE) {
      warning("Iteration has not converged. Result might be unreliable.")
    }

    # CONVERT Z VALUE TO PROBABILITY
    return(z2f(Z,UFL))
}

