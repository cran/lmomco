"parTLgld" <-
function(lmom,verbose=FALSE) {
    if(length(lmom$source) == 1 && lmom$source != "TLmoms") {
      warning("TL-moments with trim=1 are required--can not complete parameter estimation")
      return()    
    }
    else if(length(lmom$trim) == 1 && lmom$trim != 1) {
      warning("Attribute of TL-moments is not trim=1--can not complete parameter estimation")
      return()
    }  
    
    LM1 <- lmom$lambdas[1]
    LM2 <- lmom$lambdas[2]

    T3 <- lmom$ratios[3]
    T4 <- lmom$ratios[4]
    T5 <- lmom$ratios[5]

    estLa1 <- function(La2,La3,La4) {
      La1 <- LM1 - 6*La2*(1/((La3+3)*(La3+2)) - 1/((La4+3)*(La4+2)))
    }

    estLa2 <- function(LM2,La3,La4) {
      D1 <- (La3+2)*(La3+3)*(La3+4)
      D2 <- (La4+2)*(La4+3)*(La4+4)
      return((LM2/6)/( La3/D1 + La4/D2 ) )
    }

    esttau3 <- function(K,H) {
      D1 <- (K+5)*(K+4)*(K+3)*(K+2)
      D2 <- (H+5)*(H+4)*(H+3)*(H+2)
      G  <- K*(H+4)*(H+3)*(H+2)+H*(K+4)*(K+3)*(K+2)
      t3 <- (10/9)*(K*(K-1)*D2 - H*(H-1)*D1)/((K+5)*(H+5)*G)
      return(t3)
    }
    esttau4 <- function(K,H) {
      D1 <- (K+6)*(K+5)*(K+4)*(K+3)*(K+2)
      D2 <- (H+6)*(H+5)*(H+4)*(H+3)*(H+2)
      G  <- K*(H+4)*(H+3)*(H+2)+H*(K+4)*(K+3)*(K+2)
      t4 <- (5/4)*(K*(K-1)*(K-2)*D2 + H*(H-1)*(H-2)*D1)/((K+6)*(H+6)*(K+5)*(H+5)*G)
      return(t4)
    }
    esttau5 <- function(K,H) {
      D1 <- (K+7)*(K+6)*(K+5)*(K+4)*(K+3)*(K+2)
      D2 <- (H+7)*(H+6)*(H+5)*(H+4)*(H+3)*(H+2)
      G  <- K*(H+4)*(H+3)*(H+2)+H*(K+4)*(K+3)*(K+2)
      t5 <- (7/5)*(K*(K-1)*(K-2)*(K-3)*D2 - H*(H-1)*(H-2)*(H-3)*D1)/
                                         ((K+7)*(H+7)*(K+6)*(H+6)*(K+5)*(H+5)*G)
      return(t5) 
    }

    # Define the objective function
    fn <- function(x) {
      La3 <- x[1]
      La4 <- x[2]
      t3  <- esttau3(La3,La4)
      t4  <- esttau4(La3,La4)
      t5  <- esttau5(La3,La4)
      ss  <- ((T3-t3)^2 + (T4-t4)^2)
      return(ss)
    }

    # This function is a scaled down version of are.pargld.valid
    # to avoid the unnecessary overhead of computing the first
    # L-moment and building the standard parameter object.
    validgld <- function(La2,La3,La4) {
      if(is.na(La2)) return(FALSE)
      if(La2 == -Inf || La2 == Inf) return(FALSE) 
      #if(verbose == TRUE) cat(c("validgld-Checking Theorem 1.3.3: ",La2, La3, La4,"\n"))
      # Test that second L-moment is suitable
      for(F in seq(0,1,by=0.00001)) {
        tmp <- La2*(La3*F^(La3-1) + La4*(1-F)^(La4-1))
        #cat(c(La2,La3,La4,tmp))
        if(tmp < 0) return(FALSE)
      }

      #if(verbose == TRUE) cat(c("validgld-Checking by region: ",La2, La3, La4,"\n"))        
      # ratios define the curved lines in figure1.3-1 of K&D
      ratio6 <- -1/La3
      ratio5 <-  1/La4
      # See Theorem 1.3.33 of Karian and Dudewicz and figure1.3-1
      if(La3 <= -1 && La4 >=  1) {         # REGION 1
        return(FALSE) # ordinary L-moments do not exist in REGION 1
        return(TRUE)
      }
      else if(La3 >=  1 && La4 <= -1) {    # REGION 2
        return(FALSE) # ordinary L-moments do not exist in REGION 2
        return(TRUE)
      }
      else if(La3 >= 0 && La4 >= 0) {      # REGION 3
        return(TRUE) 
      }
      else if(La3 <= 0 && La4 <= 0) {      # REGION 4
        return(TRUE)
      }
      else if((La4 < ratio6 && La4 >= -1) && La3 >= 1) {  # REGION 6
        return(TRUE)
      }
      else if(La4 > ratio5 && (La3 >= -1 && La3 <= 0)) { # REGION 5
        return(TRUE)
      }
      return(FALSE)
    }

    # Are the L-moments valid?  Not fully certain that this check
    # is needed if the GLD proves to be valid through the validgld
    # function above. Early testing of pargld() before the suitabiliy
    # of the second L-moment for GLD showed that the validlmom() test
    # was needed.  Consider this for safety at any rate.
    validlmom <- function(sol,region) {
      LM3 <- sol$par[1]
      LM4 <- sol$par[2]

      T3 <- esttau3(LM3,LM4)
      T4 <- esttau4(LM3,LM4)
      T5 <- esttau5(LM3,LM4)
      #if(verbose == TRUE) cat(c("validlmom(region,Tau3,Tau4,Tau5)?:\n",region,T3,T4,T5))
      if(abs(T3) > 1)  return(FALSE)
      if(T4 < (0.25*(5*T3^2 - 1)) || T4 > 1) return(FALSE)
      if(abs(T5) > 1)  return(FALSE)
      return(TRUE)
    }

   M <- matrix(nrow = 13, ncol = 2)
   M[1,]  <- c(-2.5,2.5)   # REGION 1
   M[2,]  <- c(2.5,-2.5)   # REGION 2
   M[3,]  <- c(2.5,2.5)    # REGION 3
   M[4,]  <- c(-.5,-.5)    # REGION 4
   M[5,]  <- c(-1.5,-1.5)  # REGION 4
   M[6,]  <- c(-2.5,-2.5)  # REGION 4
   M[7,]  <- c(-3.5,-3.5)  # REGION 4
   M[8,]  <- c(-4.5,-4.5)  # REGION 4
   M[9,]  <- c(-5.5,-5.5)  # REGION 4
   M[10,] <- c(-6.5,-6.5)  # REGION 4
   M[11,] <- c(-7.5,-7.5)  # REGION 4
   M[12,] <- c(-.5,2.5)    # REGION 5
   M[13,] <- c(2.5,-.5)    # REGION 6

   EPS <- 1e-5
   SMALL <- Inf
   FinalT5diff <- Inf
   para <- matrix(nrow = 4, ncol = 1)
   if(verbose == TRUE) {
     cat("SUMMARY OF INCREMENTAL OPTIMIZATIONS\n")
     cat("  Q(F) = X + A*(F^K + (1-F)^H)\n")
     cat("--------------------------------------------------------------------------------------------\n")
     cat("Attempt      X        A        K        H            Tau5_diff     SumSqError    Diagnostics\n")
     cat("--------------------------------------------------------------------------------------------\n")
   }
   for(i in seq(1,13)) {
     r <- optim(M[i,],fn)
     e <- r$value
     K <- r$par[1]
     H <- r$par[2]
     T5diff <- T5 - esttau5(K,H)
     A <- estLa2(LM2,K,H)
     X <- estLa1(A,K,H)
     if(verbose == TRUE) cat(c(" ",i,sprintf("%6.6f",X),",", 
                                     sprintf("%6.6f",A),",",
			             sprintf("%6.6f",K),",",
				     sprintf("%6.6f",H),"   ",
			             sprintf("%6.6f",T5diff),"  ",
				     sprintf("%8.8f",e)))
     if(validgld(A,K,H) == FALSE) {
       if(verbose == TRUE) cat(c("       Invalid GLD parameters--\n"))
       next
     }
     if(verbose == TRUE) cat(c("     Valid GLD parameters--"))
     if(validlmom(r,region=i) == FALSE) {
       if(verbose == TRUE) cat(c("Invalid L-moments\n"))
       next 
     }
     if(verbose == TRUE) cat(c("Valid L-moments\n"))
     if(abs(e) < SMALL) {
       para[1] <- X
       para[2] <- A
       para[3] <- K
       para[4] <- H
       SMALL <- e
       FinalT5diff <- T5diff
     }
   }
   if(verbose == TRUE) {
     cat("--------------------------------------------------------------------------------------------\n")
   }

  return(list(type = 'gld', para = para, error = SMALL, tau5diff = FinalT5diff,
              source="parTLgld"))
}
