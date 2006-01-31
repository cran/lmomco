"are.pargld.valid" <-
function(para) {
    if(! is.gld(para)) return(FALSE)

    La2 <- para$para[2]
    La3 <- para$para[3]
    La4 <- para$para[4]

    if(La3 <= -1 && La4 >=  1) { # REGION 1
       return(TRUE)
    }
    if(La3 >=  1 && La4 <= -1) { # REGION 2
       return(TRUE)
    }
    if(La3 < 0 && La4 > 0 && La4 < 1) { # REGION V1
       warning("Parameters are invalid (region V1).")
       return(FALSE)
    }
    if(La3 > 0 && La3 < 1 && La4 < 0) { # REGION V2
       warning("Parameters are invalid (region V2).")
       return(FALSE)
    }
    if(La3 > -1 && La3 < 0 && La4 > 1) { # REGION V3
       tmp1 <- (1-La3)**(1-La3)
       tmp2 <- (La4-La3)**(La4-La3)
       tmp3 <- (La4-1)**(La4-1)
       rhs  <- -La3/La4
       if(tmp3*(tmp1/tmp2) < rhs) {
         return(TRUE)
       }
       else {
         warning("Parameters are invalid (region V3).")
         return(FALSE)
       }
    }
    if(La3 > 1 && La4 > -1 && La4 < 0) { # REGION V4
       # Unclear in Karian and Dudewicz (2000) that
       # the following same condition on V3 applies
       # in V4.  See top of page 16. However, this basic
       # test is also stated on page 17 to be an if and only if
       tmp1 <- (1-La3)**(1-La3)
       tmp2 <- (La4-La3)**(La4-La3)
       tmp3 <- (La4-1)**(La4-1)
       rhs  <- -La3/La4
       if(tmp3*(tmp1/tmp2) < rhs) {
         return(TRUE)
       }
       else {
         warning("Parameters are invalid (region V4).")
         return(FALSE)
       }
    }
    return(TRUE)
}

