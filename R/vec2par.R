"vec2par" <-
function(vec,type) {
    if(type == 'cau') {
      para <- matrix(nrow = 2, ncol = 1)
      para[,1] <- vec
      z <- list(type = 'cau', para = para)
    }
    else if(type == 'exp') {
      para <- matrix(nrow = 2, ncol = 1)
      para[,1] <- vec
      z <- list(type = 'exp', para = para)
    }
    else if(type == 'gam') {
      para <- matrix(nrow = 2, ncol = 1)
      para[,1] <- vec
      z <- list(type = 'gam', para = para)
    }
    else if(type == 'gev') {
      para <- matrix(nrow = 3, ncol = 1)
      para[,1] <- vec
      z <- list(type = 'gev', para = para)
    }
    else if(type == 'gld') {
      para <- matrix(nrow = 4, ncol = 1)
      para[,1] <- vec
      z <- list(type = 'gld', para = para)
    }
    else if(type == 'glo') {
      para <- matrix(nrow = 3, ncol = 1)
      para[,1] <- vec
      z <- list(type = 'glo', para = para)
    }
    else if(type == 'gno') {
      para <- matrix(nrow = 3, ncol = 1)
      para[,1] <- vec
      z <- list(type = 'gno', para = para)
    }
    else if(type == 'gpa') {
      para <- matrix(nrow = 3, ncol = 1)
      para[,1] <- vec
      z <- list(type = 'gpa', para = para)
    }
    else if(type == 'gum') {
      para <- matrix(nrow = 2, ncol = 1)
      para[,1] <- vec
      z <- list(type = 'gum', para = para)
    }
    else if(type == 'kap') {
      para <- matrix(nrow = 4, ncol = 1)
      para[,1] <- vec
      z <- list(type = 'kap', para = para)
    }
    else if(type == 'nor') {
      para <- matrix(nrow = 2, ncol = 1)
      para[,1] <- vec
      z <- list(type = 'nor', para = para)
    }
    else if(type == 'pe3') {
      para <- matrix(nrow = 3, ncol = 1)
      para[,1] <- vec
      z <- list(type = 'pe3', para = para)
    }
    else if(type == 'wak') {
      para <- matrix(nrow = 5, ncol = 1)
      para[,1] <- vec
      z <- list(type = 'wak', para = para)
    }
    else {
      stop("Did not find a valid distribution type.")
    }
    if(! are.par.valid(z)) {
      warning("The parameters are invalid for the distribution.")
      return()
    }
    return(z)
}

