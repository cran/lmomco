"vec2par" <-
function(vec,type,nowarn=FALSE,...) {
    if(type == 'cau') {
      z <- list(type = 'cau', para = vec, source = "vec2par")
    }
    else if(type == 'exp') {
      z <- list(type = 'exp', para = vec, source = "vec2par")
    }
    else if(type == 'gam') {
      z <- list(type = 'gam', para = vec, source = "vec2par")
    }
    else if(type == 'gev') {
      z <- list(type = 'gev', para = vec, source = "vec2par")
    }
    else if(type == 'gld') {
      z <- list(type = 'gld', para = vec, source = "vec2par")
    }
    else if(type == 'glo') {
      z <- list(type = 'glo', para = vec, source = "vec2par")
    }
    else if(type == 'gno') {
      z <- list(type = 'gno', para = vec, source = "vec2par")
    }
    else if(type == 'gpa') {
      z <- list(type = 'gpa',  para = c(vec[1],vec[2],vec[3]),
                zeta=vec[4], source = "vec2par")
    }
    else if(type == 'gum') {
      z <- list(type = 'gum', para = vec, source = "vec2par")
    }
    else if(type == 'revgum') {
      z <- list(type = 'revgum', para = c(vec[1],vec[2]),
                zeta=vec[3], source = "vec2par")
    }
    else if(type == 'kap') {
      z <- list(type = 'kap', para = vec, source = "vec2par")
    }
    else if(type == 'nor') {
      z <- list(type = 'nor', para = vec, source = "vec2par")
    }
    else if(type == 'pe3') {
      z <- list(type = 'pe3', para = vec, source = "vec2par")
    }
    else if(type == 'wak') {
      z <- list(type = 'wak', para = vec, source = "vec2par")
    }
    else if(type == 'wei') {
      z <- list(type = 'wei', para = vec, source = "vec2par")
    }
    else {
      stop("Did not find a valid distribution type")
    }
    if(! are.par.valid(z,nowarn,...)) {
      op <- options()
      if(nowarn == TRUE) options(warn=-1)
      warning("The parameters are invalid for the distribution")
      options(op)
      return()
    }
    return(z)
}

