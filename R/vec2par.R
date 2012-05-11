"vec2par" <-
function(vec,type,nowarn=FALSE,paracheck=TRUE,...) {
    if(type == 'aep4') {
      z <- list(type = 'aep4', para = vec, source = "vec2par")
      names(z$para) <- c("xi","alpha", "kappa", "h")
    }
    else if(type == 'cau') {
      z <- list(type = 'cau', para = vec, source = "vec2par")
      names(z$para) <- c("xi","alpha")
    }
    else if(type == 'exp') {
      z <- list(type = 'exp', para = vec, source = "vec2par")
      names(z$para) <- c("xi","alpha")
    }
    else if(type == 'texp') {
      z <- list(type = 'texp', para = vec, source = "vec2par")
      names(z$para) <- c("xi","alpha")
    }
    else if(type == 'gam') {
      z <- list(type = 'gam', para = vec, source = "vec2par")
      names(z$para) <- c("alpha","beta")
    }
    else if(type == 'gev') {
      z <- list(type = 'gev', para = vec, source = "vec2par")
      names(z$para) <- c("xi","alpha","kappa")
    }
    else if(type == 'gld') {
      z <- list(type = 'gld', para = vec, source = "vec2par")
      names(z$para) <- c("xi","alpha","kappa","h")
    }
    else if(type == 'glo') {
      z <- list(type = 'glo', para = vec, source = "vec2par")
      names(z$para) <- c("xi","alpha","kappa")
    }
    else if(type == 'gno') {
      z <- list(type = 'gno', para = vec, source = "vec2par")
      names(z$para) <- c("xi","alpha","kappa")
    }
    else if(type == 'gpa') {
      z <- list(type = 'gpa',  para = c(vec[1],vec[2],vec[3]),
                zeta=vec[4], source = "vec2par")
      names(z$para) <- c("xi","alpha","kappa")
    }
    else if(type == 'gum') {
      z <- list(type = 'gum', para = vec, source = "vec2par")
      names(z$para) <- c("xi","alpha")
    }
    else if(type == 'kur') {
      z <- list(type = 'kur', para = vec, source = "vec2par")
      names(z$para) <- c("alpha","beta")
    }
    else if(type == 'lap') {
      z <- list(type = 'lap', para = vec, source = "vec2par")
      names(z$para) <- c("xi","alpha")
    }
    else if(type == 'ln3') {
      z <- list(type = 'ln3', para = vec, source = "vec2par")
      names(z$para) <- c("zeta","mu","sigma")
    }
    else if(type == 'ray') {
      z <- list(type = 'ray', para = vec, source = "vec2par")
      names(z$para) <- c("xi","alpha")
    }
    else if(type == 'rice') {
      z <- list(type = 'rice', para = vec, source = "vec2par")
      names(z$para) <- c("nu", "alpha")
    }
    else if(type == 'revgum') {
      z <- list(type = 'revgum', para = c(vec[1],vec[2]),
                zeta=vec[3], source = "vec2par")
      names(z$para) <- c("xi","alpha")
    }
    else if(type == 'kap') {
      z <- list(type = 'kap', para = vec, source = "vec2par")
      names(z$para) <- c("xi","alpha","kappa","h")
    }
    else if(type == 'nor') {
      z <- list(type = 'nor', para = vec, source = "vec2par")
      names(z$para) <- c("mu","sigma")
    }
    else if(type == 'pe3') {
      z <- list(type = 'pe3', para = vec, source = "vec2par")
      names(z$para) <- c("mu","sigma","gamma")
    }
    else if(type == 'wak') {
      z <- list(type = 'wak', para = vec, source = "vec2par")
      names(z$para) <- c("xi","alpha","beta","gamma","delta")
    }
    else if(type == 'wei') {
      z <- list(type = 'wei', para = vec, source = "vec2par")
      names(z$para) <- c("zeta","beta","delta")
    }
    else {
      z <- list(type = 'user', para = vec, source = "vec2par")
      return(z)
    }

    if(! paracheck) return(z)

    if(! are.par.valid(z,nowarn,...)) {
      op <- options()
      if(nowarn == TRUE) options(warn=-1)
      warning("The parameters are invalid for the distribution")
      options(op)
      return()
    }
    return(z)
}
