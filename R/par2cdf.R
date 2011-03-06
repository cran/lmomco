"par2cdf" <-
function(x,para,...) {
    type <- para$type
    if(type == 'cau') {
      return(cdfcau(x,para))
    }
    else if(type == 'exp') {
      return(cdfexp(x,para))
    }
    else if(type == 'texp') {
      return(cdftexp(x,para))
    }
    else if(type == 'gam') {
      return(cdfgam(x,para))
    }
    else if(type == 'gev') {
      return(cdfgev(x,para))
    }
    else if(type == 'gld') {
      return(cdfgld(x,para,...))
    }
    else if(type == 'glo') {
      return(cdfglo(x,para))
    }
    else if(type == 'gno') {
      return(cdfgno(x,para))
    }
    else if(type == 'gpa') {
      return(cdfgpa(x,para))
    }
    else if(type == 'gum') {
      return(cdfgum(x,para))
    }
    else if(type == 'kur') {
      return(cdfkur(x,para))
    }
    else if(type == 'ln3') {
      return(cdfln3(x,para))
    }
    else if(type == 'ray') {
      return(cdfray(x,para))
    }
    else if(type == 'rice') {
      return(cdfrice(x,para))
    }
    else if(type == 'revgum') {
      return(cdfrevgum(x,para))
    }
    else if(type == 'kap') {
      return(cdfkap(x,para))
    }
    else if(type == 'nor') {
      return(cdfnor(x,para))
    }
    else if(type == 'pe3') {
      return(cdfpe3(x,para))
    }
    else if(type == 'wak') {
      return(cdfwak(x,para))
    }
    else if(type == 'wei') {
      return(cdfwei(x,para))
    }
    else {
      stop("Did not find a valid distribution type")
    }
}

