"lmom2par" <-
function(lmom,type,...) {
    if(type == 'exp') {
      return(parexp(lmom))
    }
    else if(type == 'gam') {
      return(pargam(lmom))
    }
    else if(type == 'gev') {
      return(pargev(lmom))
    }
    else if(type == 'gld') {
      return(pargld(lmom,...))
    }
    else if(type == 'glo') {
      return(parglo(lmom))
    }
    else if(type == 'gno') {
      return(pargno(lmom))
    }
    else if(type == 'gpa') {
      return(pargpa(lmom))
    }
    else if(type == 'gum') {
      return(pargum(lmom))
    }
    else if(type == 'kap') {
      return(parkap(lmom))
    }
    else if(type == 'nor') {
      return(parnor(lmom))
    }
    else if(type == 'pe3') {
      return(parpe3(lmom))
    }
    else if(type == 'wak') {
      return(parwak(lmom))
    }
    else if(type == 'wei') {
      return(parwei(lmom))
    }
    else {
      stop("Do not find a valid distribution type.")
    }
}

