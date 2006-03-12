"par2lmom" <-
function(para) {
    type <- para$type
    if(type == 'cau')  {
      return(lmomcau(para))
    }
    else if(type == 'exp') {
      return(lmomexp(para))
    }
    else if(type == 'gam') {
      return(lmomgam(para))
    }
    else if(type == 'gev') {
      return(lmomgev(para))
    }
    else if(type == 'gld') {
      return(lmomgld(para))
    }
    else if(type == 'glo') {
      return(lmomglo(para))
    }
    else if(type == 'gno') {
      return(lmomgno(para))
    }
    else if(type == 'gpa') {
      return(lmomgpa(para))
    }
    else if(type == 'gum') {
      return(lmomgum(para))
    }
    else if(type == 'nor') {
      return(lmomnor(para))
    }
    else if(type == 'kap') {
      return(lmomkap(para))
    }
    else if(type == 'pe3') {
      return(lmompe3(para))
    }
    else if(type == 'wak') {
      return(lmomwak(para))
    }
    else {
      stop("Did not find a valid distribution type")
    }
}

