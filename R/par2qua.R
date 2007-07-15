"par2qua" <-
function(f,para,...) {
    type <- para$type
    if(type == 'cau') {
      return(quacau(f,para))
    }
    else if(type == 'exp') {
      return(quaexp(f,para))
    }
    else if(type == 'gam') {
      return(quagam(f,para))
    }
    else if(type == 'gev') {
      return(quagev(f,para))
    }
    else if(type == 'gld') {
      return(quagld(f,para,...))
    }
    else if(type == 'glo') {
      return(quaglo(f,para))
    }
    else if(type == 'gno') {
      return(quagno(f,para))
    }
    else if(type == 'gpa') {
      return(quagpa(f,para))
    }
    else if(type == 'gum') {
      return(quagum(f,para))
    }
    else if(type == 'revgum') {
      return(quarevgum(f,para,...))
    }
    else if(type == 'kap') {
      return(quakap(f,para))
    }
    else if(type == 'nor') {
      return(quanor(f,para))
    }
    else if(type == 'pe3') {
      return(quape3(f,para))
    }
    else if(type == 'wak') {
      return(quawak(f,para))
    }
    else if(type == 'wei') {
      return(quawei(f,para))
    }
    else {
      stop("Did not find a valid distribution type")
    }
}

