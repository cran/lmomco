"lmr2par" <- function(x, type, ...) {
   lmom2par(lmoms(x), type, ...)
}


"lmom2par" <-
function(lmom,type,...) {
    if(type == 'aep4') {
      return(paraep4(lmom, ...))
    }
    else if(type == 'cau') {
      return(parcau(lmom, ...))
    }
    else if(type == 'emu') {
      return(paremu(lmom, ...))
    }
    else if(type == 'exp') {
      return(parexp(lmom, ...))
    }
    else if(type == 'gam') {
      return(pargam(lmom, ...))
    }
    else if(type == 'gep') {
      return(pargep(lmom, ...))
    }
    else if(type == 'gev') {
      return(pargev(lmom, ...))
    }
    else if(type == 'gld') {
      return(pargld(lmom,...))
    }
    else if(type == 'glo') {
      return(parglo(lmom, ...))
    }
    else if(type == 'gno') {
      return(pargno(lmom, ...))
    }
    else if(type == 'gov') {
      return(pargov(lmom, ...))
    }
    else if(type == 'gpa') {
      return(pargpa(lmom,...))
    }
    else if(type == 'gum') {
      return(pargum(lmom, ...))
    }
    else if(type == 'kap') {
      return(parkap(lmom, ...))
    }
    else if(type == 'kmu') {
      return(parkmu(lmom, ...))
    }
    else if(type == 'kur') {
      return(parkur(lmom, ...))
    }
    else if(type == 'ray') {
      return(parray(lmom, ...))
    }
    else if(type == 'rice') {
      return(parrice(lmom, ...))
    }
    else if(type == 'revgum') {
      return(parrevgum(lmom,...))
    }
    else if(type == 'lap') {
      return(parlap(lmom, ...))
    }
    else if(type == 'ln3') {
      return(parln3(lmom, ...))
    }
    else if(type == 'lmrq') {
      return(parlmrq(lmom, ...))
    }
    else if(type == 'nor') {
      return(parnor(lmom, ...))
    }
    else if(type == 'pe3') {
      return(parpe3(lmom, ...))
    }
    else if(type == 'pdq3') {
      return(parpdq3(lmom, ...))
    }
    else if(type == 'pdq4') {
      return(parpdq4(lmom, ...))
    }
    else if(type == 'sla') {
      return(parsla(lmom, ...))
    }
    else if(type == 'smd') {
      return(parsmd(lmom, ...))
    }
    else if(type == 'st3') {
      return(parst3(lmom, ...))
    }
    else if(type == 'texp') {
      return(partexp(lmom, ...))
    }
    else if(type == 'tri') {
      return(partri(lmom, ...))
    }
    else if(type == 'wak') {
      return(parwak(lmom, ...))
    }
    else if(type == 'wei') {
      return(parwei(lmom, ...))
    }
    else {
      stop("Did not find a valid distribution type")
    }
}

