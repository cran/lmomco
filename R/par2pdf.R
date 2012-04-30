"par2pdf" <-
function(f,para,...) {
    type <- para$type
    if(type == 'cau') {
      return(pdfcau(f,para))
    }
    else if(type == 'exp') {
      return(pdfexp(f,para))
    }
    else if(type == 'texp') {
      return(pdftexp(f,para))
    }
    else if(type == 'gam') {
      return(pdfgam(f,para))
    }
    else if(type == 'gev') {
      return(pdfgev(f,para))
    }
    else if(type == 'gld') {
      return(pdfgld(f,para,...))
    }
    else if(type == 'glo') {
      return(pdfglo(f,para))
    }
    else if(type == 'gno') {
      return(pdfgno(f,para))
    }
    else if(type == 'gpa') {
      return(pdfgpa(f,para))
    }
    else if(type == 'gum') {
      return(pdfgum(f,para))
    }
    else if(type == 'kur') {
      return(pdfkur(f,para))
    }
    else if(type == 'lap') {
      return(pdflap(f,para))
    }
    else if(type == 'ln3') {
      return(pdfln3(f,para))
    }
    else if(type == 'ray') {
      return(pdfray(f,para))
    }
    else if(type == 'rice') {
      return(pdfrice(f,para))
    }
    else if(type == 'revgum') {
      return(pdfrevgum(f,para))
    }
    else if(type == 'kap') {
      return(pdfkap(f,para))
    }
    else if(type == 'nor') {
      return(pdfnor(f,para))
    }
    else if(type == 'pe3') {
      return(pdfpe3(f,para))
    }
    else if(type == 'wak') {
      return(pdfwak(f,para))
    }
    else if(type == 'wei') {
      return(pdfwei(f,para))
    }
    else {
      stop("Did not find a valid distribution type")
    }
}

