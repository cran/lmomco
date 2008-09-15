"prettydist" <-
function(x) {
  getdist <- function(x) {
     if(x == "cau") return("Cauchy")
     if(x == "exp") return("Exponential")
     if(x == "gam") return("Gamma")
     if(x == "gev") return("Generalized Extreme Value")
     if(x == "gld") return("Generalized Lambda")
     if(x == "glo") return("Generalized Logistic")
     if(x == "gno") return("Generalized Normal")
     if(x == "gpa") return("Generalized Pareto")
     if(x == "gum") return("Gumbel")
     if(x == "kap") return("Kappa")
     if(x == "nor") return("Normal")
     if(x == "pe3") return("Pearson Type III")
     if(x == "ray") return("Rayleigh")
     if(x == "revgum") return("Reverse Gumbel")
     if(x == "wak") return("Wakeby")
     if(x == "wei") return("Weibull")
  }
  return(sapply(x,getdist))
}
