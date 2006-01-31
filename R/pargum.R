"pargum" <-
function(lmom) {
   euler <- 0.577215664901532861
   para <- matrix(nrow = 2, ncol = 1);
   if(! are.lmom.valid(lmom)) {
     warning("L-moments are invalid.")
     return()
   } 
   para[2] <- lmom$L2/log(2) 
   para[1] <- lmom$L1-euler*para[2] 
   return(list(type = 'gum', para=para))
}

