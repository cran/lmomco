"quagum" <-
function(f,para) {
   if(! are.pargum.valid(para)) return()
   U <- para$para[1] 
   A <- para$para[2] 
   if(f <= 0 || f >= 1) {
     warning("nonexceedance probability value invalid")
     return()
   }
   return(U-A*log(-log(f)))
}

