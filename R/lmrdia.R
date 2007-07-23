"lmrdia" <-
function() {
   step = 0.005
   n = 1
   lim <- matrix(nrow = 401, ncol = 2)
   gpa <- matrix(nrow = 401, ncol = 2)
   for(t3 in seq(-1,1,step)) {
     lim[n,1] = t3
     lim[n,2] = 0.25*(5*t3^2 - 1)
     gpa[n,1] = t3
     gpa[n,2] = (t3*(1+5*t3))/(5+t3)
     n = n + 1
   }
   n = 1
   gev <- matrix(nrow = 582, ncol = 2)
   for(k in seq(-1,1,step)) {
     h = -k
     gev[n,1] = 2*(1-3^h)/(1-2^h) - 3
     gev[n,2] = (5*(1-4^h)-10*(1-3^h)+6*(1-2^h))/(1-2^h)
     n = n + 1
   }
   for(k in seq(1-step,10,0.05)) {
     h = -k
     gev[n,1] = 2*(1-3^h)/(1-2^h) - 3
     gev[n,2] = (5*(1-4^h)-10*(1-3^h)+6*(1-2^h))/(1-2^h)
     n = n + 1
   }
   n = 1
   glo <- matrix(nrow = 401, ncol = 2)
   for(k in seq(-1,1,step)) {
     glo[n,1] = -k
     glo[n,2] = (1+5*k^2)/6
     n = n + 1
   }

   n = 1
   pIII <- matrix(nrow = 361, ncol = 2)
   for(t3 in seq(-.9,.9,step)) {
     pIII[n,1] = t3
     pIII[n,2] = 0.1224+0.30115*t3^2+0.95812*t3^4-0.57488*t3^6+0.19383*t3^8
     n = n + 1
   }

   n = 1
   ln <- matrix(nrow = 361, ncol = 2)
   for(t3 in seq(-.9,.9,step)) {
     ln[n,1] = t3
     ln[n,2] = 0.12282+0.77518*t3^2+0.12279*t3^4-0.13638*t3^6+0.11368*t3^8
     n = n + 1
   }


   exp <- matrix(nrow = 1, ncol = 2)
   exp[1,] <- c(1/3,1/6)
   gum <- matrix(nrow = 1, ncol = 2)
   gum[1,] <- c(log(9/8)/log(2),(16*log(2)-10*log(3))/log(2))
   nor <- matrix(nrow = 1, ncol = 2)
   nor[1,] <- c(0,30*pi^-1*atan(sqrt(2))-9)
   uni <- matrix(nrow = 1, ncol = 2)
   uni[1,] <- c(0,0)
   z <- list(limits = lim, exp=exp, gev = gev, glo = glo,
             gpa=gpa, gum=gum, gno=ln, nor=nor,
             pe3=pIII, uniform=uni)
   return(z)
}

