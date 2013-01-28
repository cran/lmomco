"x2xlo" <-
function(x, leftout=0, a=0) {
    pp  <- pp(x, a=a, sort=FALSE)
   xin  <- x[x > leftout]
   xlo  <- x[x <= leftout]
   ppin <- pp[x > leftout]
   pplo <- pp[x <= leftout]
   pp <- ifelse(is.null(pplo), 0, max(pplo))
   z <- list(xin=xin,  ppin=ppin,
             xout=xlo, ppout=pplo, pp=max(pplo),
             source="x2xlo")
   return(z)
}
