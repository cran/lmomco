"cdfrice" <- function(x, para=NULL) {
    f <- vector(mode="numeric")
    for(i in 1:length(x)) {
    	 if(x[i] < 0) {
    	 	f[i] <- 0
    	 } else if(x[i] == Inf) {
    	   f[i] <- 1
    	 } else {
          f[i] <- integrate(pdfrice,
                            0, x[i],
                            para=para)$value
        }
    }
    return(f)
}
