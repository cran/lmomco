"hlmomco" <-
function(x,para) {
    if(! are.par.valid(para)) return();
    the.pdf <- dlmomco(x,para);
    the.cdf <- plmomco(x,para);
    return(the.pdf/(1 - the.cdf));
}
