.lmomcohash <- new.env(hash=TRUE)


################# RICE DISTRIBUTION ######################
# START
"Lhalf" <- function(x) {
   a <- x/2; I0 <- besselI(-a, n=0); I1 <- besselI(-a, n=1)
   return(exp(x/2)*((1-x)*I0 - x*I1))
}

"RiceCooker" <- function(v, factor=5) {
  SNR  <- seq(0+v/100,factor*v, by=v/100);
  lcvs <- vector(mode="numeric")
  zetaLhalf <- zeta <- thevs <- lcvs
  t3 <- t4 <- lcvs
  j <- 0
  for(snr in SNR) {
    j <- j + 1
    mypara <- vec2par(c(v,snr), type="rice")
    lmr <- lmomrice(mypara)
    if(is.null(lmr)) break
    lcv <- lmr$ratios[2]
    if(lcv <= 0) break
     lcvs[j] <- lcv
    thevs[j] <- v
     zeta[j] <- snr
       t3[j] <- lmr$ratios[3]
       t4[j] <- lmr$ratios[4]
    zetaLhalf[j] <- sqrt(pi/2)*Lhalf(-zeta[j]^2/2)
    cat(c(lcvs[j], zeta[j], zetaLhalf[j], "\n"))
  }
  z <- list(LCV=lcvs, F.of.LCV = zeta, G.of.LCV=zetaLhalf, t3=t3, t4=t4)
  return(z)
}

try1 <- RiceCooker(1)
try2 <- RiceCooker(10)
try3 <- RiceCooker(100)

LCV <- c(try1$LCV,try2$LCV,try3$LCV)
SNR <- c(try1$F.of.LCV,try2$F.of.LCV,try3$F.of.LCV)
G.of.LCV <- c(try1$G.of.LCV,try2$G.of.LCV,try3$G.of.LCV)

xlim <- range(c(try1$LCV,try2$LCV,try3$LCV))
ylim <- range(c(try1$F.of.LCV,try2$F.of.LCV,try3$F.of.LCV))
plot(try1$LCV, try1$F.of.LCV, ylim=ylim, xlim=xlim,lwd=5,col=2,type="l")
lines(try2$LCV, try2$F.of.LCV, lwd=3,col=3)
lines(try3$LCV, try3$F.of.LCV, lwd=1,col=4)


ylim <- range(c(try1$G.of.LCV,try2$G.of.LCV,try3$G.of.LCV))
plot(try1$LCV, try1$G.of.LCV, ylim=ylim, xlim=xlim,lwd=5,col=2,type="l")
lines(try2$LCV, try2$G.of.LCV, lwd=3,col=3)
lines(try3$LCV, try3$G.of.LCV, lwd=1,col=4)

plot(c(try1$G.of.LCV,try2$G.of.LCV,try3$G.of.LCV),
     c(try1$F.of.LCV,try2$F.of.LCV,try3$F.of.LCV))


ray <- (.5*(sqrt(2)-1)*sqrt(pi))/(sqrt(pi/2))
LCVest <- seq(as.numeric(sprintf("%0.3f",min(LCV))), ray, by=.001)
LCVest <- c(LCVest,ray)
RiceNomo <- data.frame(LCV=LCV, SNR=SNR, G=G.of.LCV)
idx <- order(RiceNomo$LCV)
RiceNomo <- RiceNomo[idx,]
row.names(RiceNomo) <- NULL

SNRest <- approx(RiceNomo$LCV, RiceNomo$SNR, xout=LCVest, rule=2)$y
Gest   <- approx(RiceNomo$LCV, RiceNomo$G.of.LCV, xout=LCVest, rule=2)$y
RiceLCVnomo <- data.frame(LCV=LCVest, SNR=SNRest, G=Gest)

.RiceTable <- RiceNomo
assign("RiceTable", .RiceTable, .lmomcohash)


T3 <- c(try1$t3,try2$t3,try3$t3)
T4 <- c(try1$t4,try2$t4,try3$t4)

T3seq <- seq(mylmr$nor[1],mylmr$ray[1],by=0.001)
T3seq <- c(T3seq,mylmr$ray[1])
T4seq <- approx(T3,T4, xout=T3seq)$y
.RiceT3T4 <- data.frame(TAU3=T3seq, TAU4=T4seq)
assign("RiceT3T4", .RiceT3T4, .lmomcohash)
# END
################# RICE DISTRIBUTION ######################


save(.lmomcohash, file="sysdata.rda");


