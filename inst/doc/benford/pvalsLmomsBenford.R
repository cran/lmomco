library(lmomco)
library(Lmoments)

nmom <- 6
MB <- NULL
SSall <- NULL
for(M in 2) {
  if(M == 1) {
    bens <- 1:9
    L1 <- 3.43908699617500524
    L2 <- 1.34518434179517077
    T3 <- 0.24794090889493661
    T4 <- 0.01614509742647182
  } else if(M == 2) {
    bens <- 10:99
    L1 <- 38.59062918136093145
    L2 <- 13.81767809210059283
    T3 <-  0.22237541787527126
    T4 <-  0.03541037418894027
  } else if(M == 3) {
    bens <- 100:999
    L1 <- 390.36783537821605705
    L2 <- 138.21917489739223583
    T3 <-   0.22192482374529940
    T4 <-   0.03571514686148788
  } else {
    stop("nothing to do")
  }
  L3 <- L2 * T3
  L4 <- L2 * T4
  para <- list(para=c(M, 10))
  probs <- pmfben(bens, para=para)
  bigos <- 1E5
  ns <- 6:30
  ns <- c(ns,               seq(32, 60, by=2   )             )
  ns <- c(ns,               seq(64, 99, by=3   )             )
  ns <- c(ns, as.integer(10^seq( 2,  3, by=0.05) /  10) *  10)
  ns <- c(ns, as.integer(10^seq( 3,  4, by=0.05) / 100) * 100)
  ns <- sort(unique(as.integer(ns)))
  crtsc <- c(0.90, 0.95, 0.99, 0.995, 0.999)
  SS <- NULL
  for(n in ns) {
    message(n, "-", appendLF=FALSE)
    if(length(grep("50$", as.character(n))) | length(grep("00$", as.character(n)))) message("")

    D <- NULL
    nsim <- bigos
    if(n <= 100) nsim <- nsim*10
    simix <- seq_len(nsim)
    for(i in simix) {
      x <- sample(bens, n, replace=TRUE, prob=probs)
      if(length(unique(x)) == 1) next
      l <- Lmoments::Lmoments(x, nmom)
      D <- c(D, (l[1]-L1)^2 + (l[2]-L2)^2 + (l[3] - L3)^2 + (l[4] - L4)^2)
      #D <- c(D,                              (l[3] - L3)^2 + (l[4] - L4)^2)
    }
    D <- 10^(log10(n*D/(L1+L2+L3+L4))-M)
    CRT <- quantile(D, probs=crtsc, type=6)
    Clmr <- lmomco::lmoms(D, nmom=6)
    SS  <- rbind(SS, data.frame(M=M, n=n,
               L1=l[1], L2=l[2], L3=l[3], L4=l[4], L5=l[5], L6=l[6],
              crt10 =CRT[1], crt05= CRT[2], crt01=CRT[3], crt005=CRT[4], crt001=CRT[5],
               DL1=Clmr$lambdas[1], DL2=Clmr$lambdas[2], DT3=Clmr$ratios[3],
               DT4=Clmr$ratios[4], DT5=Clmr$ratios[5], DT6=Clmr$ratios[6]))
  }
  SSall <- rbind(SSall, SS)
  crts <-  1 - crtsc

  Bs <- NULL
  plot( SS$n, SS$crt10,  col="black",     lty=2, lwd=0.8, type="p", log="xy",
       xlim=c(range(ns)), ylim=c(min(SS$crt10), max(SS$crt001)),
       xlab="Sample size drawn from Benford distribution",
       ylab="Critical values for squared-euclidean-Lmoment distance")
  points(SS$n, SS$crt05,  col="red"      , lty=2, lwd=0.8)
  points(SS$n, SS$crt01,  col="darkgreen", lty=2, lwd=0.8)
  points(SS$n, SS$crt005, col="blue"     , lty=2, lwd=0.8)
  points(SS$n, SS$crt001, col="salmon"   , lty=2, lwd=0.8)
  mtext(paste0("Critical values for first ", M, "-signficant digit",
               ifelse(M == 1, "", "s")), font=2, line=1)

  LM <- lm(log(SS$crt10)~I(log(SS$n))+I(log(SS$n)^-2))
  lines(SS$n, exp( fitted.values(LM) ), lwd=2, col="black"    )
  print(c(M, coefficients(LM)))
  Bs <- c(Bs, coefficients(LM)[1])

  LM <- lm(log(SS$crt05)~I(log(SS$n))+I(log(SS$n)^-2))
  lines(SS$n, exp( fitted.values(LM) ), lwd=2, col="red"      )
  print(c(M, coefficients(LM)))
  Bs <- c(Bs, coefficients(LM)[1])

  LM <- lm(log(SS$crt01)~I(log(SS$n))+I(log(SS$n)^-1))
  lines(SS$n, exp( fitted.values(LM) ), lwd=2, col="darkgreen")
  print(c(M, coefficients(LM)))
  Bs <- c(Bs, coefficients(LM)[1])

  LM <- lm(log(SS$crt005)~I(log(SS$n))+I(log(SS$n)^-1))
  lines(SS$n, exp( fitted.values(LM) ), lwd=2, col="blue"     )
  print(c(M, coefficients(LM)))
  Bs <- c(Bs, coefficients(LM)[1])

  LM <- lm(log(SS$crt001)~I(log(SS$n))+I(log(SS$n)^-1))
  lines(SS$n, exp( fitted.values(LM) ), lwd=2, col="salmon"   )
  print(c(M, coefficients(LM)))
  Bs <- c(Bs, coefficients(LM)[1])

  grv <- -log(-log(crts))

  LM <- lm(Bs~grv)
  mb <- coefficients(LM)
  print(c(9999, coefficients(LM)))
  #plot(grv, Bs)
  #abline(mb[1], mb[2])
  MB <- rbind(MB, data.frame(M=M, B=mb[1], M=mb[2]))
  my_crt <- 0.01
  my_sam <- 233
  points(my_sam, exp(mb[1]+mb[2]*(-log(-log(my_crt))))/my_sam, pch=16, cex=2, col="darkgreen")

  points(my_sam, exp((-2.6607150 + 4.6154937*M) - 1.217283*(-log(-log(my_crt))))/my_sam,
                 pch=16, cex=1, col="purple")
}

mean(MB$M)
plot(MB$M, MB$B)
LM <- lm(MB$B~MB$M)
print(coefficients(LM), 8)
my_crt <- 0.01
my_sam <- 233
points(my_sam, exp((-2.6607150 + 4.6154937*M) - 1.217283*(-log(-log(my_crt))))/my_sam, pch=16, cex=1, col="purple")

