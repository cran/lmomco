"lmom.diff" <-
function(lmomparm, lmomdata) {
   print("THE FIVE DIFFERENCES BETWEEN L-MOMENTS OF DISTRIBUTION AND DATA")
   print("Mean  L2   TAU3   TAU4   TAU5")
   L1diff <- lmomparm$L1 - lmomdata$L1
   L2diff <- lmomparm$L2 - lmomdata$L2
   T3diff <- lmomparm$TAU3 - lmomdata$TAU3
   T4diff <- lmomparm$TAU4 - lmomdata$TAU4
   T5diff <- lmomparm$TAU5 - lmomdata$TAU5
   print(c(L1diff,L2diff,T3diff,T4diff,T5diff))
   return(list(L1diff = L1diff, L2diff = L2diff, T3diff = T3diff,
               T4diff = T4diff, T5diff = T5diff))
}

