load("USGSsta05405000peaks.RData")
USGSsta05405000peaks <- USGSsta05405000peaks[,-c(14)]
save(USGSsta05405000peaks,file="USGSsta05405000peaks.RData")


load("USGSsta08167000peaks.RData")
USGSsta08167000peaks <- USGSsta08167000peaks[,-c(14)]
save(USGSsta08167000peaks,file="USGSsta08167000peaks.RData")


load("USGSsta08190000peaks.RData")
USGSsta08190000peaks <- USGSsta08190000peaks[,-c(14)]
save(USGSsta08190000peaks,file="USGSsta08190000peaks.RData")
