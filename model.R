library(lme4)

d <- read.table("sane-ashi.txt", sep=" ", header=T, colClasses=list(pnro="factor"))
d <- transform(d, 
             level1=as.factor(substr(pnro, 1, 3)),
             level2=as.factor(substr(pnro, 1, 2)),
             level3=as.factor(substr(pnro, 1, 1)), 
             yr=year-2013)

# zip level trend does not fit with lme4. 
m <- lmer(log(price) ~ yr  + (1|pnro) + (1+yr|level1) + (1+yr|level2) + (1|level3), data=d, weights=d$n)

zero.na <- function (x) { x[is.na(x)]=0; x}

pred <- function (m, pnro) {
    zero.na(ranef(m)$pnro[pnro,]) +
    zero.na(ranef(m)$level1[substr(pnro, 1, 3), 1]) +
    zero.na(ranef(m)$level2[substr(pnro, 1, 2), 1]) +
    zero.na(ranef(m)$level3[substr(pnro, 1, 1), 1])
}

tpred <- function (m, pnro) {
    zero.na(ranef(m)$level1[substr(pnro, 1, 3), 2]) +
    zero.na(ranef(m)$level2[substr(pnro, 1, 2), 2]) 
}

# Oops, FIXME.
d.pono <- read.csv("../Ruutudata/rttk2012_250_ponot.csv", sep=";", header=T, colClasses="character")
pnrot <- unique(d.pono$PNRO)
write.table(data.frame(pnro=pnrot, hintataso=exp(pred(m, pnrot)), trendi=tpred(m, pnrot))[
              order(pnrot),], 
            "pnro-hinnat.txt", row.names=F, quote=F)
