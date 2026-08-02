## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 7,
                      fig.height = 3.4)

## -----------------------------------------------------------------------------
library(PeerPerformance)
data("hfdata")          # 60 monthly returns for 100 anonymized hedge funds
dim(hfdata)

## -----------------------------------------------------------------------------
set.seed(1234)
rets <- hfdata[, 1:15]
sc <- alphaScreening(rets, control = list(nCore = 1))
round(rbind(pipos = sc$pipos, pizero = sc$pizero, pineg = sc$pineg), 3)[, 1:6]

## -----------------------------------------------------------------------------
summary(sc, top = 3)

## -----------------------------------------------------------------------------
set.seed(1234)
round(confint(sc, parm = "pipos")[1:6, ], 3)

## ----screenfig----------------------------------------------------------------
plot(alphaScreening(hfdata[, 1:30], control = list(nCore = 1)))

## -----------------------------------------------------------------------------
round(sharpe(rets[, 1:6]), 3)
round(msharpe(rets[, 1:6], level = 0.95), 3)

## -----------------------------------------------------------------------------
res <- msharpeTesting(hfdata[, 1], hfdata[, 2], level = 0.95)
c(dmsharpe = res$dmsharpe, tstat = res$tstat, pval = res$pval)

## -----------------------------------------------------------------------------
focal <- hfdata[, 1]
peers <- hfdata[, 11:40]
res_xy <- alphaScreening(focal, Y = peers, control = list(nCore = 1))
round(c(pizero = res_xy$pizero, pipos = res_xy$pipos, pineg = res_xy$pineg), 3)

