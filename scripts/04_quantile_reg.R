

library(rms)
library(quantreg)

# Prepare data -----------------------------------------------------------------

  workDf <- # data

  d <- datadist(workDf)
  options(datadist = "d")

# Missing values ---------------------------------------------------------------

propMiss(workDf)

plot(naclus(workDf))
naplot(naclus(workDf))

na.pattern(workDf[, c("healthyGWG", "f26_fat_mass_kg", "f26_fat_free_mass_kg",
                      "f6_partcpt_bwgt", "f26_length")])

# Imputation -------------------------------------------------------------------

  formula <- as.formula(paste(" ~ ", paste(colnames(workDf)[-1], collapse = " +")))

  imputes <- aregImpute(formula, data = workDf, n.impute = 30, tlinear = FALSE,
                        nk = 5, burnin = 30)


  print(imputes)
  plot (imputes)


# Source code for outcome specfic models

  for (i in c(2:98)){

    qr <- fit.mult.impute(y ~ x, fitter = Rq, se = "ker", tau = i / 100,
                          imputes, workDf)

    ffma[[i]] <- data.frame(var = names,
                            b   = as.numeric(qr$coefficients),
                            se  = as.numeric(sqrt(diag(vcov(qr)))),
                            tau = as.numeric(i))

    print(i)
  }

  ffmaDf     <- as.data.frame(do.call(rbind, ffma))
  ffmaDf$out <- "FFM"


source("ffmQr.R")
source("fmQr.R")
source("lenQr.R")
source("gwQr.R")

source("combinedPlots.R")
source("qrTables.R")
source("summaryTable.R")






