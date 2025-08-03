# reanalysis data in Zhang & Gläscher (2020) [10.1126/sciadv.abb4159]

#### prereq #### 
## download the Stanfit object from https://bit.ly/3kYIHyb, due to that Github cannot host large files
## save it to the same directory as this script

#### load data and pkgs####
required_packages <- c("tidyverse", "rstan")

# Check and install missing packages
install_and_Load <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
    }
    library(pkg, character.only = TRUE) # Load the required packages
  }
}

install_and_Load(required_packages)

f <- readRDS('m6b_winning.RData')

#### data preparation ####
f$data$Tsubj <- rep(100, 185) # add a number of valid trials per participant, we assume all data are valid trials

acc <- array(0, dim = c(185, 100, 4)) # calculate accuracy, we assume all trials are valid
for (t in 2:100) {
  # calculate the accuracy of the first three trials (coplayer's)
  if (t < 4) {
    for (i in 2:t) {
      acc[, t, ] <- acc[, t, ] + 1 / (t-1) * f$data$otherReward2[, i, ]
    }
    next
  }
  acc[, t, ] <- 1 / 3 * (
    f$data$otherReward2[, t-1, ] + 
      f$data$otherReward2[, t-2, ] +
      f$data$otherReward2[, t-3, ] )
}
f$data$acc <- acc # add the accuracy to the data

con <- array(0, dim = c(185, 100, 4)) # calculate the consistency
for (t in 2:100) {
  # calculate the consistency of the first three trials (coplayer's)
  if (t < 4) {
    for (i in 2:t) {
      con[, t, ] <- con[, t, ] + 1 / (t-1) * f$data$otherWith2[, i, ]
    }
    next
  }
  con[, t, ] <- 1 / 3 * (
    f$data$otherWith2[, t-1, ] + 
      f$data$otherWith2[, t-2, ] +
      f$data$otherWith2[, t-3, ] )
}
f$data$con <- con # add the consistency to the data

f$data$coplayer <- as.integer(4) # add a coplayer number

f$data$otherWith1 <- 1 * ( #transform the logical to 0/1
  (replicate(4, f$data$choice1)
   == f$data$otherChoice1)
)
# 1 if f$data$choice1 == f$data$otherChoice1, else 0

# =============================================================================
# model fit part revised from https://github.com/lei-zhang/BayesCog_Wien/blob/master/02.binomial_globe/_scripts/binomial_globe_main.R
rstan_options(auto_write = TRUE)
options(mc.cores = 4)

modelFile <- 'sit_m7b.stan'
nIter     <- 2000
nChains   <- 4 
nWarmup   <- floor(nIter/2)
nThin     <- 1

cat("Estimating", modelFile, "model... \n")
startTime = Sys.time(); print(startTime)
cat("Calling", nChains, "simulations in Stan... \n")

fit_globe <- rstan::stan(modelFile,
                         data    = f$data,
                         chains  = nChains,
                         iter    = nIter,
                         warmup  = nWarmup,
                         thin    = nThin,
                         init    = "random",
                         seed    = 1450154626)

cat("Finishing", modelFile, "model simulation ... \n")
endTime = Sys.time(); print(endTime)  
cat("It took",as.character.Date(endTime - startTime), "\n")

# save the fit object
saveRDS(fit_globe, file = 'm7b.RData')