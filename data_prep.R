Sys.setenv(TZ="UTC")
library(PerformanceAnalytics)

# Load the updated edhec dataset
load("data/edhec.rda")

# Prep data for examples
# Abreviate column names for convenience and plotting
colnames(edhec) <- c("CA", "CTAG", "DS", "EM", "EMN", "ED", "FIA", "GM", "LSE", "MA", "RV", "SS", "FoF")

