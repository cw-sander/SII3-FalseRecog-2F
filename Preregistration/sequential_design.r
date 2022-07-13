library(rpact)
design <- getDesignGroupSequential(
    typeOfDesign = "asKD",
    kMax = 3,
    alpha = 0.05, beta = 0.2,
    gammaA = 2, gammaB = 2,
    bindingFutility = FALSE,
    sided = 1,
    tolerance = 1e-08,
    typeBetaSpending = "bsKD")

# Get nominal alpha bounds
design$stageLevels
# Get nominal futility bounds
1 - pnorm(design$futilityBounds)
# Get sample sizes
n_fixed_sample_design <- 352
infl_factor <- getDesignCharacteristics(design)$inflationFactor
round(n_fixed_sample_design * infl_factor / 3) # first interim analysis
round(n_fixed_sample_design * infl_factor / 3 * 2) # second interim analysis
round(n_fixed_sample_design * infl_factor) # final analysis
