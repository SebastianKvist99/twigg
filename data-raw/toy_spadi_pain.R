## code to prepare `toy_spadi_pain` dataset goes here

set.seed(501)

n <- 250  # sample size

# ---------------------------------------------------------
# Covariates
# ---------------------------------------------------------
sex <- sample(c("M", "F"), n, replace = TRUE, prob = c(0.45, 0.55))
age <- round(rnorm(n, mean = 55, sd = 12))
age[age < 18] <- 18 #+ abs(round(rnorm(1, mean = 0, sd = 1)))
age[age > 85] <- 85

# ---------------------------------------------------------
# Latent pain severity (theta)
# assume SPADI pain scores are higher in older adults
# and somewhat higher in females.
# ---------------------------------------------------------
theta <- rnorm(n,
               mean = 0.015*(age - 50) + ifelse(sex == "F", 0.25, 0),
               sd = 1)

# ---------------------------------------------------------
# Item parameters
# Discrimination values are moderate-to-high
# (consistent with SPADI pain item IRT analyses)
# ---------------------------------------------------------
a <- c(1.4, 1.2, 1.6, 1.3, 1.45)

# Difficulty parameters — higher = more severe pain required
b <- c(-0.1, 0.2, 0.4, 0.7, 1.0)

# ---------------------------------------------------------
# Generate 5 pain items with realistic correlations
# Using a logistic transformation mapped to 0–10 scale.
# ---------------------------------------------------------
items <- sapply(1:5, function(j) {

  # linear predictor
  eta <- a[j] * (theta - b[j])

  # logistic: convert to (0,1)
  p <- plogis(eta)

  # map to discrete 0–10
  score <- round(p * 10)

  # enforce bounds (just in case)
  pmin(pmax(score, 0), 10)
})

colnames(items) <- paste0("pain", 1:5)

# ---------------------------------------------------------
# Assemble data set
# ---------------------------------------------------------
toy_spadi_pain <- data.frame(
  sex = factor(sex),
  age = age,
  items
)


usethis::use_data(toy_spadi_pain, overwrite = TRUE)
