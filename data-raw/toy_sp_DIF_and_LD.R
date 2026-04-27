## code to prepare `toy_sp_DIF_and_LD` dataset goes here
set.seed(501)
n <- 250

sex <- sample(c("M", "F"), n, replace = TRUE, prob = c(0.45, 0.55))
age <- round(rnorm(n, mean = 55, sd = 12))
age[age < 18] <- 18
age[age > 85] <- 85

theta <- rnorm(n,
               mean = 0.015*(age - 50) + ifelse(sex == "F", 0.25, 0),
               sd = 1)

a <- c(1.4, 1.2, 1.6, 1.3, 1.45)
b <- c(-0.1, 0.2, 0.4, 0.7, 1.0)

# -------------------------
# LD latent factors
# -------------------------
ld_12 <- rnorm(n, 0, 0.6)
ld_34 <- rnorm(n, 0, 0.5)

items <- sapply(1:5, function(j) {

  eta <- a[j] * (theta - b[j])

  # -------------------------
  # DIF structure
  # -------------------------
  # Uniform DIF - items easier for females
  if (j == 2) {
    eta <- eta + ifelse(sex == "F", 0.6, 0)
  }

  # Age DIF
  if (j == 5) {
    eta <- eta + 0.025 * (age - 50)
  }

  # -------------------------
  # LOCAL DEPENDENCE
  # -------------------------
  if (j %in% c(1, 2)) {
    eta <- eta + ld_12
  }

  if (j %in% c(3, 4)) {
    eta <- eta + ld_34
  }

  # map to (0,1)
  p <- plogis(eta)
  # discitize into 0-10 scale
  score <- round(p * 10)
  # enforce bounds
  pmin(pmax(score, 0), 10)
})

colnames(items) <- paste0("pain", 1:5)

toy_sp_DIF_and_LD <- data.frame(
  sex = factor(sex),
  age = age,
  items
)


usethis::use_data(toy_sp_DIF_and_LD, overwrite = TRUE)
