# -------------------------------------------------------------------------
# 1. Installation ---------------------------------------------------------
# -------------------------------------------------------------------------

# Released version from CRAN:
install.packages("flexFitR")

# Install the development version of flexFitR from GitHub with:
# install.packages("devtools")
devtools::install_github("AparicioJohan/flexFitR")

# https://cran.r-project.org/web/packages/flexFitR/flexFitR.pdf
# https://apariciojohan.github.io/flexFitR/articles/
# Vignette 1: How to start
# Vignette 2: Modeling plant emergence and canopy growth using UAV data
# Vignette 3: Modeling with constraints
# Vignette 4: Making predictions
# Vignette 5: Plotting options

# -------------------------------------------------------------------------
# 2. Loading libraries ----------------------------------------------------
# -------------------------------------------------------------------------

library(tidyverse)
library(flexFitR)
library(ggpubr)

# -------------------------------------------------------------------------
# 3. Dataset --------------------------------------------------------------
# -------------------------------------------------------------------------

data(dt_potato)
head(dt_potato)

## Partially-replicated experiment (174/11 -> 1/2)
## Trial = Trial name (Hancock Agricultural Research Station UW-Madison)
## Plot = Experimental unit (196)
## Row & Range = Spatial coordinates (14 x 14)
## gid = Genotype identifier (185)
## DAP = Days after planting (0, 29, 36, 42, 56, 76, 92, 100)
## Canopy = Percentage of ground cover (%)
## GLI = Green leaf index (2*G-R-B)/(2*G+R+B)

# -------------------------------------------------------------------------
# 4. Exploration ----------------------------------------------------------
# -------------------------------------------------------------------------

tmp <- dt_potato |>
  explorer(
    x = DAP,
    y = c(Canopy, GLI),
    id = Plot,
    metadata = c(gid, Row, Range)
  )
names(tmp)
pluck(tmp, "summ_vars")
pluck(tmp, "summ_metadata")

# Field layout
dt_potato |>
  ggplot(aes(x = Row, y = Range, label = Plot)) +
  geom_tile(color = "white") +
  geom_text(color = "white")

# Plotting options
?plot.explorer
plot(tmp, type = "xy")
plot(tmp, type = "evolution", add_avg = TRUE)
plot(tmp, type = "var_by_x", n_row = 1)
plot(tmp, type = "x_by_var")

# -------------------------------------------------------------------------
# Regression functions and initials values --------------------------------
# -------------------------------------------------------------------------

list_funs()

# Logistic function
?fn_logistic
function(t, a, t0, k) {
  k / (1 + exp(-a * (t - t0)))
}

# Plotting functions and exploring parameters
?plot_fn
a1 <- plot_fn(fn = "fn_logistic", params = c(a = 0.2, t0 = 60, k = 100)) +
  ylim(c(NA, 100))
a1
a2 <- plot_fn(fn = "fn_logistic", params = c(a = 0.2, t0 = 60, k = 70)) +
  ylim(c(NA, 100))
a2
a3 <- plot_fn(fn = "fn_logistic", params = c(a = 0.2, t0 = 40, k = 100)) +
  ylim(c(NA, 100))
a3
a4 <- plot_fn(fn = "fn_logistic", params = c(a = 0.1, t0 = 40, k = 100)) +
  ylim(c(NA, 100))
a4
ggarrange(a1, a2, a3, a4)

# linear-plateau function
?fn_lin_plat
function(t, t1 = 45, t2 = 80, k = 0.9) {
  ifelse(
    test = t < t1,
    yes = 0,
    no = ifelse(t >= t1 & t <= t2, yes = k / (t2 - t1) * (t - t1), no = k)
  )
}
plot_fn(fn = "fn_lin_plat", params = c(t1 = 35, t2 = 62, k = 100))

# Quadratic function (linear-model)
plot_fn(fn = "fn_quad", params = c(a = 0.1, b = 5, c = -10))

# -------------------------------------------------------------------------
# Vectorized function vs Scalar function ----------------------------------
# -------------------------------------------------------------------------

categorize_scalar <- function(x) {
  if (x < 5) {
    "small"
  } else if (x <= 10) {
    "medium"
  } else {
    "large"
  }
}

categorize_scalar(x = c(3))
categorize_scalar(x = c(7))
categorize_scalar(x = c(3, 7, 11, 5, 1))

categorize_vectorized <- function(x) {
  ifelse(
    test = x < 5,
    yes = "small",
    no = ifelse(x <= 10, "medium", "large")
  )
}

categorize_vectorized(x = c(3))
categorize_vectorized(x = c(7))
categorize_vectorized(x = c(3, 7, 11, 5, 1))

# Logistic example -------------------------------------------------------

fn_logistic(t = 50, a = 0.2, t0 = 60, k = 100)
fn_logistic(t = c(50, 60, 80), a = 0.2, t0 = 60, k = 100)
