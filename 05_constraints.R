# -------------------------------------------------------------------------
# 1. Loading libraries ----------------------------------------------------
# -------------------------------------------------------------------------

library(flexFitR)
library(dplyr)
library(ggpubr)
library(agriutilities)

# -------------------------------------------------------------------------
# 2. Dataset --------------------------------------------------------------
# -------------------------------------------------------------------------

data(dt_potato)
head(dt_potato)

# -------------------------------------------------------------------------
# 3. Green Leaf Index -----------------------------------------------------
# -------------------------------------------------------------------------

tmp <- explorer(dt_potato, x = DAP, y = c(GLI), id = Plot)
p1 <- plot(tmp, type = "evolution", return_gg = TRUE, add_avg = TRUE)
p2 <- plot(tmp, type = "x_by_var", return_gg = TRUE)
ggarrange(p1, p2, nrow = 1)

# summary
mutate_if(tmp$summ_vars, is.numeric, round, 2)

# Regression function
?fn_lin_pl_lin
plot_fn(
  fn = "fn_lin_pl_lin",
  params = c(t1 = 38, t2 = 62, t3 = 90, k = 0.3, beta = -0.01),
  interval = c(0, 108)
)
fn_lin_pl_lin <- function(t, t1, t2, t3, k, beta) {
  ifelse(
    test = t < t1,
    yes = 0,
    no = ifelse(
      test = t >= t1 & t <= t2,
      yes = k / (t2 - t1) * (t - t1),
      no = ifelse(
        test = t > t2 & t <= t3,
        yes = k,
        no = k + beta * (t - t3)
      )
    )
  )
}

# Reformulate function
?fn_lin_pl_lin2
function(t, t1, t2, dt, k, beta) {
  ifelse(
    test = t < t1,
    yes = 0,
    no = ifelse(
      test = t >= t1 & t <= t2,
      yes = k / (t2 - t1) * (t - t1),
      no = ifelse(
        test = t > t2 & t <= (t2 + dt),
        yes = k,
        no = k + beta * (t - (t2 + dt))
      )
    )
  )
}

# Define constraints and bounds for the model
lower_bounds <- c(t1 = 0, t2 = 0, dt = 0, k = 0, beta = -Inf)
upper_bounds <- c(t1 = Inf, t2 = Inf, dt = Inf, k = 1, beta = 0)
# Initial values
initial_vals <- c(t1 = 38, t2 = 62, dt = 28, k = 0.32, beta = -0.01)

# Model
mod_1 <- dt_potato |>
  modeler(
    x = DAP,
    y = GLI,
    grp = Plot,
    fn = "fn_lin_pl_lin2",
    parameters = initial_vals,
    lower = lower_bounds,
    upper = upper_bounds,
    method = c("nlminb", "L-BFGS-B"), # list_methods(bounds = TRUE)
    subset = c(2, 40)
  )
print(mod_1)
plot(mod_1, id = c(2, 40))
plot(mod_1, id = c(2, 40), type = 2, label_size = 10)
plot(mod_1, id = c(2, 40), type = 3) + color_palette("npg")
plot(mod_1, id = 40, type = 4)

# Coefficients
coef(mod_1)
confint(mod_1)

# Correlation between coefficients
vcov(mod_1)[[1]] |>
  cov2cor() |>
  covcor_heat()
vcov(mod_1)[[2]] |>
  cov2cor() |>
  covcor_heat()

# Performance
performance(mod_1, metrics = c("AIC", "BIC", "AICc"))

# AUC
aucs <- predict(object = mod_1, type = "auc")
aucs

# Find t3 = t2 + dt
fun_params <- predict(object = mod_1, formula = ~ t2 + dt)
fun_params

