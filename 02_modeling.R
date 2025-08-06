# -------------------------------------------------------------------------
# 1. Loading libraries ----------------------------------------------------
# -------------------------------------------------------------------------

library(tidyverse)
library(flexFitR)
library(ggpubr)

# -------------------------------------------------------------------------
# 2. Dataset --------------------------------------------------------------
# -------------------------------------------------------------------------

data(dt_potato)
head(dt_potato)

# -------------------------------------------------------------------------
# 3. Modeling ground cover ------------------------------------------------
# -------------------------------------------------------------------------

tmp <- dt_potato |>
  explorer(
    x = DAP,
    y = Canopy,
    id = Plot,
    metadata = c(gid, Row, Range)
  )
names(tmp)
pluck(tmp, "summ_vars")
pluck(tmp, "summ_metadata")
plot(tmp, type = "xy")
plot(tmp, type = "xy", id = c(1, 2))

list_funs()

# Logistic model fitting
mod_1 <- dt_potato |>
  modeler(
    x = DAP,
    y = Canopy,
    grp = Plot,
    fn = "fn_logistic",
    parameters = c(a = 0.2, t0 = 48, k = 100),
    subset = c(1, 2)
  )
print(mod_1)
update(mod_1)

# Summary fitted models
print(mod_1)
names(mod_1)
pluck(mod_1, "param")
pluck(mod_1, "fit")

# Plotting
?plot.modeler
plot(mod_1, id = 1:2, type = 1) # Raw data with fitted curves.
plot(mod_1, id = 1:2, type = 2, label_size = 10) # Coefficients
plot(mod_1, id = 1:2, type = 3) # Fitted curves colored by group.
plot(mod_1, id = 1:2, type = 4) # Fitted curves with confidence intervals
plot(mod_1, id = 1:2, type = 5) # First-derivative
plot(mod_1, id = 1:2, type = 6) # Second-derivative

# Coefficients and standard errors
coef(mod_1)
confint(mod_1)

# Variance-covariance matrices
var_covs <- vcov(mod_1)
var_covs
cov2cor(var_covs$`1`)
cov2cor(var_covs$`2`)

# Fitted values and residuals
fitted(mod_1)
residuals(mod_1)
augment(mod_1)

# Performance
performance(mod_1)

# Predictions
?predict.modeler
predict(mod_1, x = 45, type = "point") # Point Prediction
predict(mod_1, x = 45, type = "point", se_interval = "confidence")
predict(mod_1, x = 45, type = "point", se_interval = "prediction")
predict(mod_1, x = c(0, 108), type = "auc") # AUC Prediction
predict(mod_1, x = 45, type = "fd") # First Derivative
predict(mod_1, x = 45, type = "sd") # Second Derivative
predict(mod_1, formula = ~ (a * k) / 4) # Function of the parameters
?inverse_predict.modeler
inverse_predict(mod_1, y = 50)
inverse_predict(mod_1, y = 90)

# Tangent line
?compute_tangent
tang_line <- compute_tangent(mod_1, x = 50)
tang_line

plot(mod_1, id = 1:2) +
  geom_abline(
    data = tang_line,
    mapping = aes(slope = slope, intercept = intercept),
    linetype = 2,
    color = "blue"
  ) +
  geom_point(
    data = tang_line,
    mapping = aes(x = x, y = y),
    shape = 8,
    color = "blue",
    size = 2
  )

# Tangent line at inflection point
dt_ip <- mod_1 |>
  pluck("param") |>
  mutate(x = t0) |>
  compute_tangent(object = mod_1)
dt_ip

plot(mod_1, id = 1:2) +
  geom_abline(
    data = tl_inflec_point,
    mapping = aes(slope = slope, intercept = intercept),
    linetype = 2,
    color = "blue"
  ) +
  geom_point(
    data = dt_ip,
    mapping = aes(x = x, y = y),
    shape = 8,
    color = "blue",
    size = 2
  )

# Sub-setting
sub_id_2 <- subset(mod_1, id = 2)
sub_id_2
sub_id_1 <- subset(mod_1, id = 1)
sub_id_1

# Combining
all <- c(sub_id_1, sub_id_2)
all

# -------------------------------------------------------------------------
# List of flexFitR functions used -----------------------------------------
# -------------------------------------------------------------------------

# 1. explorer
# 2. plot.explorer
# 3. modeler
# 4. update.modeler
# 5. plot.modeler
# 6. coef.modeler
# 7. confint.modeler
# 8. vcov.modeler
# 9. fitted.modeler
# 10. residuals.modeler
# 11. augment.modeler
# 12. performance.modeler
# 13. predict.modeler
# 14. inverse_predict.modeler
# 15. compute_tangent.modeler
# 16. subset.modeler
# 17. c.modeler
