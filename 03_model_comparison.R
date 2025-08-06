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
plot(tmp, type = "xy")

# (A) Logistic model
mod_1 <- dt_potato |>
  modeler(
    x = DAP,
    y = Canopy,
    grp = Plot,
    fn = "fn_logistic",
    parameters = c(a = 0.2, t0 = 48, k = 100),
    subset = c(1, 2, 3)
  )
print(mod_1)

plot(mod_1, id = 1:3, type = 1) # Raw data with fitted curves.
plot(mod_1, id = 1:3, type = 2, label_size = 10) # Coefficients
plot(mod_1, id = 1:3, type = 3) # Fitted curves colored by group.
plot(mod_1, id = 1:3, type = 4)

# (B) Linear-plateau model
plot_fn(fn = "fn_lin_plat", params = c(t1 = 35, t2 = 62, k = 100))

initials <- list(t1 = "min(x[y > 0])", t2 = 60, k = "max(y)")
mod_2 <- dt_potato |>
  modeler(
    x = DAP,
    y = Canopy,
    grp = Plot,
    fn = "fn_lin_plat",
    parameters = initials,
    subset = c(1, 2, 3)
  )
print(mod_2)

plot(mod_2, id = 1:3, type = 1) # Raw data with fitted curves.
plot(mod_2, id = 1:3, type = 2, label_size = 10) # Coefficients
plot(mod_2, id = 1:3, type = 3) # Fitted curves colored by group.

# (C) Linear-logistic model
plot_fn(fn = "fn_lin_logis", params = c(t1 = 35, t2 = 50, k = 100))

mod_3 <- dt_potato |>
  modeler(
    x = DAP,
    y = Canopy,
    grp = Plot,
    fn = "fn_lin_logis",
    parameters = c(t1 = 35, t2 = 50, k = 100),
    subset = c(1, 2, 3)
  )
print(mod_3)

plot(mod_3, id = 1:3, type = 1) # Raw data with fitted curves.
plot(mod_3, id = 1:3, type = 2, label_size = 10) # Coefficients
plot(mod_3, id = 1:3, type = 3) # Fitted curves colored by group.

# Performance
comp <- performance(mod_1, mod_2, mod_3)
comp
plot(comp, id = 1:3)

# Performance (subset of metrics)
comp <- performance(mod_1, mod_2, mod_3, metrics = c("AIC", "BIC", "AICc"))
comp
plot(comp, id = 1:3)

# Combining models
all <- c(mod_1, mod_2, mod_3)
all

plot(all, id = 1:2, type = 1, linewidth = 0.6) # Raw data + fitted curves.
plot(all, id = 1:3, type = 3, linewidth = 0.6) + color_palette("npg")

# -------------------------------------------------------------------------
# Best for some -----------------------------------------------------------
# -------------------------------------------------------------------------

id_1_lo <- subset(mod_1, id = 1)
id_2_lp <- subset(mod_2, id = 2)
id_3_ll <- subset(mod_3, id = 3)

new_object <- c(id_1_lo, id_2_lp, id_3_ll)
new_object

plot(new_object, id = 1:3)

# Predictions
predict(new_object, x = 45, type = "point") # Point Prediction
predict(new_object, x = 45, type = "point", se_interval = "confidence")
predict(new_object, x = 45, type = "point", se_interval = "prediction")
predict(new_object, x = c(0, 108), type = "auc") # AUC Prediction
predict(new_object, x = 45, type = "fd") # First Derivative
predict(new_object, x = 45, type = "sd") # Second Derivative
