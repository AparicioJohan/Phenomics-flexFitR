# -------------------------------------------------------------------------
# 1. Loading libraries ----------------------------------------------------
# -------------------------------------------------------------------------

library(tidyverse)
library(flexFitR)
library(agriutilities)
library(FactoMineR)
library(factoextra)
library(ggpubr)
library(progressr)
handlers("progress")

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

# Linear-plateau function
plot_fn(
  fn = "fn_lin_plat",
  params = c(t1 = 35, t2 = 62, k = 100),
  interval = c(0, 108),
  n_points = 2000,
  auc_label_size = 3
)

# Dynamic initialization of starting values
initials <- list(t1 = "min(x[y > 0])", t2 = 60, k = "max(y)")

# Model
mod_1 <- dt_potato |>
  modeler(
    x = DAP,
    y = Canopy,
    grp = Plot,
    keep = c(Trial, gid, Row, Range), # For further analysis
    fn = "fn_lin_plat",
    parameters = initials,
    options = list(parallel = TRUE, workers = 4)
  ) |>
  with_progress(handlers = handlers("cli"))
print(mod_1)

plot(mod_1, id = 1:10, type = 1) # Raw data with fitted curves.
plot(mod_1, id = 1:10, type = 2, label_size = 10) # Coefficients
plot(mod_1, id = 1:196, type = 3) # Fitted curves colored by group.

# -------------------------------------------------------------------------
# Deriving growth traits --------------------------------------------------
# -------------------------------------------------------------------------

# Parameters
params <- mod_1 |>
  pluck("param") |>
  select(-sse, -fn_name) |>
  rename(emergence_time = t1, time_to_max = t2, max_canopy = k)

# Expansion time
expansion_time <- mod_1 |>
  predict(formula = ~ t2 - t1, metadata = TRUE) |>
  rename(expansion_time = predicted.value) |>
  select(Trial, uid, gid, Row, Range, expansion_time)
expansion_time

# Growth rate
growth_rate <- mod_1 |>
  predict(formula = ~ k / (t2 - t1), metadata = TRUE) |>
  rename(growth_rate = predicted.value) |>
  select(Trial, uid, gid, Row, Range, growth_rate)
growth_rate

# Area under the curve summary statistic
aucs <- mod_1 |>
  predict(type = "auc", metadata = TRUE) |>
  rename(auc = predicted.value) |>
  select(Trial, uid, gid, Row, Range, auc)
aucs

# All growth-derived traits
phenotypes <- full_join(
  x = full_join(expansion_time, params),
  y = full_join(growth_rate, aucs),
  by = c("Trial", "uid", "gid", "Row", "Range")
)
phenotypes

# Yield data
yield <- read.csv("yield.csv")
head(yield)
phenotypes <- full_join(phenotypes, yield)

traits <- c(
  "expansion_time",
  "emergence_time",
  "time_to_max",
  "max_canopy",
  "growth_rate",
  "auc",
  "Yield"
)

# -------------------------------------------------------------------------
# Heritability ------------------------------------------------------------
# -------------------------------------------------------------------------

dt <- check_design_met(
  data = phenotypes,
  genotype = "gid",
  trial = "Trial",
  traits = traits,
  row = "Row",
  col = "Range"
)
print(dt)
plot(dt, type = "boxplot")

# Single trial analysis - Spatial model
sta <- single_trial_analysis(results = dt)
sta

# Heritability
sta$resum_fitted_model

# Plot spatial variation
plot(sta$fitted_models$auc$HARS20_chips$mRand$auc)
plot(sta$fitted_models$Yield$HARS20_chips$mRand$Yield)
plot(sta$fitted_models$emergence_time$HARS20_chips$mRand$emergence_time)

# Best Linear Unbiased Predictors (BLUPs)
BLUPs <- sta |>
  pluck("blues_blups") |>
  select(trait, genotype, BLUPs) |>
  spread(trait, BLUPs) |>
  column_to_rownames("genotype")
BLUPs

# Correlations
gg_cor(BLUPs, label_size = 3.5)

# PCA
res_pca <- PCA(BLUPs)
res_pca

fviz_pca_ind(res_pca)
fviz_pca_biplot(res_pca)
