# Phenomics-flexFitR

This repository contains all materials for the flexFitR module of the course Applied Phenomics: From Data Acquisition to Decision-Making in Breeding, held from August 5th to 7th.

In this module, we focus on modeling plant growth and phenotypic traits using nonlinear regression methods implemented in the {flexFitR} R package. You'll learn how to fit, compare, and interpret nonlinear models for high-throughput phenotyping (HTP) data, particularly in the context of plant breeding.

## 🗂 Repository Contents

### `01_introduction.R`  

### `02_modeling.R`  

### `03_model_comparison.R`  
Compares model fits using metrics like AIC, AICc, BIC, RMSE, and others. Introduces the `performance()` function and visual tools (e.g., radar plots) for selecting the most suitable model per experimental unit.

### `04_advance_modeling.R`  
Covers advanced features such as:
- Estimating area under the curve (AUC)
- Computing derivatives (growth rates)
- Extracting biologically meaningful parameters
- Generating predicted values and standard errors

### `05_constraints.R`  
Illustrates how to apply constraints in model fitting—e.g., fixing asymptotes or initial growth values—using bounds in the optimizer. This helps incorporate biological knowledge or experimental design features into model estimation.

---

## 📁 Additional Files

- **`yield.csv`**  
  Example dataset with plot-level yield data. 
