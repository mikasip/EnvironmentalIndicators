# README for manuscript code: Identifiable Variational Autoencoders for Interpretable Dimension Reduction and Prediction in Multivariate Spatio-Temporal Processes

This repository contains all code and data required to reproduce the results from the manuscript:
Sipilä, M., Cappello, C., De Iaco, S., Nordhausen, K., Palma, M., Taskinen, S., 2025. Identifiable Variational Autoencoders for Interpretable Dimension Reduction and Prediction in Multivariate Spatio-Temporal Processes. Manuscript

In the application of the paper, weather and air pollution dynamics in Northen Italy are analyzed using identifiable variational autoencoders (iVAE). The required iVAE methods are implemented in R package `NonlinearBSS` (https://github.com/mikasip/NonlinearBSS).

## Repository Structure
1. The subfolders with prefix `env_ind_ivae` contain Keras model files for the individual models fitted in the case study.

2. The subfolder `models` contains R objects for the fitted models. These can be loaded using `load_with_tf` function from `NonlinearBSS` R package.

3. The file `EEA_sub_aux.RData` contains the combined spatio-temporal dataset of satellite derived and in-situ observed weather and air pollution variables in Northern Italy.

4. The file `environmental_indicators.R` contains the code for reproducing all results of the paper.
  
## Reproduction Introductions

1. Clone the repository
```
git clone https://github.com/mikasip/EnvironmentalIndicators.git
cd EnvironmentalIndicators
```

2. Install required R packages
```
devtools::install_github("mikasip/NonlinearBSS")
install.packages(c("tensorflow", "ggplot2", "kernelshap", "xtable", "viridis", "gridExtra"))
```

3. Run the analysis in `environmental_indicators.R`. The model fitting steps can be skipped and the models can be directly loaded from the model files.
