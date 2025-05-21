# SUPER_GlobalFunctioning_SZ
This code was used in Giangrande et al. (2025, preprint): Polygenic Scores for Schizophrenia and Educational Attainment Predict Global Functioning Across Psychiatric Hospitalization Among People with Schizophrenia

This study examined the influence of polygenic scores for schizophrenia and educational attainment on global functioning at psychiatric admission and discharge, as well as functional change during hospitalization. Genotype, deep phenotype, and longitudinal hospitalization data (including global functioning scores collected at psychiatric admission and discharge) were drawn from the SUPER-Finland Study. 

gf_primaryLinearMixedEffectsModels.R: Fit linear-mixed effects models to longitudinal global functioning data. Plot PGS fixed effect estimates.

phenotypicAnalysesAndPlotting.R: Plot raw global functioning distributions. Estimate phenotypic associations between PGS and deep, clinically relevant phenotypic outcomes. Plot effects. 

sensitivityAnalysis.R: Fits same models as gf_primaryLinearMixedEffectsModels.R, stratified by Admission Global Functioning Tertile. 

bH_FDR_Correction.R: False-discovery rate correction for primary analysis. 
