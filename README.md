# SUPER_GlobalFunctioning_SZ
This code was used in Giangrande et al. (2025, preprint): Polygenic Scores for Schizophrenia and Educational Attainment Predict Global Functioning Across Psychiatric Hospitalization Among People with Schizophrenia. 
Preprint: https://www.medrxiv.org/content/10.1101/2025.05.20.25328039v1

This study examined the influence of polygenic scores for schizophrenia and educational attainment on global functioning at psychiatric admission and discharge, as well as functional change during hospitalization. Genotype, deep phenotype, and longitudinal hospitalization data (including global functioning scores collected at psychiatric admission and discharge) were drawn from the SUPER-Finland Study. For details on the SUPER-Finland study, see the Lähteenvuo et al. (2022) cohort profile paper (https://bmjopen.bmj.com/content/13/4/e070710), https://thl.fi/en/research-and-development/thl-biobank/for-researchers/sample-collections/super-study, and https://www.superfinland.fi/  

-gf_primaryLinearMixedEffectsModels.R: Fit linear-mixed effects models to longitudinal global functioning data. Plot PGS fixed effect estimates.

-phenotypicAnalysesAndPlotting.R: Plot raw global functioning distributions. Estimate phenotypic associations between PGS and deep, clinically relevant phenotypic outcomes. Plot effects. 

-sensitivityAnalysis.R: Fits same models as gf_primaryLinearMixedEffectsModels.R, stratified by Admission Global Functioning Tertile. 

-bH_FDR_Correction.R: False-discovery rate correction for primary analysis. 

  
