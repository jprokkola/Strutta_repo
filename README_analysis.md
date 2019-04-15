# README

Data analysis for Prokkola et al. 2018 (submitted in bioRxiv).
These scripts have only been used to analyse data in this study and **NO** guarantees are given for suitability in any other purposes.

R codes for analysis are in folder **Scripts**.

Required datasets are in folder **Datasets** – includes a README file, please read it before diving in.

##Required R libraries:
R package for ELISA absorbance value analysis:

[drc] (https://www.rdocumentation.org/packages/drc/versions/3.0-1)

Statistical models:

[lme4] (https://www.rdocumentation.org/packages/lme4/versions/1.1-18-1)

[lmerTest] (https://www.rdocumentation.org/packages/lmerTest/versions/3.0-1)

[lmfor] (https://www.rdocumentation.org/packages/lmfor/versions/1.2)

[nlme] (https://www.rdocumentation.org/packages/nlme/versions/3.1-137) 

[survival] (https://www.rdocumentation.org/packages/survival/versions/2.43-1)

[coxme] (https://www.rdocumentation.org/packages/coxme/versions/2.2-10)

[ggeffects] (https://github.com/strengejacke/ggeffects)

[car] (https://cran.r-project.org/web/packages/car/car.pdf)

Plots: 

[ggplot2] (https://www.rdocumentation.org/packages/ggplot2/versions/3.1.0)

[patchwork] (https://github.com/thomasp85/patchwork)


##Statistical analysis of metabolic rate data 
i)	Minimum metabolic rate. Data has been prepared in AV Biostatistics v. 5.2 

ii)	Average metabolic rate. This is analysed as it reflects stress sensitivity (general unhappiness with being confined in the respirometer chamber).

Use the script `Metabrate_Cort_analyses_browntrout.R`. Inputs include metabolic rate data from `Metabdata_Prokkola_etal.txt` and `Plasma_cort_results.txt`. Conduct analysis, check assumptions of analysis, and plot predicted values with confidence intervals. Details provided in the script and manuscript.
##	Statistical analysis of behaviour data
i)	The first behavior dataset: Angling selection experiment
Use the script `Behavdata_analysis.R`. Input file: `Behavdata_Prokkola_etal.txt`.

ii)	The second behavior dataset: Responses to predator olfactory cues. Use the script `ControlVsBurbot_analysis.R` and the input file `ControlVsBurbot_data.txt`. 

For both datasets, models are produced for three response variables: 

1. Activity (proportion of time spent swimming during the trial), modelled using a linear mixed model.
2. Boldness (latency to emerge from a shelter), modelled using a frailty model.
3. Exploration tendency (whether the fish entered a separate sector of the arena), modelled using a generalized linear mixed model on Bernoulli-distibuted data.

Finally, in `Behavdata_analysis.R`, we also calculate correlations between metabolic rate residuals (individual variation) and BLUPS from LMM of activity.

The scripts include the analysis, checking assumptions of analysis, and plotting results/predictions. Details are included in the script and publication.

