# README

Data analysis for Prokkola et al. (submitted in bioRxiv).
These scripts have only been used to analyse data in this study and **NO** guarantees are given for suitability in any other purposes.

R codes for analysis are in folder **Scripts**.

Required datasets are in folder **Datasets** – includes a README file, please read it before diving in. Permanent repository links for data will be added later.

##Required R packages:
Respirometry analysis:

[Fishresp] (https://cran.r-project.org/web/packages/FishResp/index.html)

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


## 1. Analysis of metabolic rate data 

i) Perform quality control and transform raw data into mg O2/kg/h oxygen consumption.

Scripts: `Metabrate_dataprep.R`,`Metabrate_slope_analysis.R`. 

Input files:  Separate file for each batch (32 batches), available in XXX (added later).

Sample information file: `Metab_sampledata_Prokkola_etal.txt`

ii)	Standard metabolic rate statistical analysis. 

Script: `Metabrate_average_analysis.R`

Input file:`Metabdata_Fishresp_Feb20.txt`.
 


##	 2. Statistical analysis of behaviour data
i)	The first behavior dataset: Angling selection experiment
Use the script `Behavdata_analysis.R`. Input file: `Behavdata_Prokkola_etal.txt`.

ii)	The second behavior dataset: Responses to predator olfactory cues. Use the script `ControlVsBurbot_analysis.R` and the input file `ControlVsBurbot_data.txt`. 

For both datasets, models are produced for three response variables: 

1. Exploration intensity (proportion of time spent swimming during the trial after emergence), modelled using a linear mixed model.
2. Latency to emerge from a shelter, which is inversely related to boldness/risk-taking behavior, modelled using a frailty model.
3. Exploration tendency (whether the fish entered a separate sector of the arena), modelled using a generalized linear mixed model on Bernoulli-distibuted data.

Finally, in `Behavdata_analysis.R`, we also calculate correlations between log **metabolic rate** - log body mass residuals (individual variation) **and** BLUPS from LMM of **exploration intensity and latency** in the individuals/trials where fish emerged from the shelter.

For details see the scripts and manuscript.

