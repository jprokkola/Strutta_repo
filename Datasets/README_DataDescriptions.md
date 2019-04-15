#README
Descriptions for datasets in this folder.

File names shown like `this`.

## Dataset 1 – Metabolic rate
`Metabdata_Prokkola_etal.txt`

> Tab-separated, dots as decimal separators. Data for oxygen consumption, one value for minimum and average consumption for each fish. The raw data obtained from Loligo intermittent flow respirometer were pre-analysed in [AV Bio-Statistics v.5.2 software] (http://www.kotikone.fi/ansvain/official.html) (for use, consult Anssi Vainikka). Column names and contents:

**Sample_ID** = ID used in blood and DNA sample collection.
	
**PIT** = PIT tag ID
	
**Fish_ID** = Number of individual from 1 to 100, used in analysing behaviour videos.
	
**group** = Population and selection line combined to one group. Used in plots.
	
**Sex** = Fish sex, -1 = female, 1 = male. Identified with a PCR assay (sex not available for all fish).
           
**Photoperiod** = Light rhythm where fish was acclimated in before and during the experiments. 12:12 = 12h light, 12h dark (from 7pm to 7am), 24 = constant light.
	
**pop** = Fish population, Hatchery = Lake Oulujärvi hatchery broodstock, Wild = River Vaarainjoki (parents captured from the wild).
	
**selection** = Angling selection line within population. hv = parents with high vulnerability to angling, lv = parents with low vulnerability to angling. 
	
**subgroups** = Photoperiod and group combined into one grouping.
	
**Metab_chamber** = Respirometer chamber during measurement (1–4).
	
**Mass\_g_postMR** = Fish wet weight immediately after respirometry in grams.
	
**Ave\_O2\_mg_h** = The average oxygen consumption of all measurement cycles during the whole measurement period, except the first and last cycle. In mg oxygen per h.
 
**Min\_O2\_mg_h** = The minimum oxygen consumption obtained as the average of four least negative slopes after excluding the first, last and the least negative slope.
	
**Temp** = Average water temperature of the respirometer during the measurements for each fish. 

## Dataset 2 – Behavioural data from angling selection lines

`Behavdata_Prokkola_etal.txt`
> Tab-separated, dots as decimal separators. Behavior experiment data including latency, activity and exploration values from three trials for (almost) all individuals. The data were produced by manual recording from video files using [AV Bio-Statistics v.5.2 software] (http://www.kotikone.fi/ansvain/official.html) (for use, consult Anssi Vainikka). 

**Sample** = ID used in blood and DNA sample collection.

**ID** = Number of individual, between 1 and 100.

**PIT** = PIT tag ID

**Group** = Population and selection line combined to one group.

**Pop** = Fish population, same as above
	
**Selection** = Angling selection line within population. hv = parents with high vulnerability to angling, lv = parents with low vulnerability to angling. 

**Photoperiod** = Light rhythm where fish was acclimated in before and during the experiments. 12:12 = 12h light, 12h dark (from 7pm to 7am), 24 = constant light.

**Sex** = Fish sex, -1 = female, 1 = male. Identified with a PCR assay (not available for all fish).

**Weight_end** = Fish body mass after all tests (measured between 26th and 29th June 2017).

**Length_end** = Fish whole body length after all tests (measured between 26th and 29th June 2017).

**Trial** = Combination of ID and behaviour trial repeat 1, 2 or 3.

**Repeat** = One of three trials for each fish, coded as -1 (the first), 0 (the second) and 1 (the third).

**Arena** = The ID of the arena where each behaviour trial was done, 1-4.

**Beh_batch** = Batch of fish taken from the same tank in the same day, 1-5.

**Out\_time_min** = Time until the whole body of fish emerged from the start box at the start of behaviour trial, in minutes. Zero indicates fish jumped out before trial started, and 9.75 indicates fish did not emerge during the trial.

**Activity** = Proportion of time fish was actively moving during the trial, calculated as 1 - (time spent still  more than 2 seconds at a time / total time outside the start box during the trial).

**Explorated** = Exploration tendency, indicated by 1 = fish entered the section of the arena behind a gate, or 0 = fish did not enter this section.



## Dataset 3 – cortisol

> Three datasets for calculating cortisol from absorbance values. Absorbance values from a spectrophotometer for the cortisol analysis of plasma samples. Samples were analysed using Enzo cortisol Elisa kit, details in the manuscript.

`Standards_cortisol.txt`

**Y_plate1** = Absorbance (neg. and blank-corrected) on plate 1

**conc_plate1** = Known concentration of cortisol in standards on plate 1

**Y_plate2** = Absorbance (neg. and blank-corrected) on plate 2

**conc_plate2** = nown concentration of cortisol in standards on plate 2

`Sample_OD_plate1.txt` and `Sample_OD_plate2.txt`

**Sample** = ID for fish

**OD** = Absorbance (neg. and blank-corrected)

**Dilution** = Plasma dilution coefficient. Multiply with this to get original values.



`Plasma_cort_results.txt` 
>The output of above cortisol concentration analysis from above data, combined with other sample details. Used for making a model and plots.


**Sample** = ID used in blood and DNA sample collection.

**Fish_ID** = Number of individual, between 1 and 100.

**PIT** = PIT-tag ID

**group** = Population and selection line combined to one group.

**pop** = Fish population: hatchery or wild.
	
**selection** = Angling selection line within population. hv = parents with high vulnerability to angling, lv = parents with low vulnerability to angling. 

**Sex** = Fish sex, -1 = female, 1 = male. Identified with a PCR assay (not available for all fish).

**Weight_end** = Fish body mass after all tests (measured between 26th and 29th June 2017).

**Length_end** = Fish whole body length after all tests (measured between 26th and 29th June 2017).

**Tank\_before\_stress_test** = Acclimation tank ID

**Temp\_on\_stress_test** = Water temperature at the time of stress test

**Stress\_test_serial** = Running number for fish captured from the same acclimation tank on the same day

**other** = Other information of the test

**cort\_ng_ml** = measured cortisol concentration ng/ml

## Dataset 4 – behaviour & predation cues

`ControlVsBurbot_data.txt`

>Tab-separated, dots as decimal separators. Behavior experiment data including latency, activity and exploration values from a total of six trials for each of 19 fish: three trials with predator cues present, three without predator cues. The data were produced by manual recording from video files using AV Biostatictics software (see above). 

**Trial** = Combination of "pm", ID and behaviour trial repeat 1-6. 

**ID** = Number of individual, between 1 and 20. Not including Nr 19.

**PIT** = PIT tag ID

**Repeat** = Running trial number for each fish, 1-6.

**Batch** = Batch of fish taken from the same tank in the same day, 1-5.

**Arena** = The ID of the arena where each behaviour trial was done, 1-4.

**Observer** = The name of data recorder.

**Group** = Either "burbot" or "control" indicating whether burbot was present upstream of the arena.

**Selection** = Fish from two selection lines were used: HV (high vulnerability), LV (low vulnerability).

**Length** = Fish whole body length in mm after the trials.

**Out\_time_min** = Time until the whole body of fish emerged from the start box at the start of behaviour trial, in minutes. Zero indicates fish jumped out before trial started, and 9.75 indicates fish did not emerge during the trial.

**Explorated** = Exploration tendency, indicated by 1 = fish entered the section of the arena behind a gate, or 0 = fish did not enter this section.

**Activity** = Proportion of time fish was actively moving during the trial, calculated as 1 - (time spent still  more than 2 seconds at a time / total time outside the start box during the trial).

**Treatment** = Same as Group, but alphabetical order reversed (control = aControl)


