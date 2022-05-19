# Code accompanying manuscript titled:
## "Stress and Rumination in Premenstrual Syndrome (PMS): identifying stable and menstrual cycle-related differences in PMS symptom severity"

## Graphical Abstract:
![alt text](https://mfr.de-1.osf.io/export?url=https://osf.io/gvsrw/?direct%26mode=render%26action=download%26public_file=True&initialWidth=848&childId=mfrIframe&parentTitle=OSF+%7C+PMS+graphical+abstract.png&parentUrl=https://osf.io/gvsrw/&format=1000x1000.jpeg)

This repository consists of three folders

1. Data
2. Figures
3. Supplemental

## How to use this directory
All scripts in this directory are made so they will run 'out-of-the-box' using R in RStudio.
You can download the entire directory as a .zip file and run as such.
Only relative paths are used and the code is preceded the automatic installation of necessary packages if these are missing.
More detailed information on used packages and version numbers can be found in the supplemental materials in ```Supplemental/supplementalMaterial.pdf```.

## Data
All preprocessed data is made available, as well as all individual scores on each individual item of each questionnaire.
The raw data is not made available due to possible privacy concerns, but will be made available upon reasonable request from Mitchel.Kappen@UGent.be

## Data Analysis
The preprocessing of the data - in which we went from raw data to usable data - was done using a script called ```preprocessingData.R```.
This script is *not* made available due to privacy concerns: the script uses personal identifiers that could be linked back to participants.
Insight into this script is possible upon reasonable request from Mitchel.Kappen@UGent.be

### Code
Code files are stored in the main directory.
Analysis is split up into ```traitAnalysis.R``` and ```stateAnalysis.R``` as has been done in the manuscript.
In ```traitAnalysis.R``` you can find a highly detailed script that analyses the data from ```Data/cleanedDataTraits.csv```, which analyses the trait questionnaires in the same order as they are described in the manuscript:
- RRS: Ruminative Response Scale
- DASS: Depression, Anxiety, Stress Scale

In ```stateAnalysis.R``` you can find a highly detailed script that analyses the data from ```Data/cleanedDataMoments.csv```, which analyses the state questionnaires in the same order as they are described in the manuscript:
- PSS: Perceived Stress Scale
- PTQ: Perseverative Thinking Questionnaire

We stored all recurring function in ```functions.R```, such as plotting functions, effect size calculations etc.

## Supplemental Material
#### Questionnaires & Cronbach's Alpha
We calculated the Cronbach's Alpha for each of the used questionnaires.
These calculations can be found in ```cronbachAlpha.R``` and uses ```Data/cleanData_allItems.csv```, which consists of each individuals' response to each individual item of each questionnaire.
Resulting Cronbach's Alpha, as well as detailed descriptions of the questionnaires used can be found in ```Supplemental/supplementalMaterial.pdf```.

#### BSRI: Brief State Rumination Inventory
Due to a strong overlap in measured constructs between the BSRI and the PTQ questionnaire and, as such, similar results, we decided to only include the results of the PTQ in the manuscript due to it displaying a more robust construct validity.
Nevertheless, we wish to be transparent about all data collected as have added an extra results section specifically dedicated to the BSRI results to ```Supplemental/supplementalMaterial.pdf``` as 'Appendix E'.
These results are calculated in the same script as the other state variables: ```traitAnalysis.R```.

#### Correlations PSS & PTQ
Due to a strong interrelation between the constructs of stress and rumination, we decided to run correlational analyses between the results of PSS and PTQ in our study.
However, since these results were not our main points of interest, we only added these to the supplemental materials in the subdirectory ```Supplemental/Figures/```.
We believe this could help other researcher to better understand our results and to put them in into perspective.
Figures and correlation coefficients have been added to ```Supplemental/supplementalMaterial.pdf``` as 'Appendix B'.

#### Copper Coil contraception
There are some discussions on the inclusion of Copper Coil IUD's to PMS research.
Despite the general consensus being that the inclusion of participants using a Copper Coil IUD is preferred, we included duplicate analyses in which we excluded these participants for transparency.
These analyses are identical to our main analyses, apart from the exclusion of participants using a Copper Coil IUD.
The analysis scripts used are ```withoutCopper_traitAnalysis.R``` and ```withoutCopper_stateAnalysis.R```.
All output from these scripts can be found in the subfolder ```Supplemental/noCopperCoil/``` and follows the same structure as the main directory.
For clarity, all plot files concerning these analyses are preceded by 'noCopperCoil_', e.g., ```noCopperCoil_PSS.jpg```.
A fully written results section in which the participants using a Copper Coil IUD has been included in ```Supplemental/supplementalMaterial.pdf``` as 'Appendix D'.

# For questions or bugs, please contact Mitchel.Kappen@UGent.be
