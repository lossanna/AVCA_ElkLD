# AVCA_ElkLD
Created: 2022-01-31  
Last updated: 2023-03-17  
  
## Description  
  Vegetation data analysis for Altar Valley Conservation Alliance Elkhorn-Las Delicias demonstration project, ten-year monitoring of stream channels with induced meandering. 
  
# Author
Contact: Lia Ossanna, lossanna@arizona.edu

# Workflow for current analysis
The order scripts should be run in to recreate the current/most recent version of analysis:
1. **Data wrangling** (in `scripts/data-wrangling/`): run `CXX-data-wrangling.R`, then `Summarise-all-channels.R` and `Summarise-perennial-diversity.R`
2. `Data-screening_veg_2012-2021.R`
3. 

# Directory
- `data/`
    - `cleaned/`
    - `Excel_LO_edited/`
    - `Excel_raw/`
    - `PimaCounty_precip/`
        - Precipitation data from Altar Valley wash, retrieved from Pima County ALERT, using gauge #6380. Access the portal here: https://webcms.pima.gov/government/flood_control/services/precipitation_and_streamflow_data/.
- `hpc-amplicon-sequencing/`
    - Separate because these parts needed to be completed on UA HPC. Includes demultiplexing and DADA2 pipeline steps.
- `output_figs/`
- `RData/`
- `RMarkdown/`
- `scripts/`
    - `data-wrangling/`
    - `sequencing/`
- `.gitignore`
- `AVCA_ElkLD.Rproj`