# AVCA_ElkLD
Created: 2022-01-31  
Last updated: 2024-02-05
  
## Description  
Complete data analysis for Altar Valley Conservation Alliance Elkhorn-Las Delicias demonstration project, which was ten-year monitoring of stream channels with rock detention structures to control erosion. 

This repository accompanies publication "Dryland rock detention structures increase herbaceous vegetation cover and stabilize shrub cover over 10 years but do not directly affect soil fertility" by Ossanna et al. (2024) in *Science of the Total Environment* <https://doi.org/10.1016/j.scitotenv.2024.170194>.  

Each main folder has its own README that describes subfolders and files.
  
## Author
Contact: Lia Ossanna, lossanna@arizona.edu

## Data
There are two main datasets, referred to in script names by `_veg-2012-2021` and `_2021`.
- `_veg-2012-2021` is the temporal vegetation cover and density data.
- `_2021` is the data collected in 2021, used for SEM, and includes:
  - 2021 vegetation cover and density (same data from temporal dataset)
  - soil chemical analysis (total nitrogen, total carbon, organic matter)
  - soil microbial analysis (16S and ITS amplicon sequencing).

# Workflow for current analysis
The order scripts should be run in to recreate the current/most recent version of analysis (in `scripts/` folder):
1. **Data wrangling**: 
- For temporal veg analysis: run `data-wrangling/CXX-data-wrangling.R`, then run `data-wrangling/Summarise-all-channels.R`.
  - See `RMarkdown/Data-wrangling_annotated-example.html` for an annotated example.
 - For LiDAR data (channel bed elevation): `Cross-section-elevation.R`.
 - For precipitation data: `PimaCounty_precip.R`.
2. **Data screening**: 
- For temporal data: `Data-screening_veg_2012-2021.R` 
- For 2021 data: `Data-screening_2021.R`.
3. **Analysis**:
- For temporal data: `ANOVA-by-Treatment3_veg-2012-2021`, `CV_veg-2012-2021.R`.
- For 2021 data: `T-test-by-Treatment3-and-NMDS_2021.R`,`SEM-2.1_candidate-models.R`.

# Directory
- `data/`
    - `cleaned/`
    - `Excel_LO_edited/`
    - `Excel_raw/`
    - `PimaCounty_precip/`
    - `publish/`
    - `README_data.md`
    - +2 files
- `figures/`
  - `2022-01-28_Data-sharing_updated-2022-09/`
  - `2022-02_SRM-AZ-and-national-2023/`
    - `AZ-SRM/`
  - `2022-02_SRM-national-2022/`
  - `2023-03-30_ALVSCE-poster-forum/`
  - `2023-07_draft-figures/`
  - `2023-09_publish-figures/`
    - Not pushed to GitHub due to large file size; figures for first submission to *STOTEN*.
  - `2023-11_draft-figures/`
  - `2023-11_SER-SW-2023-and-SRM-2024/`
  - `2023-12_publish-figures/`
      - Not pushed to GitHub due to large file size; figures for second submission to *STOTEN*.
- `hpc-amplicon-sequencing/`
    - Folder not pushed to GitHub due to file size. These parts are also separated because they needed to be completed on [UA HPC](https://uarizona.atlassian.net/wiki/spaces/UAHPC/overview).
    - Includes demultiplexing, DADA2 pipeline steps, and FAPROTAX and FUNGuild analysis.
    - For code and description of analysis that is available on GitHub, see `RMarkdown/sequencing/`.
- `RData/`
  - `.RData` files not pushed to GitHub. 
  - This folder also does not have its own README because it's pretty self-explanatory (files are named the same as scripts from which they were generated).
- `RMarkdown/`
  - `sequencing/`
  - +16 files
- `scripts/`
    - `data-wrangling/`
    - `old_pre-2023-03-24/`
      - Not intended to run - some paths may not work, as files and folders may have changed names.
    - `other-analyses/`
    - `sequencing_2021/`
    - `README_scripts.md`
    - +14 `.R` files


# Notes
- GitHub repository created for project 2022-01-31. Project began Dec 2021, so version control for first 2-3 months is not available.
- I put analyses and data prior to 2023-03-24 in their own folders. After this date, I finalized grouping by Treatment3 (treated/control) and had better organization of scripts and file names.