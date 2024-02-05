Created: 2024-01-25  
Updated: 2024-02-05 
 
 Details about `scripts/` folder. Scripts themselves have long description of their purpose at the top in comments.
 
 # Directory
- `data-wrangling_veg-2012-2021/`
    - `C12-data-wrangling.R`, `C13-data-wrangling.R`, `C19-data-wrangling.R`, `C21-data-wrangling.R`
        - Convert Excel spreadsheets into R-friendly format, standardize names, address trace values.
    - `Fixes-for-common-and-scientific-names.R`
      - List of all names that were changed/standardized during data cleaning. Instances of each change are documented in `CXX-data-wrangling.R` scripts. Not intended to run as code.
    - `Plant-functional-groups-by-common-name.xlsx`
      - A spreadsheet I created that categorizes plants by functional group and invasive/native status (using common names). Each cell has a comment of the scientific name. All species checked with USDA Plants.
    - `Summarise-all-channels.R`
- `other-analyses/`
    - Scripts will run, but these analyses are less important and sometimes not fully pursued.
    - `ALVSCE-2023-figs.R`    
    - `Change-over-time_veg-2012-2021.R`
    - `Correlation.R`
    - `PCA_2021.R`
    - `Percent-change_veg-2012-2021.R`
    - `SEM-2.0_candidate-models.R`
    - `SER-SW-2023_SRM_2024-figs.R`
- `old_pre-2023-03-24/`
    - Created 2023-03-24 as a place for prior analysis that has since been discontinued (have been replaced by better grouping and statistical models); usually uses data from `data/cleaned/old-summarised/` folder.
    - Lines of code to write out figures have been deleted (unless for SRM 2022 & SRM 2023), so figures won't accidentally be overwritten by old analysis (no need to save old figures in their own folder to write out separately again).
    - Not intended to run - some paths may not work, as files and folders may have changed names.
- `sequencing_2021/`
    - `16S_prelim-stats.R`
    - `FAPROTAX-input.R`
    - `FAPROTAX-reads.R`
    - `FAPROTAX-stats.R`
    - `FUNGuild-input.R`
    - `FUNGuild-reads.R`
    - `FUNGuild-stats.R`
    - `ITS_prelim-stats.R`

- `ANOVA-by-Treatment3_veg-2012-2021.R`    
- `Cross-section-elevation.R`
- `CV_veg-2021-2012.R`
- `Data-for-publishing.R`
- `Data-screening_2021.R`
- `Data-screening_veg-2012-2021.R`
- `PimaCounty_precip.R`
- `Publish_16S-ITS.R`
- `Publish_CV.R`
- `Publish_SEM.R`
- `Publish_temporal-veg.R`
- `Publish_t-test_2021.R`
- `SEM-2.1_candidate-models.R`
- `T-test-by-Treatment3-and-NMDS_2021.R`