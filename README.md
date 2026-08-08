# Summer Session 2026 (Credit to: Jonathan Williams, @cthonathon)

This repository contains scripts and materials for a classroom project on Chicago restaurant inspections.  All data is created through API calls and processed through the scripts, so this repo does not keep a static copy of any data files.

## Structure

- `scripts/` contains the data science pipeline
- `data/` contains local data assets (not tracked in Git)
- `presentation.pptx` contains slides for the lesson

## Pipeline

1. `1_data_import.r`
2. `2_data_processing.R`
3. `3_modeling.R`
4. `4_evaluation.R`
5. `5_dashboard.R`
6. `run_pipeline.R` runs scripts 1 - 4 (but not the dashboard)
