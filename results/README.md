# Results based on the full survey data

This folder contains the results of the analysis based on the full survey data, as reported in STORCITO deliverables D5.2 and D5.3. They were created with `00_run_all.R` on 29.10.2026; the software versions are listed in `sessionInfo.txt`.

File names start with the dataset: `DE_`, `NO_`, `NL_`, or `ALL_` for the pooled data. The subfolders contain the robustness checks of the machine-learning results (`seeds/`), the regional data for the map application (`app/`) and aggregated subgroup results (`share/`). A description of the main files is given in the main README.

The full survey data are not public. Running the code with the synthetic dataset writes results to `output/` instead; these are meaningless and differ from the results here.