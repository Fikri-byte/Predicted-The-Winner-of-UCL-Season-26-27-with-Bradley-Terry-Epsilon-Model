# Predicting the 2026/2027 UEFA Champions League Winner with Agresti- and David-Modified Bradley-Terry Models

Paper and R code for **Case 3, Mathematics Profession Seminar and Scientific Writing Competition 2026** (HIMATIKA, Faculty of Mathematics and Natural Sciences, Universitas Padjadjaran).

**Authors:** Fikri Priardani, Adrian Yusuf Kurniadi, Christian Edbert

---

## Overview

Since the 2024/2025 season, the UEFA Champions League (UCL) has used a 36-team league phase. Teams ranked 1–8 qualify directly for the round of 16, and teams ranked 9–24 enter the playoff round. This project predicts the most likely 2026/2027 champion using three variants of the Bradley-Terry model:

| Stage | Model | Why |
|---|---|---|
| League phase | **David** Bradley-Terry (penalized likelihood) | Handles draws and home advantage |
| Knockout phase | **Agresti** Bradley-Terry (penalized likelihood) | Handles home advantage, no draws |
| Final | **Standard** Bradley-Terry | Single match with no home advantage |

Parameters are estimated with the **Majorize-Minimization (MM)** algorithm. The fitted team strengths are then weighted by **UEFA club coefficients** and renormalized, which mainly helps teams with limited historical data (Como, Sabah, LASK, Viking, and AEK Athens).

## Key results

- **Data:** 3,366 historical matches (2024/2025 to 2026/2027 seasons, source: FBref) filtered down to **740 matches** involving only the 36 teams in the 2026/2027 UCL.
- **League phase:** the model places FC Bayern München first (22 points), followed by Paris Saint-Germain, Arsenal, Liverpool, Real Madrid, FC Barcelona, and Aston Villa (21 points each). LASK finishes last.
- **Knockout phase and final:** in the deterministic simulation, the final is Paris Saint-Germain vs Arsenal, with win probabilities of 0.75 for PSG and 0.25 for Arsenal.
- **Evaluation (126 UCL matches):**

  | Metric | Model | Uniform baseline (1/3 each) | Improvement |
  |---|---|---|---|
  | Log loss | 0.903 | 1.099 | 17.83% |
  | Brier score | 0.534 | 0.667 | 19.83% |

- **Limitation:** 17 of the 126 matches ended in draws, but the draw probability was never the highest outcome probability, so predicted league-phase points are less accurate for drawn matches.
- **Clustering:** 12 teams with low title chances (Feyenoord, RB Leipzig, Villarreal, VfB Stuttgart, RC Lens, Como 1907, Viking FK, Slovan Bratislava, AEK Athens, Sabah FK, SK Slavia Praha, LASK) are advised to prioritize wins against Pot 3 and Pot 4 opponents and aim for draws against Pot 1 and Pot 2 opponents.

The full methodology, theory, and references are in the PDF paper in `docs/` (written in Indonesian).

## Repository structure

```
.
├── README.md
├── docs/
│   └── Case3_UCL_2026_2027_Bradley_Terry.pdf   # full paper (Indonesian)
├── data/
│   ├── Before_Hope.csv     # per-team match results (Team, Comp, Venue, Result, GF, GA, Opponent)
│   └── UCL.xlsx            # 36 teams: club coefficient, market value, ELO
├── pictures/
│   ├── Probability_The_Winner_of_UCL.png   # knockout bracket and probabilities
│   └── Club_Clustering.png                 # points vs title probability clustering   
└── R/
    ├── Bradley_Terry_Original.R
    ├── Bradley_Terry_With_David_Modification.R
    ├── Bradley_Terry_With_Agresti_Modification_and_Simulation_for_KO_phase.R
    └── Plot_Clustering.R
```

## File descriptions

| File | Purpose |
|---|---|
| `Bradley_Terry_Original.R` | Standard Bradley-Terry model with MM, used for the final |
| `Bradley_Terry_With_David_Modification.R` | Data cleaning, W/L/T matrix construction, David penalized-likelihood model, and UEFA coefficient weighting for the league phase |
| `Bradley_Terry_With_Agresti_Modification_and_Simulation_for_KO_phase.R` | Agresti penalized-likelihood model and simulation from the round of 16 to the semifinals |
| `Plot_Clustering.R` | Plot of league points vs log10 title probability, grouped by rank block |
| `Before_Hope.csv` | Match data collected from FBref (retrieved 15 October 2026) |
| `UCL.xlsx` | UEFA club coefficients, market values, and ELO for each team |

## How to run

1. Install the required R packages:

   ```r
   install.packages(c("dplyr", "tidyverse", "readxl", "ggimage",
                      "ggrepel", "ggpubr", "png", "jpeg", "abind"))
   ```

2. Update the file paths in each script. The scripts still use the authors' local paths, for example:

   ```r
   read.csv("Kuliah Statistika Unpad/My Lomba/Mention/Before Hope.csv")
   ```

   Change them to paths relative to the repository root, such as `read.csv("data/Before_Hope.csv")`, and run from the root folder (or open it as an RStudio Project).

3. Run the scripts in the order of the analysis:
   1. `Bradley_Terry_With_David_Modification.R` (league phase)
   2. `Bradley_Terry_With_Agresti_Modification_and_Simulation_for_KO_phase.R` (knockout phase)
   3. `Bradley_Terry_Original.R` (final)
   4. `Plot_Clustering.R` (clustering plot)

## Notes

- Some input files referenced by the scripts are not included yet: `Prediksi Simulasi UCL.xlsx` (predicted standings and probabilities used for the simulation and the plot), `logo_tim.xlsx`, and `Background1.jpg` (team logos and plot background). Add them to `data/` or adjust the scripts.
- In `Bradley_Terry_Original.R`, the "Simulasi Terakhir" section uses `Simulasi5` before it is created. Reorder those lines (compute `Kemenangan_Tim1` and `Kekalahan_Tim1` first, then build `Simulasi5`) so the script runs from top to bottom.
- The Agresti script contains a section marked `# Simulasi Babak 16 besar (JANGAN DIRUN)` ("do not run"). It follows the specific simulated pairings, so avoid re-running it, since the draw is random.
- Predictions are deterministic: each match outcome is taken as the most probable result, and injuries, fatigue, and tactical changes are not modeled.

## Data sources

- Match results: [FBref](https://fbref.com/en/)
- Club coefficients: [UEFA club coefficients](https://www.uefa.com/nationalassociations/uefarankings/club/?year=2027)

## Main references

- Agresti, A. (1990). *Categorical Data Analysis*.
- David, H. A. (1988). *The Method of Paired Comparisons*.
- Yan, T. (2014). Ranking in the generalized Bradley-Terry models when the strong connection condition fails. arXiv:1411.1168.
- Wu, T. T., & Lange, K. (2010). The MM alternative to EM. *Statistical Science*, 25(4), 492–505.
- Gneiting, T., & Raftery, A. E. (2007). Strictly proper scoring rules, prediction, and estimation. *JASA*, 102(477), 359–378.
- Brier, G. W. (1950). Verification of forecasts expressed in terms of probability. *Monthly Weather Review*, 78(1), 1–3.

The full reference list is in the paper.
