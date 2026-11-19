# Drinking water quality under payment-linked verification

Data and code for Ecklu, Nagel, MacDonald and Thomas, *"Tying payment to proof: drinking water
quality in seven carbon-financed water projects with payment-linked verification"*
(PLOS Water, under review, PWAT-D-26-00197). Archived at Zenodo: https://doi.org/10.5281/zenodo.24357864

The scripts reproduce every number, table and figure in the paper from the files in `data/`, except the sampling-location map (Fig 1), whose coordinates are not published.

## Data

`data/` holds the compartment bag test (CBT) and presence/absence *E. coli* results for seven
Gold Standard safe drinking water projects, as recorded by the implementers in the mWater
platform (exports of 3 September 2026; Amazi Water, 18 November 2026). Names, enumerator identities, response codes, free text
and all GPS coordinates are removed. School, household and water system identifiers are replaced by
salted hash codes, which group records but cannot be traced back to a site.

| File | Project | Country | Columns |
|------|---------|---------|---------|
| `LifeStraw_WQ.csv` | LifeStraw school filters | Kenya | deployment, month, sample_type (water point / filter), school_code, ecoli_mpn, ecoli_risk |
| `AmaziMeza_WQ.csv` | Amazi Meza school filters | Rwanda | deployment, district, school_code, ecoli_risk |
| `Asili_WQ.csv` | Asili piped system and kiosks | DRC | deployment, sample_location, ecoli_risk |
| `Helvetas_WQ.csv` | Ranovola (Helvetas) piped systems | Madagascar | deployment, sample_location, ecoli_mpn, ecoli_risk |
| `Burundi_WQ.csv` | Amazi Water piped systems | Burundi | deployment, household_code, sample_location, system_code, ecoli_mpn, ecoli_risk |
| `MWA_baseline.csv` | DRIP-FUNDI boreholes, 2024 baseline | Kenya | ecoli_pa |
| `MWA_monitor.csv` | DRIP-FUNDI, March 2026 monitoring | Kenya | deployment, month, county, sample_location, ecoli_mpn, ecoli_risk |
| `DRIP_first_monitor.csv` | DRIP-FUNDI round labeled first monitoring (excluded: no traceable source record) | Kenya | ecoli_pa |
| `WaterMission_baseline.csv` | Water Mission source assessments, 2024 | Tanzania | site, ecoli_cfu, ecoli_pa |
| `WaterMission_monitor.csv` | Water Mission delivered water, 2026 | Tanzania | system_code, ecoli_pa |

`ecoli_risk` holds the six mWater descriptors, which `R/prep.R` combines into the four WHO
categories (low, intermediate, high, very high risk). The `deployment` column names each round;
the paper's Table 3 gives the type of every round (baseline, monitoring, internal,
special-purpose campaign, excluded).

## Code

Run from the repository root, in order:

```sh
Rscript R/prep.R         # partitions rounds, applies exclusions -> prepped.rds; prints sample sizes
Rscript R/model.R        # hierarchical multinomial model -> results/institutional_model.csv, model.rds
Rscript R/sensitivity.R  # special campaigns, country fixed effect, period x country -> results/institutional_sensitivity.csv
Rscript R/rounds.R       # every round: n, % detectable E. coli, % very high risk -> results/rounds.csv (Table 3)
Rscript R/figs.R         # Figs 2-7 as PDF and 300 dpi TIFF -> figures/
Rscript R/render_map.R   # Fig 1 (needs the non-public coordinates file; documents how the map was drawn)
```

Requires R with `readr`, `dplyr`, `tidyr`, `stringr`, `ggplot2`, `scales`, `brms` (and Stan),
`data.table`, `sf`, `rnaturalearth`, `rnaturalearthdata` and `ggspatial`. The models use the
seed 20260529.

The Fig 1 base layer is Natural Earth 1:50m admin-0 countries (public domain,
<https://www.naturalearthdata.com>).

## License

Code: MIT (see `LICENSE`). Data: CC BY 4.0.
