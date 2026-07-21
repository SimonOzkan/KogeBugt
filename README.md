# Cumulative Impact Assessment - Køge Bugt

Analyser af samlede påvirkninger på økosystemkomponenter.

## Struktur

- **R/** - Genbrugelige funktioner
- **scripts/01_ecosystem_components/** - Behandling af økosystem data
- **scripts/02_pressure_factors/** - Behandling af påvirkningsfaktorer
- **scripts/03_grid_setup.R** - Grid definition
- **scripts/04_cumulative_analysis.R** - Samlet analyse
- **config/paths.R** - Sti-konfiguration til Teams-folder
- **docs/metadata_log.md** - Data-metadata og kilder


## Brug

```r
source("scripts/00_setup.R")  # Altid først

# Opbygning af scripts 


```
cumulative-impact-assessment/
├── scripts/
│   ├── 00_setup.R                     ← Master setup
│   ├── 01_ecosystem_components/
│   │   ├── natura2000_processing.R    ← Standalone
│   │   ├── habitat_processing.R       ← Standalone
│   │   └── ...                        ← Standalone
│   ├── 02_pressure_factors/
│   │   ├── fishing_pressure.R         ← Standalone
│   │   ├── shipping_traffic.R         ← Standalone
│   │   └── ...                        ← Standalone
│   └── 03_grid_setup.R
├── config/
│   └── paths.R
├── outputs/
└── README.md
```



# Data
Alt input data og resultater gemmes i den fælles Teams folder via config/paths.R.

