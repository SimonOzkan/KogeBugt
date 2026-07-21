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
source("scripts/00_setup.R")  # Altid først!