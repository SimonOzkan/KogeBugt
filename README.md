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
Alt input data og resultater gemmes i den fælles Teams folder.
