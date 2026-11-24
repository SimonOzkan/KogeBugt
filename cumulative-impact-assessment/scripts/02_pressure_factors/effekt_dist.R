##### Effektdistance ##########

# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")

# Hent tilgængelige project paths
PATHS <- set_project_paths()



### skript til effektdistance for punktkilder ### 
# I ØKOMAR, simple linear decay function from the point source and to the limit of the effect distance


effekt_distance <- tribble(
  ~Pressure,                                                     ~Median, ~Mean, ~Max, ~Min, ~Stdev, ~n,
  "Dumped chemical munitions",                                    5,      11.6,   50,   0,   18.1,   19,
  "Aquacultures: fish and shellfish farms",                       5,      10,     50,   0,    7,     20,
  "Sea cables",                                                   0,       0.10,   1,   0,    0.37,  20,
  "Offshore oil and gas installations",                           1,       3.5,   25,   0,    5.77,  20,
  "Oil and gas pipelines",                                        0,       0.2,    1,   0,    0.47,  20,
  "Heat and power plants",                                        1,       3.1,   10,   0,    3.67,  14,
  "Disposal sites for construction and dredged material",         5,       8.9,   50,   0,   12.9,   20,
  "Dredging in harbours and shipping lanes",                      5,       6.4,   50,   0,   12.7,   14,
  "Excavation sites in production",                               1,       5.1,   50,   0,   10.9,   20,
  "Offshore wind turbines",                                       1,       4.3,   50,   0,   10.0,   20,
  "Bridges and costal constructions",                             1,       3.2,   25,   0,    7.5,   20,
  "Coastal habitat modification (coastal protection and piers)",  1,       3,     25,   0,    6.8,   14,
  "Lighthouses",                                                  0,       5.4,   50,   0,   13.2,   14,
  "Military areas",                                               7.5,    13.4,   50,   0,   15.3,   20,
  "Marine ports: industrial",                                     5,      10.1,   50,   0,   14.0,   14,
  "Marine ports and marinas: recreational",                       3,       5.6,   50,   0,   10.9,   20,
  "Mussel dredging",                                              1,       1.2,   10,   0,    2.6,   18
)

