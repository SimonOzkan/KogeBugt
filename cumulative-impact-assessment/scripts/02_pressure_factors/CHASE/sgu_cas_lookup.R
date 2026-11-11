library(httr2); library(jsonlite)

library(httr2); library(dplyr); library(purrr); library(rlang)

base <- "https://api.sgu.se/oppnadata/miljogifter-analysresultat-provplatser/ogc/features/v1"

# brug ORIGINAL-stavningen (ikke din tolower'ede PARAM) fra din hentede dataframe
params <- unique(vatten$PARAMETERNAMN)

get_cas <- function(navn) {
  flt <- sprintf("parameternamn='%s'", gsub("'", "''", navn))   # escape apostrof til CQL
  r <- request(paste0(base, "/collections/analysresultat/items")) |>
    req_url_query(f = "json", limit = 1,
                  filter = flt, `filter-lang` = "cql2-text") |>
    req_perform() |> resp_body_json(simplifyVector = FALSE)
  if (length(r$features) == 0) return(NA_character_)
  r$features[[1]]$properties$cas_kod %||% NA_character_
}

cas_lookup <- tibble(PARAMETERNAMN = params) |>
  mutate(CAS = map_chr(PARAMETERNAMN, possibly(get_cas, NA_character_)))

write.csv(cas_lookup, file.path(PATHS$input_pressure, "kemisk_forurening","SGU", "cas_lookup.csv"), row.names = FALSE)

# join til din dataframe
vatten <- vatten |> left_join(cas_lookup, by = "PARAMETERNAMN")
