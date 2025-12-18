source("utils.R")
library(tidycensus)
library(readxl)
library(gridExtra)

female_pop <- get_acs(
  geography = "county", state = "NC", variables = "B01001_026",
  year = 2023, survey = "acs5"
) %>%
  transmute(county = clean_county(NAME), pop = estimate)

df_cc <- read_excel("Updated_Cervical Cancer County Data_Hotspot Resource.xlsx") %>%
  mutate(county = clean_county(county))

df_svi <- read_excel("svi.xlsx") %>%
  mutate(county = clean_county(county))

df <- df_cc %>%
  left_join(df_svi, by = "county") %>%
  mutate(
    cases     = as.numeric(incidence),
    mortality = as.numeric(mortality),
    screening = as.numeric(screening),
    svi       = as.numeric(svi)
  ) %>%
  left_join(female_pop, by = "county") %>%
  mutate(
    cc_incidence_rate = (cases / pop) * 100000,
    cc_mortality_rate = (mortality / pop) * 100000,
    cc_mortality_rate1 = if_else(is.na(cc_incidence_rate), NA_real_, cc_mortality_rate),
    screening1         = if_else(is.na(cc_incidence_rate), NA_real_, screening),
    svi1               = if_else(is.na(cc_incidence_rate), NA_real_, svi)
  )

df <- df %>%
  mutate(
    incidence_pct = pct_rank(cc_incidence_rate),
    mortality_pct = pct_rank(cc_mortality_rate1),
    screening_pct = pct_rank_protective(screening1),
    svi_pct       = svi1
  )

comp <- make_composite(df %>% select(incidence_pct, mortality_pct, screening_pct, svi_pct))
df <- df %>%
  mutate(
    composite_raw = comp$composite_raw,
    composite_pct = comp$composite_pct,
    composite_cat = make_tercile(composite_pct)
  )

nc_map <- get_nc_counties() %>% left_join(df, by = "county")

t_inc <- map_fill(nc_map, cc_incidence_rate, "Incidence Rate per 100K")
t_mor <- map_fill(nc_map, cc_mortality_rate, "Mortality Rate per 100K")
t_scr <- map_fill(nc_map, screening, "Screening Rate", reverse = TRUE)
t_svi <- map_fill(nc_map, svi, "Social Vulnerability Index")
t_cmp <- map_composite_highlights(nc_map, composite_pct, "Cervical Cancer Burden Index")

grid.arrange(arrangeGrob(t_inc, t_mor, t_scr, t_svi, t_cmp, nrow = 2, ncol = 3))
