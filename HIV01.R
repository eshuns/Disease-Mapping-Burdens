source("utils.R")
library(readxl)
library(gridExtra)

df_svi <- read_excel("svi.xlsx") %>% mutate(county = clean_county(county))
df_hiv <- read_excel("hiv.xlsx") %>% mutate(county = clean_county(county))

df <- df_svi %>%
  left_join(df_hiv, by = "county") %>%
  mutate(rate = as.numeric(rate), svi = as.numeric(svi)) %>%
  mutate(
    svi_pct = svi,
    hiv_inc_pct = pct_rank(rate)
  )

comp <- make_composite(df %>% select(hiv_inc_pct, svi_pct))
df <- df %>% mutate(composite_pct = comp$composite_pct)

nc_map <- get_nc_counties() %>% left_join(df, by = "county")

t_inc <- map_fill(nc_map, rate, "HIV Incidence Rate per 10K Population")
t_svi <- map_fill(nc_map, svi_pct, "Social Vulnerability Index")
t_cmp <- map_composite_highlights(nc_map, composite_pct, "HIV Burden Index")

grid.arrange(arrangeGrob(t_inc, t_svi, t_cmp, nrow = 3, ncol = 1))









