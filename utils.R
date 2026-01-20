# R/utils.R ---------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(tigris)
  library(sf)
  library(RColorBrewer)
})

options(tigris_use_cache = TRUE)

# -----------------------------
# 1) County name cleaning (shared)
# -----------------------------
clean_county <- function(x) {
  x %>%
    stringr::str_remove(" County, North Carolina$") %>%
    stringr::str_remove(" County$") %>%
    stringr::str_to_title() %>%
    stringr::str_replace("^Mcdowell$", "McDowell")
}

# -----------------------------
# 2) NC counties shapefile (shared)
# -----------------------------
get_nc_counties <- function(year = 2024, cb = TRUE) {
  tigris::counties(state = "NC", cb = cb, year = year, class = "sf") %>%
    transmute(
      county   = clean_county(NAME),
      geometry = geometry
    ) %>%
    filter(!sf::st_is_empty(geometry) & !is.na(geometry))
}

# -----------------------------
# 3) Percent-rank helper (safe + consistent missingness)
# -----------------------------
pct_rank <- function(x) {
  # percent_rank() returns NA when x is all NA; this wrapper is explicit + stable
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  dplyr::percent_rank(x)
}

# Protective metric (higher is better, so reverse)
pct_rank_protective <- function(x) {
  1 - pct_rank(x)
}

# -----------------------------
# 4) Build composite percentile from components (vectorized)
#    components: a tibble/data.frame of columns already on [0,1] scale (or NA)
# -----------------------------
make_composite <- function(components_df) {
  composite_raw <- components_df %>% mutate(across(everything(), as.numeric)) %>% rowSums(na.rm = FALSE)
  
  composite_rank <- rank(composite_raw, ties.method = "average", na.last = "keep")
  n_nonmiss <- sum(!is.na(composite_rank))
  
  composite_pct <- ifelse(
    is.na(composite_rank) | n_nonmiss <= 1,
    NA_real_,
    (composite_rank - 1) / (n_nonmiss - 1)
  )
  
  list(
    composite_raw  = composite_raw,
    composite_rank = composite_rank,
    composite_pct  = composite_pct
  )
}

# Optional 3-level category
make_tercile <- function(p) {
  cut(
    p,
    breaks = c(0, 0.33, 0.66, 1),
    labels = c("Low", "Medium", "High"),
    include.lowest = TRUE
  )
}

# -----------------------------
# 5) Map theme (shared)
# -----------------------------
theme_map <- function(title_size = 18) {
  ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none",
      plot.margin = ggplot2::margin(0, 0, 0, 0),
      plot.title = ggplot2::element_text(hjust = 0.5, size = title_size, face = "bold")
    )
}

# -----------------------------
# 6) Generic choropleth maker (shared)
#    var: unquoted column name in nc_map (sf tibble)
# -----------------------------
map_fill <- function(nc_map, var, title, palette = "Reds", reverse = FALSE, title_size = 18) {
  cols <- brewer.pal(4, palette)
  if (reverse) cols <- rev(cols)
  
  ggplot(nc_map) +
    geom_sf(aes(fill = {{ var }}), color = "black", linewidth = 0.2) +
    scale_fill_gradientn(colours = cols, na.value = "grey80") +
    ggtitle(title) +
    theme_map(title_size)
}

# -----------------------------
# 7) Composite map with top-20% outline + >90% asterisk
# -----------------------------
map_composite_highlights <- function(nc_map, pct_col = composite_pct,
                                     title = "Burden Index",
                                     outline_q = 0.8, star_q = 0.9,
                                     palette = "Reds", title_size = 18) {
  nc_map <- nc_map %>%
    mutate(high_burden = {{ pct_col }} >= quantile({{ pct_col }}, outline_q, na.rm = TRUE))
  
  stars <- nc_map %>%
    st_centroid() %>%
    filter({{ pct_col }} > star_q)
  
  ggplot(nc_map) +
    geom_sf(aes(fill = {{ pct_col }}), color = "black", linewidth = 0.2) +
    geom_sf(data = subset(nc_map, high_burden), fill = NA, color = "blue", linewidth = 0.8) +
    geom_text(
      data = stars,
      aes(
        x = st_coordinates(geometry)[, 1],
        y = st_coordinates(geometry)[, 2],
        label = "*"
      ),
      size = 7,
      color = "black"
    ) +
    scale_fill_gradientn(colours = brewer.pal(4, palette), na.value = "grey80") +
    ggtitle(title) +
    theme_map(title_size)
}
