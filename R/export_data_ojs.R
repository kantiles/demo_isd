# ---------------- #
# OJS file from ISD
# ---------------- #

library(tidyverse)

# Import ------------------------------------------------------------------
df_isd_assemble <-
  read_rds(here::here("../../clients/drees/drees_isd/data/isd_assemble.rds"))

# Cleaning ----------------------------------------------------------------
# national value
df_isd_filter_ojs_nat <- df_isd_assemble |>
  mutate(value = round(value, 2)) |>
  filter(niv_geo %in% c("fr_ent") &
           annee >= 2009 &
           is_isd == TRUE &
           isd %in% c("C22", "C23", "C24")) |>
  select(-is_isd, -niv_geo, -code_geo) |>
  rename(value_nat = value)

# build diff with national value
df_isd_filter_ojs <- df_isd_assemble |>
  mutate(
    value = round(value, 1),
    value = if_else(
      annee == 2022 &
        code_geo == "972" &
        indicator == "part_accouch_20_24",
      16.8,
      value
    )
  ) |>
  filter(niv_geo %in% c("dep") &
           annee >= 2009 &
           is_isd == TRUE &
           isd %in% c("C22", "C23", "C24")) |>
  select(-is_isd, -niv_geo) |>
  left_join(df_isd_filter_ojs_nat, by = join_by(annee, isd, indicator)) |>
  mutate(
    diff = value - value_nat,
    indicator = fct_recode(
      indicator,
      "Before 28 weeks" = "tx_tr_gde_prema_av28sa",
      "Before 32 weeks" = "tx_gde_prema_av32sa",
      "Before 37 weeks" = "tx_prema_av37sa",
      "Less than 1.5kg" = "part_nais_moins_1500g",
      "Less than 2.5kg" = "part_nais_moins_2500g",
      "Under 20 years old" = "part_accouch_m20",
      "20-24 years old" = "part_accouch_20_24",
      "25-29 years old" = "part_accouch_25_29",
      "30-34 years old" = "part_accouch_30_34",
      "35-39 years old" = "part_accouch_35_39",
      "40 years old and above" = "part_accouch_40p"
    ),
    isd = fct_recode(
      isd,
      "Premature births by week of amenorrhea" = "C22",
      "Births according to child weight" = "C23",
      "Births according to mother age" = "C24"
    )
  )

# grid OJS
facet_data <- geofacet::fr_departements_grid1 |>
  as_tibble() |>
  left_join(df_isd_filter_ojs |>
              filter(annee == 2022), by = join_by(code == code_geo))

# Export ------------------------------------------------------------------
# only init version that need transpose
ojs_define(data_init = df_isd_filter_ojs)

ojs_define(data_grid_init = facet_data)
