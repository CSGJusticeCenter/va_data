library(tidyverse)
library(csgjcr)

load(csg_sp_path("jr_data_library", "data", "raw", "bjs", "nps", "icpsr_38871-v1",
                 "icpsr_38871", "ds0001", "38871-0001-Data.rda"))

nps <- read_rds(csg_sp_path("jr_data_library", "data", "analysis", "bjs", "nps", "bjs_nps_state.rds"))

nps |>
  filter(indicator == "Prison admissions under jurisdiction") %>%
  filter(group_cat == "adm_type") |>
  count(group)


da38871.0001 |>
  janitor::clean_names() |>
  as_tibble() |>
  transmute(
    year,
    state_name = csg_state_convert(state, "abbr", "name"),
    state_abbr = as.character(state),
    jurtotm,
    jurtotf,
    whitem,
    whitef,
    blackm,
    blackf,
    hispm,
    hispf,
    aianm,
    aianf,
    asianm,
    asianf,
    nhpim,
    nhpif,
    apim,
    apif,
    tworacem,
    tworacef,
    addracem,
    addracef,
    unkracem,
    unkracef
  ) |>
  mutate(
    across(where(is.numeric), \(x) if_else(is.na(x), 0, x)),
    asian_pim = asianm + nhpim + apim,
    asian_pif = asianf + nhpif + apif,
    otherm = tworacem + addracem,
    otherf = tworacef + addracef
  ) %>%
  select(
    -c(asianm, nhpim, apim, asianf, nhpif, apif,
       tworacem, addracem, tworacef, addracef)
  ) |>
  pivot_longer(-c(year, state_name, state_abbr)) |>
  mutate(
    sex = if_else(str_ends(name, "f"), "Female", "Male"),
    var = str_sub(name, 1, nchar(name) - 1)
  ) |>
  mutate(
    var = case_match(
      var,
      "aian" ~ "amer_ind",
      "asian_pi" ~ "asian",
      "unkrace" ~ "unknown",
      "jurtot" ~ "total",
      .default = var
    )
  ) |>
  select(-name) |>
  pivot_wider(
    names_from = var,
    values_from = value
  ) |>
  relocate(unknown, .after = other)

admissions |>
  filter(is.na(state_name)) |>view()

da38871.0001 |>
  janitor::clean_names() |>
  as_tibble() %>%
  transmute(
    year,
    state_name = csg_state_convert(state, "abbr", "name"),
    state_abbr = as.character(state),
    jurtotm,
    jurtotf,
    whitem,
    whitef,
    blackm,
    blackf,
    hispm,
    hispf,
    aianm,
    aianf,
    asianm,
    asianf,
    nhpim,
    nhpif,
    apim,
    apif,
    tworacem,
    tworacef,
    addracem,
    addracef,
    unkracem,
    unkracef,
    commitm,
    commitf,
    parnewm,
    parnewf,
    parnom,
    parnof,
    adcrnewm,
    adcrnewf,
    adcrnom,
    adcrnof,
    adtransm,
    adtransf,
    adawesm,
    adawesf,
    adretm,
    adretf,
    adothm,
    adothf
  ) |>
  mutate(
    asian_pim = asianm + nhpim + apim,
    asian_pif = asianf + nhpif + apif,
    otherm = tworacem + addracem,
    otherf = tworacef + addracef,
    adm_rel_violm = parnewm + parnom + adcrnewm + adcrnom,
    adm_rel_violf = parnewf + parnof + adcrnewf + adcrnof,
    adm_othm = adtransm + adawesm + adretm + adothm,
    adm_othm = adtransf + adawesf + adretf + adothf
  ) %>%
  select(
    -c(asianm, nhpim, apim, asianf, nhpif, apif,
       tworacem, addracem, tworacef, addracef,
       parnewm, parnom, adcrnewm, adcrnom,
       parnewf, parnof, adcrnewf, adcrnof,
       adtransm, adawesm, adretm, adothm,
       adtransf, adawesf, adretf, adothf)
  ) |>
  pivot_longer(-c(year, state_name, state_abbr)) |>
  mutate(
    sex = if_else(str_ends(name, "f"), "Female", "Male"),
    var = str_sub(name, 1, nchar(name) - 1)
  ) |>
  filter(!is.na(state_name))

admissions <- da38871.0001 |>
  janitor::clean_names() |>
  as_tibble() |>
  transmute(
    year,
    state_name = csg_state_convert(state, "abbr", "name"),
    state_abbr = as.character(state),
    adtotm,
    adtotf,
    commitm,
    commitf,
    parnewm,
    parnewf,
    parnom,
    parnof,
    adcrnewm,
    adcrnewf,
    adcrnom,
    adcrnof,
    adtransm,
    adtransf,
    adawolm,
    adawolf,
    adescapm,
    adescapf,
    adawesm,
    adawesf,
    adretm,
    adretf,
    adothm,
    adothf
  ) %>%
  rowwise() |>
  mutate(
    adm_viol_newm  = sum(parnewm, adcrnewm, na.rm = TRUE),
    adm_viol_newf  = sum(parnewf, adcrnewf, na.rm = TRUE),
    adm_viol_techm = sum(parnom, adcrnom, na.rm = TRUE),
    adm_viol_techf = sum(parnof, adcrnof, na.rm = TRUE),     
    adm_othm       = sum(adtransm, adawolm, adescapf, adawesm, adretm, adothm, na.rm = TRUE),
    adm_othf       = sum(adtransf, adawolf, adescapf, adawesf, adretf, adothf, na.rm = TRUE)
  ) %>%
  ungroup() |>
  select(
    -c(parnewm, parnom, adcrnewm, adcrnom,
       parnewf, parnof, adcrnewf, adcrnof,
       adtransm, adawesm, adretm, adothm,
       adtransf, adawesf, adretf, adothf,
       adawolm, adawolf, adescapm, adescapf)
  ) |>
  pivot_longer(-c(year, state_name, state_abbr), values_to = "n", names_to = "adm_type") %>%
  mutate(
    sex = if_else(str_ends(adm_type, "f"), "f", "m"),
    adm_type = str_sub(adm_type, 1, nchar(adm_type) - 1),
    adm_type = case_match(
      adm_type,
      "adtot"  ~ "adm_total",
      "commit" ~ "adm_new_commit",
      .default = adm_type
    )
  ) |>
  pivot_wider(names_from = sex, values_from = n) |>
  filter(!is.na(state_name))

admissions |>write_csv("nps-admissions.csv")


releases <- da38871.0001 |>
  janitor::clean_names() |>
  as_tibble() |>
  transmute(
    year,
    state_name = csg_state_convert(state, "abbr", "name"),
    state_abbr = as.character(state),
    rltotm,
    rltotf,
    rlunexpm,
    rlunexpf,
    rluncomm,
    rluncomf,
    rlunothm,
    rlunothf,
    rlcoprom,
    rlcoprof,
    rlcosupm,
    rlcosupf,
    rlcodpm,
    rlcodpf,
    rlcoothm,
    rlcoothf,
    rldeathm,
    rldeathf,
    rlawolm,
    rlawolf,
    rlescapm,
    rlescapf,
    rlawesm,
    rlawesf,
    rltranm,
    rltranf,
    rlbondm,
    rlbondf,
    rlothm,
    rlothf
  ) %>%
  rowwise() |>
  mutate(
    rel_totalm   = rltotm,
    rel_totalf   = rltotf,
    rel_uncondm  = sum(rlunexpm, rluncomm, rlunothm, na.rm = TRUE),
    rel_uncondf  = sum(rlunexpf, rluncomf, rlunothf, na.rm = TRUE),
    rel_condm    = sum(rlcoprom, rlcosupm, rlcodpm, rlcoothm, na.rm = TRUE),
    rel_condf    = sum(rlcoprof, rlcosupf, rlcodpf, rlcoothf, na.rm = TRUE),
    rel_othm     = sum(rldeathm, rlawolm, rlescapm, rlawesm, rltranm, rlbondm, rlothm, na.rm = TRUE),
    rel_othf     = sum(rldeathf, rlawolf, rlescapf, rlawesf, rltranf, rlbondf, rlothf, na.rm = TRUE)
  ) %>%
  ungroup() |>
  select(-starts_with("rl")) |>
  pivot_longer(-c(year, state_name, state_abbr), values_to = "n", names_to = "rel_type") %>%
  mutate(
    sex = if_else(str_ends(rel_type, "f"), "f", "m"),
    rel_type = str_sub(rel_type, 1, nchar(rel_type) - 1),
    rel_type
  ) |>
  pivot_wider(names_from = rel_type, values_from = n) |>
  filter(!is.na(state_name))

writexl::write_xlsx(releases, "nps-releases.xlsx")