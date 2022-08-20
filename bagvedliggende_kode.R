#' ---
#' title: Bagvedliggende R-kode for DM i Ping Pong
#' output:
#'    html_document:
#'      theme: united
#'      df_print: paged
#'      code_folding: show
#'      code_download: yes
#'      toc: true
#'      toc_float:
#'        smooth_scroll: yes
#' ---

# Opsætning ========================================================================================
#+ eval=F, warning=F, message=F

# source(file = "input_dm_ping_pong.R")
suppressPackageStartupMessages(lapply(c(
  "formattable", "lubridate", "readxl", "cellranger", "openxlsx", "writexl", "dplyr", "tidyr",
  "stringr", "kableExtra", "ggplot2",  "forcats", "plotDK", "httr", "rvest", "pdftools"),
  require, character.only = TRUE))
Sys.setlocale("LC_TIME", "Danish")
options(OutDec= ",")
options(knitr.kable.NA = "")

#' # Data
# Data #############################################################################################

#' ## Indlæsning og manipulation
#+ eval=F, warning=F, message=F

# Tilmeldinger
tbl0_join <- read_excel(
  tbl0_input$k_data,
  col_names = c("k_deltager_id", "k_ordredato", "🏓 DM i Ping Pong", "🥳 Fest om aftenen"),
  range = cell_cols("A:D")) %>%
  slice_tail(n = -5) %>%
  pivot_longer(
    cols = c("🏓 DM i Ping Pong", "🥳 Fest om aftenen"),
    names_to = "k_billettype",
    values_to = "k_status") %>%
  filter(!is.na(k_status)) %>%
  mutate(k_ordredato = convertToDateTime(k_ordredato)) %>%
  arrange(k_ordredato) %>%
  group_by(k_deltager_id, k_billettype, k_status) %>%
  mutate(k_gentilmelding = case_when(
    grepl("Tilmeldt", k_status) ~ paste0(cumsum(!duplicated(year(k_ordredato))), ". gang"))) %>%
  ungroup() %>%
  filter(between(
    year(k_ordredato), substr(tbl0_input$k_ordredato, 1, 4), substr(tbl0_input$k_ordredato, 6, 9))) %>%
  add_count(k_deltager_id, k_status, name = "k_billetantal") %>%
  mutate(k_status = case_when(
    grepl("Tilmeldt", k_status) ~ "✔️ Tilmeldt",
    grepl("Afbud",    k_status) ~ "❌ Afbud")) %>%
  mutate(k_tilmeldingstype = case_when(
    grepl("Afbud", k_status)                                                      ~ "❌ Afbud",
    as.Date(k_ordredato) <= dmy(tbl0_input$k_tilmeldingsfrist) & !is.na(k_status) ~ "🎫 Ordinær",
    as.Date(k_ordredato)  > dmy(tbl0_input$k_tilmeldingsfrist) & !is.na(k_status) ~ "🏃 Drive-in")) %>%
  
  # Deltagere
  left_join(
    y = read_excel(
      tbl0_input$k_data,
      col_names = c(
        "k_spiller_id", "k_navn",    "k_køn",   "k_klub",
        "k_rang1",      "k_rating2", "k_rang3", "k_deltager_id"),
      range = cell_cols("F:M")), by = "k_deltager_id") %>%
  mutate(k_klub = case_when(grepl("Ingen klub", k_klub) ~ "Ingen klub", TRUE ~ k_klub)) %>%
  mutate(k_rating = case_when(
    !is.na(k_rang1)  ~ paste0("[", k_rang1, "] ", k_rating2),
    is.na(k_rating2) ~ "-",
                TRUE ~ k_rating2)) %>%
  mutate(k_født = as.Date(if_else(
    paste0(substring(k_spiller_id, 5, 6)) <= substr(year(Sys.time()), 3, 4),
    paste0(
      substr(year(Sys.time()), 1, 2), substr(k_spiller_id, 5, 6), "-",
      substr(k_spiller_id, 3, 4), "-",
      substr(k_spiller_id, 1, 2)),
    paste0(
      as.numeric(substr(year(Sys.time()), 1, 2))-1, substr(k_spiller_id, 5, 6), "-",
      substr(k_spiller_id, 3, 4), "-",
      substr(k_spiller_id, 1, 2))))) %>%
  select(-k_spiller_id) %>%
  mutate(k_alder = trunc((k_født %--% Sys.time()) / years(1))) %>%
  mutate(k_aldersgruppe = case_when(
    k_alder <= 17 ~ "Ungdom",
    k_alder <= 39 ~ "Senior",
    k_alder >= 40 ~ "Veteran")) %>%
  group_by(k_deltager_id, k_status) %>%
  mutate(k_navn_klub_alder_status = case_when(
    grepl("Ingen klub", k_klub) ~ paste0(k_navn, " (", k_alder, " år) ", str_c(substr(
      k_billettype, 1, 1), collapse = "")),
    TRUE ~ paste0(k_navn, ", ", k_klub, " (", k_alder, " år) ", str_c(substr(
      k_billettype, 1, 1), collapse = "")))) %>%
  ungroup() %>%
  select(-k_navn) %>%
  
  # k_klub_d2
  left_join(
    y = read_excel(
      tbl0_input$k_data, col_names = c("k_klub", "k_postnr"),
      range = cell_cols("P:Q")), by = "k_klub") %>%
  mutate(k_postnr = na_if(k_postnr, "(Ukendt postnr.)")) %>%
  mutate(k_landsdel = case_when(
    k_postnr <= 3699 ~ "Sjælland",
    k_postnr <= 3799 ~ "Bornholm",
    k_postnr <= 4999 ~ "Sjælland",
    k_postnr <= 5999 ~ "Fyn",
    k_postnr <= 9999 ~ "Jylland")) %>%
  mutate(k_region = case_when(
    k_postnr <= 3799 ~ "Hovedstaden",
    k_postnr <= 4999 ~ "Sjælland",
    k_postnr <= 6899 ~ "Syddanmark",
    k_postnr <= 6999 ~ "Midtjylland",
    k_postnr <= 7199 ~ "Syddanmark",
    k_postnr <= 7699 ~ "Midtjylland",
    k_postnr <= 7799 ~ "Nordjylland",
    k_postnr <= 8999 ~ "Midtjylland",
    k_postnr <= 9999 ~ "Nordjylland")) %>%
  
  mutate_at(c(
    "k_billetantal",
    "k_rang1",
    "k_rating2",
    "k_rang3",
    "k_alder",
    "k_postnr"), as.numeric) %>%
  mutate(k_gentilmelding = as.factor(k_gentilmelding)) %>%
  mutate(
    k_køn = factor(k_køn, levels = c(
      "♂️ Herre",
      "♀️ Dame"), ordered = T),
    k_aldersgruppe = factor(k_aldersgruppe, levels = c(
      "Ungdom",
      "Senior",
      "Veteran"), ordered = T),
    k_billettype  = factor(k_billettype, levels = c(
      "🏓 DM i Ping Pong",
      "🥳 Fest om aftenen"), ordered = T),
    k_status = factor(k_status, levels = c(
      "✔️ Tilmeldt",
      "❌ Afbud"), ordered = T),
    k_tilmeldingstype  = factor(k_tilmeldingstype, levels = c(
      "🎫 Ordinær",
      "🏃 Drive-in",
      "❌ Afbud"), ordered = T),
    k_landsdel = factor(k_landsdel, levels = c(
      "Jylland",
      "Fyn",
      "Sjælland",
      "Bornholm",
      "Ingen klub"), ordered = T),
    k_region = factor(k_region, levels = c(
      "Nordjylland",
      "Midtjylland",
      "Syddanmark",
      "Sjælland",
      "Hovedstaden",
      "Ingen klub"), ordered = T)) %>%
  arrange(desc(k_ordredato))

#' ## Statistik
#+ eval=F, warning=F, message=F

tbl0_stat <- data.frame(
  
  # Sidst opdateret
  k_sidst_opdateret = paste0(
    "<i>Sidst opdateret ", format(
      floor_date(Sys.time(), "30 minutes"),
      "%d.%m.%Y kl. %H:%M"), "</i>"),
  
  # Billetantal total
  k_billetantal_total = paste0(
    tbl0_join %>%
      count(k_tilmeldingstype) %>%
      mutate(pct = percent(n/sum(n), digits = 0)) %>%
      mutate(label = paste0(substring(k_tilmeldingstype, 1, 1), " ", n, " (", pct, ")")) %>%
      summarise(label = str_c(label, collapse = " ∙ "))),
  
  # Billetantal billettype
  k_billetantal_billettype = paste0(
    tbl0_join %>% filter(
    grepl("Tilmeldt", k_status)) %>%
    count(k_billettype) %>%
    mutate(pct = percent(n/sum(n), digits = 0)) %>%
    mutate(label = paste0(substring(k_billettype, 1, 1), " ", n)) %>%
    summarise(label = str_c(label, collapse = " ∙ "))),
  
  # Deltagerantal total
  k_deltagerantal_total = paste0(
    tbl0_join %>%
      distinct(k_deltager_id, k_status, .keep_all = T) %>%
      add_count(k_deltager_id) %>%
      filter(grepl("Tilmeldt", k_status) | n == 1) %>%
      count(k_status) %>%
      mutate(k_status = case_when(
        grepl("Tilmeldt", k_status) ~ "Tilmeldt",
        grepl("Afbud", k_status) ~ '"Totalafbud"')) %>%
      mutate(pct = percent(n/sum(n), digits = 0)) %>%
      mutate(label = paste0("👤 ", n, " ", k_status, " (", pct, ")")) %>%
      summarise(label = str_c(label, collapse = " ∙ "))),
  
  # Gentilmeldinger
  k_deltagerantal_gentilmelding = paste0(
    tbl0_join %>%
      mutate(k_gentilmelding = case_when(
        substr(k_gentilmelding, 1, 1) <= 1 ~ "Debutant", TRUE ~ "Gentilmelding")) %>%
      filter(grepl("Tilmeldt", k_status)) %>%
      distinct(k_deltager_id, k_gentilmelding, .keep_all = T) %>%
      add_count(k_deltager_id) %>%
      filter(grepl("Gentilmelding", k_gentilmelding) | n == 1) %>%
      count(k_gentilmelding) %>%
      mutate(pct = percent(n/sum(n), digits = 0)) %>%
      mutate(label = paste0("👤 ", n, " ", k_gentilmelding, " (", pct, ")")) %>%
      summarise(label = str_c(label, collapse = " ∙ "))),
  
  # Deltagerantal køn
  k_deltagerantal_køn = paste0(
    tbl0_join %>% filter(
      grepl("Tilmeldt", k_status)) %>%
      distinct(k_deltager_id, .keep_all = T) %>%
      count(k_køn) %>%
      mutate(pct = percent(n/sum(n), digits = 0)) %>%
      mutate(label = paste0("👤 ", n, " ", substring(k_køn, first = 4), " (", pct, ")")) %>%
      summarise(label = str_c(label, collapse = " ∙ "))),
  
  # Deltagerantal aldersgruppe
  k_deltagerantal_aldersgruppe = paste0(
    tbl0_join %>% filter(
      grepl("Tilmeldt", k_status)) %>%
      distinct(k_deltager_id, .keep_all = T) %>%
      count(k_aldersgruppe) %>%
      mutate(pct = percent(n/sum(n), digits = 0)) %>%
      mutate(label = paste0("👤 ", n, " ", k_aldersgruppe, " (", pct, ")")) %>%
      summarise(label = str_c(label, collapse = " ∙ "))),
  
  # Alder
  k_alder = paste0(
    "👤 Min. ", tbl0_join %>% filter(
      grepl("Tilmeldt", k_status)) %>%
      distinct(k_deltager_id, .keep_all = T) %>%
      summarise(min(k_alder)), " år",
    " ∙ 👤 Gns. ", tbl0_join %>% filter(
      grepl("Tilmeldt", k_status)) %>%
      distinct(k_deltager_id, .keep_all = T) %>%
      summarise(round(mean(k_alder), 0)), " år",
    " ∙ 👤 Maks. ", tbl0_join %>% filter(
      grepl("Tilmeldt", k_status)) %>%
      distinct(k_deltager_id, .keep_all = T) %>%
      summarise(max(k_alder)), " år"),
  
  # Deltagerantal_landsdel
  k_deltagerantal_landsdel = paste0(
    "<b>Landsdel</b>: ", tbl0_join %>% filter(
      grepl("Tilmeldt", k_status) &
        !grepl("Ingen klub", k_klub)) %>%
      distinct(k_deltager_id, .keep_all = T) %>%
      count(k_landsdel) %>%
      mutate(pct = percent(n/sum(n), digits = 0)) %>%
      mutate(label = paste0("👤 ", n, " ", k_landsdel, " (", pct, ")")) %>%
      summarise(label = str_c(label, collapse = " ∙ "))),
  
  # Deltagerantal region
  k_deltagerantal_region = paste0(
    "<b>Region</b>: ", tbl0_join %>% filter(
      grepl("Tilmeldt", k_status) &
        !grepl("Ingen klub", k_klub)) %>%
      distinct(k_deltager_id, .keep_all = T) %>%
      count(k_region) %>%
      mutate(pct = percent(n/sum(n), digits = 0)) %>%
      mutate(label = paste0("👤 ", n, " ", k_region, " (", pct, ")")) %>%
      summarise(label = str_c(label, collapse = " ∙ "))),
  
  # Deltagerantal ingen klub
  k_deltagerantal_ingen_klub = paste0(
    tbl0_join %>% filter(
      grepl("Tilmeldt", k_status)) %>%
      distinct(k_deltager_id, .keep_all = T) %>%
      count(k_klub) %>%
      mutate(pct = percent(n/sum(n), digits = 0)) %>%
      mutate(label = paste0("👤 ", n, " ", k_klub, " (", pct, ")")) %>%
      filter(grepl("Ingen klub", k_klub)) %>%
      summarise(label = str_c(label, collapse = " ∙ "))),
  
  # Klubantal
  k_klubantal = paste0(
    "🛖 ", tbl0_join %>% filter(
      grepl("Tilmeldt", k_status) &
        !grepl("Ingen klub", k_klub)) %>%
      distinct(k_klub, .keep_all = T) %>%
      summarise(n()), " klubber"),

  # Billetantal Ping Pong (numerisk)
  k_num_billetantal_ping_pong = as.numeric(
    tbl0_join %>% filter(
      grepl("Tilmeldt", k_status) &
        grepl("Ping Pong", k_billettype)) %>%
      distinct(k_deltager_id, .keep_all = T) %>%
      summarise(n())),
  
  # Billetantal fest (numerisk)
  k_num_billetantal_fest = as.numeric(
    tbl0_join %>% filter(
      grepl("Tilmeldt", k_status) &
        grepl("Fest", k_billettype)) %>%
      distinct(k_deltager_id, .keep_all = T) %>%
      summarise(n())),
  
  # Antal puljer (numerisk)
  k_num_antal_puljer = as.numeric(
    tbl0_join %>% filter(
      grepl("Tilmeldt", k_status) &
        grepl("Ping Pong", k_billettype)) %>%
      distinct(k_deltager_id, .keep_all = T) %>%
      summarise(ceiling(n()/tbl0_input$k_puljeantal))),
  
  # Præmiepenge total i pct.
  k_num_præmie_total = tbl0_input$k_præmie_A1+tbl0_input$k_præmie_A2+tbl0_input$k_præmie_A3+
    tbl0_input$k_præmie_A4+tbl0_input$k_præmie_B1+tbl0_input$k_præmie_B2,
  
  # Præmiepenge for A-slutspil i pct.
  k_num_præmie_A = tbl0_input$k_præmie_A1+tbl0_input$k_præmie_A2+
    tbl0_input$k_præmie_A3+tbl0_input$k_præmie_A4,
  
  # Præmiepenge for B-slutspil i pct.
  k_num_præmie_B = tbl0_input$k_præmie_B1+tbl0_input$k_præmie_B2,
  
  check.names = F)

#' # Præmier
# Præmier ##########################################################################################

#' ## Pengepræmier
#+ eval=F, warning=F, message=F

tbl1_præmier_penge <- data.frame(
  " " = c(
    "Præmiesum",
    "A-slutspil",
    "1. plads",
    "2. plads",
    "3. plads",
    "4. plads",
    "B. slutspil",
    "1. plads",
    "2. plads"),
  "Foreløbig" = c(
    paste("kr.", format(round(
      0.001+tbl0_input$k_præmiepenge*tbl0_stat$k_num_billetantal_ping_pong*
        tbl0_stat$k_num_præmie_total, 0), big.mark = ".")),
    paste("kr.", format(round(
      0.001+tbl0_input$k_præmiepenge*tbl0_stat$k_num_billetantal_ping_pong*
        tbl0_stat$k_num_præmie_A,     0), big.mark = ".")),
    paste("kr.", format(round(
      0.001+tbl0_input$k_præmiepenge*tbl0_stat$k_num_billetantal_ping_pong*
        tbl0_input$k_præmie_A1,       0), big.mark = ".")),
    paste("kr.", format(round(
      0.001+tbl0_input$k_præmiepenge*tbl0_stat$k_num_billetantal_ping_pong*
        tbl0_input$k_præmie_A2,       0), big.mark = ".")),
    paste("kr.", format(round(
      0.001+tbl0_input$k_præmiepenge*tbl0_stat$k_num_billetantal_ping_pong*
        tbl0_input$k_præmie_A3,       0), big.mark = ".")),
    paste("kr.", format(round(
      0.001+tbl0_input$k_præmiepenge*tbl0_stat$k_num_billetantal_ping_pong*
        tbl0_input$k_præmie_A4,       0), big.mark = ".")),
    paste("kr.", format(round(
      0.001+tbl0_input$k_præmiepenge*tbl0_stat$k_num_billetantal_ping_pong*
        tbl0_stat$k_num_præmie_B,     0), big.mark = ".")),
    paste("kr.", format(round(
      0.001+tbl0_input$k_præmiepenge*tbl0_stat$k_num_billetantal_ping_pong*
        tbl0_input$k_præmie_B1,       0), big.mark = ".")),
    paste("kr.", format(round(
      0.001+tbl0_input$k_præmiepenge*tbl0_stat$k_num_billetantal_ping_pong*
        tbl0_input$k_præmie_B2,       0), big.mark = "."))),
  "Potentiel" = c(
    paste("kr.", format(round(
      0.001+tbl0_input$k_præmiepenge*tbl0_input$k_billetantal_ping_pong_maks*
        tbl0_stat$k_num_præmie_total, 0), big.mark = ".")),
    paste("kr.", format(round(
      0.001+tbl0_input$k_præmiepenge*tbl0_input$k_billetantal_ping_pong_maks*
        tbl0_stat$k_num_præmie_A,     0), big.mark = ".")),
    paste("kr.", format(round(
      0.001+tbl0_input$k_præmiepenge*tbl0_input$k_billetantal_ping_pong_maks*
        tbl0_input$k_præmie_A1,       0), big.mark = ".")),
    paste("kr.", format(round(
      0.001+tbl0_input$k_præmiepenge*tbl0_input$k_billetantal_ping_pong_maks*
        tbl0_input$k_præmie_A2,       0), big.mark = ".")),
    paste("kr.", format(round(
      0.001+tbl0_input$k_præmiepenge*tbl0_input$k_billetantal_ping_pong_maks*
        tbl0_input$k_præmie_A3,       0), big.mark = ".")),
    paste("kr.", format(round(
      0.001+tbl0_input$k_præmiepenge*tbl0_input$k_billetantal_ping_pong_maks*
        tbl0_input$k_præmie_A4,       0), big.mark = ".")),
    paste("kr.", format(round(
      0.001+tbl0_input$k_præmiepenge*tbl0_input$k_billetantal_ping_pong_maks*
        tbl0_stat$k_num_præmie_B,     0), big.mark = ".")),
    paste("kr.", format(round(
      0.001+tbl0_input$k_præmiepenge*tbl0_input$k_billetantal_ping_pong_maks*
        tbl0_input$k_præmie_B1,       0), big.mark = ".")),
    paste("kr.", format(round(
      0.001+tbl0_input$k_præmiepenge*tbl0_input$k_billetantal_ping_pong_maks*
        tbl0_input$k_præmie_B2,       0), big.mark = "."))),
  "Pct." = c(
    paste0(digits(0.001+100*tbl0_stat$k_num_præmie_total, 0), "%"),
    paste0(digits(0.001+100*tbl0_stat$k_num_præmie_A,     1), "%"),
    paste0(digits(0.001+100*tbl0_input$k_præmie_A1,       1), "%"),
    paste0(digits(0.001+100*tbl0_input$k_præmie_A2,       0), "%"),
    paste0(digits(0.001+100*tbl0_input$k_præmie_A3,       0), "%"),
    paste0(digits(0.001+100*tbl0_input$k_præmie_A4,       0), "%"),
    paste0(digits(0.001+100*tbl0_stat$k_num_præmie_B,     1), "%"),
    paste0(digits(0.001+100*tbl0_input$k_præmie_B1,       1), "%"),
    paste0(digits(0.001+100*tbl0_input$k_præmie_B2,       0), "%")),
  check.names = F)
kbl1_præmier_penge <- tbl1_præmier_penge %>%
  kbl(col.names = NA, align = "lrrr", escape = F) %>%
  kable_classic(position = "l", full_width = F, html_font = "verdana") %>%
  add_header_above(
    c("💰 <b>Præmiepenge</b> (afrundet)" = 4),
    italic = T, align = "c", font_size = "small", escape = F,
    extra_css = "border:hidden;border-bottom:1.5px solid #111111") %>%
  pack_rows(start_row = 3, end_row = 6) %>%
  pack_rows(start_row = 8, end_row = 9) %>%
  row_spec(0, background = tbl0_input$k_farve1, color = "#FFFFFF") %>%
  row_spec(1, bold = T, background = tbl0_input$k_farve2) %>%
  row_spec(c(2, 7), background = tbl0_input$k_farve2) %>%
  column_spec(c(3, 4), italic = T, color = tbl0_input$k_farve1) %>%
  footnote(paste0(
    "<i style=font-size:80%>Foreløbig = ",
    tbl0_stat$k_num_billetantal_ping_pong, " deltagere x kr. ",
    tbl0_input$k_præmiepenge, " (maks ",
    tbl0_input$k_billetantal_ping_pong_maks, " deltagere).<br>",
    "Diplomer uddeles til alle gave-/præmietagere.</i>"),
    general_title = "", escape = F)
kbl1_præmier_penge

#' ## Gaver
#+ eval=F, warning=F, message=F

tbl1_præmier_yngst_ældst <- tbl0_join %>% filter(
  grepl("Tilmeldt", k_status) &
    grepl("Ping Pong", k_billettype) &
    (k_født == max(k_født) |
       k_født == min(k_født))) %>%
  mutate(k_alder = paste0(k_alder, " år")) %>%
  mutate(k_født  = format(k_født, "%d.%m.%Y")) %>%
  select(
    "Navn"  = k_navn_klub_alder_status,
    "Født"  = k_født)
kbl1_præmier_yngst_ældst <- tbl1_præmier_yngst_ældst %>%
  kbl(col.names = NA, align = "lc", escape = F) %>%
  kable_classic(position = "l", full_width = F, html_font = "verdana") %>%
  add_header_above(
    c("🎁 Gave til <b>yngste</b>- og <b>ældste</b> Ping Pong deltager" = 2),
    italic = T, align = "c", font_size = "small", escape = F,
    extra_css = "border:hidden;border-bottom:1.5px solid #111111") %>%
  row_spec(0, background = tbl0_input$k_farve1, color = "#FFFFFF")
kbl1_præmier_yngst_ældst

#' # Deltagere
# Deltagere ########################################################################################

#' ## Foreløbige deltagere
#+ eval=F, warning=F, message=F

tbl2_deltagere_foreløbig <- tbl0_join %>%
  distinct(k_deltager_id, k_status, .keep_all = T) %>%
  arrange(
    k_status,
    desc(k_født),
    k_deltager_id) %>%
  mutate(Nr. = case_when(grepl("Tilmeldt", k_status) ~ row_number())) %>%
  select(
    Nr.,
    "Navn" = k_navn_klub_alder_status,
    k_status)
kbl2_deltagere_foreløbig <- tbl2_deltagere_foreløbig %>%
  kbl(col.names = NA, align = "cl", escape = F, caption = "Foreløbige deltagere") %>%
  kable_classic(position = "l", full_width = F, html_font = "verdana") %>%
  add_header_above(
    c("Sorteret efter fødselsdato" = 3),
    italic = T, align = "c", font_size = "small", escape = F,
    extra_css = "border:hidden;border-bottom:1.5px solid #111111") %>%
  row_spec(0, background = tbl0_input$k_farve1, color = "#FFFFFF") %>%
  row_spec(which(grepl("Afbud", tbl2_deltagere_foreløbig$k_status)),
           strikeout = T, italic = T, color = tbl0_input$k_farve1) %>%
  remove_column(3)
kbl2_deltagere_foreløbig

#' ## Puljer
#+ eval=F, warning=F, message=F

tbl2_deltagere_puljer <- tbl0_join %>%
  filter(
    grepl("Tilmeldt", k_status) &
      grepl("Ping Pong", k_billettype)) %>%
  arrange(
    k_rang1,
    desc(k_rating2),
    k_rang3,
    k_deltager_id) %>%
  add_row(k_deltager_id = rep(
    NA, tbl0_stat$k_num_antal_puljer*tbl0_input$k_puljeantal-
      tbl0_stat$k_num_billetantal_ping_pong+tbl0_stat$k_num_antal_puljer)) %>%
  mutate(Seedningslag = ceiling(row_number()/tbl0_stat$k_num_antal_puljer)) %>%
  group_by(Seedningslag) %>%
  mutate(Puljenr. = case_when(
    Seedningslag %% 2 == 1 ~ row_number(),
    Seedningslag %% 1 == 0 ~ order(row_number(), decreasing = T))) %>%
  ungroup() %>%
  mutate(Nr. = row_number()) %>%
  arrange(Puljenr.) %>%
  filter(!is.na(k_deltager_id)) %>%
  select(
    Nr.,
    "Navn og klub" = k_navn_klub_alder_status,
    "Alder"        = k_alder,
    "Rating"       = k_rating,
    Seedningslag)

kbl2_deltagere_puljer <- tbl2_deltagere_puljer %>%
  kbl(col.names = NA, align = "clcr", escape = F,
      caption = paste0(tbl0_input$k_puljeantal, "-mandspuljer efter snake-system")) %>%
  kable_classic(position = "l", full_width = F, html_font = "verdana") %>%
  add_header_above(
    c("Bemærk: Puljerne kan ændre sig ved drive-in/afbud" = 5),
    italic = T, align = "c", font_size = "small", escape = F,
    extra_css = "border:hidden;border-bottom:1.5px solid #111111") %>%
  row_spec(0, background = tbl0_input$k_farve1, color = "#FFFFFF") %>%
  row_spec(which(tbl2_deltagere_puljer$Seedningslag == "1"), bold = T,
           extra_css = "border:hidden;border-top:0.7px solid #111111") %>%
  remove_column(5)
kbl2_deltagere_puljer

#' ## Kun til festen inkl. afbud
#+ eval=F, warning=F, message=F

tbl2_deltagere_fest <- tbl0_join %>%
  filter(grepl("Fest", k_billettype) &
           k_billetantal == 1 |
           grepl("Afbud", k_status)) %>%
  distinct(k_deltager_id, k_status, .keep_all = T) %>%
  arrange(
    k_status,
    k_billetantal,
    match(k_billettype, "Fest"),
    desc(k_født),
    k_deltager_id) %>%
  mutate(Nr. = case_when(grepl("Tilmeldt", k_status) ~ row_number())) %>%
  select(
    Nr.,
    "Navn" = k_navn_klub_alder_status,
    k_status)
kbl2_deltagere_fest <- tbl2_deltagere_fest %>%
  kbl(col.names = NA, align = "cl", escape = F, caption = "Kun med til festen") %>%
  kable_classic(position = "l", full_width = F, html_font = "verdana") %>%
  add_header_above(
    c("Sorteret efter fødselsdato" = 3),
    italic = T, align = "c", font_size = "small", escape = F,
    extra_css = "border:hidden;border-bottom:1.5px solid #111111") %>%
  row_spec(0, background = tbl0_input$k_farve1, color = "#FFFFFF") %>%
  row_spec(which(grepl("Afbud", tbl2_deltagere_fest$k_status)),
           strikeout = T, italic = T, color = tbl0_input$k_farve1) %>%
  remove_column(3)
kbl2_deltagere_fest

#' # Grafer
# Grafer ###########################################################################################

#' ## Tilmeldingstype
#+ eval=F, warning=F, message=F

graf1_tilmeldingstype <- tbl0_join %>%
  ggplot(mapping = aes(y = fct_rev(k_billettype), fill = k_tilmeldingstype)) +
  geom_bar(position = position_stack(reverse = T)) +
  geom_text(aes(label = ..count..), stat = "count", position = position_stack(reverse = T),
            vjust = 0.4, hjust = 1, size = 5, color = "#FFFFFF") +
  scale_fill_manual(values = c(
    "🎫 Ordinær"  = "#635994",
    "🏃 Drive-in" = "#FF6723",
    "❌ Afbud"    = "#fF2f60")) +
  labs(title = "Tilmeldingstype",
       subtitle = paste0(tbl0_stat$k_deltagerantal_total)) +
  guides(fill = guide_legend(title = element_blank())) +
  xlab(element_blank()) + ylab(element_blank()) +
  theme() +
  theme(legend.position      = "right",
        legend.direction     = "vertical",
        plot.title           = element_text(hjust = 0.5),
        plot.subtitle        = element_text(hjust = 0.5),
        axis.text.x          = element_blank(),
        axis.ticks.x         = element_blank(),
        axis.ticks.y         = element_blank(),
        panel.background     = element_blank(),
        panel.grid.major     = element_blank(),
        panel.grid.minor     = element_blank())
graf1_tilmeldingstype

#' ## Gentilmeldinger
#+ eval=F, warning=F, message=F

graf2_gentilmelding <- tbl0_join %>%
  filter(grepl("Tilmeldt", k_status)) %>%
  ggplot(mapping = aes(y = fct_rev(k_billettype), fill = k_gentilmelding)) +
  geom_bar(position = position_stack(reverse = T)) +
  geom_text(aes(label = ..count..), stat = "count", position = position_stack(reverse = T),
            vjust = 0.4, hjust = 1, size = 5, color = "#FFFFFF") +
  labs(title = "Gentilmeldinger siden 2021",
       subtitle = paste0(tbl0_stat$k_deltagerantal_gentilmelding)) +
  guides(fill = guide_legend(title = element_blank())) +
  xlab(element_blank()) + ylab(element_blank()) +
  theme() +
  theme(legend.position      = "right",
        legend.direction     = "vertical",
        plot.title           = element_text(hjust = 0.5),
        plot.subtitle        = element_text(hjust = 0.5),
        axis.text.x          = element_blank(),
        axis.ticks.x         = element_blank(),
        axis.ticks.y         = element_blank(),
        panel.background     = element_blank(),
        panel.grid.major     = element_blank(),
        panel.grid.minor     = element_blank())
graf2_gentilmelding

#' ## Klubber
#+ eval=F, warning=F, message=F

graf3_klubber <- tbl0_join %>%
  filter(grepl("Tilmeldt", k_status)) %>%
  ggplot(mapping = aes(y = fct_rev(fct_infreq(k_klub)), fill = k_billettype)) +
  geom_bar(position = position_stack(reverse = T)) +
  geom_text(aes(label = ..count..), stat = "count", position = position_stack(reverse = T),
            vjust = 0.4, hjust = 1, size = 5, color = "#FFFFFF") +
  scale_fill_manual(values = c(
    "🏓 DM i Ping Pong" = "#398FCC",
    "🥳 Fest om aftenen" = "#FFB02E")) +
  labs(title = "Klubber",
       subtitle = paste0(tbl0_stat$k_klubantal)) +
  guides(fill = guide_legend(title = element_blank())) +
  xlab(element_blank()) + ylab(element_blank()) +
  theme() +
  theme(legend.position      = "right",
        legend.direction     = "vertical",
        plot.title           = element_text(hjust = 0.5),
        plot.subtitle        = element_text(hjust = 0.5),
        axis.text.x          = element_blank(),
        axis.ticks.x         = element_blank(),
        axis.ticks.y         = element_blank(),
        panel.background     = element_blank(),
        panel.grid.major     = element_blank(),
        panel.grid.minor     = element_blank())
graf3_klubber

#' ## Deltagere fordelt på Danmarkskort
#+ eval=F, warning=F, message=F

graf4_DK <- tbl0_join %>% filter(
  !is.na(k_postnr) & grepl("Tilmeldt", k_status)) %>%
  distinct(k_deltager_id, .keep_all = T) %>%
  mutate(k_region = case_when(
    k_region == "Nordjylland" ~ "nordjylland",
    k_region == "Midtjylland" ~ "midtjylland",
    k_region == "Syddanmark"  ~ "syddanmark",
    k_region == "Sjælland"    ~ "sjaelland",
    k_region == "Hovedstaden" ~ "hovedstaden")) %>%
  count(k_postnr, name = "Deltagerantal", .drop = FALSE) %>%
  plotDK::plotDK(
    value        = "Deltagerantal",
    id           = "k_postnr",
    plotlevel    = "zipcode",
    show_missing = T,
    show_borders = F,
    interactive  = F,
    titel        = "Deltagere fordelt på Danmarkskort")
graf4_DK

#' # TRUE/FALSE
# TRUE/FALSE #######################################################################################

#' ## BilletFix eventordre
#+ eval=F, warning=F, message=F

if(tbl0_input$k_eventordre_T_F == T) {
  
  # Hentning af eventordre
  list3_eventordre <- content(GET(
    url = paste0("https://billetfix.dk/api/v3/events/",
                 tbl0_input$k_eventordre_url_uuid, "/orders"),
    config = c(
      add_headers(Authorization = tbl0_input$k_eventordre_token),
      content_type("application/json"))))
  
  # Udtrækning af relevante listelementer indsættes i tabel
  tbl3_eventordre <- data.frame(
    k_id     = do.call(rbind, as.list(
      list3_eventordre$orders %>% sapply(., `[[`, "tickets") %>%
        sapply(., '[', seq(max(sapply(., length)))) %>%
        sapply(., `[[`, c("purchase_uuid")))),
    k_navn   = do.call(rbind, as.list(
      list3_eventordre$orders %>% sapply(., `[[`, "tickets") %>%
        sapply(., '[', seq(max(sapply(., length)))) %>%
        sapply(., `[[`, c("full_name")))),
    k_billettype      = do.call(rbind, as.list(
      list3_eventordre$orders %>% sapply(., `[[`, "tickets") %>%
        sapply(., '[', seq(max(sapply(., length)))) %>%
        sapply(., `[[`, c("ticket_type_name")))),
    k_billetpris      = do.call(rbind, as.list(
      list3_eventordre$orders %>% sapply(., `[[`, "tickets") %>%
        sapply(., '[', seq(max(sapply(., length)))) %>%
        sapply(., `[[`, c("price"))))) %>%
    left_join(
      y = data.frame(
        k_id        = gsub("-", "", sapply(list3_eventordre$orders, `[[`, c("uuid"))),
        k_ordredato = sapply(list3_eventordre$orders, `[[`, c("date")),
        k_status    = sapply(list3_eventordre$orders, `[[`, c("state"))),
      by = "k_id") %>%
    mutate_at(c("k_ordredato"), as_datetime) %>%
    mutate(k_billettype = fct_rev(k_billettype),
           k_status = as.factor(k_status)) %>%
    mutate_at(c("k_billetpris"), as.numeric) %>%
    mutate(k_ordredato = k_ordredato + hours(2)) %>%
    arrange(desc(k_ordredato)) %>%
    select(k_navn, k_ordredato, k_billettype, k_status, k_billetpris)
  View(tbl3_eventordre)
  shell.exec(gsub("\\\\", "/", normalizePath(tbl0_input$k_data)))
  cat(paste0(
    tbl3_eventordre %>%
      filter(grepl("PAID", k_status)) %>%
      summarise(label = paste("💰 kr.", format(sum(k_billetpris), big.mark = "."), "(PAID)")), "\n",
    tbl3_eventordre %>%
      filter(grepl("PAID", k_status)) %>%
      group_by(k_billettype) %>%
      summarise(label = paste("kr.", format(sum(k_billetpris), big.mark = "."), "(PAID)")) %>%
      mutate(label = paste(substr(k_billettype, 1, 1), label)) %>%
      summarise(label = str_c(label, collapse = "\n")), "\n\n",
    tbl3_eventordre %>%
      count(k_status) %>%
      mutate(pct = percent(n/sum(n), digits = 0)) %>%
      mutate(label = paste0("🎫 ", k_status, " ", n, " (", pct, ")")) %>%
      summarise(label = str_c(label, collapse = "\n")), "\n\n",
    tbl3_eventordre %>%
      count(k_status, k_billettype) %>%
      mutate(pct = percent(n/sum(n), digits = 0)) %>%
      mutate(label = paste0(substr(k_billettype, 1, 1), " ", k_status, " ", n, " (", pct, ")")) %>%
      summarise(label = str_c(label, collapse = "\n")), "\n\n"))
} else if (tbl0_input$k_eventordre_T_F == F) {"tbl0_input$k_eventordre_T_F = F"}

#' ## PDF til PNG for indbydelsesplakat
#+ eval=F, warning=F, message=F

if(tbl0_input$k_indbydelsesplakat_png_T_F == T) {
  pdf_convert(pdf       = paste0(tbl0_input$k_indbydelsesplakat, ".pdf"),
              filenames = paste0(tbl0_input$k_indbydelsesplakat, ".png"),
              verbose   = F,
              dpi       = 300)
  shell.exec(gsub("\\\\", "/", normalizePath(paste0(tbl0_input$k_indbydelsesplakat, ".png"))))
} else if (tbl0_input$k_indbydelsesplakat_png_T_F == F) {"tbl0_input$k_indbydelsesplakat_png_T_F = F"}

#' ## Webscraping af ratinglisten
#+ eval=F, warning=F, message=F

if(tbl0_input$k_webscraping_rating_T_F == T) {
  tbl3_webscraping_rating <- data.frame()
  for (side in seq(from = 1, to = 50, by = 1)) {
    link <- paste0("https://bordtennisportalen.dk/DBTU/Ranglister/Udskriv/?params=,59,42022,,,,,True,,,,,", 
                   side-1, ",,,0,,,,,")
    
    tbl3_webscraping_rating <- rbind(tbl3_webscraping_rating, data.frame(
      "Plac"       = read_html(link) %>% html_nodes(".rank")                        %>% html_text(),
      "Spiller_Id" = read_html(link) %>% html_nodes(".playerid")                    %>% html_text(),
      "Navn"       = read_html(link) %>% html_nodes(".name")                        %>% html_text(),
      "Rating"     = read_html(link) %>% html_nodes(".name+ .pointsw")              %>% html_text(),
      "Plus_minus" = read_html(link) %>% html_nodes(".pointsw:nth-child(5)")        %>% html_text(),
      "Kampe"      = read_html(link) %>% html_nodes(".pointsw~ .pointsw+ .pointsw") %>% html_text(),
      stringsAsFactors = FALSE)) %>% filter(Spiller_Id != "Spiller-Id") %>%
      mutate_at(c("Plac", "Rating", "Plus_minus", "Kampe"), as.numeric)
    print(paste("Side:", side))
  }
  tbl3_webscraping_rating <- tbl3_webscraping_rating %>%
    separate(Navn, into = c("Navn", "Klub"), sep=",.", extra = "merge")
  tbl3_join_webscraping_rating <- read_excel(
    tbl0_input$k_data, col_names = c("Spiller_Id"), range = cell_cols("F:F")) %>%
    slice_tail(n = -3) %>%
    left_join(
      y = tbl3_webscraping_rating,
      by = "Spiller_Id") %>%
    select(Plac, Spiller_Id, Navn, Klub, Rating, Plus_minus, Kampe)
  write_xlsx(tbl3_webscraping_rating, path = "Filer\\Webscraping rating.xlsx")
  write_xlsx(tbl3_join_webscraping_rating, path = "Filer\\Webscraping join rating.xlsx")
  shell.exec(normalizePath("Filer\\Webscraping join rating.xlsx"))
} else if(tbl0_input$k_webscraping_rating_T_F == F) {"tbl0_input$k_webscraping_rating_T_F = F"}

#' ## Webscraping af BTEX Ping Pong bat
#+ eval=F, warning=F, message=F

tbl3_webscraping_btex <- data.frame()
link <- paste0("https://www.btex.dk/sanwei-wcpp-sandpapirsbat.html")

tbl3_webscraping_btex <- rbind(tbl3_webscraping_btex, data.frame(
  "produkt"      = read_html(link) %>% html_nodes(".name")                        %>% html_text(),
  "pris"         = read_html(link) %>% html_nodes(".price")                       %>% html_text(),
  "lagerstatus"  = read_html(link) %>% html_nodes(".title span")                  %>% html_text(),
  "levering"     = read_html(link) %>% html_nodes("#product_addtocart_form .txt") %>% html_text(),
  stringsAsFactors = FALSE))
