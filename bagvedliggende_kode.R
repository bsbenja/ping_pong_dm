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

suppressWarnings(suppressPackageStartupMessages(lapply(c(
  "readxl", "cellranger", "openxlsx", "writexl", "dplyr", "tidyr", "stringr", "formattable",
  "lubridate", "kableExtra", "ggplot2", "forcats", "plotDK", "httr", "rvest", "pdftools", "rmarkdown"),
  require, character.only = TRUE)))
Sys.setlocale("LC_TIME", "Danish")
options(OutDec= ",")
options(knitr.kable.NA = "")

#' # Data
# Data #############################################################################################

#' ## Indlæsning og manipulation
#+ eval=F, warning=F, message=F

# Importer tilmeldinger fra Excel
tbl0_join_alle <- read_excel(
  tbl0_input$k_data,
  col_names = c("k_deltager_id", "k_navn", "k_klub", "k_køn", "k_ordredato",
                "k_event_år_billettype", "k_status", "k_rang1", "k_rating2",
                "k_rang3", "k_slutspil", "k_placering", "k_præmiepenge", "k_præmiepenge_pct"),
  range = cell_cols("A:N")) %>%
  slice_tail(n = -5) %>%
  
  # Left join Bordtennisklub_d1 fra Excel
  left_join(
    y = read_excel(
      tbl0_input$k_data, col_names = c("k_klub", "k_postnr"),
      range = cell_cols("Q:R")), by = "k_klub") %>%
  
  # Left join Event_år_billettype_d1 fra Excel
  left_join(
    y = read_excel(
      tbl0_input$k_data, col_names = c(
        "k_event_år", "k_billettype", "k_billettype_tilvalg", "k_billetpris",
        "k_arrangørpris", "k_billetantal_maks", "k_event_år_billettype"),
      range = cell_cols("W:AC")), by = "k_event_år_billettype") %>%
  
  # Left join Event_år_d2 fra Excel
  left_join(
    y = read_excel(
      tbl0_input$k_data, col_names = c(
        "k_eventnr", "k_event", "k_eventdato", "k_tilmeldingsfrist", "k_åbningsdato",
        "k_ratingopdatering", "k_puljeantal", "k_eventsted", "k_eventadresse", "k_eventpostnr",
        "k_eventby", "k_uuid", "k_token", "k_event_år"),
      range = cell_cols("V:AI")), by = "k_event_år") %>%
  
  # k_deltager_id
  mutate(across("k_deltager_id", as.character)) %>%
  
  # k_navn
  mutate(across("k_navn", as.character)) %>%
  
  # k_klub
  mutate(across("k_klub", .fns = factor, ordered = T)) %>%
  
  # k_køn
  mutate(across("k_køn", .fns = factor, levels = c(
    "♂️ Herre",
    "♀️ Dame"), ordered = T)) %>%
  
  # k_ordredato
  mutate(across("k_ordredato", convertToDateTime)) %>%
  
  # k_status
  mutate(k_status = case_when(
    grepl("Tilmeldt", k_status) ~ "✔️ Tilmeldt",
    grepl("Afbud",    k_status) ~ "❌ Afbud")) %>%
  mutate(across("k_status", .fns = factor, levels = c(
    "✔️ Tilmeldt",
    "❌ Afbud"), ordered = T)) %>%
  
  # k_rang1
  mutate(across("k_rang1", as.integer)) %>%
  
  # k_rating2
  mutate(across("k_rating2", as.integer)) %>%
  
  # k_rang3
  mutate(across("k_rang3", as.integer)) %>%
  
  # k_slutspil
  mutate(across("k_slutspil", .fns = factor, ordered = T)) %>%
  
  # k_placering
  mutate(across("k_placering", as.integer)) %>%
  
  # k_præmiepenge
  mutate(across("k_præmiepenge", as.numeric)) %>%
  
  # k_præmiepenge_pct
  mutate(across("k_præmiepenge_pct", as.numeric)) %>%
  
  # k_postnr
  mutate(across("k_postnr", as.character)) %>%
  
  # k_event_år
  mutate(across("k_event_år", .fns = factor, ordered = T)) %>%
  
  # k_billettype
  mutate(across("k_billettype", .fns = factor, levels = c(
    "🏓 DM i Ping Pong",
    "🥳 Fest om aftenen",
    "🍴 Restaurant Flammen",
    "🎉 The Old Irish Pub",
    "🥪 Frokost"), ordered = T)) %>%
  
  # k_billettype_tilvalg
  mutate(across("k_billettype_tilvalg", .fns = factor, ordered = T)) %>%
  
  # k_billetpris
  mutate(across("k_billetpris", as.numeric)) %>%
  
  # k_arrangørpris
  mutate(across("k_arrangørpris", as.numeric)) %>%
  
  # k_billetantal_maks
  mutate(across("k_billetantal_maks", as.integer)) %>%
  
  # k_eventnr
  mutate(across("k_eventnr", as.integer)) %>%
  
  # k_event
  mutate(across("k_event", .fns = factor, ordered = T)) %>%
  
  # k_eventdato
  mutate(across("k_eventdato", convertToDateTime)) %>%
  
  # k_tilmeldingsfrist
  mutate(across("k_tilmeldingsfrist", convertToDateTime)) %>%
  
  # k_åbningsdato
  mutate(across("k_åbningsdato", convertToDateTime)) %>%
  
  # k_ratingopdatering
  mutate(across("k_ratingopdatering", convertToDate)) %>%
  
  # k_puljeantal
  mutate(across("k_puljeantal", as.integer)) %>%
  
  # k_eventsted
  mutate(across("k_eventsted", .fns = factor, ordered = T)) %>%
  
  # k_eventadresse
  mutate(across("k_eventadresse", .fns = factor, ordered = T)) %>%
  
  # k_eventpostnr
  mutate(across("k_eventpostnr", as.integer)) %>%
  
  # k_eventby
  mutate(across("k_eventby", .fns = factor, ordered = T)) %>%
  
  # k_uuid
  mutate(across("k_uuid", as.character)) %>%
  
  # k_token
  mutate(across("k_token", as.character)) %>%
  
  # k_eventår
  mutate(k_eventår = year(k_eventdato)) %>%
  mutate(across("k_eventår", .fns = factor, ordered = T)) %>%
  
  # k_event_ping_pong_år
  mutate(k_event_ping_pong_år = paste(k_event, "i Ping Pong", k_eventår)) %>%
  mutate(across("k_event_ping_pong_år", .fns = factor, ordered = T)) %>%
  
  # k_første_ordredato
  group_by(k_event_år, k_deltager_id) %>%
  mutate(k_første_ordredato = min(k_ordredato)) %>%
  ungroup() %>%
  
  # k_billetsalg_pr_tilmelding
  add_count(k_event_år, k_deltager_id, k_status, name = "k_billetsalg_pr_tilmelding") %>%
  mutate(k_billetsalg_pr_tilmelding = paste(k_billetsalg_pr_tilmelding, "stk. billetsalg")) %>%
  mutate(across("k_billetsalg_pr_tilmelding", .fns = factor, ordered = T)) %>%
  
  # k_klokkeslætsinterval
  mutate(k_klokkeslætsinterval = case_when(
    hour(k_første_ordredato) >= 18 ~ "[>=18] Aften",
                              TRUE ~ "[>=00] Øvrig")) %>%
  mutate(across("k_klokkeslætsinterval", .fns = factor, levels = c(
    "[>=18] Aften",
    "[>=00] Øvrig"), ordered = T)) %>%
  
  # k_tilmeldingstype
  mutate(k_tilmeldingstype = case_when(
    grepl("Tilmeldt", k_status) & as_date(k_første_ordredato) <= k_tilmeldingsfrist ~ "🎫 Ordinær",
    grepl("Tilmeldt", k_status) & as_date(k_første_ordredato) >  k_tilmeldingsfrist ~ "🏃 Drive-in",
    grepl("Afbud",    k_status)                                                     ~ "❌ Afbud")) %>%
  mutate(across("k_tilmeldingstype", .fns = factor, levels = c(
    "🎫 Ordinær",
    "🏃 Drive-in",
    "❌ Afbud"), ordered = T)) %>%
  
  # k_slutspil_placering
  mutate(k_slutspil_placering = case_when(
    is.na(k_slutspil) | is.na(k_placering) ~ NA_character_,
    TRUE ~ paste0(substr(k_slutspil, 1, 1), k_placering))) %>%
  mutate(across("k_slutspil_placering", .fns = factor, ordered = T)) %>%
  
  # Landsdel
  mutate(k_landsdel = case_when(
    suppressWarnings(as.integer(k_postnr)) <= 3699 ~ "Sjælland",
    suppressWarnings(as.integer(k_postnr)) <= 3799 ~ "Bornholm",
    suppressWarnings(as.integer(k_postnr)) <= 4999 ~ "Sjælland",
    suppressWarnings(as.integer(k_postnr)) <= 5999 ~ "Fyn",
    suppressWarnings(as.integer(k_postnr)) <= 9999 ~ "Jylland",
    TRUE ~ "(Ukendt landsdel)")) %>%
  mutate(across("k_landsdel", .fns = factor, levels = c(
    "Jylland",
    "Fyn",
    "Sjælland",
    "Bornholm",
    "(Ukendt landsdel)"), ordered = T)) %>%
  
  # Region
  mutate(k_region = case_when(
    suppressWarnings(as.integer(k_postnr)) <= 3799 ~ "Hovedstaden",
    suppressWarnings(as.integer(k_postnr)) <= 4999 ~ "Sjælland",
    suppressWarnings(as.integer(k_postnr)) <= 6899 ~ "Syddanmark",
    suppressWarnings(as.integer(k_postnr)) <= 6999 ~ "Midtjylland",
    suppressWarnings(as.integer(k_postnr)) <= 7199 ~ "Syddanmark",
    suppressWarnings(as.integer(k_postnr)) <= 7699 ~ "Midtjylland",
    suppressWarnings(as.integer(k_postnr)) <= 7799 ~ "Nordjylland",
    suppressWarnings(as.integer(k_postnr)) <= 8999 ~ "Midtjylland",
    suppressWarnings(as.integer(k_postnr)) <= 9999 ~ "Nordjylland",
    TRUE ~ "(Ukendt region)")) %>%
  mutate(across("k_region", .fns = factor, levels = c(
    "Nordjylland",
    "Midtjylland",
    "Syddanmark",
    "Sjælland",
    "Hovedstaden",
    "(Ukendt region)"), ordered = T)) %>%
  
  # k_antal_gentilmelding
  arrange(k_ordredato, k_billettype) %>%
  group_by(k_deltager_id, k_billettype, k_status) %>%
  mutate(k_antal_gentilmelding = case_when(
    grepl("Tilmeldt", k_status) ~ paste0(cumsum(!duplicated(year(k_ordredato))), ". gang"),
    TRUE ~ as.character(k_status))) %>%
  ungroup() %>%
  mutate(across("k_antal_gentilmelding", .fns = factor, ordered = T)) %>%
  
  # k_gentilmelding
  mutate(k_gentilmelding = case_when(
    substr(k_antal_gentilmelding, 1, 1) == 1 ~ "Debutant",
    substr(k_antal_gentilmelding, 1, 1) >= 2 ~ "Gentilmelding",
    TRUE ~ as.character(k_status))) %>%
  mutate(across("k_gentilmelding", .fns = factor, levels = c(
    "Debutant",
    "Gentilmelding",
    "❌ Afbud"), ordered = T)) %>%
  
  # k_rating
  mutate(k_rating = case_when(
    !is.na(k_rang1)  ~ paste0("[", k_rang1, "] ", k_rating2),
    is.na(k_rating2) ~ "-",
                TRUE ~ as.character(k_rating2))) %>%
  mutate(across("k_rating", as.character)) %>%
  
  # k_ratinggruppe
  mutate(k_ratinggruppe = case_when(
    as.numeric(k_rating2) >= 2000 ~ "Elite",
                             TRUE ~ "Amatør")) %>%
  mutate(across("k_ratinggruppe", .fns = factor, levels = c(
    "Elite",
    "Amatør"), ordered = T)) %>%
  
  # k_født
  mutate(k_født = as_date(if_else(
    substr(k_deltager_id, 5, 6) <= substr(year(Sys.time()), 3, 4),
    paste0(
      substr(year(Sys.time()), 1, 2), substr(k_deltager_id, 5, 6), "-",
      substr(k_deltager_id, 3, 4), "-",
      substr(k_deltager_id, 1, 2)),
    paste0(
      as.numeric(substr(year(Sys.time()), 1, 2))-1, substr(k_deltager_id, 5, 6), "-",
      substr(k_deltager_id, 3, 4), "-",
      substr(k_deltager_id, 1, 2))))) %>%
  
  # k_alder
  mutate(k_alder = trunc((k_født %--% as_date(k_eventdato)) / years(1))) %>%
  mutate(across("k_alder", as.integer)) %>%
  
  # k_aldersgruppe
  mutate(k_aldersgruppe = case_when(
    k_alder <= 17 ~ "Ungdom",
    k_alder <= 39 ~ "Senior",
    k_alder >= 40 ~ "Veteran")) %>%
  mutate(across("k_aldersgruppe", .fns = factor, levels = c(
    "Ungdom",
    "Senior",
    "Veteran"), ordered = T)) %>%
  
  # k_spillertype
  mutate(k_spillertype = case_when(
    suppressWarnings(as.integer(str_sub(k_deltager_id, -4))) >= 0 ~ "👤 Ikke-bordtennisspiller",
    TRUE ~ "🏓 Bordtennisspiller")) %>%
  mutate(across("k_spillertype", .fns = factor, levels = c(
    "🏓 Bordtennisspiller",
    "👤 Ikke-bordtennisspiller"), ordered = T)) %>%
  
  # k_billetantal_billettype_status
  add_count(k_event_år_billettype, k_billettype, k_status,
            name = "k_billetantal_billettype_status") %>%
  group_by(k_event_år_billettype, k_billettype, k_status) %>%
  mutate(k_billetantal_billettype_status = case_when(
    !is.na(k_deltager_id) ~ k_billetantal_billettype_status)) %>%
  ungroup() %>%
  group_by(k_event_år_billettype) %>%
  fill(k_billetantal_billettype_status, .direction = "updown") %>%
  ungroup() %>%
  
  # k_navn_klub
  mutate(k_navn_klub = case_when(
    is.na(k_deltager_id) ~ NA_character_,
    grepl("Ingen klub", k_klub) ~ paste0(k_navn),
    TRUE ~ paste0(k_navn, ", ", k_klub))) %>%
  mutate(across("k_navn_klub", as.character)) %>%
  
  # k_navn_billettype
  group_by(k_event_år, k_deltager_id, k_status) %>%
  arrange(k_billettype, desc(k_første_ordredato)) %>%
  mutate(k_navn_billettype = case_when(
    is.na(k_deltager_id) ~ NA_character_,
    grepl("Ingen klub", k_klub) ~ paste0(k_navn, " (", k_alder, " år) ", str_c(substr(
      k_billettype, 1, 1), collapse = "")),
    TRUE ~ paste0(k_navn, ", ", k_klub, " (", k_alder, " år) ", str_c(substr(
      k_billettype, 1, 1), collapse = "")))) %>%
  ungroup() %>%
  mutate(across("k_navn_billettype", as.character)) %>%
  
  # Sorter efter (1) k_første_ordredato, (2) k_billettype
  arrange(desc(k_første_ordredato), k_billettype)

# Aktuelle tilmeldinger
tbl0_join_aktuel <- tbl0_join_alle %>% filter(
  k_eventår == tbl0_input$k_eventår)

#' # Statistik
# Statistik ########################################################################################
#+ eval=F, warning=F, message=F

tbl0_stat <- data.frame(
  
  # Billetantal total
  k_billetantal_total = paste0(
    tbl0_join_aktuel %>%
      count(k_tilmeldingstype) %>%
      mutate(pct = percent(n/sum(n), digits = 0)) %>%
      mutate(label = paste0(substring(k_tilmeldingstype, 1, 1), " ", n, " (", pct, ")")) %>%
      summarise(label = str_c(label, collapse = " ∙ "))),
  
  # Billetantal billettype
  k_billetantal_billettype = paste0(
    tbl0_join_aktuel %>% filter(
      grepl("Tilmeldt", k_status)) %>%
    count(k_billettype) %>%
    mutate(pct = percent(n/sum(n), digits = 0)) %>%
    mutate(label = paste0(substring(k_billettype, 1, 1), " ", n)) %>%
    summarise(label = str_c(label, collapse = " ∙ "))),
  
  # Deltagerantal total
  k_deltagerantal_total = paste0(
    tbl0_join_aktuel %>%
      distinct(k_deltager_id, k_status, .keep_all = T) %>%
      add_count(k_deltager_id) %>%
      filter((grepl("Tilmeldt", k_status) | n == 1) & !is.na(k_status)) %>%
      count(k_status) %>%
      mutate(k_status = case_when(
        grepl("Tilmeldt", k_status) ~ "Tilmeldt",
        grepl("Afbud", k_status) ~ '"Totalafbud"')) %>%
      mutate(pct = percent(n/sum(n), digits = 0)) %>%
      mutate(label = paste0("👤 ", n, " ", k_status, " (", pct, ")")) %>%
      summarise(label = str_c(label, collapse = " ∙ "))),
  
  # Gentilmeldinger
  k_deltagerantal_gentilmelding = paste0(
    tbl0_join_aktuel %>%
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
    tbl0_join_aktuel %>% filter(
      grepl("Tilmeldt", k_status)) %>%
      distinct(k_deltager_id, .keep_all = T) %>%
      count(k_køn) %>%
      mutate(pct = percent(n/sum(n), digits = 0)) %>%
      mutate(label = paste0("👤 ", n, " ", substring(k_køn, first = 4), " (", pct, ")")) %>%
      summarise(label = str_c(label, collapse = " ∙ "))),
  
  # Deltagerantal aldersgruppe
  k_deltagerantal_aldersgruppe = paste0(
    tbl0_join_aktuel %>% filter(
      grepl("Tilmeldt", k_status)) %>%
      distinct(k_deltager_id, .keep_all = T) %>%
      count(k_aldersgruppe) %>%
      mutate(pct = percent(n/sum(n), digits = 0)) %>%
      mutate(label = paste0("👤 ", n, " ", k_aldersgruppe, " (", pct, ")")) %>%
      summarise(label = str_c(label, collapse = " ∙ "))),
  
  # Alder
  k_alder = paste0(
    "👤 Yngst ", tbl0_join_aktuel %>% filter(
      grepl("Tilmeldt", k_status)) %>%
      distinct(k_deltager_id, .keep_all = T) %>%
      summarise(min(k_alder)), " år",
    " ∙ 👤 Gns. ", tbl0_join_aktuel %>% filter(
      grepl("Tilmeldt", k_status)) %>%
      distinct(k_deltager_id, .keep_all = T) %>%
      summarise(round(mean(k_alder), 0)), " år",
    " ∙ 👤 Ældst ", tbl0_join_aktuel %>% filter(
      grepl("Tilmeldt", k_status)) %>%
      distinct(k_deltager_id, .keep_all = T) %>%
      summarise(max(k_alder)), " år"),
  
  # Deltagerantal_landsdel
  k_deltagerantal_landsdel = paste0(
    "<b>Landsdel</b>: ", tbl0_join_aktuel %>% filter(
      grepl("Tilmeldt", k_status) &
        !grepl("Ingen klub", k_klub)) %>%
      distinct(k_deltager_id, .keep_all = T) %>%
      count(k_landsdel) %>%
      mutate(pct = percent(n/sum(n), digits = 0)) %>%
      mutate(label = paste0("👤 ", n, " ", k_landsdel, " (", pct, ")")) %>%
      summarise(label = str_c(label, collapse = " ∙ "))),
  
  # Deltagerantal region
  k_deltagerantal_region = paste0(
    "<b>Region</b>: ", tbl0_join_aktuel %>% filter(
      grepl("Tilmeldt", k_status) &
        !grepl("Ingen klub", k_klub)) %>%
      distinct(k_deltager_id, .keep_all = T) %>%
      count(k_region) %>%
      mutate(pct = percent(n/sum(n), digits = 0)) %>%
      mutate(label = paste0("👤 ", n, " ", k_region, " (", pct, ")")) %>%
      summarise(label = str_c(label, collapse = " ∙ "))),
  
  # Deltagerantal ingen klub
  k_deltagerantal_ingen_klub = paste0(
    tbl0_join_aktuel %>% filter(
      grepl("Tilmeldt", k_status)) %>%
      distinct(k_deltager_id, .keep_all = T) %>%
      count(k_klub) %>%
      mutate(pct = percent(n/sum(n), digits = 0)) %>%
      mutate(label = paste0("👤 ", n, " Ingen klub (", pct, ")")) %>%
      filter(grepl("Ingen klub", k_klub)) %>%
      summarise(label = str_c(label, collapse = " ∙ "))),
  
  # Klubantal
  k_klubantal = paste0(
    "🛖 ", tbl0_join_aktuel %>% filter(
      grepl("Tilmeldt", k_status) &
        !grepl("Ingen klub", k_klub)) %>%
      distinct(k_klub) %>%
      summarise(n = n()) %>%
      mutate(n = ifelse(n == 1, paste(n, "klub"), paste(n, "klubber")))),
  
  # Deltagerantal ratinggruppe
  k_deltagerantal_ratinggruppe = paste0(
    tbl0_join_aktuel %>% filter(
      grepl("Tilmeldt", k_status) &
        grepl("Ping Pong", k_billettype)) %>%
      count(k_ratinggruppe) %>%
      mutate(pct = percent(n/sum(n), digits = 0)) %>%
      mutate(label = paste0("🏓 ", n, " ", k_ratinggruppe, " (", pct, ")")) %>%
      summarise(label = str_c(label, collapse = " ∙ "))),
  
  # Rating
  k_rating = paste0(
    "🏓 Min. ", tbl0_join_aktuel %>% filter(
      grepl("Tilmeldt", k_status) &
        grepl("Ping Pong", k_billettype)) %>%
      summarise(min(k_rating2, na.rm = T)), " rating",
    " ∙ 🏓 Gns. ", tbl0_join_aktuel %>% filter(
      grepl("Tilmeldt", k_status) &
        grepl("Ping Pong", k_billettype)) %>%
      summarise(round(mean(k_rating2, na.rm = T), 0)), " rating",
    " ∙ 🏓 Maks. ", tbl0_join_aktuel %>% filter(
      grepl("Tilmeldt", k_status) &
        grepl("Ping Pong", k_billettype)) %>%
      summarise(max(k_rating2, na.rm = T)), " rating"),
  
  # Antal forskudte tilmeldinger
  k_antal_forskudte_tilmeldinger = paste0(
    "👤 ", tbl0_join_aktuel %>% filter(
      grepl("Tilmeldt", k_status)) %>%
      group_by(k_deltager_id) %>%
      filter(n() > 1) %>%
      count(k_ordredato) %>%
      ungroup() %>%
      distinct(k_deltager_id, .keep_all = T) %>%
      summarise(sum(n == 1)), " Forskudt tilmelding"),
  
  # Billetantal gns.
  k_billetantal_gns = paste(
    "🎫 Billetantal gns.:",
    tbl0_join_aktuel %>% filter(
      grepl("Tilmeldt", k_status)) %>%
      summarise(n())/
    tbl0_join_aktuel %>% filter(
      grepl("Tilmeldt", k_status)) %>%
      summarise(n_distinct(k_deltager_id))),
  
  # Økonomi
  k_økonomi = paste0(
    "💰 Omsætning kr. ", tbl0_join_aktuel %>% filter(
      grepl("Tilmeldt", k_status)) %>%
      summarise(format(round(sum(k_billetpris, na.rm = T), 0), big.mark = ".")),
    " ∙ 💰 Arrangørpris kr. ", tbl0_join_aktuel %>% filter(
      grepl("Tilmeldt", k_status)) %>%
      summarise(format(round(sum(-k_arrangørpris, na.rm = T), 0), big.mark = ".")),
    " ∙ 💰 Over-/underskud arrangør kr. ", tbl0_join_aktuel %>% filter(
      grepl("Tilmeldt", k_status)) %>%
      summarise(format(round(sum(k_billetpris, -k_arrangørpris, na.rm = T), 0), big.mark = "."))),
  
  # Billetantal Ping Ping maks. (heltal)
  k_int_billetantal_ping_pong_maks = as.integer(
    tbl0_join_aktuel %>% filter(
      grepl("Ping Pong", k_billettype)) %>%
      summarise(max(k_billetantal_maks, na.rm = T))),

  # Billetantal Ping Pong (heltal)
  k_int_billetantal_ping_pong = as.integer(
    tbl0_join_aktuel %>% filter(
      grepl("Tilmeldt", k_status) &
        grepl("Ping Pong", k_billettype)) %>%
      distinct(k_deltager_id, .keep_all = T) %>%
      summarise(n())),
  
  # Præmiepenge pr. deltager (numerisk)
  k_num_præmiepenge_pr_deltager = as.numeric(
    tbl0_join_aktuel %>% filter(
      grepl("Ping Pong", k_billettype)) %>%
      summarise(max(k_arrangørpris, na.rm = T))),
  
  # Antal puljer (heltal)
  k_int_antal_puljer = as.integer(
    tbl0_join_aktuel %>% filter(
      grepl("Tilmeldt", k_status) &
        grepl("Ping Pong", k_billettype)) %>%
      distinct(k_deltager_id, .keep_all = T) %>%
      summarise(ceiling(n()/unique(tbl0_join_aktuel$k_puljeantal)))),
  
  # Sidst opdateret
  k_sidst_opdateret = paste0(
    "<i>Nedenstående vises for ", unique(tbl0_join_aktuel$k_event_ping_pong_år), ".</i><br>",
    "<i class=bi-arrow-repeat>&nbsp;Sidst opdateret ",
    format(floor_date(Sys.time(), "30 minutes"), "%d.%m.%Y kl. %H:%M."), "</i>"),
  
  # Status CTA/plakat
  k_status_cta_plakat = if(tbl0_input$k_status_1_2_3_4 == 1) {
    "<img src=Filer/Forside.jpg style=width:30em;max-width:100%;border-radius:5px>"
  } else if(tbl0_input$k_status_1_2_3_4 == 2) {
    paste0(
      "![[<i style=font-size:80%>[Klik her for teaserplakat som PDF til udskrift]</i>]",
      "(Filer/Teaserplakat-DM-i-Ping-Pong-{{< var var.event_år >}}.pdf){target=_blank}]",
      "(Filer/Teaserplakat-DM-i-Ping-Pong-{{< var var.event_år >}}.png)",
      "{width=30em}")
  } else if(tbl0_input$k_status_1_2_3_4 == 3 | tbl0_input$k_status_1_2_3_4 == 4) {
    paste0(
      "<a style=display:inline-block;background:#398FCC;color:#FFFFFF;text-align:center;font-weight:bold;",
      "font-size:150%;width:20em;max-width:100%;line-height:20px;border-radius:40px;padding:10px;",
      "text-decoration:none href=indbydelse_tilmelding.qmd#tilmelding class=bi-tags-fill>",
      "&nbsp;Tilmeld<br><i style=font-weight:normal;font-size:60%>ALLE kan deltage</i></a>",
      "<br><br>",
      "![[<i style=font-size:80%>[Klik her for indbydelesplakat som PDF til udskrift]</i>]",
      "(Filer/Indbydelsesplakat-DM-i-Ping-Pong-{{< var var.event_år >}}.pdf){target=_blank}]",
      "(Filer/Indbydelsesplakat-DM-i-Ping-Pong-{{< var var.event_år >}}.png)",
      "{width=30em}")
  },
  
  # Status forside DM
  k_status_forside_dm = if(tbl0_input$k_status_1_2_3_4 == 1) {
    paste("<i>Nærmere information om DM i Ping Pong {{< var var.event_år >}} følger.</i>")
  } else if(tbl0_input$k_status_1_2_3_4 == 2) {
    paste(
      "<i>DM i Ping Pong {{< var var.event_år >}} afholdes {{< var var.event_dag_måned >}} i",
      "[{{< var var.lokation_sted >}}]({{< var var.lokation_link >}}){target=_blank}.",
      "Der åbnes for tilmelding {{< var var.event_åbning_dag_måned >}},", 
      'hvor der vil komme en fane med hhv. "Indbydelse & tilmelding" samt "Præmier & deltagere",',
      "som vil blive opdateret løbende.</i>")
  } else if(tbl0_input$k_status_1_2_3_4 == 3 | tbl0_input$k_status_1_2_3_4 == 4) {
    paste0(
      "<p><b>DM i Ping Pong {{< var var.event_år >}}</b></p>",
      "<ul>",
      "<li><i class=bi-tags-fill></i>&nbsp;[<b>Indbydelse & tilmelding</b>](indbydelse_tilmelding.qmd)",
      ":&nbsp;<i>Indbydelse, tidsplan, praktisk info samt tilmelding/betaling for DM og fest.</i></li>",
      "<li><i class=bi-arrow-repeat></i>&nbsp;[<b>Præmier & deltagere</b>](præmier_deltagere.qmd)",
      ":&nbsp;<i>Præmier og deltagere opdateres løbende.</i></li>",
      "</ul><hr>")
  },
  
  # Status forside Facebook
  k_status_forside_facebook = if(tbl0_input$k_status_1_2_3_4 == 1) {
    paste0(
      "<i class=bi-box-arrow-up-right></i>&nbsp;",
      "[<b>Facebook</b>](https://www.facebook.com/{{< var var.facebook_side >}}){target=_blank}:&nbsp;",
      "<i>Like og følg den officielle Facebook-side Ping Pong DK for at holde dig opdateret.</i>")
  } else if(tbl0_input$k_status_1_2_3_4 == 2 | tbl0_input$k_status_1_2_3_4 == 3 |
            tbl0_input$k_status_1_2_3_4 == 4) {
    paste0(
      "<i class=bi-box-arrow-up-right></i>&nbsp;",
      "[<b>Facebook</b>](https://www.facebook.com/events/{{< var var.facebook_event >}}){target=_blank}:&nbsp;",
      "<i>Del budskabet via Facebook-begivenheden ved at trykke deltager/interesseret og inviter folk.&nbsp;",
      "Like og følg også gerne den officielle Facebook-side Ping Pong DK (medarrangør),&nbsp;",
      "hvis du ikke allerede gør det, hvor bl.a. videoer fra tidligere DM kan ses.</i>")
  },
  
  check.names = F)

#' # Præmier
# Præmier ##########################################################################################

#' ## Pengepræmier
#+ eval=F, warning=F, message=F

tbl1_præmier_penge <- tbl0_join_alle %>% filter(
  grepl("Ping Pong", k_billettype) &
  (!is.na(k_slutspil) | !is.na(k_placering) | !is.na(k_præmiepenge))) %>%
  select(
    k_event_år_billettype,
    k_eventår,
    k_slutspil,
    k_placering,
    k_slutspil_placering,
    k_præmiepenge,
    k_præmiepenge_pct,
    k_billetantal_maks,
    k_billetantal_billettype_status) %>%
  
  group_by(k_event_år_billettype) %>%
  mutate(k_potentiel_præmiesum = sum(k_præmiepenge)) %>%
  ungroup() %>%
  mutate(k_aktuel_præmiepenge = k_potentiel_præmiesum*k_billetantal_billettype_status/
           k_billetantal_maks*k_præmiepenge_pct) %>%
  
  mutate(k_rank = "3") %>%
  group_by(k_event_år_billettype) %>%
  bind_rows(summarise(., across(where(is.numeric), sum), .groups = "keep")) %>% ungroup() %>%
  mutate(k_rank = ifelse(!is.na(k_rank), k_rank, "1")) %>%
  
  group_by(k_event_år_billettype, k_slutspil) %>%
  bind_rows(summarise(., across(where(is.numeric), sum), .groups = "keep")) %>% ungroup() %>%
  mutate(k_rank = ifelse(!is.na(k_rank), k_rank, "2")) %>%
  group_by(k_event_år_billettype) %>%
  fill(k_eventår, .direction = "updown") %>%
  ungroup() %>%
  
  mutate(across("k_slutspil", as.character)) %>%
  mutate(across("k_slutspil_placering", as.character)) %>%
  mutate(k_slutspil = ifelse(!is.na(k_slutspil), k_slutspil, "1")) %>%
  arrange(desc(k_eventår), k_slutspil, k_rank, k_placering) %>%
  distinct(k_event_år_billettype, k_slutspil, k_placering, .keep_all = T) %>%
  mutate(k_slutspil_placering = case_when(
    k_rank == "1" ~ "Præmiesum",
    k_rank == "2" ~ k_slutspil,
    k_rank == "3" ~ k_slutspil_placering,
    TRUE ~ NA_character_)) %>%
  
  mutate(k_aktuel_præmiepenge = paste("kr.", format(round(
    0.001+k_aktuel_præmiepenge), big.mark = "."))) %>%
  mutate(k_præmiepenge = paste("kr.", format(round(
    0.001+k_præmiepenge), big.mark = "."))) %>%
  mutate(k_præmiepenge_pct = paste0(signif(
    0.001+100*k_præmiepenge_pct, 3), "%")) %>%
  
  filter(k_eventår == tbl0_input$k_eventår) %>%
  select(
    " "         = k_slutspil_placering,
    "Aktuel"    = k_aktuel_præmiepenge,
    "Potentiel" = k_præmiepenge,
    "Pct."      = k_præmiepenge_pct,
    "k_rank"    = k_rank)

kbl1_præmier_penge <- tbl1_præmier_penge %>%
  kbl(col.names = NA, align = "lrrrr", escape = F,
      caption = "<i class=bi-cash-stack style=font-size:90%>&nbsp;<b>Præmiepenge</b> (afrundet)</i>") %>%
  kable_classic(position = "l", full_width = F, html_font = "verdana") %>%
  row_spec(0, background = "var_start_var.farve_1_var_slut", color = "#FFFFFF") %>%
  row_spec(which(tbl1_præmier_penge$k_rank == "1"),
           bold = T, background = "var_start_var.farve_2_var_slut") %>%
  row_spec(which(tbl1_præmier_penge$k_rank == "2"),
           background = "var_start_var.farve_2_var_slut") %>%
  column_spec(c(3, 4), italic = T, color = "var_start_var.farve_1_var_slut") %>%
  footnote(paste0(
    "<i style=font-size:80%>Foreløbig = ",
    tbl0_stat$k_int_billetantal_ping_pong, " deltagere x kr. ",
    tbl0_stat$k_num_præmiepenge_pr_deltager, " (maks. ",
    tbl0_stat$k_int_billetantal_ping_pong_maks, " deltagere).<br>",
    "Diplomer uddeles til alle gave-/præmietagere.</i>"),
    general_title = "", escape = F) %>%
  remove_column(5) %>%
  gsub("var_start_", "{{< var ", .) %>% gsub("_var_slut", " >}}", .)
kbl1_præmier_penge

#' ## Gaver
#+ eval=F, warning=F, message=F

tbl1_præmier_yngst_ældst <- tbl0_join_aktuel %>% filter(
  grepl("Tilmeldt", k_status) & grepl("Ping Pong", k_billettype)) %>%
    filter(k_født == max(k_født) | k_født == min(k_født)) %>%
  mutate(k_født  = format(k_født, "%d.%m.%Y")) %>%
  select(
    "Navn"  = k_navn_billettype,
    "Født"  = k_født)
kbl1_præmier_yngst_ældst <- tbl1_præmier_yngst_ældst %>%
  kbl(col.names = NA, align = "lc", escape = F,
      caption = paste0(
      "<i class=bi-gift style=font-size:90%>&nbsp;",
      "Gave til <b>yngste</b>- og <b>ældste</b> Ping Pong deltager<br>",
      "for at hylde mangfoldigheden</i>")) %>%
  kable_classic(position = "l", full_width = F, html_font = "verdana") %>%
  row_spec(0, background = "var_start_var.farve_1_var_slut", color = "#FFFFFF") %>%
  gsub("var_start_", "{{< var ", .) %>% gsub("_var_slut", " >}}", .)
kbl1_præmier_yngst_ældst

#' # Deltagere
# Deltagere ########################################################################################

#' ## Foreløbige deltagere
#+ eval=F, warning=F, message=F

tbl2_deltagere_foreløbig <- tbl0_join_aktuel %>%
  distinct(k_deltager_id, k_status, .keep_all = T) %>%
  arrange(
    k_status,
    k_første_ordredato) %>%
  mutate(Nr. = case_when(grepl("Tilmeldt", k_status) ~ row_number())) %>%
  select(
    Nr.,
    "Navn" = k_navn_billettype,
    k_status)
kbl2_deltagere_foreløbig <- tbl2_deltagere_foreløbig %>%
  kbl(col.names = NA, align = "cl", escape = F, caption = "Foreløbige deltagere") %>%
  kable_classic(position = "l", full_width = F, html_font = "verdana") %>%
  add_header_above(
    c("Sorteret efter ordredato" = 3),
    italic = T, align = "c", font_size = "small", escape = F,
    extra_css = "border:hidden;border-bottom:1.5px solid #111111") %>%
  row_spec(0, background = "var_start_var.farve_1_var_slut", color = "#FFFFFF") %>%
  row_spec(which(grepl("Afbud", tbl2_deltagere_foreløbig$k_status)),
           strikeout = T, italic = T, color = "var_start_var.farve_1_var_slut") %>%
  remove_column(3) %>%
  gsub("var_start_", "{{< var ", .) %>% gsub("_var_slut", " >}}", .)
kbl2_deltagere_foreløbig

#' ## Puljer
#+ eval=F, warning=F, message=F

tbl2_deltagere_puljer <- tbl0_join_aktuel %>%
  filter(
    grepl("Tilmeldt", k_status) &
      grepl("Ping Pong", k_billettype)) %>%
  arrange(
    k_rang1,
    desc(k_rating2),
    k_rang3,
    k_deltager_id) %>%
  add_row(k_deltager_id = rep(
    NA, tbl0_stat$k_int_antal_puljer*unique(tbl0_join_aktuel$k_puljeantal)-
      tbl0_stat$k_int_billetantal_ping_pong+tbl0_stat$k_int_antal_puljer)) %>%
  mutate(Seedningslag = ceiling(row_number()/tbl0_stat$k_int_antal_puljer)) %>%
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
    "Navn"   = k_navn_billettype,
    "Rating" = k_rating,
    Seedningslag)
kbl2_deltagere_puljer <- tbl2_deltagere_puljer %>%
  kbl(col.names = NA, align = "clr", escape = F,
      caption = paste0(unique(tbl0_join_aktuel$k_puljeantal), "-mandspuljer efter snake-system")) %>%
  kable_classic(position = "l", full_width = F, html_font = "verdana") %>%
  add_header_above(
    c("Bemærk: Puljerne kan ændre sig ved drive-in/afbud" = 4),
    italic = T, align = "c", font_size = "small", escape = F,
    extra_css = "border:hidden;border-bottom:1.5px solid #111111") %>%
  row_spec(0, background = "var_start_var.farve_1_var_slut", color = "#FFFFFF") %>%
  row_spec(which(tbl2_deltagere_puljer$Seedningslag == "1"), bold = T,
           extra_css = "border:hidden;border-top:0.7px solid #111111") %>%
  remove_column(4) %>%
  gsub("var_start_", "{{< var ", .) %>% gsub("_var_slut", " >}}", .)
kbl2_deltagere_puljer

#' ## Kun til festen inkl. afbud
#+ eval=F, warning=F, message=F

tbl2_deltagere_andet <- tbl0_join_aktuel %>%
  filter(!grepl("Ping Pong", k_billettype) &
           substr(k_billetsalg_pr_tilmelding, 1, 1) == 1 |
           grepl("Afbud", k_status)) %>%
  distinct(k_deltager_id, k_status, .keep_all = T) %>%
  arrange(
    k_status,
    desc(substr(k_billetsalg_pr_tilmelding, 1, 1)),
    desc(k_født),
    k_deltager_id) %>%
  mutate(Nr. = case_when(grepl("Tilmeldt", k_status) ~ row_number())) %>%
  select(
    Nr.,
    "Navn" = k_navn_billettype,
    k_status)

if(nrow(tbl2_deltagere_andet) == 0) {
  kbl2_deltagere_andet <- data.frame() %>% kbl()
} else {
kbl2_deltagere_andet <- tbl2_deltagere_andet %>%
  kbl(col.names = NA, align = "cl", escape = F, caption = "Andet end Ping Pong + evt. afbud") %>%
  kable_classic(position = "l", full_width = F, html_font = "verdana") %>%
  add_header_above(
    c("Sorteret efter fødselsdato" = 3),
    italic = T, align = "c", font_size = "small", escape = F,
    extra_css = "border:hidden;border-bottom:1.5px solid #111111") %>%
  row_spec(0, background = "var_start_var.farve_1_var_slut", color = "#FFFFFF") %>%
  row_spec(which(grepl("Afbud", tbl2_deltagere_andet$k_status)),
           strikeout = T, italic = T, color = "var_start_var.farve_1_var_slut") %>%
  remove_column(3) %>%
  gsub("var_start_", "{{< var ", .) %>% gsub("_var_slut", " >}}", .)
kbl2_deltagere_andet
}

#' # Resultater
# Resultater #######################################################################################

#' ## DM-vindere statistik
#+ eval=F, warning=F, message=F

tbl3_dm_resultater <- tbl0_join_alle %>%
  filter(k_placering == "1" & grepl("A-slutspil", k_slutspil) & !is.na(k_deltager_id)) %>%
  add_row(k_eventår = "2020", k_navn_klub = "Aflyst pga. Covid-19") %>%
  arrange(desc(k_eventår)) %>%
  select(
    "År"   = k_eventår,
    "Navn" = k_navn_klub)

kbl3_dm_resultater <- tbl3_dm_resultater %>%
  kbl(col.names = NA, align = "cl", escape = F,
      caption = "<i class=bi-trophy></i>&nbsp;DM-vindere statistik") %>%
  kable_classic(position = "l", full_width = F, html_font = "verdana") %>%
  row_spec(0, background = "#1C2833", color = "#FFFFFF") %>%
  row_spec(which(tbl3_dm_resultater$År == "2020"),
           strikeout = T, italic = T, color = "#5D6D7E")
kbl3_dm_resultater

#' ## Resultater sidste DM
#+ eval=F, warning=F, message=F

tbl3_resultater_sidste_dm <- tbl0_join_alle %>%
  filter(!is.na(k_deltager_id) & !is.na(k_slutspil)) %>%
  filter(k_eventår < as.integer(tbl0_input$k_eventår)) %>%
  filter(k_eventnr == min(k_eventnr)) %>%
  arrange(k_eventnr, k_slutspil, k_placering) %>%
  select(
    "Placering"  = k_slutspil_placering,
    "Navn"       = k_navn_klub,
    "k_slutspil" = k_slutspil,
    "k_eventår"  = k_eventår)

if(nrow(tbl3_resultater_sidste_dm) == 0) {
  kbl3_resultater_sidste_dm <- data.frame() %>% kbl()
} else {
kbl3_resultater_sidste_dm <- tbl3_resultater_sidste_dm %>%
  kbl(col.names = NA, align = "cl", escape = F,
      caption = paste("<i class=bi-trophy></i>&nbsp;Resultater sidste DM", unique(tbl3_resultater_sidste_dm$k_eventår))) %>%
  kable_classic(position = "l", full_width = F, html_font = "verdana") %>%
  row_spec(0, background = "#1C2833", color = "#FFFFFF") %>%
  remove_column(c(3, 4)) %>%
  pack_rows(index = table(as.character(tbl3_resultater_sidste_dm$k_slutspil)))
kbl3_resultater_sidste_dm
}

#' # Dashboards
# Dashboards #######################################################################################

#' ## Tilmeldingstype
#+ eval=F, warning=F, message=F

graf1_tilmeldingstype <- tbl0_join_aktuel %>%
  filter(!is.na(k_deltager_id)) %>%
  ggplot(mapping = aes(y = fct_rev(k_billettype), fill = k_tilmeldingstype)) +
  geom_bar(position = position_stack(reverse = T)) +
  geom_text(aes(label = after_stat(count)), stat = "count", position = position_stack(reverse = T),
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

graf2_gentilmelding <- tbl0_join_aktuel %>%
  filter(grepl("Tilmeldt", k_status)) %>%
  ggplot(mapping = aes(y = fct_rev(k_billettype), fill = k_antal_gentilmelding)) +
  geom_bar(position = position_stack(reverse = T)) +
  geom_text(aes(label = after_stat(count)), stat = "count", position = position_stack(reverse = T),
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

graf3_klubber <- tbl0_join_aktuel %>%
  filter(grepl("Tilmeldt", k_status) & grepl("Ping Pong|Fest", k_billettype)) %>%
  ggplot(mapping = aes(y = fct_rev(fct_infreq(k_klub)), fill = k_billettype)) +
  geom_bar(position = position_stack(reverse = T)) +
  geom_text(aes(label = after_stat(count)), stat = "count", position = position_stack(reverse = T),
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

graf4_DK <- tbl0_join_aktuel %>% filter(
  grepl("Tilmeldt", k_status) &
    !grepl("Ingen klub", k_klub)) %>%
  mutate(across("k_postnr", as.integer)) %>%
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
                unique(tbl0_join_aktuel$k_uuid), "/orders"),
    config = c(
      add_headers(Authorization = paste("Token", unique(tbl0_join_aktuel$k_token))),
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

if(tbl0_input$k_plakat_png_T_F == T) {
  pdf_convert(
    pdf       = paste0("Filer/Teaserplakat-DM-i-Ping-Pong.pdf"),
    filenames = paste0("Filer/Teaserplakat-DM-i-Ping-Pong.png"),
    verbose   = F,
    dpi       = 300)
  pdf_convert(
    pdf       = paste0("Filer/Indbydelsesplakat-DM-i-Ping-Pong.pdf"),
    filenames = paste0("Filer/Indbydelsesplakat-DM-i-Ping-Pong.png"),
    verbose   = F,
    dpi       = 300)
  shell.exec(gsub("\\\\", "/", normalizePath(paste0(
    "Filer/Teaserplakat-DM-i-Ping-Pong.png"))))
  shell.exec(gsub("\\\\", "/", normalizePath(paste0(
    "Filer/Indbydelsesplakat-DM-i-Ping-Pong.png"))))
} else if (tbl0_input$k_plakat_png_T_F == F) {"tbl0_input$k_plakat_png_T_F = F"}

#' ## Webscraping af ratinglisten
#+ eval=F, warning=F, message=F

if(tbl0_input$k_webscraping_rating_T_F == T) {
  tbl4_webscraping_rating <- data.frame()
  url_1 <- ifelse(
    nrow(rbind(tbl4_webscraping_rating, data.frame(
      "k_deltager_id" = read_html(
        paste0("https://bordtennisportalen.dk/DBTU/Ranglister/Udskriv/?params=,59,4",
               as.numeric(format(unique(tbl0_join_aktuel$k_ratingopdatering), "%Y")), ",",
               format(unique(tbl0_join_aktuel$k_ratingopdatering), "%m/%d/%Y"),
               ",,,,True,,,,,", "0", ",,,0,,,,,")) %>% html_nodes(".playerid") %>% html_text(),
      stringsAsFactors = FALSE)) %>% filter(k_deltager_id != "Spiller-Id")) > 0,
    paste0("https://bordtennisportalen.dk/DBTU/Ranglister/Udskriv/?params=,59,4",
           as.numeric(format(unique(tbl0_join_aktuel$k_ratingopdatering), "%Y")), ",",
           format(unique(tbl0_join_aktuel$k_ratingopdatering), "%m/%d/%Y")),
    paste0("https://bordtennisportalen.dk/DBTU/Ranglister/Udskriv/?params=,59,4",
           as.numeric(format(unique(tbl0_join_aktuel$k_ratingopdatering), "%Y"))-1, ",",
           format(unique(tbl0_join_aktuel$k_ratingopdatering), "%m/%d/%Y")))
  
  for (side in seq(from = 1, to = 50, by = 1)) {
  url_2 <- paste0(url_1, ",,,,True,,,,,", side-1, ",,,0,,,,,")
  
  tbl4_webscraping_rating <- rbind(tbl4_webscraping_rating, data.frame(
    "Plac"          = read_html(url_2) %>% html_nodes(".rank")                        %>% html_text(),
    "k_deltager_id" = read_html(url_2) %>% html_nodes(".playerid")                    %>% html_text(),
    "Navn"          = read_html(url_2) %>% html_nodes(".name")                        %>% html_text(),
    "Rating"        = read_html(url_2) %>% html_nodes(".name+ .pointsw")              %>% html_text(),
    "Plus_minus"    = read_html(url_2) %>% html_nodes(".pointsw:nth-child(5)")        %>% html_text(),
    "Kampe"         = read_html(url_2) %>% html_nodes(".pointsw~ .pointsw+ .pointsw") %>% html_text(),
    stringsAsFactors = FALSE)) %>% filter(k_deltager_id != "Spiller-Id") %>%
    mutate_at(c("Plac", "Rating", "Plus_minus", "Kampe"), as.numeric)
  print(paste("Side", side))
  }
  
  tbl4_webscraping_rating <- tbl4_webscraping_rating %>%
    separate(Navn, into = c("Navn", "Klub"), sep = ",.", extra = "merge")
  tbl4_join_webscraping_rating <- tbl0_join_aktuel %>%
    arrange(desc(k_ordredato)) %>%
    left_join(
      y = tbl4_webscraping_rating,
      by = "k_deltager_id") %>%
    select(Plac, k_deltager_id, Navn, Klub, Rating, Plus_minus, Kampe)
  write_xlsx(tbl4_webscraping_rating, path = "Filer\\Webscraping rating.xlsx")
  write_xlsx(tbl4_join_webscraping_rating, path = "Filer\\Webscraping join rating.xlsx")
  shell.exec(normalizePath("Filer\\Webscraping join rating.xlsx"))
} else if(tbl0_input$k_webscraping_rating_T_F == F) {"tbl0_input$k_webscraping_rating_T_F = F"}

#' ## Webscraping af BTEX Ping Pong bat
#+ eval=F, warning=F, message=F

tbl5_webscraping_btex <- data.frame()
link <- paste0("https://www.btex.dk/sanwei-wcpp-sandpapirsbat.html")

tbl5_webscraping_btex <- rbind(tbl5_webscraping_btex, data.frame(
  "produkt"      = read_html(link) %>% html_nodes(".name")                        %>% html_text(),
  "pris"         = read_html(link) %>% html_nodes(".price")                       %>% html_text(),
  "lagerstatus"  = read_html(link) %>% html_nodes(".title span")                  %>% html_text(),
  "levering"     = read_html(link) %>% html_nodes("#product_addtocart_form .txt") %>% html_text(),
  stringsAsFactors = FALSE))
