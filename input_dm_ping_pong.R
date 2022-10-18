#' ---
#' title: Input for DM i Ping Pong
#' output: html_document
#' ---

#+ eval=F, warning=F, message=F
# Kopier/indsæt for offentliggørelse af hjemmeside via Terminal (Alt+Shift+M) =>
# quarto publish quarto-pub --no-prompt --no-browser

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
tbl0_input <- data.frame(
  # Flere inputmuligheder
  k_status_1_2_3_4             = 4,            # 1/2/3/4 for tilmelding hhv. lukket, teaser, åben og endelig
  k_eventordre_T_F             = F,            # T/F for hentning af eventordre
  k_plakat_png_T_F             = F,            # T/F for plakater fra PDF til PNG
  k_webscraping_rating_T_F     = F,            # T/F for webscraping af rating
  
  # Én inputmulighed
  k_billetantal_ping_pong_maks = 96,           # Billetantal Ping Pong maks
  k_præmiepenge_pr_deltager    = 100,          # Præmiepenge pr. deltager i kr.
  k_præmie_A1                  = 0.375,        # Præmiepenge for A1 i pct.
  k_præmie_A2                  = 0.250,        # Præmiepenge for A2 i pct.
  k_præmie_A3                  = 0.150,        # Præmiepenge for A3 i pct.
  k_præmie_A4                  = 0.100,        # Præmiepenge for A4 i pct.
  k_præmie_B1                  = 0.075,        # Præmiepenge for B1 i pct.
  k_præmie_B2                  = 0.050,        # Præmiepenge for B2 i pct.
  k_puljeantal                 = 6,            # Antal spillere pr. pulje
  k_ordredato_år               = "2022-2023",  # ÅÅÅÅ-ÅÅÅÅ
  k_tilmelding_åbning          = "02-05-2023", # DD-MM-ÅÅÅÅ
  k_tilmeldingsfrist           = "05-06-2022", # DD-MM-ÅÅÅÅ
  k_farve1                     = "#3D88CB",    # Farve 1
  k_farve2                     = "#D6EAF8",    # Farve 2
  k_url_facebook_side          = "https://www.facebook.com/100183334928231", # URL Facebook-side
  k_url_facebook_event         = "https://www.facebook.com/events/452302656395308", # URL Facebook-event
  k_data                       = "Filer/Deltagere og tilmeldinger til DM i Ping Pong.xlsx", # Sti til data
  k_eventordre_uuid            = "3e60537b3a1541a48e2e0871a6c520fa", # BilletFix UUID (fra URL)
  k_eventordre_token           = "Token 77df96dc37b92daa27c050b8f7f1aafa9b47b7b0") # BilletFix API-nøgle
source(file = "bagvedliggende_kode.R")
