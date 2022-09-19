#' ---
#' title: Input for DM i Ping Pong
#' output: html_document
#' ---

#+ eval=F, warning=F, message=F
# Kopier/indsæt for offentliggørelse af hjemmeside via Terminal (Alt+Shift+M) =>
# quarto publish quarto-pub --no-prompt --no-browser

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
tbl0_input <- data.frame(
  # 2 inputmuligheder
  k_eventordre_T_F             = F,            # T/F for hentning af eventordre
  k_indbydelsesplakat_png_T_F  = F,            # T/F for indbydelsesplakat fra PDF til PNG
  k_webscraping_rating_T_F     = F,            # T/F for webscraping af rating
  k_foreløbig_endelig          = 2,            # 1/2 for hhv. foreløbig og endelig
  
  # 1 inputmulighed
  k_billetantal_ping_pong_maks = 72,           # Billetantal Ping Pong maks
  k_præmiepenge_pr_deltager    = 100,          # Præmiepenge pr. deltager i kr.
  k_præmie_A1                  = 0.375,        # Præmiepenge for A1 i pct.
  k_præmie_A2                  = 0.250,        # Præmiepenge for A2 i pct.
  k_præmie_A3                  = 0.150,        # Præmiepenge for A3 i pct.
  k_præmie_A4                  = 0.100,        # Præmiepenge for A4 i pct.
  k_præmie_B1                  = 0.075,        # Præmiepenge for B1 i pct.
  k_præmie_B2                  = 0.050,        # Præmiepenge for B2 i pct.
  k_puljeantal                 = 6,            # Antal spillere pr. pulje
  k_ordredato_år               = "2022-2022",  # ÅÅÅÅ-ÅÅÅÅ
  k_tilmeldingsfrist           = "05-06-2022", # DD-MM-ÅÅÅÅ
  k_farve1                     = "#3D88CB",    # Farve 1
  k_farve2                     = "#D6EAF8",    # Farve 2
  k_data                       = "Filer/Deltagere og tilmeldinger til DM i Ping Pong.xlsx",
  k_indbydelsesplakat          = "Filer/Indbydelsesplakat-DM-i-Ping-Pong-2023",
  k_eventordre_url_uuid        = "3e60537b3a1541a48e2e0871a6c520fa",
  k_eventordre_token           = "Token 77df96dc37b92daa27c050b8f7f1aafa9b47b7b0",
  k_cta                        = paste0(
    "<a style=display:inline-block;background:#398FCC;color:#FFFFFF;text-align:center;font-weight:bold;",
    "font-size:150%;width:20em;max-width:100%;line-height:20px;border-radius:20px;padding:10px;",
    "text-decoration:none href=indbydelse_tilmelding.qmd#tilmelding class=bi-tags-fill>",
    "&nbsp;Tilmeld<br><i style=font-weight:normal;font-size:50%>DM i Ping Pong 2023</i></a><br><br>"),
  k_indbydelse_tilmelding      = paste0(
    "<i class=bi-tags-fill></i>&nbsp;[<b>Indbydelse & tilmelding</b>](indbydelse_tilmelding.qmd)"),
  k_præmier_deltagere          = paste0(
    "<i class=bi-arrow-repeat></i>&nbsp;[<b>Præmier & deltagere</b>](præmier_deltagere.qmd)"))
source(file = "bagvedliggende_kode.R")