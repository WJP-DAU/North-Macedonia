## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            North Macedonia Country Report - RunMe File
##
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     November 13th, 2023
##
## This version:      February 5th, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 0.  Presettings                                                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Required Packages, Fonts, ggplot theme, color palettes, comparison countries and other general routines are
# loaded from the following script:
source("Code/settings.R")

# Loading functions for sections
source("Code/S00.R")
source("Code/S01.R")
source("Code/S02.R")
source("Code/S03.R")
source("Code/S04.R")
source("Code/S05.R")

# Loading plotting functions from GitHub
source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/loading.R")
loadVIZ(set = "NM")

# Loading data
master_data.df <- 
  read_dta(file.path(path2SP, "6. Country Reports/North-Macedonia/Data/NMdata.dta")) %>%
  mutate(
    latestYear = max(year),
    ethnigroup = case_when(
      ethni == "Macedonian" ~ "Macedonian",
      TRUE ~ "Other"
    ),
    relgroup = case_when(
      relig %in% c("C57 - Orthodox Christian", "C60 - Protestant", "C67 - Roman Catholic") ~ "Christian",
      TRUE ~ "Other"
    )
  )

roli_balcans <- roli %>%
  filter(country %in% c("Albania", "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Greece", "Kosovo", 
                        "Montenegro", "North Macedonia", "Romania", "Serbia"))
  
# Cleaning the Outputs directory for this country
ordnung.fn()

# Defining Main Country
mainCountry <- "North Macedonia"

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##  Section 0: Country Overview                                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Overview - Factor Scores (Rose Chart)
# roli_fs.fn()

# Overview - Subfactor Scores (Dots Chart)
 roli_sfs.fn()

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##  Section 1: Authoritarianism, Fundamental Freedoms, and Accountability                                   ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Figure 1
data_PAB <- figure_PAB.fn()

# Figure 2
figure_PABGS.fn()
data_PABGS <- figure_PABGS.fn(group = "govSupp")

# Figure 3
data_AROL <- figure_AROL.fn()

# Figure 4
data_FFOT <- figure_FFOT.fn()

# Figure 5
figure_FFD.fn()
data_FFD <- figure_FFD.fn(group = "govSupp")

# Figure 6.1
data_PAOT <- figure_PAOT.fn()

# Figure 6.2
data_PACG <- figure_PACG.fn()


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##  Section 2: Corruption and Trust                                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Figure 7
data_PCOT <- figure_PCOT.fn()

# Figure 8
data_ACB <- figure_ACB.fn()

# Figure 9
data_BVOT <- figure_BVOT.fn()

# Figure 10
data_BVAG <- figure_BVAG.fn()

# Figure 11
data_TIOT <- figure_TIOT.fn()

# Figure 12
figure_PCTE.fn()
data_PCTE <- figure_PCTE.fn(group = "govSupp")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##  Section 3:Security and Criminal Justice                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Figure 13_1
data_TCEP1 <- figure_TCEP1.fn()

# Figure 13_2
data_TCEP2 <- figure_TCEP2.fn()

# Figure 14_1
data_PSOT1 <- figure_PSOT1.fn()

# Figure 14_2
data_PSOT2 <- figure_PSOT2.fn()

# Figure 15_1
data_PCJS1 <- figurePCJS_1.fn()

# Figure 15_2
figurePCJS_2.fn()
data_PCJS2 <- figurePCJS_2.fn(group = "govSupp")

# Figure 16
data_TCJA <- figure_TCJA.fn()

# Figure 17
data_POP <- figure_POP.fn()

# Figure 18
data_PTCV <- figure_PTCV.fn()

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##  Section 4:Access to Justice                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

a2j_section <- legal_assigment(master_data.df)

a2j_tables.fn(a2j_section = a2j_section)

a2j_plot1.fn(a2j_section = a2j_section)

a2j_plot2.fn(a2j_section = a2j_section)

# Justice Gap
jgap_data.df <- jgap.fn(data = master_data.df)
jgap_bars.fn(data = jgap_data.df)
jgap_logit.fn(data = jgap_data.df)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##  Section 5:Socio Demographics                                                                ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Figure 22
data_PFFCG <- figure_PFFCG.fn()

# Figure 23
data_POPCG <- figure_POPCG.fn()
figure_POPCG.fn()
figure_POPCG_bars.fn()

# Figure 24
data_PABFS <- figure_PABFS.fn()

# Figure 25
data_PCTFS <- figure_PCTFS.fn()

# Figure 26
figure_PCJSI.fn()


list_of_dataPoints <- list(
  "Chart 1"    = data_PAB, 
  "Chart 2"    = data_PABGS,
  "Chart 3"    = data_AROL,
  "Chart 4"    = data_FFOT,
  "Chart 5"    = data_FFD, 
  "Chart 6.1"  = data_PAOT,
  "Chart 6.2"  = data_PACG, 
  "Chart 7"    = data_PCOT,
  "Chart 8"    = data_ACB, 
  "Chart 9.1"  = data_BVOT,
  "Chart 9.2"  = data_BVAG, 
  "Chart 10"   = data_TIOT,
  "Chart 11"   = data_PCTE, 
  "Chart 12.1" = data_TCEP1,
  "Chart 12.2" = data_TCEP2, 
  "Chart 13.1" = data_PSOT1,
  "Chart 13.2" = data_PSOT2, 
  "Chart 14.1" = data_PCJS1,
  "Chart 14.2" = data_PCJS2, 
  "Chart 15"   = data_TCJA,
  "Chart 16"   = data_POP, 
  "Chart 17"   = data_PTCV,
  "Chart 19"   = data_PFFCG, 
  "Chart 20"   = data_POPCG,
  "Chart 21"   = data_PABFS, 
  "Chart 22"   = data_PCTFS
)

openxlsx::write.xlsx(list_of_dataPoints,
                     file = paste0("Outputs/dataPoints.xlsx"))

