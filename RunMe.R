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
## This version:      January 9th, 2023
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
#roli_fs.fn()

# Overview - Subfactor Scores (Dots Chart)
#roli_sfs.fn()

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##  Section 1: Authoritarianism, Fundamental Freedoms, and Accountability                                   ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Figure 1
figure_PAB.fn()

# Figure 2
figure_PABGS.fn()
figure_PABGS.fn(group = "govSupp")

# Figure 3
figure_AROL.fn()

# Figure 4
figure_FFOT.fn()

# Figure 5
figure_FFD.fn()
figure_FFD.fn(group = "govSupp")

# Figure 6.1
figure_PAOT.fn()

# Figure 6.2
figure_PACG.fn()


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##  Section 2: Corruption and Trust                                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Figure 7
figure_PCOT.fn()

# Figure 8
figure_ACB.fn()

# Figure 9
figure_BVOT.fn()

# Figure 10
figure_BVAG.fn()

# Figure 11
figure_TIOT.fn()

# Figure 12
figure_PCTE.fn()
figure_PCTE.fn(group = "govSupp")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##  Section 3:Security and Criminal Justice                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Figure 13_1
figure_TCEP1.fn()

# Figure 13_2
figure_TCEP2.fn()

# Figure 14_1
figure_PSOT1.fn()

# Figure 14_2
figure_PSOT2.fn()

# Figure 15_1
figurePCJS_1.fn()

# Figure 15_2
figurePCJS_2.fn()
figurePCJS_2.fn(group = "govSupp")

# Figure 16
figure_TCJA.fn()

# Figure 17
figure_POP.fn()

# Figure 18
figure_PTCV.fn()

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##  Section 4:Access to Justice                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

a2j_section <- legal_assigment(master_data.df)

a2j_tables.fn(a2j_section = a2j_section)

a2j_plot1.fn(a2j_section = a2j_section)

a2j_plot2.fn(a2j_section = a2j_section)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##  Section 5:Socio Demographics                                                                ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Figure 22
figure_PFFCG.fn()

# Figure 23
figure_POPCG.fn()
figure_POPCG_bars.fn()

# Figure 24
figure_PABFS.fn()

# Figure 25
figure_PCTFS.fn()

# Figure 26
figure_PCJSI.fn()
