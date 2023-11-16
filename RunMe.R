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
## This version:      November 14th, 2023
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
source("Code/S01.R")
source("Code/S02.R")
source("Code/S03.R")

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

# Cleaning the Outputs directory for this country
ordnung.fn()

# Defining Main Country
mainCountry <- "North Macedonia"

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##  Section 1: Authoritarianism, Fundamental Freedoms, and Accountability                                   ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Figure 1
figure_PAB.fn()

# Figure 2
figure_PABGS.fn()

# Figure 3
#figure_AROL.fn()

# Figure 4
figure_FFOT.fn()

# Figure 5
#figure_FFD.fn()

# Figure 6
figure_PAOT.fn()


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

# Figure 11
figure_TIOT.fn()

# Figure 12
figure_PCTE.fn()

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##  Section 3:Security and Criminal Justice                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Figure 13
figure_TCEP.fn()

# Figure 14_1
figure_PSOT1.fn()

# Figure 14_2
figure_PSOT2.fn()

# Figure 15_1
figurePCJS_1.fn()

# Figure 15_2
figurePCJS_2.fn()

# Figure 16
figure_TCJA.fn()

# Figure 17
figure_POP.fn()

# Figure 18
figure_PTCV.fn()

