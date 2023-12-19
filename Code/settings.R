## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            North Macedonia Country Report - Settings
##
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
##                    Artha P. Pillai             (apillai@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     November 13th, 2023
##
## This version:      November 21st, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Required packages                                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Required packages
library(pacman)

# Notes: ggsankey and ggwaffle need to be installed from Github's developer version. Run the following lines 
# of code in order to install: 
# devtools::install_github("davidsjoberg/ggsankey")
# devtools::install_github("liamgilbey/ggwaffle")
#devtools::install_github("ctoruno/WJPr")

p_load(char = c(
  # Visualizations
  "showtext", "ggtext", "ggsankey", "ggwaffle", "ggplotify", "gridExtra", "patchwork", "ggh4x", "ggrepel",
  
  # Data Loading and Saving
  "haven", "readxl", "writexl", "openxlsx",
  
  # Utilities
  "margins", "quarto", "kableExtra", "WJPr",
  
  # Good 'ol Tidyverse
  "tidyverse"
  
))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  SharePoint Path                                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SharePoint path
if (Sys.info()["user"] == "ctoruno") {
  path2SP <- "/Users/ctoruno/OneDrive - World Justice Project/Data Analytics/"
  
}
if (Sys.info()["user"] == "santiagopardo") {
  path2SP <- "/Users/santiagopardo/OneDrive - World Justice Project/Data Analytics/"
  
}
if (Sys.info()["user"] == "apillai") {
  path2SP <-"/Users/apillai/OneDrive - World Justice Project/Data Analytics/"
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Fonts                                                                                                ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Loading fonts
path2fonts <- paste0(path2SP, "6. Country Reports/0. Fonts/")
font_add(family     = "Lato Full",
         regular    = paste0(path2fonts, "Lato-Regular.ttf"),
         italic     = paste0(path2fonts, "Lato-LightItalic.ttf"),
         bold       = paste0(path2fonts, "Lato-Bold.ttf"),
         bolditalic = paste0(path2fonts, "Lato-BoldItalic.ttf"))
font_add(family  = "Lato Light",
         regular = paste0(path2fonts, "Lato-Light.ttf"))
font_add(family  = "Lato Black",
         regular = paste0(path2fonts, "Lato-Black.ttf"))
font_add(family  = "Lato Black Italic",
         regular = paste0(path2fonts, "Lato-BlackItalic.ttf"))
font_add(family  = "Lato Medium",
         regular = paste0(path2fonts, "Lato-Medium.ttf"))
showtext_auto()

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 4.  WJP theme                                                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Defining a ggplot WJP theme
WJP_theme <- function() {
  theme(panel.background   = element_blank(),
        plot.background    = element_blank(),
        panel.grid.major   = element_line(size     = 0.25,
                                          colour   = "#5e5c5a",
                                          linetype = "dashed"),
        panel.grid.minor   = element_blank(),
        axis.title.y       = element_text(family   = "Lato Full",
                                          face     = "plain",
                                          size     = 3.514598*.pt,
                                          color    = "#524F4C",
                                          margin   = margin(0, 10, 0, 0)),
        axis.title.x       = element_text(family   = "Lato Full",
                                          face     = "plain",
                                          size     = 3.514598*.pt,
                                          color    = "#524F4C",
                                          margin   = margin(10, 0, 0, 0)),
        axis.text.y        = element_text(family   = "Lato Full",
                                          face     = "plain",
                                          size     = 3.514598*.pt,
                                          color    = "#524F4C"),
        axis.text.x = element_text(family = "Lato Full",
                                   face   = "plain",
                                   size   = 3.514598*.pt,
                                   color  = "#524F4C"),
        axis.ticks  = element_blank(),
        plot.margin  = unit(c(0, 0, 0, 0), "points")
  ) 
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 5.  Color Palette                                                                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

mainCOLOR      <- c("#2a2a9A")
lickertPalette <- c("#003B8A", "#9395D3", "#d9d9d9", "#FC8F72", "#FA4D57")
wafflePalette  <- c("#003B8A", "#FFEFD7", "#FC8F72", "#d9d9d9")
binPalette     <- c("#003b8a", "#fa4d57")
barsPalette    <- c("#2a2a9A", "#E2E2F7")
glinesPalette  <- c("#2a2a94", "#a90099", "#3273ff")
rosePalette    <- c("#20204a", "#12006b", "#2e2e95", "#4e43dd", "#756ef9", "#9c94ff", "#b1a6ff",
                    "#cfb3ff", "#e2a4ff", "#f2aadc", "#ffd7f5")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 7.  Creating a function that will reset the Outputs directory                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Defining a function to delete previous outputs and create the country directory
ordnung.fn <- function(){
  
  # Listing previous outputs
  prevOutputs <- list.files("Outputs", 
                            include.dirs = F, 
                            full.names   = T, 
                            recursive    = T)
  
  # Deleting previous outputs
  file.remove(prevOutputs)
  
  # Creating folders for each chart output within the country directory
  for (plot in 1:22) {
    dir.create(file.path("Outputs", 
                         paste0("imgChart", plot),
                         fsep = "/"), 
               showWarnings = FALSE)
  }
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 8.  Creating a saving function                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

saveIT.fn <- function(chart, n, suffix = NULL, w, h) {
  ggsave(plot   = chart,
         file   = file.path("Outputs", 
                            paste0("imgChart", n),
                            paste0("figure_", n, suffix, ".svg"),
                            fsep = "/"), 
         width  = w, 
         height = h,
         units  = "mm",
         dpi    = 72,
         device = "svg")
} 

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 9.  Creating a to_percentage function                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

to_percentage.fn <- function(value){
  perc <- paste0(format(round(value, 0),
                        nsmall = 0),
                 "%")
  return(perc)
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 10.  Preparing order data in the logit                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

condition_categories <- function(main_data, group_var, name_var) {
  
  condition <-  main_data %>%
    group_by({{group_var}}) %>%
    summarise(N_obs = sum(counter, na.rm = T)) %>%
    ungroup() %>%
    mutate(variable = as.character({{name_var}})) %>%
    rename(category = {{group_var}}) %>%
    drop_na()
  
  return(condition)
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 10.  Preparing data for A2J section                                                                  ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

legal_assigment <- function(data_subset) {
  
  target_variables <- c(
    # Problem selected
    "q21",
    # Violence during the dispute
    "q22b",
    # Obtain information from the internet
    "q23",
    # Contact in the law school
    "q26",
    # Reasons for no representation
    "q27",
    # Reasons for not going anybody
    "q29",
    # Advice
    "q25_1", "q25_2", "q25_3", "q25_4", "q25_5", "q25_6", "q25_7", "q25_8", "q25_9", "q25_99",
    # Representation
    "q24", 
    # Claim to court (A2J)
    "q28", 
    # Persistance of the problem
    "q34_merge", "q30", "q34",
    # Statements problem settled
    "q31", "q35",
    # Somebody acting on your behalf
    "q27a", "q27b", "q27c", "q27d", "q27e", "q27f", "q27g",
    # Who initiated contact
    "q28a", "q28b", "q28c", "q28d", "q28e", "q28f", "q28g",
    # Fair Process
    "q36a", 
    # Slow Process
    "q36b", 
    # Expensive Process
    "q36c", 
    # Time of the process
    "q37b", "EXP22_q43",
    # Outcome in favor
    "q37a",
    # Months to solve the problem
    "q37b",
    # Incur in cost
    "q37c",
    # Economic Issues
    "q37d",
    # Satisfaction with the outcome
    "q38", "q39",
    # Problem description
    "q40_1", "q40_2", "q40_3", "q40_4", "q40_5", "q40_6", "q40_7",
    # Sources of help
    "q41a", "q41b", "q41c",
    # Confidence in the system
    "q41d",
    # Hardships: Stress-related illness, injuries, or physical ill healt
    "q42a",
    # Hardships: Relationship breakdown or damage to a family relationship
    "q42b",
    # Hardships: Loss of income, loss of employment
    "q42c",
    # Hardships: Problem with alcohol or drugs
    "q42d",
    # Sociodemographics
    "Urban", "fin", "age","white", "gend", "edu", "COLOR")
  
  a <- data_subset %>%
    mutate(a2j_consumer    = 
             if_else(
               q19_A1 == 1 |
                 q19_A2 == 1 | 
                 q19_A3 == 1, 1, 0),
           a2j_land        = 
             if_else(
               q19_B1 == 1 | 
                 q19_B2 == 1 | 
                 q19_B3 == 1 | 
                 q19_B4 == 1, 1, 0),
           a2j_housing     = 
             if_else(
               q19_C1 == 1 | 
                 q19_C2 == 1 |
                 q19_C3 == 1 |
                 q19_C4 == 1, 1, 0),
           a2j_family      = 
             if_else(
               q19_D1 == 1  | 
                 q19_D2 == 1  |
                 q19_D3 == 1  | 
                 q19_D4 == 1  | 
                 q19_D5 == 1  | 
                 q19_D6 == 1, 1, 0),
           a2j_education   = 
             if_else(
               q19_E1 == 1 |
                 q19_E2 == 1, 1, 0),
           a2j_accidental  = 
             if_else(
               q19_F1 == 1 |
                 q19_F2 == 1, 1, 0),
           a2j_employment  = 
             if_else(
               q19_G1 == 1 |
                 q19_G2 == 1 |
                 q19_G3 == 1, 1, 0),
           a2j_public      = 
             if_else(
               q19_H1 == 1 | 
                 q19_H2 == 1 | 
                 q19_J4 == 1, 1, 0),
           a2j_law         = 
             if_else(
               q19_I1 == 1, 1, 0),
           a2j_id          = 
             if_else(
               q19_J1 == 1 | 
                 q19_J2 == 1 | 
                 q19_J3 == 1, 1, 0),
           a2j_money       = 
             if_else(
               q19_K1 == 1 | 
                 q19_K2 == 1 | 
                 q19_K3 == 1 | 
                 q19_L1 == 1 | 
                 q19_L2 == 1, 1, 0),
           a2j_community   = 
             if_else(
               q19_H3 == 1 | 
                 q19_E3 == 1, 1, 0)) %>%
    mutate(
      legal           = 
        if_else(
          a2j_consumer    == 1  | 
            a2j_land        == 1  | 
            a2j_housing     == 1  | 
            a2j_family      == 1  | 
            a2j_education   == 1  | 
            a2j_accidental  == 1  |
            a2j_employment  == 1  | 
            a2j_public      == 1  | 
            a2j_law         == 1  | 
            a2j_id          == 1  | 
            a2j_money       == 1  | 
            a2j_community   == 1  | 
            q19_99          == 1, 1, 0, 0)) %>%
    group_by(country) %>%
    filter(year == 2023) %>%
    ungroup() %>%
    select(country, year, ends_with(target_variables), starts_with("a2j_"), legal) %>%
    mutate(color_bin     =  
             if_else(
               COLOR == 1 | COLOR == 2 | COLOR == 3 | COLOR == 4, "Lighter Skin Tone", 
               if_else(
                 COLOR == 5 | COLOR == 6 | COLOR == 7 | COLOR == 8 | COLOR == 9 | COLOR == 10, "Darker Skin Tone", NA_character_)),
           age_bin       =  
             if_else(
               age < 31, "Less than 30 years", 
               if_else(
                 age > 30, "More than 30 years", NA_character_)),
           fin_bin       =  
             if_else(
               fin == 1 | fin == 2, "Financially Insercured",
               if_else(
                 fin == 3 | fin == 4 | fin == 5, "Financially Secured", NA_character_)),
           area          =  
             if_else(
               Urban == 1, "Urban", "Rural"),
           gender        =  
             if_else(
               gend == 1, "Male", "Female"),
           diploma_bin   =  
             if_else(
               edu == 4 | edu == 5 | edu == 6, "High Education Level",
               if_else(
                 edu < 5, "No High Education Level", NA_character_)))
  
  return(a)
}

aes_function <- function(mainData, 
                         panel = "v1") {
  
  if(panel == "v1") {
    
    data2plot <- mainData %>%
      mutate(empty_value = 1 - value) %>%
      pivot_longer(!problem,
                   names_to = "group",
                   values_to = "value") %>%
      mutate(x_pos = c(1.2, 1.2, 2.2, 2.2, 3.2, 3.2, 4.2, 4.2),
             order_value = c(1,1,2,2,3,3,4,4),
             empty_value = 1 - value,
             label = paste0(round(value*100,0), "%"),
             multiplier = if_else(group == "empty_value", 0, 1),
             label = if_else(multiplier == 0, NA_character_, label))
    return(data2plot) 
    
  } else {
    
    data2plot <- mainData %>%
      mutate(empty_value = 1 - value) %>%
      pivot_longer(!help,
                   names_to = "group",
                   values_to = "value") %>%
      mutate(x_pos = c(3.2, 3.2, 2.2, 2.2, 1.2, 1.2),
             empty_value = 1 - value,
             label = paste0(round(value*100,0), "%"),
             multiplier = if_else(group == "empty_value", 0, 1),
             label = if_else(multiplier == 0, NA_character_, label))
    return(data2plot)
    
  }
  
}