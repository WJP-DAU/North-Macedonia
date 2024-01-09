## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            North Macedonia Country Report - Country Overview Functions
##
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
##                    Artha Pillai                (apillai@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     December 15th, 2023
##
## This version:      December 15th, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    ROLI Factor Scores (Rose Chart)                                                                       ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

roli_fs.fn <- function(nchart = 21, data = roli_balcans){
  
  # Subsetting data
  data2plot <- roli_balcans %>%
    filter(year == 2023 & country == "North Macedonia") %>%
    select(f1, f2, f3, f4, f5, f6, f7, f8) %>%
    pivot_longer(everything(),
                 names_to  = "category",
                 values_to = "avg") %>%
    mutate(
      valab = format(round(avg, 2),
                     nsmall = 2),
      label = case_when(
        category == "f1" ~ "Constraints on\nGovernment Powers",
        category == "f2" ~ "Absence of\nCorruption",
        category == "f3" ~ "Open\nGovernment",
        category == "f4" ~ "Fundamental\nRights",
        category == "f5" ~ "Order and\nSecurity",
        category == "f6" ~ "Regulatory\nEnforcement",
        category == "f7" ~ "Civil\nJustice",
        category == "f8" ~ "Criminal\nJustice"
      ),
      order_value = case_when(
        category == "f1" ~ 1,
        category == "f2" ~ 2,
        category == "f3" ~ 3,
        category == "f4" ~ 4,
        category == "f5" ~ 5,
        category == "f6" ~ 6,
        category == "f7" ~ 7,
        category == "f8" ~ 8
      ),
      across(label,
             function(raw_label){
               html <- paste0("<span style='color:#2E2E95;font-size:2.911678mm;font-weight:bold'>",  
                              valab, "</span>",
                              "<br>",
                              "<span style='color:#524F4C;font-size:2.911678mm'>",
                              str_replace_all(raw_label, "\\n", "<br>"),
                              "</span>")
               return(html)
             }),
      avg = avg-0.1 # I substract 0.1 just for aesthetic purposes
    )
  
  # Defining colors
  colors4plot        <- rep("#2E2E95", 8)
  
  # Applying plotting function
  chart <- LAC_roseChart(data          = data2plot,
                         target_var    = "avg",
                         grouping_var  = "category",
                         alabels_var   = "label",
                         plabels_var   = "valab",
                         order_var     = "order_value",
                         colors        = colors4plot)
  
  # Saving panels
  saveIT.fn(chart  = chart,
            n      = nchart,
            suffix = NULL,
            w      = 88.91933,
            h      = 75.56386)
  
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    ROLI Sub-factor Scores (Dots Chart)                                                                   ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

roli_sfs.fn <- function(nchart = 22, data = roli_balcans){
  
  # Sub-setting data
  data2plot <- roli_balcans %>%
    filter(year == 2023) %>%
    select(country, starts_with("sf")) %>%
    select(-sf52) %>%
    mutate(
      country = if_else(country != mainCountry,
                        "Balcan region",
                        "North Macedonia")
    ) %>%
    group_by(country) %>%
    summarise(across(everything(),
                     \(x) mean(x, na.rm = T))) %>%
    pivot_longer(!country,
                 values_to = "value2plot",
                 names_to  = "category") %>%
    mutate(
      factor    = str_extract(category, "(?<=sf)\\d{1}"),
      factor    = as.integer(factor),
      order_var = str_extract(category, "(?<=sf\\d{1})\\d{1}"),
      order_var = as.integer(order_var),
      category = case_when(
        category == "sf11" ~ "1.1 Limits by legislature     ",
        category == "sf12" ~ "1.2 Limits by judiciary       ",
        category == "sf13" ~ "1.3 Independent auditing      ",
        category == "sf14" ~ "1.4 Government sanctions      ",
        category == "sf15" ~ "1.5 Non-governmental checks   ",
        category == "sf16" ~ "1.6 Lawful transition of power",
        
        category == "sf21" ~ "2.1 In the executive branch   ",
        category == "sf22" ~ "2.2 In the judiciary          ",
        category == "sf23" ~ "2.3 In the police/military    ",
        category == "sf24" ~ "2.4 In the legislature        ",
        
        category == "sf31" ~ "3.1 Publicized data           ",
        category == "sf32" ~ "3.2 Right to information      ",
        category == "sf33" ~ "3.3 Civi Participation        ",
        category == "sf34" ~ "3.4 Complaint mechanisms      ",
        
        category == "sf41" ~ "4.1 No discrimination         ",
        category == "sf42" ~ "4.2 Right to life and security",
        category == "sf43" ~ "4.3 Due process of law        ",
        category == "sf44" ~ "4.4 Freedom of expression     ",
        category == "sf45" ~ "4.5 Freedom ofreligion        ",
        category == "sf46" ~ "4.6 Right to privacy          ",
        category == "sf47" ~ "4.7 Freedom of association    ",
        category == "sf48" ~ "4.8 Labor rights",
        
        category == "sf51" ~ "5.1 Absence of crime          ",
        category == "sf53" ~ "5.3 Absence of violent redress",
        
        category == "sf61" ~ "6.1 Effective enforcement     ",
        category == "sf62" ~ "6.2 No improper influence     ",
        category == "sf63" ~ "6.3 No unreasonable delay     ",
        category == "sf64" ~ "6.4 Respect for due process   ",
        category == "sf65" ~ "6.5 No expropriation          ",
        
        category == "sf71" ~ "7.1 Accessibility             ",
        category == "sf72" ~ "7.2 No discrimination         ",
        category == "sf73" ~ "7.3 No corruption             ",
        category == "sf74" ~ "7.4 No improper gov't influence",
        category == "sf75" ~ "7.5 No unreasonable delay     ",
        category == "sf76" ~ "7.6 Effective enforcement     ",
        category == "sf77" ~ "7.7 Effective ADRs",
        
        category == "sf81" ~ "8.1 Effective investigations  ",
        category == "sf82" ~ "8.2 Effective adjudication    ",
        category == "sf83" ~ "8.3 Effective correct. system ",
        category == "sf84" ~ "8.4 No discrimination         ",
        category == "sf85" ~ "8.5 No corruption             ",
        category == "sf86" ~ "8.6 No improper gov't influence",
        category == "sf87" ~ "8.7 Due process of law        "
      ),
    )
  
  colors4plot = c("Balcan region"   = "#fa4d57",
                  "North Macedonia" = "#003b8a")
  
  shapes4plot = c("Balcan region"   = 18,
                  "North Macedonia" = 16)
  
  # Plotting individual panels
  imap(c("A" = 1, "B" = 2, "C" = 3, "D" = 4,
         "E" = 5, "F" = 6, "G" = 7, "H" = 8),
       function(cfactor, panelName) {
         
         data2plot <- data2plot %>%
           filter(factor %in% cfactor)
         
         chart <- NM_dotsChart(data         = data2plot,
                               target_var   = "value2plot",
                               grouping_var = "country",
                               labels_var   = "category",
                               colors       = colors4plot,
                               order_var    = "order_var",
                               diffShp      = T,
                               shapes       = shapes4plot,
                               draw_ci      = F,
                               y_upper      = 1,
                               dsize        = 1.5,
                               fsize        = 9,
                               fsize2       = 8)
         
         # Defining height
         if (max(data2plot$order_var) == 3 ) {
           h = 17.57299
         }
         if (max(data2plot$order_var) == 4 ) {
           h = 28.11678
         }
         if (max(data2plot$order_var) == 5 ) {
           h = 34.57299
         }
         if (max(data2plot$order_var) == 6 ) {
           h = 39.01204
         }
         if (max(data2plot$order_var) == 7 ) {
           h = 46.57299
         }
         if (max(data2plot$order_var) == 8 ) {
           h = 50.96167
         }
         
         # Saving panels
         saveIT.fn(chart  = chart,
                   n      = nchart,
                   suffix = panelName,
                   w      = 87.16203,
                   h      = h)
         
       })
}




