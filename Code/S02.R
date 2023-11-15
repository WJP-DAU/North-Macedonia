## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            LAC Country Reports - Section II Functions
##
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
##                    A. Santiago Pardo G.        (spardo@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     November 13th, 2023
##
## This version:      November 13th, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Perceptions of Corruption by Institution Over Time (PCOT)                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure_PCOT.fn <- function(nchart = 7, data = master_data.df) {
  
  # Variables to plot
  vars4plot = list("Legislative"  = c("q2a"), 
                   "Police"       = c("q2d"), 
                   "Executive"    = c("q2b", "q2c"), 
                   "Judiciary"    = c("q2e", "q2f", "q2g"))
  
  # Defining data frame for plot
  data2plot <- data %>%
    filter(country == mainCountry) %>%
    select(year, all_of(unlist(vars4plot))) %>%
    mutate(
      across(!year,
             ~if_else(.x == 3 | .x == 4, 1,
                      if_else(!is.na(.x)  & .x != 99, 0, 
                              NA_real_)))
    ) %>%
    group_by(year) %>%
    summarise(across(everything(),
                     \(x) mean(x, na.rm = TRUE))) %>%
    pivot_longer(!year,
                 names_to  = "category",
                 values_to = "value") %>%
    mutate(value = value*100,
           label = paste0(format(round(value, 0),
                                 nsmall = 0),
                          "%")) %>%
    filter(year >= 2014)
  
  # Pulling minimum and maximum available year
  minyear <- 2013
  maxyear <- 2023
  
  # Creating a vector for yearly axis
  x.axis.values <- seq(minyear, maxyear, by = 2)
  sec.ticks     <- seq(minyear, maxyear, by = 1)
  x.axis.labels <- paste0("'", str_sub(x.axis.values, start = -2))
  
  # Saving data points
  # write.xlsx(as.data.frame(data2plot %>% ungroup()), 
  #            file      = file.path("Outputs", 
  #                                  str_replace_all(mainCountry, " ", "_"),
  #                                  "dataPoints.xlsx",
  #                                  fsep = "/"), 
  #            sheetName = paste0("Chart_", nchart),
  #            append    = T,
  #            row.names = T)
  
  # Plotting each panel of Figure 8
  imap(c("A" = "Legislative", 
         "B" = "Police", 
         "C" = "Executive", 
         "D" = "Judiciary"),
       function(varSet, panelName) {
         
         # Filtering data2plot to leave the variable for each panel
         data2plot <- data2plot %>%
           filter(str_detect(category, varSet))
         
         # Defining colors4plot
         colors4plot    <- glinesPalette[1:length(vars4plot[[varSet]])]
         naming_vector  <- paste0(varSet, as.character(1:length(vars4plot[[varSet]])))
         
         if (length(vars4plot[[varSet]]) == 1) {
           naming_vector <- str_remove(naming_vector, "1")
         }
         
         names(naming_vector)
         
         # Applying plotting function
         chart <- LAC_lineChart(data           = data2plot,
                                target_var     = "value",
                                grouping_var   = "year",
                                ngroups        = data2plot$category, 
                                labels_var     = "label",
                                colors_var     = "category",
                                colors         = colors4plot,
                                repel          = F,
                                custom.axis    = T,
                                x.breaks       = x.axis.values,
                                x.labels       = x.axis.labels,
                                sec.ticks      = sec.ticks
         )
         
         # Saving panels
         saveIT.fn(chart  = chart,
                   n      = nchart,
                   suffix = panelName,
                   w      = 91.37955,
                   h      = 76.9697)
       })
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Attitudes Towards Corrupt Behaviors (ACB)                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure_ACB.fn <- function(nchart = 8, data = master_data.df){
  
  # Defining variables to use in the plot
  vars4plot <- list(
    "Offered"   = c("CAR_q2c"),
    "Requested" = c("CAR_q2b", "CAR_q2f", "CAR_q2g"),
    "Nepotism"  = c("CAR_q2a", "CAR_q2d", "CAR_q2e")
  )
  
  # Defining data frame for plot
  data2plot <- master_data.df %>%
    filter(year == latestYear & country == mainCountry) %>%
    select(unlist(vars4plot, 
                  use.names = F)) %>%
    mutate(
      across(everything(),
             ~case_when(
               .x == 1  ~ "Always Acceptable",
               .x == 2  ~ "Usually Acceptable",
               .x == 3  ~ "Sometimes Acceptable",
               .x == 4  ~ "Not Acceptable",
               .x == 99 ~ "DK/NA"
             ))
    ) %>%
    pivot_longer(everything(),
                 names_to   = "variable",
                 values_to  = "statement") %>%
    group_by(variable, statement) %>%
    summarise(count = n()) %>%
    mutate(count     = if_else(statement == "DK/NA", 
                               count/2, 
                               count),
           statement = if_else(statement == "DK/NA", 
                               "Don't know (positive)", 
                               statement))
  
  # Splitting DK/NA
  data2plot <- data2plot %>%
    bind_rows(
      data2plot %>%
        filter(statement %in% c("Don't know (positive)")) %>%
        mutate(statement = "Don't know (negative)")
    ) %>%
    arrange(variable, statement)
  
  # Labeling and percentages
  data2plot <- data2plot %>%
    filter(!is.na(statement)) %>%
    group_by(variable) %>%
    mutate(
      n = sum(count),
      perc = count/n,
      direction = if_else(statement %in% c("Not Acceptable", "Don't know (positive)"),
                          "Positive",
                          "Negative"),
      value2plot  = if_else(direction == "Positive", perc*100, perc*-100),
      value_label = to_percentage.fn(round(abs(value2plot), 0)),
      labels = case_when(
        variable == "CAR_q2b" ~ "A public officer asking for a bribe to \nspeed up administrative procedures",
        variable == "CAR_q2f" ~ "A law enforcement officer (police, \ncustoms, immigration, civil guard, \nmilitary police) asking for a bribe",
        variable == "CAR_q2g" ~ "A company official asking for a bribe\nfrom a job applicant",
        variable == "CAR_q2c" ~ "A private citizen offering a bribe \nto a public official to speed up \nadministrative procedures                 ",
        variable == "CAR_q2a" ~ "A public officer being recruited on \nthe basis of family ties and \nfriendship networks",
        variable == "CAR_q2d" ~ "An elected official taking public funds\nfor private use",
        variable == "CAR_q2e" ~ "An elected official using stolen public \nfunds to assist his or her community"
      ),
      order_value = case_when(
        variable == "CAR_q2b"  ~ 2,
        variable == "CAR_q2f"  ~ 3,
        variable == "CAR_q2g"  ~ 1,
        variable == "CAR_q2c"  ~ 1,
        variable == "CAR_q2a"  ~ 1,
        variable == "CAR_q2d"  ~ 3,
        variable == "CAR_q2e"  ~ 2
      ),
      statement = if_else(statement %in% c("Don't know (positive)", "Don't know (negative)"),
                          "Don't know",
                          statement),
      statement = factor(statement,
                         levels = c("Not Acceptable", "Always Acceptable", "Usually Acceptable", "Sometimes Acceptable", "Don't know"))
    )
  
  # Defining color palette
  colors4plot <- lickertPalette
  names(colors4plot) <- c("Not Acceptable", "Sometimes Acceptable", "Don't know", "Usually Acceptable", "Always Acceptable")
  
  # Saving data points
  # write.xlsx(as.data.frame(data2plot %>% ungroup()), 
  #            file      = file.path("Outputs", 
  #                                  str_replace_all(mainCountry, " ", "_"),
  #                                  "dataPoints.xlsx",
  #                                  fsep = "/"), 
  #            sheetName = paste0("Chart_", nchart), 
  #            append    = T,
  #            row.names = T)
  
  # Plotting each panel of Figure 12
  imap(c("A" = "Offered", 
         "B" = "Requested", 
         "C" = "Nepotism"),
       function(varSet, panelName) {
         
         # Filtering data2plot to leave the variable for each panel
         data2plot <- data2plot %>%
           filter(variable %in% vars4plot[[varSet]])
         
         # Applying plotting function
         chart <- NM_divBars(data           = data2plot,
                             target_var     = "value2plot",
                             rows_var       = "labels",
                             grouping_var   = "statement",
                             negative_value = "Negative",
                             colors         = colors4plot,
                             labels_var     = "value_label",
                             custom_order   = TRUE,
                             order_var      = "order_value")
         
         # Defining height
         if (length(vars4plot[[varSet]]) == 3 ) {
           h = 47.44707
         }
         
         if (length(vars4plot[[varSet]]) == 1 ) {
           h = 32.23357
         }
         
         # Saving panels
         saveIT.fn(chart  = chart,
                   n      = nchart,
                   suffix = panelName,
                   w      = 189.7883,
                   h      = h)
       })
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Bribery Victimization Over Time (BVOT)                                                                ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure_BVOT.fn <- function(nchart = 9, data = master_data.df) {
  
  # Defining variables to include in plot
  vars4plot <- c("q4a", "q4b", "q4c", "q4d", "q4e")
  
  # Defining data frame for plot
  data2plot <- data %>%
    filter(year >= 2017) %>%
    select(year, latestYear, all_of(unlist(vars4plot, use.names = F))) %>%
    mutate(across(!c(year, latestYear),
                  ~if_else(.x == 99, NA_real_, as.double(.x)))) %>%
    group_by(year, latestYear) %>%
    summarise(across(everything(),
                     \(x) mean(x, na.rm = TRUE))) %>%
    pivot_longer(!c(year, latestYear),
                 names_to   = "category",
                 values_to  = "value2plot") %>%
    mutate(
      value2plot  = value2plot*100,
      highlighted = if_else(year == latestYear, 
                            "Highlighted", 
                            "Regular"),
      labels      = to_percentage.fn(value2plot),
      year        = factor(as.character(year), 
                           levels = c("2017", "2023"))
    )
  
  
  # Saving data points
  # write.xlsx(as.data.frame(data2plot %>% select(!highlighted) %>% ungroup()), 
  #            file      = file.path("Outputs", 
  #                                  str_replace_all(mainCountry, " ", "_"),
  #                                  "dataPoints.xlsx",
  #                                  fsep = "/"), 
  #            sheetName = paste0("Chart_", nchart),
  #            append    = T,
  #            row.names = T)
  
  # Defining colors
  colors4plot <- barsPalette
  names(colors4plot) <- c("Highlighted", "Regular")
  
  # Plotting each panel of Figure 5
  panelVector <- c("A" = vars4plot[1], 
                   "B" = vars4plot[2], 
                   "C" = vars4plot[3], 
                   "D" = vars4plot[4], 
                   "E" = vars4plot[5])
  
  imap(panelVector,
       function(tvar, panelName) {
         
         # Filtering data2plot to leave the variable for each panel
         data2plot <- data2plot %>%
           filter(category %in% tvar)
         
         # Applying plotting function
         chart <- LAC_barsChart(data           = data2plot,
                                target_var     = "value2plot",
                                grouping_var   = "year",
                                labels_var     = "labels",
                                colors_var     = "highlighted",
                                colors         = colors4plot,
                                direction      = "horizontal")

         
         # Saving panels
         saveIT.fn(chart  = chart,
                   n      = nchart,
                   suffix = panelName,
                   w      = 86.81057,
                   h      = 22.60219)
         
       })
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Bribery Victimization Across Agents (BVAG)                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# vars4plot <- c("CAR_q8a", "CAR_q8b", "CAR_q8d", "CAR_q8e", "CAR_q8f", "CAR_q8i")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Trust in Institutions Over Time (TIOT)                                                                ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure_TIOT.fn <- function(nchart = 11, data = master_data.df) {
  
  # Variables to plot
  vars4plot = list("Community"    = c("q1a"), 
                   "Police"       = c("q1d"), 
                   "Executive"    = c("q1b", "q1c"), 
                   "Judiciary"    = c("q1e", "q1f", "q1g"))
  
  # Defining data frame for plot
  data2plot <- data %>%
    filter(country == mainCountry) %>%
    select(year, all_of(unlist(vars4plot))) %>%
    mutate(
      across(!year,
             ~if_else(.x == 1 | .x == 2, 1,
                      if_else(!is.na(.x) & .x != 99, 0, 
                              NA_real_)))
    ) %>%
    group_by(year) %>%
    summarise(across(everything(),
                     mean,
                     na.rm = T)) %>%
    pivot_longer(!year,
                 names_to  = "category",
                 values_to = "value") %>%
    mutate(value = value*100,
           label = paste0(format(round(value, 0),
                                 nsmall = 0),
                          "%")) %>%
    filter(year >= 2014)
  
  # Pulling minimum and maximum available year
  minyear <- 2013
  maxyear <- 2023
  
  # Creating a vector for yearly axis
  x.axis.values <- seq(minyear, maxyear, by = 2)
  sec.ticks     <- seq(minyear, maxyear, by = 1)
  x.axis.labels <- paste0("'", str_sub(x.axis.values, start = -2))
  
  # Saving data points
  # write.xlsx(as.data.frame(data2plot %>% ungroup()), 
  #            file      = file.path("Outputs", 
  #                                  str_replace_all(mainCountry, " ", "_"),
  #                                  "dataPoints.xlsx",
  #                                  fsep = "/"), 
  #            sheetName = paste0("Chart_", nchart),
  #            append    = T,
  #            row.names = T)
  
  # Plotting each panel of Figure 12
  imap(c("A" = "Community", 
         "B" = "Police", 
         "C" = "Executive", 
         "D" = "Judiciary"),
       function(varSet, panelName) {
         
         # Filtering data2plot to leave the variable for each panel
         data2plot <- data2plot %>%
           filter(str_detect(category, varSet))
         
         # Defining colors4plot
         colors4plot    <- glinesPalette[1:length(vars4plot[[varSet]])]
         naming_vector  <- paste0(varSet, as.character(1:length(vars4plot[[varSet]])))
         
         if (length(vars4plot[[varSet]]) == 1) {
           naming_vector <- str_remove(naming_vector, "1")
         }
         
         names(naming_vector)
         
         # Applying plotting function
         chart <- LAC_lineChart(data           = data2plot,
                                target_var     = "value",
                                grouping_var   = "year",
                                ngroups        = data2plot$category, 
                                labels_var     = "label",
                                colors_var     = "category",
                                colors         = colors4plot,
                                repel          = F,
                                custom.axis    = T,
                                x.breaks       = x.axis.values,
                                x.labels       = x.axis.labels,
                                sec.ticks      = sec.ticks
         )
         
         # Saving panels
         saveIT.fn(chart  = chart,
                   n      = nchart,
                   suffix = panelName,
                   w      = 91.37955,
                   h      = 76.9697)
         
       })
} 

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Perceptions of Corruption and Trust, by Ethnicity (PCTE)                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure_PCTE.fn <- function(nchart = 12, data = master_data.df) {
  
  alpha <- 0.05
  
  # Variables to plot
  vars4plot <- list("Corruption" = c("q2a","q2d","q2b", "q2c","q2e", "q2f", "q2g"),
                    "Trust"      = c("q1a","q1d","q1b", "q1c","q1e", "q1f", "q1g"))
  
  imap(c("ethnigroup" = "Macedonian",
         "relgroup"   = "Christian"),
       function(blue_category, target) {
         
         # Defining data frame for plot
         data2plot <- data %>%
           filter(country == mainCountry) %>%
           filter(year == latestYear) %>%
           mutate(party = case_when(
             paff3 == "The Democratic Party" ~ "Democratic Party",
             paff3 == "The Republican Party" ~ "Republican Party"
           )) %>%
           select(party, all_of(unlist(vars4plot, 
                                       use.names = F))) %>%
           mutate(
             across(starts_with("q2"),
                    ~if_else(.x == 3 | .x == 4, 1,
                             if_else(!is.na(.x)  & .x != 99, 0, 
                                     NA_real_))),
             across(starts_with("q1"),
                    ~if_else(.x == 1 | .x == 2, 1,
                             if_else(!is.na(.x) & .x != 99, 0, 
                                     NA_real_)))
           ) %>%
           group_by(party) %>%
           mutate(obs = n()) %>%
           ungroup() %>%
           group_by(party) %>%
           summarise(
             across(c(all_of(unlist(vars4plot, 
                                    use.names = F))),
                    mean, 
                    na.rm = T,
                    .names = "{col}_mean"),
             across(c(all_of(unlist(vars4plot, 
                                    use.names = F))),
                    sd,
                    na.rm = T,
                    .names = "{col}_sd"),
             n_obs = mean(obs, na.rm = T),
             n_obs = as.character(n_obs)
           ) %>%
           drop_na() %>%
           pivot_longer(!c(party,n_obs),
                        names_to      = c("category", "stat"),
                        names_pattern = "(.*)_(.*)",
                        values_to     = "value") %>%
           pivot_wider(c(category,party,n_obs),
                       names_from  = stat,
                       values_from = value) %>%
           mutate(
             n_obs  = as.numeric(n_obs),
             labels = case_when(
               category == "q2a" ~ "Members of congress",
               category == "q2d" ~ "Police officers",
               category == "q2b" ~ "Local government officers",
               category == "q2c" ~ "National government officers",
               category == "q2e" ~ "Prosecutors",
               category == "q2f" ~ "Public defense attorneys",
               category == "q2g" ~ "Judges and magistrates",
               category == "q1a" ~ "People living in their country",
               category == "q1d" ~ "Police officers",
               category == "q1b" ~ "Local government officers",
               category == "q1c" ~ "National government officers",
               category == "q1e" ~ "Prosecutors",
               category == "q1f" ~ "Public defense attorneys",
               category == "q1g" ~ "Judges and magistrates"
               
               
             ),
             lower = mean - qt(1- alpha/2, (n() - 1))*sd/sqrt(n_obs),
             upper = mean + qt(1- alpha/2, (n() - 1))*sd/sqrt(n_obs)
           ) %>%
           rename(values = mean) %>%
           mutate(batch = if_else(str_detect(category, "q1"), "trust", "corruption")) %>%
           mutate(order_values =
                    case_when(
                      category %in% c("q1a", "q2a") ~ 1,
                      category %in% c("q1d", "q2d") ~ 2,
                      category %in% c("q1b", "q2b") ~ 3,
                      category %in% c("q1c", "q2c") ~ 4,
                      category %in% c("q1e", "q2e") ~ 5,
                      category %in% c("q1g", "q2g") ~ 1,
                      category %in% c("q1f", "q2f") ~ 6
                    ))
         
         # Defining color palette
         colors4plot <- binPalette
         names(colors4plot) <- data2plot %>% distinct(party) %>% arrange(party) %>% pull(party)
         
         imap(c("A" = "corruption",
                "B" = "trust"),
              function(varSet, panelName) {
                
                # Filtering data2plot to leave the variable for each panel
                data2plot <- data2plot %>%
                  filter(batch %in% varSet)
                
                # Applying plotting function
                chart <- errorDotsChart(data2plot = data2plot,
                                        labels = "labels",
                                        group = "party",
                                        category = "category",
                                        values = values,
                                        lower = lower,
                                        upper = upper, 
                                        colors4plot = colors4plot, 
                                        custom_order = F, 
                                        order_values = order_values)
                # Saving panels
                saveIT.fn(chart  = chart,
                          n      = nchart,
                          suffix = panelName,
                          w      = 189.7883,
                          h      = 54.12481)
                
              })
       })
}