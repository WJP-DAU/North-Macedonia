## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            North Macedonia Country Report - Section V Functions
##
## Author(s):         Artha P. Pillai             (apillai@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     January 5th, 2024
##
## This version:      January 9th, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Perceptions of Fundamental Freedoms by Cultural Group(PAOT)                                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

figure_PFFCG.fn <- function(nchart = 22, data = master_data.df, group = "ethnicity") {
  
  # Defining variables to use in the plot
  vars4plot <- list("Expression"    = c("q46c_G2", "q46f_G2", "q46g_G2", "q46c_G1", "q46e_G2"),
                    "Election"      = c("q46d_G2", "q46f_G1", "q46a_G2","q46d_G1", "q46e_G1"),
                    "Religion"      = c("q46h_G2")
                    )
  #Defining data
  data2plot <- data %>%
    filter(country == mainCountry & year == latestYear) %>%
    select(ethni,relig, CAR_q59_G1, CAR_q59_G2, unlist(vars4plot, use.names = FALSE)) %>%
    mutate(
      religr = case_when(
        relig %in% c("C57 - Orthodox Christian") ~ "Orthodox Christian",
        relig %in% c("G6 - Sunni Muslim")        ~ "Sunni Muslim"
      ),
      ethnicity = case_when(
          ethni == "Macedonian" ~ "Macedonian",
          ethni == "Albanian"   ~ "Albanian"
      ),
      govSupp = case_when(
        !is.na(CAR_q59_G1) & !is.na(CAR_q59_G2) ~ NA_character_,
        CAR_q59_G1 == 1   | CAR_q59_G2 == 1     ~ "Gov. Supporter",
        CAR_q59_G1 == 2   | CAR_q59_G2 == 2     ~ "Non Gov. Supporter",
        CAR_q59_G1 == 99  | CAR_q59_G2 == 99    ~ NA_character_,
        is.na(CAR_q59_G1) & is.na(CAR_q59_G2)   ~ NA_character_
      ),
      across(!c(ethni, ethnicity, relig, religr, govSupp),
             ~ if_else(.x == 1 | .x == 2, 1,
                       if_else(!is.na(.x) & .x != 99, 0, 
                               NA_real_)))
    ) 
  
  if(group == "religion") {
    
    data2plot <- data2plot %>%
      select(!c(CAR_q59_G1, CAR_q59_G2, relig,ethni,ethnicity, govSupp)) %>%
      filter(!is.na(religr)) %>%
      pivot_longer(!c(religr), names_to = "category", values_to = "value") %>%
      group_by(religr, category) %>%
      summarise(
        mean_value = mean(value, na.rm = TRUE),
        sd_value = sd(value, na.rm = TRUE),
        n_obs = n()
      ) %>%
      ungroup()%>%
      drop_na()
    
  } else if(group == "ethnicity") {
    
    data2plot <- data2plot %>%
      select(!c(CAR_q59_G1, CAR_q59_G2, relig, religr,ethni, govSupp)) %>%
      filter(!is.na(ethnicity)) %>%
      pivot_longer(!c(ethnicity), names_to = "category", values_to = "value") %>%
      group_by(ethnicity, category) %>%
      summarise(
        mean_value = mean(value, na.rm = TRUE),
        sd_value = sd(value, na.rm = TRUE),
        n_obs = n()
      ) %>%
      ungroup()%>%
      drop_na()
      
    
  } else {
    
    data2plot <- data2plot %>%
      select(!c(CAR_q59_G1, CAR_q59_G2, relig,ethni,ethnicity, religr)) %>%
      filter(!is.na(govSupp)) %>%
      pivot_longer(!c(govSupp), names_to = "category", values_to = "value") %>%
      group_by(govSupp, category) %>%
      summarise(
        mean_value = mean(value, na.rm = TRUE),
        sd_value = sd(value, na.rm = TRUE),
        n_obs = n()
      ) %>%
      ungroup()%>%
      drop_na()
  }
  
  data2plot <- data2plot %>%
    mutate(
      labels = case_when(
        category == 'q46c_G2'       ~ paste("People can express opinions  \nagainst the government"),
        category == 'q46f_G2'       ~ paste("Civil society organizations can express  \nopinions against the government"),
        category == 'q46g_G2'       ~ paste("Political parties can express \nopinions against the government"),
        category == 'q46c_G1'       ~ paste("The media can express opinions\nagainst the government"),
        category == 'q46e_G2'       ~ paste("The media can expose cases\nof corruption"),
        category == 'q46d_G2'       ~ paste("People can attend community meetings"),
        category == 'q46f_G1'       ~ paste("People can join any \npolitical organization"),
        category == 'q46a_G2'       ~ paste("People can organize \naround an issue or petition"),
        category == 'q46d_G1'       ~ paste("Local government officials \nare elected through a clean process"),
        category == 'q46e_G1'       ~ paste("People can vote freely without \nfeeling harassed or pressured"),
        category == 'q46h_G2'       ~ paste("Religious minorities can observe their   \nholy days"),
      ),
      mean_value = mean_value * 100,
      sd_value = sd_value*100,
      order_value = case_when(
        category     == 'q46c_G2' ~ 1,
        category     == 'q46f_G2' ~ 2,
        category     == 'q46g_G2' ~ 3,
        category     == 'q46c_G1' ~ 4,
        category     == 'q46e_G2' ~ 5,
        category     == 'q46d_G2' ~ 6,
        category     == 'q46f_G1' ~ 7,
        category     == 'q46a_G2' ~ 8,
        category     == 'q46d_G1' ~ 9,
        category     == 'q46e_G1' ~ 10,
        category     == 'q46h_G2' ~ 11
      )
    )
  
  if(group == "religion") {
    
    # Defining color palette
    colors4plot <- c("Orthodox Christian" = "#a90099", 
                     "Sunni Muslim"       = "#3273ff")
  } else if(group == "ethnicity") {
    
    # Defining color palette
    colors4plot <- c("Macedonian" = "#1a2589", 
                     "Albanian"       = "#855af0")
  } else{
    # Defining color palette
    colors4plot <- c("Non Gov. Supporter" = "#a90099", 
                     "Gov. Supporter"       = "#3273ff")
  }
  
  # Saving data points
  data_PFFCG = data2plot %>% ungroup()
  
  # Plotting each panel of Figure
  imap(c("A" = "Expression", 
         "B" = "Election", 
         "C" = "Religion"),
       function(varSet, panelName) {
         
         # Filtering data2plot to leave the variable for each panel
         data2plot <- data2plot %>%
           filter(category %in% vars4plot[[varSet]])
         
         if(group == "religion") {
           
           # Applying plotting function
           chart <- NM_dotsChart(data         = data2plot,
                                 target_var   = "mean_value",
                                 sd_var       = "sd_value",
                                 n_obs        = "n_obs", 
                                 alpha        = 0.05,
                                 grouping_var = "religr",
                                 labels_var   = "labels",
                                 colors       = colors4plot,
                                 order_var    = "order_value")
         } else if(group == "ethnicity") {
           
           # Applying plotting function
           chart <- NM_dotsChart(data         = data2plot,
                                 target_var   = "mean_value",
                                 sd_var       = "sd_value",
                                 n_obs        = "n_obs", 
                                 alpha        = 0.05,
                                 grouping_var = "ethnicity",
                                 labels_var   = "labels",
                                 colors       = colors4plot,
                                 order_var    = "order_value")
         } else {
           
           # Applying plotting function
           chart <- NM_dotsChart(data         = data2plot,
                                 target_var   = "mean_value",
                                 sd_var       = "sd_value",
                                 n_obs        = "n_obs", 
                                 alpha        = 0.05,
                                 grouping_var = "govSupp",
                                 labels_var   = "labels",
                                 colors       = colors4plot,
                                 order_var    = "order_value")
           
         }
         
         # Defining height
         
         if (length(vars4plot[[varSet]]) == 5 ) {
           h = 60
         }
         
         if (length(vars4plot[[varSet]]) == 1 ) {
           h = 20
         }
         # Saving panels
         saveIT.fn(chart  = chart,
                   n      = nchart,
                   suffix = paste0(panelName, "_", group),
                   w      = 195,
                   h      = h)
       })
        
  return(data_PFFCG)
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Perceptions of Police by Cultural Group (POPCG)                                                       ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure_POPCG.fn <- function(nchart = 23, data = master_data.df) {
  
  # Defining variables to include in plot
  vars4plot <- c("q48c_G2", "q48a_G2","q48b_G1", "EXP_q22h_G2",
                 "q48a_G1", "q48c_G1", "q18a","q18c")
  
  # Defining data frame for plot
  data2plot <- data %>%
    filter(year == latestYear) %>%
    select(ethni, all_of(unlist(vars4plot, use.names = F))) %>%
    mutate(
      ethnicity =
        case_when(
          ethni == "Macedonian" ~ "Macedonian",
          ethni == "Albanian"   ~ "Albanian"
        ),
      across(all_of(c("q48c_G2", "q48a_G2","q48b_G1", "EXP_q22h_G2",
                       "q48a_G1", "q48c_G1")),
                   ~case_when(
                     .x == 1  ~ 1,
                     .x == 2  ~ 1,
                     .x == 3  ~ 0,
                     .x == 4  ~ 0)),
      across(all_of(c("q18a","q18c")),
         ~if_else(.x == 99, NA_real_, as.double(.x))),
    )
  
  data2plot <- data2plot %>%
    select(!c(ethni)) %>%
    filter(!is.na(ethnicity)) %>%
    pivot_longer(!c(ethnicity), names_to = "category", values_to = "value") %>%
    group_by(ethnicity, category) %>%
    summarise(value2plot = mean(value, na.rm = TRUE)) %>%
    ungroup()%>%
    drop_na()%>%
    mutate(
      value2plot  = value2plot * 100,
      highlighted = if_else(ethnicity == "Albanian", "Highlighted", "Regular"),
      labels      = to_percentage.fn(value2plot),
      ethnicity      = factor(ethnicity, levels = c("Macedonian","Albanian"))
    )
  
  # Defining colors
  colors4plot <- c("Macedonian" = "#1a2589",
                   "Albanian"   = "#b299ff")  
  names(colors4plot) <- c("Regular","Highlighted")
  
  # Saving data points
  data_POPCG = data2plot %>% ungroup()
  
  # Plotting each panel of Figure 5
  panelVector <- c("A" = vars4plot[1], 
                   "B" = vars4plot[2], 
                   "C" = vars4plot[3], 
                   "D" = vars4plot[4],
                   "E" = vars4plot[5], 
                   "F" = vars4plot[6], 
                   "G" = vars4plot[7], 
                   "H" = vars4plot[8])
  
  imap(panelVector,
       function(tvar, panelName) {
         
         # Filtering data2plot to leave the variable for each panel
         data2plot <- data2plot %>%
           filter(category %in% tvar)
         
         # Applying plotting function
         chart <- LAC_barsChart(data           = data2plot,
                                target_var     = "value2plot",
                                grouping_var   = "ethnicity",
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
  
  return(data_POPCG)
}
  
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Perceptions of Authoritarian Behavior, by Financial Security (PABFS) ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure_PABFS.fn <- function(nchart = 24, data = master_data.df, group = "income") {
  
  # Defining variables to use in the plot
  vars4plot <- list(
    "Independent" = c("CAR_q67_G1", "CAR_q67_G2", "CAR_q68_G1", "CAR_q61_G1"),
    "Judiciary"   = c("CAR_q66_G1", "CAR_q65_G1", "CAR_q64_G1"),
    "Media"       = c("CAR_q64_G2", "CAR_q60_G2", "CAR_q65_G2", "CAR_q60_G1")
  )
  
  # Defining data and adding income group
  data2plot <- data %>%
    filter(country == mainCountry & year == latestYear) %>%
    select(fin, relig, CAR_q59_G1, CAR_q59_G2, unlist(vars4plot, use.names = FALSE)) %>%
    mutate(
      incomeGroup = case_when(
        fin == 1 | fin == 2                  ~ "Low Income",
        fin == 3 | fin == 4 | fin == 5       ~ "High Income",
        TRUE                                 ~ NA_character_
      ),
      religr = case_when(
        relig %in% c("C57 - Orthodox Christian") ~ "Orthodox Christian",
        relig %in% c("G6 - Sunni Muslim")        ~ "Sunni Muslim"
      ),
      govSupp = case_when(
        !is.na(CAR_q59_G1) & !is.na(CAR_q59_G2) ~ NA_character_,
        CAR_q59_G1 == 1   | CAR_q59_G2 == 1     ~ "Gov. Supporter",
        CAR_q59_G1 == 2   | CAR_q59_G2 == 2     ~ "Non Gov. Supporter",
        CAR_q59_G1 == 99  | CAR_q59_G2 == 99    ~ NA_character_,
        is.na(CAR_q59_G1) & is.na(CAR_q59_G2)   ~ NA_character_
      ),
      across(!c(fin,incomeGroup,relig, religr, govSupp),
             ~ if_else(.x == 1 | .x == 2, 1,
                       if_else(!is.na(.x) & .x != 99, 0, 
                               NA_real_)))
    ) 
  
  # Data processing based on the group
  if(group == "religion") {
    data2plot <- data2plot %>%
      select(!c(CAR_q59_G1, CAR_q59_G2, relig, govSupp)) %>%
      filter(!is.na(religr)) %>%
      pivot_longer(!c(religr), names_to = "category", values_to = "value") %>%
      group_by(religr, category) %>%
      summarise(
        mean_value = mean(value, na.rm = TRUE),
        sd_value = sd(value, na.rm = TRUE),
        n_obs = n()
      ) %>%
      ungroup()
    
  } else if(group == "govSupp") {
    data2plot <- data2plot %>%
      select(!c(CAR_q59_G1, CAR_q59_G2, relig, religr)) %>%
      filter(!is.na(govSupp)) %>%
      pivot_longer(!c(govSupp), names_to = "category", values_to = "value") %>%
      group_by(govSupp, category) %>%
      summarise(
        mean_value = mean(value, na.rm = TRUE),
        sd_value = sd(value, na.rm = TRUE),
        n_obs = n()
      ) %>%
      ungroup()
    
  } else if(group == "income") {
    data2plot <- data2plot %>%
      select(!c(CAR_q59_G1, CAR_q59_G2,fin,relig, religr,govSupp)) %>%
      filter(!is.na(incomeGroup)) %>%
      pivot_longer(!c(incomeGroup), names_to = "category", values_to = "value") %>%
      group_by(incomeGroup, category) %>%
      summarise(
        mean_value = mean(value, na.rm = TRUE),
        sd_value = sd(value, na.rm = TRUE),
        n_obs = n()
      ) %>%
      ungroup()
  }
  
  data2plot <- data2plot %>%
    mutate(
      labels = case_when(
        category == "CAR_q60_G1" ~ "Censor information that comes \nfrom abroad",
        category == "CAR_q61_G1" ~ "Censor opinions from opposition \ngroups",
        category == "CAR_q60_G2" ~ "Resort to misinformation to shape \npublic opinion in their favor",
        category == "CAR_q64_G2" ~ "Attack or attempt to discredit the \nmedia and civil society organizations\nthat criticize them",
        category == "CAR_q67_G1" ~ "Attack or attempt to discredit \nopposition parties",
        category == "CAR_q67_G2" ~ "Attack or attempt to discredit the \nelectoral system and other \nsupervisory organs", 
        category == "CAR_q64_G1" ~ "Seek to limit the courts' competencies \nand freedom to interpret the law",
        category == "CAR_q66_G1" ~ "Seek to influence the promotion and \nremoval of judges",
        category == "CAR_q65_G2" ~ "Prosecute and convict journalists and \nleaders of civil society organizations     ",
        category == "CAR_q68_G1" ~ "Prosecute and convict members of\nopposition parties                                   ",
        category == "CAR_q65_G1" ~ "Refuse to comply with court rulings \nthat are not in their favor"
        
      ),
      mean_value = mean_value * 100,
      sd_value = sd_value*100,
      order_value = case_when(
        category == "CAR_q60_G1" ~ 4,
        category == "CAR_q61_G1" ~ 4,
        category == "CAR_q60_G2" ~ 1,
        category == "CAR_q64_G2" ~ 2,
        category == "CAR_q67_G1" ~ 1,
        category == "CAR_q67_G2" ~ 3, 
        category == "CAR_q64_G1" ~ 3,
        category == "CAR_q66_G1" ~ 1,
        category == "CAR_q65_G2" ~ 3,
        category == "CAR_q68_G1" ~ 2,
        category == "CAR_q65_G1" ~ 2
      )
    )
  
  if(group == "income") {
    
    # Defining color palette for income group
    colors4plot <- c("Low Income" = "#25408b", 
                     "High Income" = "#de3956")
    
  }else if(group == "religion") {
    
    # Defining color palette
    colors4plot <- c("Orthodox Christian" = "#a90099", 
                     "Sunni Muslim"       = "#3273ff")
  } else{
    # Defining color palette
    colors4plot <- c("Non Gov. Supporter" = "#a90099", 
                     "Gov. Supporter"       = "#3273ff")
  }
  
  # Saving data points
  data_PABFS = data2plot %>% ungroup()
  
  # Plotting each panel of Figure
  imap(c("A" = "Independent", 
         "B" = "Judiciary", 
         "C" = "Media"),
       function(varSet, panelName) {
         
         # Filtering data2plot to leave the variable for each panel
         data2plot <- data2plot %>%
           filter(category %in% vars4plot[[varSet]])
         
         if(group == "religion") {
           
           # Applying plotting function
           chart <- NM_dotsChart(data         = data2plot,
                                 target_var   = "mean_value",
                                 sd_var       = "sd_value",
                                 n_obs        = "n_obs", 
                                 alpha        = 0.05,
                                 grouping_var = "religr",
                                 labels_var   = "labels",
                                 colors       = colors4plot,
                                 order_var    = "order_value")
         } else if(group == "income") {
           
           # Applying plotting function
           chart <- NM_dotsChart(data         = data2plot,
                                 target_var   = "mean_value",
                                 sd_var       = "sd_value",
                                 n_obs        = "n_obs", 
                                 alpha        = 0.05,
                                 grouping_var = "incomeGroup",
                                 labels_var   = "labels",
                                 colors       = colors4plot,
                                 order_var    = "order_value")
         } else {
           
           # Applying plotting function
           chart <- NM_dotsChart(data         = data2plot,
                                 target_var   = "mean_value",
                                 sd_var       = "sd_value",
                                 n_obs        = "n_obs", 
                                 alpha        = 0.05,
                                 grouping_var = "govSupp",
                                 labels_var   = "labels",
                                 colors       = colors4plot,
                                 order_var    = "order_value")
           
         }
         
         # Defining height
         if (length(vars4plot[[varSet]]) == 3 ) {
           h = 47.44707
         }
         
         if (length(vars4plot[[varSet]]) == 4 ) {
           h = 56.23357
         }
         
         # Saving panels
         saveIT.fn(chart  = chart,
                   n      = nchart,
                   suffix = paste0(panelName, "_", group),
                   w      = 189.7883,
                   h      = h)
       })
  
  return(data_PABFS)
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Perceptions of Corruption and Trust, by Financial Security (PCTFS)                                   ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


figure_PCTFS.fn <- function(nchart = 25, data = master_data.df, group = "income") {
  
  # Defining variables to use in the plot
  vars4plot <- list("Corruption" = c("q2a","q2d","q2b", "q2c","q2e", "q2f", "q2g"),
                    "Trust"      = c("q1a","q1d","q1b", "q1c","q1e", "q1f", "q1g"))
  
  # Defining data frame for plot
  data2plot <- data %>%
    filter(country == mainCountry & year == latestYear) %>%
    select(fin,relig, CAR_q59_G1, CAR_q59_G2,
           unlist(vars4plot, 
                  use.names = F)) %>%
    mutate(
      incomeGroup = case_when(
        fin == 1 | fin == 2                  ~ "Low Income",
        fin == 3 | fin == 4 | fin == 5       ~ "High Income",
        TRUE                                 ~ NA_character_
      ),
      religr = case_when(
        relig %in% c("C57 - Orthodox Christian") ~ "Orthodox Christian",
        relig %in% c("G6 - Sunni Muslim")        ~ "Sunni Muslim"
      ),
      govSupp = case_when(
        !is.na(CAR_q59_G1) & !is.na(CAR_q59_G2) ~ NA_character_,
        CAR_q59_G1 == 1   | CAR_q59_G2 == 1     ~ "Gov. Supporter",
        CAR_q59_G1 == 2   | CAR_q59_G2 == 2     ~ "Non Gov. Supporter",
        CAR_q59_G1 == 99  | CAR_q59_G2 == 99    ~ NA_character_,
        is.na(CAR_q59_G1) & is.na(CAR_q59_G2)   ~ NA_character_
      ),
      across(!c(fin, incomeGroup, relig, religr, govSupp, all_of(vars4plot[["Corruption"]])),
             ~ if_else(.x == 1 | .x == 2, 1,
                       if_else(!is.na(.x) & .x != 99, 0, 
                               NA_real_))),
      across(!c(fin, incomeGroup, relig, religr, govSupp, all_of(vars4plot[["Trust"]])),
             ~if_else(.x == 3 | .x == 4, 1,
                      if_else(!is.na(.x)  & .x != 99, 0, 
                              NA_real_)))
    ) 
  
  if(group == "religion") {
    
    data2plot <- data2plot %>%
      select(!c(CAR_q59_G1, CAR_q59_G2, relig, govSupp)) %>%
      filter(!is.na(religr)) %>%
      pivot_longer(!c(religr), names_to = "category", values_to = "value") %>%
      group_by(religr, category) %>%
      summarise(
        mean_value = mean(value, na.rm = TRUE),
        sd_value = sd(value, na.rm = TRUE),
        n_obs = n()
      ) %>%
      ungroup()
    
  } else if(group == "income") {
    data2plot <- data2plot %>%
      select(!c(CAR_q59_G1, CAR_q59_G2,fin,relig, religr,govSupp)) %>%
      filter(!is.na(incomeGroup)) %>%
      pivot_longer(!c(incomeGroup), names_to = "category", values_to = "value") %>%
      group_by(incomeGroup, category) %>%
      summarise(
        mean_value = mean(value, na.rm = TRUE),
        sd_value = sd(value, na.rm = TRUE),
        n_obs = n()
      ) %>%
      ungroup()
  } else if(group == "govSupp"){
    
    data2plot <- data2plot %>%
      select(!c(CAR_q59_G1, CAR_q59_G2, relig, religr)) %>%
      filter(!is.na(govSupp)) %>%
      pivot_longer(!c(govSupp), names_to = "category", values_to = "value") %>%
      group_by(govSupp, category) %>%
      summarise(
        mean_value = mean(value, na.rm = TRUE),
        sd_value = sd(value, na.rm = TRUE),
        n_obs = n()
      ) %>%
      ungroup()
  }
  
  data2plot <- data2plot %>%
    mutate(
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
      value2plot = mean_value*100,
      sd_value = sd_value*100,
      order_value =
        case_when(
          category %in% c("q1a", "q2a") ~ 1,
          category %in% c("q1d", "q2d") ~ 2,
          category %in% c("q1b", "q2b") ~ 3,
          category %in% c("q1c", "q2c") ~ 4,
          category %in% c("q1e", "q2e") ~ 5,
          category %in% c("q1g", "q2g") ~ 1,
          category %in% c("q1f", "q2f") ~ 6
        )
    )
  
  if(group == "income") {
    # Defining color palette for income group
    colors4plot <- c("Low Income" = "#25408b", 
                     "High Income" = "#de3956")
    
  }else if(group == "religion") {
    
    # Defining color palette
    colors4plot <- c("Orthodox Christian" = "#a90099", 
                     "Sunni Muslim"       = "#3273ff")
  } else{
    # Defining color palette
    colors4plot <- c("Non Gov. Supporter" = "#a90099", 
                     "Gov. Supporter"       = "#3273ff")
  }
  
  # Saving data points
  data_PCTFS = data2plot %>% ungroup()
  
  # Plotting each panel of Figure 12
  imap(c("A" = "Corruption", 
         "B" = "Trust"),
       function(varSet, panelName) {
         
         # Filtering data2plot to leave the variable for each panel
         data2plot <- data2plot %>%
           filter(category %in% vars4plot[[varSet]])
         
         if(group == "religion") {
           
           # Applying plotting function
           chart <- NM_dotsChart(data         = data2plot,
                                 target_var   = "value2plot",
                                 sd_var       = "sd_value",
                                 n_obs        = "n_obs", 
                                 alpha        = 0.05,
                                 grouping_var = "religr",
                                 labels_var   = "labels",
                                 colors       = colors4plot,
                                 order_var    = "order_value")
         } else if(group == "income") {
           
           # Applying plotting function
           chart <- NM_dotsChart(data         = data2plot,
                                 target_var   = "value2plot",
                                 sd_var       = "sd_value",
                                 n_obs        = "n_obs", 
                                 alpha        = 0.05,
                                 grouping_var = "incomeGroup",
                                 labels_var   = "labels",
                                 colors       = colors4plot,
                                 order_var    = "order_value")
         }  else {
           
           # Applying plotting function
           chart <- NM_dotsChart(data         = data2plot,
                                 target_var   = "value2plot",
                                 sd_var       = "sd_value",
                                 n_obs        = "n_obs", 
                                 alpha        = 0.05,
                                 grouping_var = "govSupp",
                                 labels_var   = "labels",
                                 colors       = colors4plot,
                                 order_var    = "order_value")
           
         }
         
         # Saving panels
         saveIT.fn(chart  = chart,
                   n      = nchart,
                   suffix = paste0(panelName,"_",group),
                   w      = 189.7883,
                   h      = 54.12481)
         
       })
  
  return(data_PCTFS)
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Perceptions of Corruption and Trust, by Financial Security (PCTFS)                                   ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure_POPCG_bars.fn <- function(nchart = 27, data = master_data.df) {
  
  # Defining variables to use in the plot
  vars4plot <- list("q2c","q2e", "q2f", "q2g","q1c","q1e", "q1f", "q1g")
  
  # Defining data frame for plot
  data2plot <- data %>%
    filter(country == mainCountry & year == latestYear) %>%
    select(fin,
           unlist(vars4plot, 
                  use.names = F)) %>%
    mutate(
      incomeGroup = case_when(
        fin == 1 | fin == 2                  ~ "Financially Insecure",
        fin == 3 | fin == 4 | fin == 5       ~ "Financially Secure",
        TRUE                                 ~ NA_character_
      ),
      across(!c(fin, incomeGroup, all_of(c("q2c", "q2e", "q2f", "q2g"))),
             ~ if_else(.x == 1 | .x == 2, 1,
                       if_else(!is.na(.x) & .x != 99, 0, 
                               NA_real_))),
      across(!c(fin, incomeGroup, all_of(c("q1c","q1e", "q1f", "q1g"))),
             ~if_else(.x == 3 | .x == 4, 1,
                      if_else(!is.na(.x)  & .x != 99, 0, 
                              NA_real_)))
    ) %>%
    select(!fin)
  
  data2plot <- data2plot %>%
    filter(!is.na(incomeGroup)) %>%
    pivot_longer(!c(incomeGroup), names_to = "category", values_to = "value") %>%
    group_by(incomeGroup, category) %>%
    summarise(
      mean_value = mean(value, na.rm = TRUE),
      sd_value = sd(value, na.rm = TRUE),
      n_obs = n()
    ) %>%
    ungroup()
  
  data2plot <- data2plot %>%
    mutate(
      value2plot = mean_value*100,
      sd_value = sd_value*100,
      order_value =
        case_when(
          incomeGroup %in% c("Financially Insecure") ~ 2,
          incomeGroup %in% c("Financially Secure") ~ 1,
        ),
      highlighted = if_else(incomeGroup == "Financially Secure", "Highlighted", "Regular"),
      labels = paste0(round(value2plot, 0), "%"),
      incomeGroup = factor(incomeGroup, levels = c("Financially Secure","Financially Insecure"))
      
    )
  
  # Plotting each panel of Figure 5
  panelVector <- c("A" = vars4plot[1], 
                   "B" = vars4plot[2], 
                   "C" = vars4plot[3], 
                   "D" = vars4plot[4],
                   "E" = vars4plot[5], 
                   "F" = vars4plot[6], 
                   "G" = vars4plot[7], 
                   "H" = vars4plot[8])
  
  # Defining color palette for income group
  
  colors4plot <- c("High Income" = "#25408b", 
                   "Low Income" = "#FA4D57") 
  
  names(colors4plot) <- c("Highlighted", "Regular")
  
  imap(panelVector,
       function(tvar, panelName) {
         
         # Filtering data2plot to leave the variable for each panel
         data2plot <- data2plot %>%
           filter(category %in% tvar)
         
         # Applying plotting function
         chart <- LAC_barsChart(data           = data2plot,
                                target_var     = "value2plot",
                                grouping_var   = "incomeGroup",
                                labels_var     = "labels",
                                colors_var     = "highlighted",
                                colors         = colors4plot,
                                direction      = "horizontal", 
                                order_var      = "order_value")
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
##    Perceptions of the Criminal Justice System by Income (PCJSI)                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure_PCJSI.fn <- function(nchart = 26, data = master_data.df, group = "income") {
  
  # Defining variables to use in the plot
  vars4plot <- list("Set1" = c( "q49a", "q49b_G2", "q49e_G2", "q49c_G2", "q49e_G1"),
                    "Set2" = c( "q49d_G1", "EXP_q23d_G1", "q49c_G1", "q49b_G1"))
  
  # Defining data frame for plot
  data2plot <- data %>%
    filter(country == mainCountry & year == latestYear) %>%
    select(fin,relig, CAR_q59_G1, CAR_q59_G2,
           unlist(vars4plot, 
                  use.names = F)) %>%
    mutate(
      incomeGroup = case_when(
        fin == 1 | fin == 2                  ~ "Low Income",
        fin == 3 | fin == 4 | fin == 5       ~ "High Income",
        TRUE                                 ~ NA_character_
      ),
      religr = case_when(
        relig %in% c("C57 - Orthodox Christian") ~ "Orthodox Christian",
        relig %in% c("G6 - Sunni Muslim")        ~ "Sunni Muslim"
      ),
      govSupp = case_when(
        !is.na(CAR_q59_G1) & !is.na(CAR_q59_G2) ~ NA_character_,
        CAR_q59_G1 == 1   | CAR_q59_G2 == 1     ~ "Gov. Supporter",
        CAR_q59_G1 == 2   | CAR_q59_G2 == 2     ~ "Non Gov. Supporter",
        CAR_q59_G1 == 99  | CAR_q59_G2 == 99    ~ NA_character_,
        is.na(CAR_q59_G1) & is.na(CAR_q59_G2)   ~ NA_character_
      ))%>%
      mutate(
        
        # We need to concatenate variables q49d_G1 and EXP_q23d_G1 into a single one
        q49d_G1_merge = rowSums(across(c(q49d_G1, EXP_q23d_G1)), 
                                na.rm = T),
        q49d_G1_merge = if_else(is.na(q49d_G1) & is.na(EXP_q23d_G1), NA_real_, q49d_G1_merge),
        
        # Transforming everything into binary variables
        across(!c(incomeGroup,religr, govSupp),
               ~if_else(.x == 1 | .x == 2, 1,
                        if_else(!is.na(.x) & .x != 99, 0, NA_real_)))
      )
      
      if(group == "religion") {
        
        data2plot <- data2plot %>%
          select(!c(CAR_q59_G1, CAR_q59_G2, relig, govSupp)) %>%
          filter(!is.na(religr)) %>%
          pivot_longer(!c(religr), names_to = "category", values_to = "value") %>%
          group_by(religr, category) %>%
          summarise(
            mean_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE),
            n_obs = n()
          ) %>%
          ungroup()
        
      } else if(group == "income") {
        data2plot <- data2plot %>%
          select(!c(CAR_q59_G1, CAR_q59_G2,fin,relig, religr,govSupp)) %>%
          filter(!is.na(incomeGroup)) %>%
          pivot_longer(!c(incomeGroup), names_to = "category", values_to = "value") %>%
          group_by(incomeGroup, category) %>%
          summarise(
            mean_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE),
            n_obs = n()
          ) %>%
          ungroup()
      } else if(group == "govSupp"){
        
        data2plot <- data2plot %>%
          select(!c(CAR_q59_G1, CAR_q59_G2, relig, religr)) %>%
          filter(!is.na(govSupp)) %>%
          pivot_longer(!c(govSupp), names_to = "category", values_to = "value") %>%
          group_by(govSupp, category) %>%
          summarise(
            mean_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE),
            n_obs = n()
          ) %>%
          ungroup()
      }
      
      data2plot <- data2plot %>%
        mutate(
          labels = case_when(
            category == 'q49a'          ~ "Is effective in bringing people who \ncommit crimes to justice",
            category == 'q49b_G2'       ~ "Ensures equal treatment of victims by allowing all \nvictims to seek justice regardless of who they are",
            category == 'q49e_G2'       ~ "Safeguards the presumption of innocence by \ntreating those accused of crimes as innocent \nuntil proven guilty",
            category == 'q49c_G2'       ~ "Ensures equal treatment of the accused by giving all \na fair trial regardless of who they are",
            category == 'q49e_G1'       ~ "Gives appropriate punishments that \nfit the crime",
            category == 'q49d_G1_merge' ~ "Ensures uniform quality by providing \nequal service regardless of where they live",
            category == 'q49c_G1'       ~ "Ensures everyone has access to the \njustice system",
            category == 'q49b_G1'       ~ "Ensures timeliness by dealing with \ncases promptly and efficiently"
          ),
          value2plot = mean_value*100,
          sd_value = sd_value*100,
          order_value =
            case_when(
              category     == 'q49a'          ~ 1,
              category     == 'q49b_G2'       ~ 2,
              category     == 'q49e_G2'       ~ 3,
              category     == 'q49c_G2'       ~ 4,
              category     == 'q49e_G1'       ~ 5,
              category     == 'q49d_G1_merge' ~ 6,
              category     == 'q49c_G1'       ~ 7,
              category     == 'q49b_G1'       ~ 8
            )
        )
      
      if(group == "income") {
        # Defining color palette for income group
        colors4plot <- c("Low Income" = "#25408b", 
                         "High Income" = "#de3956")
        
      }else if(group == "religion") {
        
        # Defining color palette
        colors4plot <- c("Orthodox Christian" = "#a90099", 
                         "Sunni Muslim"       = "#3273ff")
      } else{
        # Defining color palette
        colors4plot <- c("Non Gov. Supporter" = "#a90099", 
                         "Gov. Supporter"       = "#3273ff")
      }
      
      # Plotting each panel of Figure 12
      imap(c("A" = "Set1", 
             "B" = "Set2"),
           function(varSet, panelName) {
             
             # Filtering data2plot to leave the variable for each panel
             data2plot <- data2plot %>%
               filter(category %in% vars4plot[[varSet]])
             
             if(group == "religion") {
               
               # Applying plotting function
               chart <- NM_dotsChart(data         = data2plot,
                                     target_var   = "value2plot",
                                     sd_var       = "sd_value",
                                     n_obs        = "n_obs", 
                                     alpha        = 0.05,
                                     grouping_var = "religr",
                                     labels_var   = "labels",
                                     colors       = colors4plot,
                                     order_var    = "order_value")
             } else if(group == "income") {
               
               # Applying plotting function
               chart <- NM_dotsChart(data         = data2plot,
                                     target_var   = "value2plot",
                                     sd_var       = "sd_value",
                                     n_obs        = "n_obs", 
                                     alpha        = 0.05,
                                     grouping_var = "incomeGroup",
                                     labels_var   = "labels",
                                     colors       = colors4plot,
                                     order_var    = "order_value")
             }  else {
               
               # Applying plotting function
               chart <- NM_dotsChart(data         = data2plot,
                                     target_var   = "value2plot",
                                     sd_var       = "sd_value",
                                     n_obs        = "n_obs", 
                                     alpha        = 0.05,
                                     grouping_var = "govSupp",
                                     labels_var   = "labels",
                                     colors       = colors4plot,
                                     order_var    = "order_value")
               
             }
             
             # Saving panels
             saveIT.fn(chart  = chart,
                       n      = nchart,
                       suffix = paste0(panelName,"_",group),
                       w      = 189.7883,
                       h      = 60)
             
           })
}