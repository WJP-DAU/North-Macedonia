security.universe <- function(master_data) {
  
  security.universe <- master_data %>%
    filter(country %in% mainCountry) %>%
    filter(year == latestYear) %>%
    select(country,
           # All variables related with security
           starts_with("EXP_q8"), starts_with("q8"), CAR_q47a_12, CAR_q47b_5,
           # Security perception
           q9, 
           # Sociodemographics 
           COLOR, fin, gend, disability2, disability, Urban, age, edu, ethni,
           # Variables related to institutions performance
           q48b_G1, q48f_G1, q49a, CAR_q58_G1, q48f_G2, q48g_G2, 
           # Trust in institutions
           q1c, q1d, q1e, q1g, q1i, q41d) %>%
    # This variable assigns the victim condition to each observation
    mutate(victim = if_else(EXP_q8a_1 == 1 | EXP_q8a_2 == 1 | EXP_q8a_3 == 1 | EXP_q8a_4 == 1 | EXP_q8a_5 == 1 | EXP_q8a_6 == 1 | EXP_q8a_7 == 1 |
                              EXP_q8a_8 == 1 | EXP_q8a_9 == 1 | EXP_q8a_10 == 1 | EXP_q8a_11 == 1 | EXP_q8a_12 == 1 | EXP_q8a_13 == 1|
                              EXP_q8b_1 == 1 | EXP_q8b_2 == 1 | EXP_q8b_3 == 1 | EXP_q8b_4 == 1 |  CAR_q47a_12 == 1 | CAR_q47b_5 == 1|
                              q8b_1 == 1 | q8b_2 == 1 | q8b_3 == 1 | q8b_4 == 1 | q8b_5 == 1 | q8b_6 == 1 | q8b_7 == 1 | q8b_8 == 1 | q8b_9 == 1 |
                              q8b_10 == 1 | q8b_11 == 1 | q8b_12 == 1 | q8b_13 == 1 | q8b_14 == 1 | q8b_15 == 1 | q8b_16 == 1 | q8b_17 == 1, 1, 0, 0))
  
  return(security.universe)
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Types of Crimes Experienced by People                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Upper Panel

figure_TCEP.fn <- function(nchart = 12, data = master_data.df) 
{
  
  security_universe <- security.universe(master_data = data) # This function assign the victim condition and select the main variables to security secction
  
  # Panel A
  
  data2plot <- security_universe %>%
    #filter(victim == 1) %>%
    mutate(prop_crimes = if_else(EXP_q8a_1 == 1 | EXP_q8a_2 == 1 | EXP_q8a_3 == 1|  EXP_q8a_4 == 1 |
                                   EXP_q8a_5 == 1|  EXP_q8a_6 == 1 | EXP_q8a_8 == 1|
                                   q8b_1 == 1 | q8b_2 == 1 | q8b_3 == 1 | q8b_4 == 1 | q8b_5 == 1 |
                                   q8b_6 == 1 | q8b_7 == 1 | q8b_8 == 1 | q8b_10  == 1, 1, 0, 0),
           life_crimes = if_else(EXP_q8a_7 == 1 | EXP_q8a_12 == 1 |EXP_q8b_1 == 1 | EXP_q8b_2 == 1 | 
                                   EXP_q8b_3 == 1 |
                                   q8b_9 == 1 | q8b_14 == 1 | q8b_15 == 1 | q8b_16 == 1 | q8b_17 == 1, 1, 0, 0),
           corr_crimes = if_else(EXP_q8a_9 == 1|  EXP_q8a_10 == 1| EXP_q8a_11 == 1 | q8b_11 == 1 |
                                   q8b_12 == 1 | q8b_13 == 1, 1, 0, 0)) %>%
    summarise(prop_crimes = round(mean(prop_crimes, na.rm=T), 2),
              life_crimes = round(mean(life_crimes, na.rm=T), 2),
              corr_crimes = round(mean(corr_crimes, na.rm=T), 2))  %>%
    pivot_longer(cols=c(prop_crimes,life_crimes,corr_crimes), names_to = "category", values_to = "value2plot") %>%
    mutate(category = case_when(category == "prop_crimes" ~ "Property crimes",
                                category == "life_crimes" ~ "Crimes against life and integrity \nof individuals",
                                category == "corr_crimes" ~ "Corruption, financial, \nand commercial crimes"),
           order_value = case_when(category ==  "Property crimes" ~ 3,
                                   category == "Crimes against life and integrity \nof individuals" ~ 2,
                                   category == "Corruption, financial, \nand commercial crimes" ~ 1))
  
  
  crimes <- lollipop_chart(data2plot = data2plot, 
                           categories = category,
                           order_value = order_value)
  
  saveIT.fn(chart  = crimes,
            n      = nchart,
            suffix = "A",
            w      = 175.027,
            h      = 46.74415)
}


# Lower Panel



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Perceptions of Security Over Time                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Upper Panel

figure_PSOT1.fn <- function(nchart = 13, data = master_data.df) {
  
  # Defining data frame for plot
  data2plot <- data %>%
    filter(country == mainCountry) %>% 
    select(year, q9) %>%
    mutate(
      q9   = case_when(
        q9 == 1 | q9 == 2    ~ 1,
        q9 == 3 | q9 == 4    ~ 0,
        q9 == 99 | is.na(q9) ~ NA_real_,
      )
    ) %>%
    group_by(year) %>%
    summarise(across(everything(),
                     mean,
                     na.rm = T)) %>%
    mutate(value2plot = q9*100,
           label      = to_percentage.fn(value2plot),
           category   = mainCountry) %>%
    filter(year >= 2014)
  
  # Pulling minimum and maximum available year
  minyear <- min(data2plot %>% pull(year))
  if (minyear %% 2 != 0) {
    minyear <- minyear - 1
  }
  maxyear <- max(data2plot %>% pull(year))
  if (maxyear %% 2 != 0) {
    maxyear <- maxyear + 1
  }
  
  if (mainCountry == "Haiti") {
    x.axis.values <- c(2021, 2022)
    x.axis.labels <- c("'21", "'22")
  }
  
  # Creating a vector for yearly axis
  x.axis.values <- seq(minyear, maxyear, by = 2)
  sec.ticks     <- seq(minyear, maxyear, by = 1)
  x.axis.labels <- paste0("'", str_sub(x.axis.values, start = -2))
  
  
  # Defining colors4plot
  colors4plot <- mainCOLOR
  names(colors4plot) <- mainCountry
  
  # Applying plotting function
  chart <- LAC_lineChart(data           = data2plot,
                         target_var     = "value2plot",
                         grouping_var   = "year",
                         ngroups        = 1, 
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
            suffix = "A",
            w      = 189.7883,
            h      = 68.88612)
} 

# Lower Panel
figure_PSOT2.fn <- function(nchart = 13, data = master_data.df) {
  
  security_universe <- security.universe(master_data = data) # This function assign the victim condition and select the main variables to security secction
  
  if (mainCountry == "Bahamas"  | mainCountry == "Peru"    | 
      mainCountry == "Barbados" | mainCountry == "Dominica"|
      mainCountry == "St. Lucia"| mainCountry == "St. Vincent and the Grenadines"|
      mainCountry == "Grenada") {
    
    perception <- security_universe %>%
      mutate(unsafe_bin    =  if_else(q9 == 1 | q9 == 2, 1, 
                                      if_else(q9 == 3 | q9 ==4, 0, NA_real_)),
             victim        =  if_else(victim == 1, "Victim", "Non Victim"),
             white         =  if_else(COLOR == 1 | COLOR == 2 | COLOR == 3 | COLOR == 4, "White" , 
                                      if_else(COLOR == 5 | COLOR == 6 | COLOR == 7 | COLOR == 8 | COLOR == 9 | COLOR == 10, "No White", NA_character_)),
             young         =  if_else(age < 3, "Less than 35 years", 
                                      if_else(age > 2, "More than 35 years", NA_character_)),
             poor          =  if_else(fin == 1 | fin == 2, "Poor",
                                      if_else(fin == 3 | fin == 4 | fin == 5, "No Poor", NA_character_)),
             area          =  if_else(Urban == 1, "Urban", "Rural"),
             gender        =  if_else(gend == 1, "Male", "Female"),
             diploma       =  if_else(edu == 4 | edu == 5 | edu == 6| edu == 7, "High Education Level", 
                                      if_else(edu < 4, "No High Education Level", NA_character_))) # We transform the variable of security perception in a dummy variable, the values 3 and 4 reference to unsafe people feeling
  } else if (mainCountry == "United States") {
    
    perception <- security_universe %>%
      mutate(unsafe_bin    =  if_else(q9 == 1 | q9 == 2, 1, 
                                      if_else(q9 == 3 | q9 ==4, 0, NA_real_)),
             victim        =  if_else(victim == 1, "Victim", "Non Victim"), 
             young         =  if_else(age < 30, "Less than 30 years", 
                                      if_else(age > 29, "More than 30 years", NA_character_)),
             poor          =  if_else(fin == 1 | fin == 2, "Poor",
                                      if_else(fin == 3 | fin == 4 | fin == 5, "No Poor", NA_character_)),
             area          =  if_else(Urban == 1, "Urban", "Rural"),
             gender        =  if_else(gend == 1, "Male", "Female"),
             diploma       =  if_else(edu == 5 | edu == 6| edu == 7, "High Education Level", 
                                      if_else(edu < 5, "No High Education Level", NA_character_)),
             white         =  if_else(ethni == "White", "White", "No white", NA_character_)
      ) # We transform the variable of security perception in a dummy variable, the values 3 and 4 reference to unsafe people feeling
    
  } else {
    
    perception <- security_universe %>%
      mutate(unsafe_bin    =  if_else(q9 == 1 | q9 == 2, 1, 
                                      if_else(q9 == 3 | q9 ==4, 0, NA_real_)),
             victim        =  if_else(victim == 1, "Victim", "Non Victim", NA_character_),
             white         =  if_else(COLOR == 1 | COLOR == 2 | COLOR == 3 | COLOR == 4, "White" , 
                                      if_else(COLOR == 5 | COLOR == 6 | COLOR == 7 | COLOR == 8 | COLOR == 9 | COLOR == 10 | COLOR == 11, "No White", NA_character_)),
             young         =  if_else(age < 30, "Less than 30 years", 
                                      if_else(age > 29, "More than 30 years", NA_character_)),
             poor          =  if_else(fin == 1 | fin == 2, "Poor",
                                      if_else(fin == 3 | fin == 4 | fin == 5, "No Poor", NA_character_)),
             area          =  if_else(Urban == 1, "Urban", "Rural", NA_character_),
             gender        =  if_else(gend == 1, "Male", "Female", NA_character_),
             diploma       =  if_else(edu == 4 | edu == 5 | edu == 6| edu == 7, "High Education Level", 
                                      if_else(edu < 4, "No High Education Level", NA_character_))) # We transform the variable of security perception in a dummy variable, the values 3 and 4 reference to unsafe people feeling
    
  } 
  condition <- perception %>%
    select(victim, white, young, poor, area, gender, diploma) %>%
    mutate(counter = 1)
  
  victim  <- condition_categories(main_data = condition, group_var = victim, name_var = "victim")
  color   <- condition_categories(main_data = condition, group_var = white, name_var = "white")
  age     <- condition_categories(main_data = condition, group_var = young, name_var = "young")
  income  <- condition_categories(main_data = condition, group_var = poor, name_var = "poor")
  area    <- condition_categories(main_data = condition, group_var = area, name_var = "area")
  gender  <- condition_categories(main_data = condition, group_var = gender, name_var = "gender")
  diploma <- condition_categories(main_data = condition, group_var = diploma, name_var = "diploma")
  
  selectables <- rbind(victim, color, age, income, area, gender, diploma) %>%
    group_by(variable) %>%
    summarise(min_group = min(N_obs, na.rm = T),
              total_group = sum(N_obs, na.rm = T)) %>%
    filter(min_group > 30) %>%
    filter(min_group != total_group) %>%
    pull(variable)
  
  logit_demo <- function(mainData, Yvar) {
    
    if (mainCountry == "Bahamas"  | mainCountry == "Peru"    | 
        mainCountry == "Barbados" | mainCountry == "Dominica"|
        mainCountry == "St. Lucia"| mainCountry == "St. Vincent and the Grenadines"|
        mainCountry == "Grenada") {
      
      logit_data <- perception %>%
        select(unsafe_bin, all_of(selectables)) %>%
        rowid_to_column("id") %>%
        pivot_longer(cols = !c(unsafe_bin, id), names_to = "categories", values_to = "values") %>%
        mutate(values = if_else(categories %in% "young" & values %in% "More than 35 years", "1More than 35 years", values),
               values = if_else(categories %in% "gender" & values %in% "Male", "1Male", values)) %>%
        pivot_wider(id_cols = c(unsafe_bin, id), names_from = categories, values_from = values)
      
    } else if(mainCountry == "United States") {
      
      logit_data <- perception %>%
        select(unsafe_bin, all_of(selectables)) %>%
        rowid_to_column("id") %>%
        pivot_longer(cols = !c(Yvar, id), names_to = "categories", values_to = "values") %>%
        mutate(values = if_else(categories %in% "young" & values %in% "More than 30 years", "1More than 30 years", values),
               values = if_else(categories %in% "gender" & values %in% "Male", "1Male", values),
               values = if_else(categories %in% "white" & values %in% "White", "1White", values)) %>%
        pivot_wider(id_cols = c(Yvar, id), names_from = categories, values_from = values)
      
    } else {
      
      logit_data <- perception %>%
        select(unsafe_bin, all_of(selectables)) %>%
        rowid_to_column("id") %>%
        pivot_longer(cols = !c(unsafe_bin, id), names_to = "categories", values_to = "values") %>%
        mutate(values = if_else(categories %in% "young" & values %in% "More than 30 years", "1More than 30 years", values),
               values = if_else(categories %in% "gender" & values %in% "Male", "1Male", values)) %>%
        pivot_wider(id_cols = c(unsafe_bin, id), names_from = categories, values_from = values)
      
    }
    
    logit_data<- logit_data %>%
      select(all_of(selectables),
             unsafe_bin) # We didn't include the non answer
    
    formula <- selectables %>%
      t() %>%
      as.data.frame() %>%
      unite(., formula, sep = "+") %>%
      as.character()
    
    models <- lapply(list("unsafe_bin"), 
                     function(depVar) {
                       formula  <- as.formula(paste(depVar, "~", formula))
                       logit  <- glm(formula,  
                                     data   = logit_data, 
                                     family = "binomial")})
    
    summaryreg <- bind_rows(as.data.frame(coef(summary(models[[1]]))))
    
    # write.xlsx(as.data.frame(summaryreg %>% ungroup()), 
    #            file      = file.path("Outputs", 
    #                                  str_replace_all(mainCountry, " ", "_"),
    #                                  "dataPoints.xlsx",
    #                                  fsep = "/"), 
    #            sheetName = paste0("Chart_", nchart, "B", "reg"),
    #            append    = T,
    #            row.names = T)
    
    margEff    <- margins_summary(models[[1]], data = models[[1]]$model)
    
    data2plot <- margEff
    if (mainCountry == "Bahamas"  | mainCountry == "Peru"    | 
        mainCountry == "Barbados" | mainCountry == "Dominica"|
        mainCountry == "St. Lucia"| mainCountry == "St. Vincent and the Grenadines"|
        mainCountry == "Grenada") {
      
      data2plot$factor <- recode(data2plot$factor, "genderFemale" = "Female", "poorPoor" = "Financially \ninsecure", "victimVictim" = "Previous crime \nvictimization",
                                 "areaUrban" = "Urban", "whiteWhite" = "Light skin \ntone", "youngLess than 35 years" = "Younger than 35",
                                 "diplomaNo High Education Level" = "No high school \ndiploma")
    } else if(mainCountry == "United States") {
      
      data2plot$factor <- recode(data2plot$factor, "genderFemale" = "Female", "poorPoor" = "Financially \ninsecure", "victimVictim" = "Previous crime \nvictimization",
                                 "areaUrban" = "Urban", "youngLess than 30 years" = "Younger than 30",
                                 "diplomaNo High Education Level" = "No Bachelor's \ndegree", "whiteNo white" = "Non-white")
    }
    else {
      
      data2plot$factor <- recode(data2plot$factor, "genderFemale" = "Female", "poorPoor" = "Financially \ninsecure", "victimVictim" = "Previous crime \nvictimization",
                                 "areaUrban" = "Urban", "whiteWhite" = "Light skin \ntone", "youngLess than 30 years" = "Younger than 30",
                                 "diplomaNo High Education Level" = "No high school \ndiploma") 
    }
    
    data2plot <- data2plot %>%
      mutate(category = mainCountry,
             order_variable = if_else(factor %in% "Female", 1,
                                      if_else(factor %in% "White", 2,
                                              if_else(factor %in% "Poor", 3,
                                                      if_else(factor %in% "Victim", 4,
                                                              if_else(factor %in% "Urban", 5, 
                                                                      if_else(factor %in% "Young", 6, 7)))))))
  }
  
  data2plot <- logit_demo(mainData = perception, Yvar = 'unsafe_bin')
  
  
  logit_plot <- logit_demo_panel(mainData = data2plot, line_size = 1.5)
  
  saveIT.fn(chart  = logit_plot,
            n      = nchart,
            suffix = "B",
            w      = 175.027,
            h      = 81.89012)
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Perceptions of the Criminal Justice System                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Criminal Justice Actors                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure_TCJA.fn <- function(nchart = 15, data = master_data.df) {
  
  # Variables to plot
  vars4plot = list("Trust"         = c("q1e", "q1f", "q1g"), 
                   "Corruption"    = c("q2e", "q2f", "q2g"), 
                   "Effectiveness" = c("q48f_G2", "q48h_G1", "q48g_G2"))
  
  # Defining data frame for plot
  data2plot <- data %>%
    filter(country == mainCountry) %>%
    select(year, all_of(unlist(vars4plot))) %>%
    mutate(
      across(starts_with(c("Trust", "Effect")),
             ~if_else(.x == 1 | .x == 2, 1,
                      if_else(!is.na(.x) & .x != 99, 0, 
                              NA_real_))),
      across(starts_with("Corruption"),
             ~if_else(.x == 3 | .x == 4, 1,
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
  minyear <- min(data2plot %>% pull(year))
  if (minyear %% 2 != 0) {
    minyear <- minyear - 1
  }
  maxyear <- max(data2plot %>% pull(year))
  if (maxyear %% 2 != 0) {
    maxyear <- maxyear + 1
  }
  
  if (mainCountry == "Haiti") {
    x.axis.values <- c(2021, 2022)
    x.axis.labels <- c("'21", "'22")
  }
  
  # Creating a vector for yearly axis
  x.axis.values <- seq(minyear, maxyear, by = 2)
  sec.ticks     <- seq(minyear, maxyear, by = 1)
  x.axis.labels <- paste0("'", str_sub(x.axis.values, start = -2))
  
  
  # Plotting each panel of Figure 16
  imap(c("A" = "Trust", 
         "B" = "Corruption", 
         "C" = "Effectiveness"),
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
                                repel          = T,
                                custom.axis    = T,
                                x.breaks       = x.axis.values,
                                x.labels       = x.axis.labels,
                                sec.ticks      = sec.ticks
         )
         
         # Saving panels
         saveIT.fn(chart  = chart,
                   n      = nchart,
                   suffix = panelName,
                   w      = 189.7883,
                   h      = 49.90729)
         
       })
} 












# Experimental figure 16
treemap_function <- function(data2plot, margin_top) {
  a <- ggplot(data = data2plot, aes(area = value, fill = group, label = label)) +
    geom_treemap() +
    geom_treemap_text() +
    scale_fill_manual(values = c("q48c_G2" = "#2a2a94", "EXP_q22i_G2" = "#4a4a49", "EXP_q22h_G2" = "#f3f3f3")) +
    coord_flip(clip = "off") +
    WJP_theme() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          plot.margin = margin(margin_top, 10, -15, 0),
          plot.background = element_blank())
  
  return(a)
}


figure_POP.fn <- function(nchart = 35, data = master_data.df) {
  
  # Panel A: Serve the Public
  panelA <- data %>%
    filter(year == latestYear & country == mainCountry) %>%
    select(q48c_G2, EXP_q22i_G2, EXP_q22h_G2) %>%
    summarise(across(everything(), mean, na.rm = TRUE)) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
    mutate(
      empty_value = 1 - value,
      group = variable,
      label = paste0(format(round(value * 100, 0), nsmall = 0), "%"),
      label = ifelse(value == 0, NA_character_, label)
    )

  
  panelA <- panelA %>%
    group_by(group) %>%
    mutate(value = value / sum(value))
  
  treemap_A <- treemap_function(data2plot = panelA, margin_top = 0)
  
  saveIT.fn(chart = treemap_A,
            n = nchart,
            suffix = "a",
            w = 82.59305,
            h = 45.33831)
  
  #Other panels here
  

}
