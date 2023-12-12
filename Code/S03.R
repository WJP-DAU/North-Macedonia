## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:           North Macedonia Report - Section I Functions
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
##    Types of Crimes Experienced by People (TCEP)                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Upper Panel

figure_TCEP1.fn <- function(nchart = 13, data = master_data.df) 
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
figure_TCEP2.fn <- function(nchart = 13, data = master_data.df) {
  
  security_universe <- security.universe(master_data = data)
  
  victims <- security_universe %>%
    summarise(victim = round(mean(victim, na.rm = T),2)) %>%
    mutate(non_victim = 1 - victim)
  
  report <- security_universe %>%
    mutate(EXP_q8d = case_when(
      EXP_q8d == 1 ~ 1,
      EXP_q8d == 0 ~ 0,
      EXP_q8d == 99 ~ NA_real_
    )) %>%
    mutate(EXP_q8d = if_else(EXP_q8d == 1, 1, 0)) %>%
    filter(victim == 1) %>%
    summarise(report = round(mean(EXP_q8d, na.rm = T),2)) %>%
    mutate(non_report = 1 - report)
  
  fill_report <- security_universe %>%
    filter(victim == 1) %>%
    select(EXP_q8d, EXP_q8f) %>%
    mutate(across(everything(), 
                  ~if_else(.x == 1, 1,
                           if_else(!is.na(.x) & .x != 99, 0, 
                                   NA_real_)))) %>%
    group_by(EXP_q8d) %>%
    summarise(fill_report = round(mean(EXP_q8f, na.rm = T),2)) %>%
    mutate(non_fill_report = 1 - fill_report) %>%
    filter(EXP_q8d == 1) %>%
    select(!EXP_q8d)
  
  t1 <- sample(x = c("Victim", "Non-Victim"), size = 1000, replace = TRUE, prob = c(1,0))
  t2 <- sample(x = c("Non-Report", "Report"), size = 1000, replace = TRUE, prob = c(report$non_report, report$report))
  t3 <- sample(x = c("Non-Official", "Official"), size = 1000, replace = TRUE, prob = c(fill_report$non_fill_report,fill_report$fill_report))
  
  d <- data.frame(cbind(t1, t2))
  names(d) <- c("Victim", "Report")
  
  df <- d %>%
    mutate(`Official Crime Report` = if_else(Report %in% "Report", t3, " ")) 
  
  data2plot <- df %>%
    make_long(Victim, Report, `Official Crime Report`)
  
  y <- c(1, 900, -300, 600, 75)
  x <- c(0.7, 2, 2.3, 3.3, 3.3)
  
  label <- c(paste0("<span style='color:#003b8a;font-size:4.217518mm'>", '**',victims$victim*100, "%",'**',"</span> <br> <span style='color:#222221;font-size:3.514598mm'> of ", "North Macedonian" ,"s","<br>were victims <br>of a crime in <br> the last 12 <br> months"),
             paste0("<span style='color:#003b8a;font-size:4.217518mm'>", '**',report$report*100, "%",'**',"</span> <br> <span style='color:#222221;font-size:3.514598mm'> reported <br> the crime"),
             paste0("<span style='color:#fa4d57;font-size:4.217518mm'>", '**',report$non_report*100, "%",'**',"</span> <br> <span style='color:#222221;font-size:3.514598mm'> did not report <br>the crime"),
             paste0("<span style='color:#003b8a;font-size:4.217518mm'>", '**',fill_report$fill_report*100, "%",'**',"</span> <br> <span style='color:#222221;font-size:3.514598mm'> filed an official <br> crime report"),
             paste0("<span style='color:#fa4d57;font-size:4.217518mm'> ", '**',fill_report$non_fill_report*100, "%",'**',"</span> <br> <span style='color:#222221;font-size:3.514598mm'> did not file an <br>official crime report"))
  
  label.df <- data.frame(label)
  
  pl <- ggplot(data = data2plot, aes(x = x, 
                                     next_x = next_x,
                                     node = node,
                                     next_node = next_node,
                                     fill = factor(node))) +
    geom_sankey(flow.alpha = 0.5,
                node.color = "white",
                show.legend = FALSE) +
    geom_richtext(data = label.df, aes(x = x, label = label, y = y, 
                                       next_x = NULL, node = NULL, 
                                       next_node = NULL, fill = NULL, family = "Lato Medium"), 
                  fill = NA, label.color = NA, hjust = 0.5, vjust = 0.5) +
    scale_y_continuous(expand = expansion(mult = c(0,0.2))) +
    scale_x_discrete(position = "top") +
    scale_fill_manual(values = c("Victim" = "#003b8a",
                                 'Non-Victim' = "#003b8a",
                                 "Report" = "#003b8a",
                                 "Non-Report" = "white",
                                 "Official" = "#003b8a",
                                 "Non-Official" = "#fa4d57",
                                 ' ' = "white")) +
    theme_sankey(base_size = 10, base_rect_size = 10) +
    theme(legend.position = "none",
          panel.background   = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(family ="Lato Full", 
                                     size = 3.514598*.pt,
                                     color = "Black"),
          axis.text.y = element_blank(),
          axis.ticks.x = element_blank());pl
  
  saveIT.fn(chart  = pl,
            n      = nchart,
            suffix = "B",
            w      = 175.027,
            h      = 94.54267)
  
  # Reasons table 
  
  reasons <- security_universe %>%
    filter(victim == 1) %>%
    filter(EXP_q8d == 0) %>%
    filter(EXP_q8h != 99) %>%
    mutate(counter = 1,
           total_people = n(),
           category   = case_when(
             EXP_q8h  == 3  ~ "Respondent was afraid or embarrased",
             EXP_q8h  == 4  ~ "Respondent was afraid or embarrased",
             EXP_q8h  == 9  ~ "Respondent was afraid or embarrased",
             EXP_q8h  == 11 ~ "Respondent was afraid or embarrased",
             EXP_q8h  == 2  ~ "Respondent did not think reporting would help",
             EXP_q8h  == 5  ~ "Respondent did not think reporting would help",
             EXP_q8h  == 6  ~ "Respondent did not think reporting would help",
             EXP_q8h  == 7  ~ "Respondent did not think reporting would help",
             EXP_q8h  == 10 ~ "Respondent did not trust the police",
             EXP_q8h  == 1  ~ "Respondent had administrative issues",
             EXP_q8h  == 8  ~ "Respondent had administrative issues",
             EXP_q8h  == 12 ~ "Other",
           )) %>%
    group_by(category) %>%
    summarise(reasons = sum(counter, na.rm = T), universe = mean(total_people, na.rm = T))
  
  reasons2table <- reasons %>%
    arrange(-reasons) %>%
    mutate(proportion = paste0(round(reasons/universe,2)*100, "%"))
  
  # Saving data points
  write.xlsx(as.data.frame(reasons2table %>% ungroup()), 
             file      = file.path("Outputs", 
                                   paste0("imgChart", nchart),
                                   "reasons.xlsx",
                                   fsep = "/"),
             row.names = T)
  
}



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Perceptions of Security Over Time (PSOT)                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Upper Panel

figure_PSOT1.fn <- function(nchart = 14, data = master_data.df) {
  
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
figure_PSOT2.fn <- function(nchart = 14, data = master_data.df) {
  
  security_universe <- security.universe(master_data = data) # This function assign the victim condition and select the main variables to security secction
    
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
      
    logit_data <- perception %>%
        select(unsafe_bin, all_of(selectables)) %>%
        rowid_to_column("id") %>%
        pivot_longer(cols = !c(unsafe_bin, id), names_to = "categories", values_to = "values") %>%
        mutate(values = if_else(categories %in% "young" & values %in% "More than 30 years", "1More than 30 years", values),
               values = if_else(categories %in% "gender" & values %in% "Male", "1Male", values)) %>%
        pivot_wider(id_cols = c(unsafe_bin, id), names_from = categories, values_from = values)
      
    
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
    
    margEff    <- margins_summary(models[[1]], data = models[[1]]$model)
    
    data2plot <- margEff
    data2plot$factor <- recode(data2plot$factor, "genderFemale" = "Female", "poorPoor" = "Financially \ninsecure", "victimVictim" = "Previous crime \nvictimization",
                                 "areaUrban" = "Urban", "whiteWhite" = "Light skin \ntone", "youngLess than 30 years" = "Younger than 30",
                                 "diplomaNo High Education Level" = "No high school \ndiploma") 
    
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
##    Perceptions of the Criminal Justice System (PCJS)                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Upper Panel

figurePCJS_1.fn <- function(nchart = 15, data = master_data.df) {
  
  # Defining which years to show in the plot: Two latest years for each country
  yrs <- data %>%
    filter(country == mainCountry) %>%
    group_by(year) %>%
    summarise() %>%
    slice_max(order_by = year,
              n = 2) %>%
    pull(year)
  
  # Defining data frame for plot
  data2plot <- data %>%
    filter(country == mainCountry & year %in% yrs) %>%
    select(year, latestYear, q49a, q49b_G2, q49e_G2, q49c_G2, q49e_G1, q49d_G1, EXP_q23d_G1, q49c_G1, q49b_G1) %>%
    mutate(
      
      # We need to concatenate variables q49d_G1 and EXP_q23d_G1 into a single one
      q49d_G1_merge = rowSums(across(c(q49d_G1, EXP_q23d_G1)), 
                              na.rm = T),
      q49d_G1_merge = if_else(is.na(q49d_G1) & is.na(EXP_q23d_G1), NA_real_, q49d_G1_merge),
      
      # Transforming everything into binary variables
      across(!c(year, latestYear),
             ~if_else(.x == 1 | .x == 2, 1,
                      if_else(!is.na(.x) & .x != 99, 0, NA_real_)))
    ) %>%
    select(year, latestYear, q49a, q49b_G2, q49e_G2, q49c_G2, q49e_G1, q49d_G1_merge, q49c_G1, q49b_G1) %>%
    group_by(year) %>%
    summarise(latestYear = first(latestYear),
              across(everything(),
                     mean,
                     na.rm = T)) %>%
    pivot_longer(!c(year, latestYear),
                 names_to  = "category",
                 values_to = "value4radar") %>%
    mutate(
      order_value = case_when(
        category     == 'q49a'          ~ 1,
        category     == 'q49b_G2'       ~ 2,
        category     == 'q49e_G2'       ~ 3,
        category     == 'q49c_G2'       ~ 4,
        category     == 'q49e_G1'       ~ 5,
        category     == 'q49d_G1_merge' ~ 6,
        category     == 'q49c_G1'       ~ 7,
        category     == 'q49b_G1'       ~ 8
      ),
      valuelabel = to_percentage.fn(value4radar*100),
      label = case_when(
        category == 'q49a'          ~ paste("Is **effective** in bringing<br>people who commit<br>crimes to justice"),
        category == 'q49b_G2'       ~ paste("Ensures **equal treatment<br>of victims** by allowing all<br>",
                                            "victims to seek justice<br>regardless of who they are"),
        category == 'q49e_G2'       ~ paste("Safeguards the<br>**presumption of<br>innocence** by treating<br>those",
                                            "accused of<br>crimes as innocent<br>until proven guilty"),
        category == 'q49c_G2'       ~ paste("Ensures **equal treatment of<br>the accused** by giving all a<br>",
                                            "fair trial regardless of who<br>they are"),
        category == 'q49e_G1'       ~ paste("Gives **appropriate<br>punishments** that fit<br>the crime"),
        category == 'q49d_G1_merge' ~ paste("Ensures **uniform quality** by<br>providing equal service<br>",
                                            "regardless of where<br>they live",
                                            "</span>"),
        category == 'q49c_G1'       ~ paste("Ensures everyone<br>has **access** to the<br>justice system"),
        category == 'q49b_G1'       ~ paste("Ensures **timeliness**<br>by dealing with<br>cases promptly",
                                            "and<br>efficiently")
      ),
      across(label,
             ~paste0("<span style='color:", "#003b8a", ";font-size:6.326276mm;font-weight:bold'>",  
                     valuelabel,
                     "</span>",
                     "<br>",
                     "<span style='color:#524F4C;font-size:3.514598mm;font-weight:bold'>",
                     label,
                     "</span>")),
      label = if_else(year != latestYear, 
                      NA_character_, 
                      label)
    )
  
  
  # Defining color palette
  colors4plot <- binPalette
  names(colors4plot) <- yrs
  
  chart <- LAC_radarChart(data          = data2plot,
                          axis_var      = "category",         
                          target_var    = "value4radar",     
                          label_var     = "label", 
                          order_var     = "order_value",
                          colors        = colors4plot)
  
  # Saving panels
  saveIT.fn(chart  = chart,
            n      = nchart,
            suffix = "A",
            w      = 189.7883,
            h      = 183.1106)
}

# Lower Panel

figurePCJS_2.fn <- function(nchart = 15, data = master_data.df) {
  
  # Defining data frame for plot
  data2plot <- data %>%
    filter(country == mainCountry) %>%
    filter(year == latestYear) %>%
    select(relig, q49a, q49b_G2, q49e_G2, q49c_G2, q49e_G1, q49d_G1, EXP_q23d_G1, q49c_G1, q49b_G1) %>%
    mutate(religr = case_when(
      relig %in% c("C57 - Orthodox Christian") ~ "Orthodox Christian",
      relig %in% c("G6 - Sunni Muslim")        ~ "Sunni Muslim"
    )) %>%
    mutate(
      
      # We need to concatenate variables q49d_G1 and EXP_q23d_G1 into a single one
      q49d_G1_merge = rowSums(across(c(q49d_G1, EXP_q23d_G1)), 
                              na.rm = T),
      q49d_G1_merge = if_else(is.na(q49d_G1) & is.na(EXP_q23d_G1), NA_real_, q49d_G1_merge),
      
      # Transforming everything into binary variables
      across(!c(religr),
             ~if_else(.x == 1 | .x == 2, 1,
                      if_else(!is.na(.x) & .x != 99, 0, NA_real_)))
    ) %>%
    select(religr, q49a, q49b_G2, q49e_G2, q49c_G2, q49e_G1, q49d_G1_merge, q49c_G1, q49b_G1) %>%
    group_by(religr) %>%
    summarise(across(everything(),
                     mean,
                     na.rm = T)) %>%
    pivot_longer(!c(religr),
                 names_to  = "category",
                 values_to = "value4radar") %>%
    mutate(
      order_value = case_when(
        category     == 'q49a'          ~ 1,
        category     == 'q49b_G2'       ~ 2,
        category     == 'q49e_G2'       ~ 3,
        category     == 'q49c_G2'       ~ 4,
        category     == 'q49e_G1'       ~ 5,
        category     == 'q49d_G1_merge' ~ 6,
        category     == 'q49c_G1'       ~ 7,
        category     == 'q49b_G1'       ~ 8
      ),
      valuelabel = to_percentage.fn(value4radar*100),
      label = case_when(
        category == 'q49a'          ~ paste("Is **effective** in bringing<br>people who commit<br>crimes to justice"),
        category == 'q49b_G2'       ~ paste("Ensures **equal treatment<br>of victims** by allowing all<br>",
                                            "victims to seek justice<br>regardless of who they are"),
        category == 'q49e_G2'       ~ paste("Safeguards the<br>**presumption of<br>innocence** by treating<br>those",
                                            "accused of<br>crimes as innocent<br>until proven guilty"),
        category == 'q49c_G2'       ~ paste("Ensures **equal treatment of<br>the accused** by giving all a<br>",
                                            "fair trial regardless of who<br>they are"),
        category == 'q49e_G1'       ~ paste("Gives **appropriate<br>punishments** that fit<br>the crime"),
        category == 'q49d_G1_merge' ~ paste("Ensures **uniform quality** by<br>providing equal service<br>",
                                            "regardless of where<br>they live",
                                            "</span>"),
        category == 'q49c_G1'       ~ paste("Ensures everyone<br>has **access** to the<br>justice system"),
        category == 'q49b_G1'       ~ paste("Ensures **timeliness**<br>by dealing with<br>cases promptly",
                                            "and<br>efficiently")
      ),
      across(label,
             ~paste0("<br> <br>",
                     "<span style='color:#524F4C;font-size:3.514598mm;font-weight:bold'>",
                     label,
                     "</span>"))
    ) %>%
    drop_na() %>%
    rename(year = religr) 
  
  
  # Defining color palette
  colors4plot <- c("Orthodox Christian" = "#a90099", 
                   "Sunni Muslim"       = "#3273ff")
  
  chart <- LAC_radarChart(data          = data2plot,
                          axis_var      = "category",         
                          target_var    = "value4radar",     
                          label_var     = "label", 
                          order_var     = "order_value",
                          colors        = colors4plot,
                          latestYear    = "Orthodox Christian")
  
  # Saving panels
  saveIT.fn(chart  = chart,
            n      = nchart,
            suffix = "B",
            w      = 189.7883,
            h      = 183.1106)
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Criminal Justice Actors (TCJA)                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure_TCJA.fn <- function(nchart = 16, data = master_data.df) {
  
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

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Perceptions of the Police (POP)                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


figure_POP.fn <- function(nchart = 17, data = master_data.df) {
  
  # Panel A: Serve the Public
  panelA <- data %>%
    filter(year == latestYear & country == mainCountry) %>%
    select(q48c_G2, EXP_q22i_G2 , EXP_q22h_G2) %>%
    mutate(
      q48c_G2 = case_when(
        q48c_G2 == 1  ~ 1,
        q48c_G2 == 2  ~ 1,
        q48c_G2 == 3  ~ 0,
        q48c_G2 == 4  ~ 0
      ),
      EXP_q22i_G2 = case_when(
        EXP_q22i_G2 == 1  ~ 1,
        EXP_q22i_G2 == 2  ~ 1,
        EXP_q22i_G2 == 3  ~ 0,
        EXP_q22i_G2 == 4  ~ 0
      ),
      EXP_q22h_G2 = case_when(
        EXP_q22h_G2 == 1  ~ 1,
        EXP_q22h_G2 == 2  ~ 1,
        EXP_q22h_G2 == 3  ~ 0,
        EXP_q22h_G2 == 4  ~ 0
      ),
    ) %>%
    summarise(across(everything(),
                     mean,
                     na.rm = T)) %>%
    pivot_longer(everything(),
                 names_to  = "variable",
                 values_to = "value") %>%
    mutate(
      empty_value = 1 - value) %>%
    pivot_longer(!variable,
                 names_to = "group",
                 values_to = "value") %>%
    mutate(
      x_pos = if_else(variable %in% "q48c_G2", 3.15,
                      if_else(variable %in% "EXP_q22i_G2", 2.15,
                              if_else(variable %in% "EXP_q22h_G2", 1.15, NA_real_))),
      variable = case_when(
        variable == "q48c_G2" ~ "Are available to help when needed",
        variable == "EXP_q22i_G2" ~ "Serve the interests of the community",
        variable == "EXP_q22h_G2" ~ "Serve the interests of regular citizens"
      ),
      multiplier = if_else(group == "empty_value", 0, 1),
      label      = paste0(format(round(value*100, 0), nsmall = 0),
                          "%"),
      label = if_else(multiplier == 0, NA_character_, label),
    );panelA
  
  a <- horizontal_edgebars (data2plot    = panelA,
                            y_value      = value,
                            x_var        = variable,
                            group_var    = group,
                            label_var    = label,
                            x_lab_pos    = x_pos,
                            y_lab_pos    = 0,
                            bar_color    = "#2a2a94",
                            margin_top   = 0);a
  
  saveIT.fn(chart  = a,
            n      = nchart,
            suffix = "a",
            w      = 82.59305,
            h      = 45.33831)
  
  # Panel B: Crime Control and Safety
  
  panelB <- data %>%
    filter(year == latestYear & country == mainCountry) %>%
    select(EXP_q24e_G2, q48a_G2, q48b_G2, q48b_G1) %>%
    mutate(
      across(everything(),
             ~ case_when(
               .x == 1  ~ 1,
               .x == 2  ~ 1,
               .x == 3  ~ 0,
               .x == 4  ~ 0,
               .x == 99 ~ NA_real_,
               is.na(.x) ~ NA_real_
             ))
    ) %>%
    summarise(across(everything(),
                     mean,
                     na.rm = T)) %>%
    pivot_longer(everything(),
                 names_to  = "variable",
                 values_to = "value") %>%
    mutate(empty_value = 1 - value) %>%
    pivot_longer(!variable,
                 names_to = "group",
                 values_to = "value") %>%
    mutate(
      x_pos = if_else(variable %in% "EXP_q24e_G2", 1.1,
                      if_else(variable %in% "q48b_G1", 2.15,
                              if_else(variable %in% "q48a_G2", 3.15, 4.15))),
      variable = case_when(
        variable == "q48b_G2"     ~ "Help them feel safe",
        variable == "q48a_G2"     ~ "Resolve security problems in  the community",
        variable == "q48b_G1"     ~ "Perform effective and lawful investigations",
        variable == "EXP_q24e_G2" ~ "Respond to crime reports",
      ),
      multiplier = if_else(group == "empty_value", 0, 1),
      label      = paste0(format(round(value*100, 0), nsmall = 0),
                          "%"),
      label = if_else(multiplier == 0, NA_character_, label)
    );panelB
  
  b <- horizontal_edgebars (data2plot    = panelB,
                            y_value      = value,
                            x_var        = variable,
                            group_var    = group,
                            label_var    = label,
                            x_lab_pos    = x_pos,
                            y_lab_pos    = 0,
                            bar_color    = "#2a2a94",
                            margin_top   = 0);b
  
  saveIT.fn(chart  = b,
            n      = nchart,
            suffix = "b",
            w      = 82.59305,
            h      = 59.74817)
  
  # Panel C: Due Process
  panelC <- data %>%
    filter(year == latestYear & country == mainCountry) %>%
    select(q48a_G1, EXP_q22e_G1, q48c_G1, q48d_G2) %>%
    mutate(
      across(everything(),
             ~ case_when(
               .x == 1  ~ 1,
               .x == 2  ~ 1,
               .x == 3  ~ 0,
               .x == 4  ~ 0,
               .x == 99 ~ NA_real_,
               is.na(.x) ~ NA_real_
             )),
      EXP_q22e_G1 = if_else(EXP_q22e_G1 == 1, 0, 
                            if_else(EXP_q22e_G1 == 0, 1, NA_real_))
    ) %>%
    summarise(across(everything(),
                     mean,
                     na.rm = T)) %>%
    pivot_longer(everything(),
                 names_to  = "variable",
                 values_to = "value") %>%
    mutate(empty_value = 1 - value) %>%
    pivot_longer(!variable,
                 names_to = "group",
                 values_to = "value") %>%
    mutate(
      x_pos = if_else(variable %in% "EXP_q22e_G1", 4.15,
                      if_else(variable %in% "q48d_G2", 3.15,
                              if_else(variable %in% "q48a_G1", 2.15, 1.15))),
      variable = case_when(
        variable == "q48a_G1"     ~ "Act lawfully",
        variable == "EXP_q22e_G1" ~ "Do not use excessive force",
        variable == "q48c_G1"     ~ "Respect the rights of suspects",
        variable == "q48d_G2"     ~ "Treat all people with respect",
      ),
      multiplier = if_else(group == "empty_value", 0, 1),
      label      = paste0(format(round(value*100, 0), nsmall = 0),
                          "%"),
      label = if_else(multiplier == 0, NA_character_, label)
    );panelC
  
  c <- horizontal_edgebars (data2plot    = panelC,
                            y_value      = value,
                            x_var        = variable,
                            group_var    = group,
                            label_var    = label,
                            x_lab_pos    = x_pos,
                            y_lab_pos    = 0,
                            bar_color    = "#2a2a94",
                            margin_top   = 0);c
  
  saveIT.fn(chart  = c,
            n      = nchart,
            suffix = "c",
            w      = 82.59305,
            h      = 59.74817)
  
  # Panel D: Discrimination
  
  panelD <- data %>%
    filter(year == latestYear & country == mainCountry) %>%
    select(q18a, EXP_q17g, q18c, q18d, q18e) %>%
    mutate(
      across(everything(),
             ~ case_when(
               .x == 0  ~ 0,
               .x == 1  ~ 1,
               .x == 99 ~ NA_real_,
               is.na(.x) ~ NA_real_
             ))
    ) %>%
    summarise(across(everything(),
                     mean,
                     na.rm = T)) %>%
    pivot_longer(everything(),
                 names_to  = "variable",
                 values_to = "value") %>%
    mutate(empty_value = 1 - value) %>%
    pivot_longer(!variable,
                 names_to = "group",
                 values_to = "value") %>%
    mutate(
      x_pos = if_else(variable %in% "EXP_q17g", 1.15,
                      if_else(variable %in% "q18d", 2.15,
                              if_else(variable %in% "q18e", 3.15, 
                                      if_else(variable %in% "q18c", 4.15, 5.15)))),
      variable = case_when(
        variable == "q18a"     ~ "Economic status",
        variable == "EXP_q17g" ~ "Skin color",
        variable == "q18c"     ~ "Ethnic background",
        variable == "q18d"     ~ "Religion",
        variable == "q18e"     ~ "Foreigner status"
      ),
      multiplier = if_else(group == "empty_value", 0, 1),
      label      = paste0(format(round(value*100, 0), nsmall = 0),
                          "%"),
      label = if_else(multiplier == 0, NA_character_, label)
    );panelD
  
  d <- horizontal_edgebars (data2plot    = panelD,
                            y_value      = value,
                            x_var        = variable,
                            group_var    = group,
                            label_var    = label,
                            x_lab_pos    = x_pos,
                            y_lab_pos    = 0,
                            bar_color    = "#2a2a94",
                            margin_top   = 0);d
  
  saveIT.fn(chart  = d,
            n      = nchart,
            suffix = "d",
            w      = 82.59305,
            h      = 74.86094)
  
  # Panel E: Discrimination
  
  panelE <- data %>%
    filter(year == latestYear & country == mainCountry) %>%
    select(q2d, q48e_G2, EXP_q22k_G2, EXP_q22j_G2) %>%
    mutate(
      across(everything(),
             ~ case_when(
               .x == 1  ~ 1,
               .x == 2  ~ 1,
               .x == 3  ~ 0,
               .x == 4  ~ 0,
               .x == 99 ~ NA_real_,
               is.na(.x) ~ NA_real_
             )),
      EXP_q22k_G2 = if_else(EXP_q22k_G2 == 1, 0, 
                            if_else(EXP_q22k_G2 == 0, 1, NA_real_)),
      EXP_q22j_G2 = if_else(EXP_q22j_G2 == 1, 0, 
                            if_else(EXP_q22j_G2 == 0, 1, NA_real_))
    ) %>%
    summarise(across(everything(),
                     mean,
                     na.rm = T)) %>%
    pivot_longer(everything(),
                 names_to  = "variable",
                 values_to = "value") %>%
    mutate(empty_value = 1 - value) %>%
    pivot_longer(!variable,
                 names_to = "group",
                 values_to = "value") %>%
    mutate(
      x_pos = if_else(variable %in% "q2d", 4.15,
                      if_else(variable %in% "EXP_q22k_G2", 3.15,
                              if_else(variable %in% "q48e_G2", 2.15, 
                                      if_else(variable %in% "EXP_q22j_G2", 1.15, NA_real_)))),
      variable = case_when(
        variable == "q2d"         ~ "Are not involved in corrupt practices",
        variable == "q48e_G2"     ~ "Investigate crimes in an independent manner",
        variable == "EXP_q22k_G2" ~ "Do not serve the interests of gangs",
        variable == "EXP_q22j_G2" ~ "Do not serve the interests of politicians"
      ),
      multiplier = if_else(group == "empty_value", 0, 1),
      label      = paste0(format(round(value*100, 0), nsmall = 0),
                          "%"),
      label = if_else(multiplier == 0, NA_character_, label)
    );panelE
  
  e <- horizontal_edgebars (data2plot    = panelE,
                            y_value      = value,
                            x_var        = variable,
                            group_var    = group,
                            label_var    = label,
                            x_lab_pos    = x_pos,
                            y_lab_pos    = 0,
                            bar_color    = "#2a2a94",
                            margin_top   = 0);e
  
  saveIT.fn(chart  = e,
            n      = nchart,
            suffix = "e",
            w      = 82.59305,
            h      = 59.74817)
  
  # Panel F: Trust and Safety
  
  panelF <- data %>%
    filter(year == latestYear & country == mainCountry) %>%
    select(q1d, EXP_q8d, q9) %>%
    mutate(
      across(everything(),
             ~ case_when(
               .x == 1  ~ 1,
               .x == 2  ~ 1,
               .x == 3  ~ 0,
               .x == 4  ~ 0,
               .x == 0  ~ 0,
               .x == 99 ~ NA_real_,
               is.na(.x) ~ NA_real_
             ))
    ) %>%
    summarise(across(everything(),
                     mean,
                     na.rm = T)) %>%
    pivot_longer(everything(),
                 names_to  = "variable",
                 values_to = "value") %>%
    mutate(empty_value = 1 - value) %>%
    pivot_longer(!variable,
                 names_to = "group",
                 values_to = "value") %>%
    mutate(
      x_pos = if_else(variable %in% "q9", 3.15,
                      if_else(variable %in% "q1d", 2.15,
                              if_else(variable %in% "EXP_q8d", 1.15, NA_real_))),
      variable = case_when(
        variable == "q1d"     ~ "Trust the police",
        variable == "EXP_q8d" ~ "Report a crime when they are a victim",
        variable == "q9"      ~ "Feel safe in their neighborhoods"
      ),
      multiplier = if_else(group == "empty_value", 0, 1),
      label      = paste0(format(round(value*100, 0), nsmall = 0),
                          "%"),
      label = if_else(multiplier == 0, NA_character_, label)
    );panelF
  
  f <- horizontal_edgebars (data2plot    = panelF,
                            y_value      = value,
                            x_var        = variable,
                            group_var    = group,
                            label_var    = label,
                            x_lab_pos    = x_pos,
                            y_lab_pos    = 0,
                            bar_color    = "#2a2a94",
                            margin_top   = 0);f
  
  saveIT.fn(chart  = f,
            n      = nchart,
            suffix = "f",
            w      = 82.59305,
            h      = 45.33831)
  
  # Panel G: Accountability
  
  panelG <- data %>%
    filter(year == latestYear & country == mainCountry) %>%
    select(q48d_G1, EXP_q22f_G1, EXP_q22g_G1, EXP_q22h_G1) %>%
    mutate(
      across(everything(),
             ~ case_when(
               .x == 1  ~ 1,
               .x == 2  ~ 1,
               .x == 3  ~ 0,
               .x == 4  ~ 0,
               .x == 99 ~ NA_real_,
               is.na(.x) ~ NA_real_
             ))
    ) %>%
    summarise(across(everything(),
                     mean,
                     na.rm = T)) %>%
    pivot_longer(everything(),
                 names_to  = "variable",
                 values_to = "value") %>%
    mutate(empty_value = 1 - value) %>%
    pivot_longer(!variable,
                 names_to = "group",
                 values_to = "value") %>%
    mutate(
      x_pos = if_else(variable %in% "q48d_G1", 4.15,
                      if_else(variable %in% "EXP_q22h_G1", 3.15,
                              if_else(variable %in% "EXP_q22g_G1", 2.15, 
                                      if_else(variable %in% "EXP_q22f_G1", 1.15, NA_real_)))),
      variable = case_when(
        variable == "q48d_G1"         ~ "Are held accountable for violating laws",
        variable == "EXP_q22f_G1"     ~ "Are held accountable for seeking bribes",
        variable == "EXP_q22g_G1"     ~ "Are held accountable for accepting bribes",
        variable == "EXP_q22h_G1"     ~ "Are investigated for misconduct"
      ),
      multiplier = if_else(group == "empty_value", 0, 1),
      label      = paste0(format(round(value*100, 0), nsmall = 0),
                          "%"),
      label = if_else(multiplier == 0, NA_character_, label)
    );panelG
  
  g <- horizontal_edgebars (data2plot    = panelG,
                            y_value      = value,
                            x_var        = variable,
                            group_var    = group,
                            label_var    = label,
                            x_lab_pos    = x_pos,
                            y_lab_pos    = 0,
                            bar_color    = "#2a2a94",
                            margin_top   = 0);g
  
  saveIT.fn(chart  = g,
            n      = nchart,
            suffix = "g",
            w      = 82.59305,
            h      = 59.74817)
  
  
}



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Perceptions of the Treatment of Crime Victims (PTCV)                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


figure_PTCV.fn <- function(nchart = 18, data = master_data.df) {
  
  # Defining variables to use in rose chart
  vars4plot <- c("EXP_q24a_G1", "EXP_q24b_G1", "EXP_q24c_G1", "EXP_q24d_G1", "EXP_q24a_G2", "EXP_q24b_G2",
                 "EXP_q24c_G2", "EXP_q24d_G2", "EXP_q24f_G2", "EXP_q24g_G2", "EXP_q23f_G1")
  
  # Defining data frame for plot
  data2plot <- data %>%
    filter(country == mainCountry & year == latestYear) %>%
    select(all_of(vars4plot)) %>%
    mutate(
      across(everything(), 
             ~if_else(.x == 1 | .x == 2, 1, 
                      if_else(!is.na(.x) & .x != 99, 0,
                              NA_real_)))
    ) %>%
    summarise(across(everything(),
                     mean, 
                     na.rm = T)) %>%
    pivot_longer(everything(),
                 names_to  = "category",
                 values_to = "avg") %>%
    #arrange(desc(avg)) %>%
    mutate(
      percentage = to_percentage.fn(avg*100),
      order_value = case_when (
        category == "EXP_q24c_G1" ~ 1,
        category == "EXP_q24d_G1" ~ 2,
        category == "EXP_q24a_G2" ~ 3,
        category == "EXP_q24b_G2" ~ 4,
        category == "EXP_q24c_G2" ~ 5,
        category == "EXP_q24d_G2" ~ 6,
        category == "EXP_q24f_G2" ~ 7,
        category == "EXP_q24g_G2" ~ 8,
        category == "EXP_q23f_G1" ~ 9,
        category == "EXP_q24a_G1" ~ 10,
        category == "EXP_q24b_G1" ~ 11
      )) %>%
    arrange(order_value) %>%
    mutate(
      label = case_when(
        category == "EXP_q24c_G1" ~ "Receive effective and\ntimely **medical and\npsychological care**",
        category == "EXP_q24d_G1" ~ "Receive **information\nand legal advice**\nwhen going to the\nauthorities",
        category == "EXP_q24a_G2" ~ "Receive **protection**\nfrom the police if\ntheir safety is in\ndanger",
        category == "EXP_q24b_G2" ~ paste0("Receive protection\nduring criminal\nproceedings", 
                                           " to\n**prevent repeat\nvictimization**"),
        category == "EXP_q24c_G2" ~ "Receive adequate\ncare and protection\nas **victims of sexual\ncrimes**",
        category == "EXP_q24d_G2" ~ "Receive adequate\ncare and protection\nas **victims of\ndomestic violence**",
        category == "EXP_q24f_G2" ~ paste0("Receive a **clear\nexplanation** of\nthe process when\nreporting",
                                           " a crime to\nthe police"),
        category == "EXP_q24g_G2" ~ "Are addressed by\nthe police using\n**accessible language**",
        category == "EXP_q23f_G1" ~ "Are **guaranteed\ntheir rights** in\ncriminal justice\nproceedings",
        category == "EXP_q24a_G1" ~ "Receive **prompt and\ncourteous attention**\nwhen they report a\ncrime",
        category == "EXP_q24b_G1" ~ "Are **believed** when\nthey report a crime"
      ),
      
      
      # Converting labels into HTML syntax
      across(label,
             function(raw_label){
               html <- paste0("<span style='color:#000000;font-size:6.326276mm;font-weight:bold'>",  
                              percentage, "</span>",
                              "<br>",
                              "<span style='color:#524F4C;font-size:3.514598mm'>",
                              str_replace_all(raw_label, "\\n", "<br>"),
                              "</span>")
               return(html)
             })
    )
  
  
  # Defining colors
  colors4plot        <- rosePalette
  names(colors4plot) <- data2plot %>% arrange(order_value) %>% pull(category)
  
  # Applying plotting function
  chart <- LAC_roseChart(data = data2plot,
                         target_var    = "avg",
                         grouping_var  = "category",
                         alabels_var   = "label",
                         plabels_var   = "percentage",
                         order_var     = "order_value",
                         colors        = colors4plot)
  
  # Saving panels
  saveIT.fn(chart  = chart,
            n      = nchart,
            suffix = NULL,
            w      = 189.7883,
            h      = 168.7007)
}
