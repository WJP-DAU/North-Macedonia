## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            LAC Country Reports - Section I Functions
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
##    Perceptions of Authoritarian Behaviors (PAB)                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure_PAB.fn <- function(nchart = 1, data = master_data.df){
  
  # Defining variables to use in the plot
  vars4plot <- list(
    "Independent" = c("CAR_q67_G1", "CAR_q67_G2", "CAR_q68_G1", "CAR_q61_G1"),
    "Judiciary"   = c("CAR_q66_G1", "CAR_q65_G1", "CAR_q64_G1"),
    "Media"       = c("CAR_q64_G2", "CAR_q60_G2", "CAR_q65_G2", "CAR_q60_G1")
  )
  
  
  # Defining data frame for plot
  data2plot <- data %>%
    filter(year == latestYear & country == mainCountry) %>%
    select(unlist(vars4plot, 
                  use.names = F)) %>%
    mutate(
      across(everything(),
             ~case_when(
               .x == 1  ~ "Strongly agree",
               .x == 2  ~ "Agree",
               .x == 3  ~ "Disagree",
               .x == 4  ~ "Strongly disagree",
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
      direction = if_else(statement %in% c("Agree", "Strongly agree", "Don't know (positive)"),
                          "Positive",
                          "Negative"),
      value2plot  = if_else(direction == "Positive", perc*100, perc*-100),
      value_label = to_percentage.fn(round(abs(value2plot), 0)),
      labels = case_when(
        variable == "CAR_q60_G1" ~ "Censor information that comes \nfrom abroad",
        variable == "CAR_q61_G1" ~ "Censor opinions from opposition \ngroups",
        variable == "CAR_q60_G2" ~ "Resort to misinformation to shape \npublic opinion in their favor",
        variable == "CAR_q64_G2" ~ "Attack or attempt to discredit the \nmedia and civil society organizations\nthat criticize them",
        variable == "CAR_q67_G1" ~ "Attack or attempt to discredit \nopposition parties",
        variable == "CAR_q67_G2" ~ "Attack or attempt to discredit the \nelectoral system and other \nsupervisory organs", 
        variable == "CAR_q64_G1" ~ "Seek to limit the courts' competencies \nand freedom to interpret the law",
        variable == "CAR_q66_G1" ~ "Seek to influence the promotion and \nremoval of judges",
        variable == "CAR_q65_G2" ~ "Prosecute and convict journalists and \nleaders of civil society organizations     ",
        variable == "CAR_q68_G1" ~ "Prosecute and convict members of\nopposition parties                                   ",
        variable == "CAR_q65_G1" ~ "Refuse to comply with court rulings \nthat are not in their favor"
      ),
      order_value = case_when(
        variable == "CAR_q60_G1" ~ 4,
        variable == "CAR_q61_G1" ~ 2,
        variable == "CAR_q60_G2" ~ 3,
        variable == "CAR_q64_G2" ~ 2,
        variable == "CAR_q67_G1" ~ 3,
        variable == "CAR_q67_G2" ~ 4, 
        variable == "CAR_q64_G1" ~ 1,
        variable == "CAR_q66_G1" ~ 2,
        variable == "CAR_q65_G2" ~ 1,
        variable == "CAR_q68_G1" ~ 1,
        variable == "CAR_q65_G1" ~ 3
      ),
      statement = if_else(statement %in% c("Don't know (positive)", "Don't know (negative)"),
                          "Don't know",
                          statement),
      statement = factor(statement,
                         levels = c("Strongly agree", "Agree", "Strongly disagree", "Disagree", "Don't know"))
    )
  
  # Defining color palette
  colors4plot <- lickertPalette
  names(colors4plot) <- c("Strongly agree", "Agree", "Don't know", "Disagree", "Strongly disagree")

  # Plotting each panel of Figure 12
  imap(c("A" = "Independent", 
         "B" = "Judiciary", 
         "C" = "Media"),
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
         
         if (length(vars4plot[[varSet]]) == 4 ) {
           h = 56.23357
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
##    Perceptions of Authoritarian Behavior, by Support for the Current Administration (PABGS)              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure_PABGS.fn <- function(nchart = 2, data = master_data.df){
  
  # Defining variables to use in the plot
  vars4plot <- list(
    "Independent" = c("CAR_q67_G1", "CAR_q67_G2", "CAR_q68_G1", "CAR_q61_G1"),
    "Judiciary"   = c("CAR_q66_G1", "CAR_q65_G1", "CAR_q64_G1"),
    "Media"       = c("CAR_q64_G2", "CAR_q60_G2", "CAR_q65_G2", "CAR_q60_G1")
  )
  
  # Defining data frame for plot
  data2plot <- data %>%
    filter(country == mainCountry & year == latestYear) %>%
    select(relig, 
           unlist(vars4plot, 
                  use.names = F)) %>%
    mutate(
      religr = case_when(
        relig %in% c("C57 - Orthodox Christian") ~ "Orthodox Christian",
        relig %in% c("G6 - Sunni Muslim")        ~ "Sunni Muslim"
      ),
      across(!c(relig, religr),
             ~if_else(.x == 1 | .x == 2, 1,
                      if_else(!is.na(.x) & .x != 99, 0, 
                              NA_real_)))
    ) %>%
    group_by(religr) %>%
    select(-relig) %>%
    filter(!is.na(religr)) %>%
    summarise(across(everything(),
                     \(x) mean(x, na.rm = TRUE))) %>%
    pivot_longer(!religr,
                 names_to   = "category",
                 values_to  = "value2plot") %>%
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
      value2plot = round(value2plot*100,1),
      
      order_value = case_when(
        category == "CAR_q60_G1" ~ 4,
        category == "CAR_q61_G1" ~ 2,
        category == "CAR_q60_G2" ~ 3,
        category == "CAR_q64_G2" ~ 2,
        category == "CAR_q67_G1" ~ 3,
        category == "CAR_q67_G2" ~ 4, 
        category == "CAR_q64_G1" ~ 1,
        category == "CAR_q66_G1" ~ 2,
        category == "CAR_q65_G2" ~ 1,
        category == "CAR_q68_G1" ~ 1,
        category == "CAR_q65_G1" ~ 3
        
      )
    )
  
  # Defining color palette
  colors4plot <- c("Orthodox Christian" = "#a90099", 
                   "Sunni Muslim"       = "#3273ff")
  
  # Plotting each panel of Figure 12
  imap(c("A" = "Independent", 
         "B" = "Judiciary", 
         "C" = "Media"),
       function(varSet, panelName) {
         
         # Filtering data2plot to leave the variable for each panel
         data2plot <- data2plot %>%
           filter(category %in% vars4plot[[varSet]])
         
         # Applying plotting function
         chart <- NM_dotsChart(data         = data2plot,
                                target_var   = "value2plot",
                                grouping_var = "religr",
                                labels_var   = "labels",
                                colors       = colors4plot,
                                order_var    = "order_value")
         
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
                   suffix = panelName,
                   w      = 189.7883,
                   h      = h)
         
       })
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Attitudes Towards Authoritarianism and Rule of Law (AROL)                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure_AROL.fn <- function(nchart = 3, data = master_data.df) {
  
  # Defining variables to use
  vars4plot <- c("q50", "q51", "q52", "CAR_q73", "CAR_q74")
  
  # Defining data frame for plot
  data2plot <- data %>%
    filter(country == mainCountry & year == latestYear) %>%
    select(all_of(vars4plot)) %>%
    mutate(
      q52 = case_when(
        q52 == 1 ~ 4,
        q52 == 2 ~ 3,
        q52 == 3 ~ 2,
        q52 == 4 ~ 1,
        q52 == 5 ~ 5,
        q52 == 99 ~ 99
      ),
      across(everything(),
             ~case_when(
               .x == 3 | .x == 4 ~ "Positive",
               .x < 3   ~ "Negative",
               .x %in% c(5, 99)  ~ "Neutral/No answer"
             )),
      across(everything(),
             \(x) factor(x, levels = c("Positive", "Neutral/No answer", "Negative")))
      
    )
  
  # Defining colors for plot
  colors4plot <- c("#003B8A","#d9d9d9", "#fa4d57")
  names(colors4plot) <- c("Positive", "Neutral/No answer", "Negative")
  
  # Plotting each figure panel
  names(vars4plot) <- c("A", "B", "C", "D", "E")
  imap(vars4plot,
       function(var, panelName) {
         
         # Filtering data2plot to leave the variable for each panel
         data2plot <- data2plot %>%
           rename(category = all_of(var))
         
         # Preparing waffle data
         waffle_data <- waffle_iron(data2plot, 
                                    aes_d(group = category),
                                    sample_size = 0.25,
                                    rows = 12)
         
         # Plotting waffle
         chart <- ggplot(waffle_data, 
                         aes(x, y, fill = group),
                         color = "white",
                         size = 0.75) + 
           geom_waffle() + 
           coord_equal() + 
           scale_fill_manual(values = colors4plot) + 
           theme_waffle() +
           theme(
             axis.title.x      = element_blank(),
             axis.title.y      = element_blank(),
             legend.position   = "none"
           )
         
         # Saving panels
         saveIT.fn(chart  = chart,
                   n      = nchart,
                   suffix = panelName,
                   w      = 403.4756,         # Idk why, but I have to multiply the dimensions by 4
                   h      = 119.4963)         # Idk why, but I have to multiply the dimensions by 4
       })
}



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Fundamental Freedoms Over Time (FFOT)                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure_FFOT.fn <- function(nchart = 4, data = master_data.df) {
  
  # Defining data frame for plot
  data2plot <- data %>%
    filter(country == mainCountry) %>%
    select(year,
           q46c_G2, q46f_G2, q46g_G2, q46c_G1, q46e_G2,
           q46d_G2, q46f_G1, q46a_G2,
           q46d_G1, q46e_G1, q46h_G2) %>%
    mutate(
      across(!c(year),
             ~ case_when(
               .x == 1 | .x == 2 ~ 1,
               .x == 3 | .x == 4 ~ 0
             ))
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
  
  # Plotting each panel of Figure 5
  imap(c("A" = "q46c_G2", "B" = "q46f_G2", "C" = "q46g_G2", "D" = "q46c_G1", "E" = "q46e_G2",
         "F" = "q46d_G2", "G" = "q46f_G1", "H" = "q46a_G2",
         "I" = "q46d_G1", "J" = "q46e_G1", "K" = "q46h_G2"),
       function(var4plot, panelName) {
         
         # Filtering data2plot to leave the variable for each panel
         data2plot <- data2plot %>%
           filter(category %in% var4plot)
         
         # Defining colors4plot
         colors4plot <- mainCOLOR
         names(colors4plot) <- var4plot
         
         # Applying plotting function
         chart <- LAC_lineChart(data           = data2plot,
                                target_var     = "value",
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
                   suffix = panelName,
                   w      = 90.67663,
                   h      = 45.68977)
         
       })
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Fundamental Freedoms, by DEMOGRAPHIC (FFD)                                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   

c("ethnigroup" = "Macedonian",
  "relgroup"   = "Christian")

figure_FFD.fn <- function(nchart = 5, data = master_data.df){
  
  # Defining variables to include in plot
  vars4plot <- list("Expression"    = c("q46c_G2", "q46f_G2", "q46g_G2", "q46c_G1", "q46e_G2"),
                   "Participation" = c("q46d_G2", "q46f_G1", "q46a_G2"),
                   "Election"      = c("q46d_G1", "q46e_G1"),
                   "Religion"      = c("q46h_G2"))
  
  # Defining data frame for plot
  data2plot <- data %>%
    mutate(religr = case_when(
      relig %in% c("C57 - Orthodox Christian") ~ "Orthodox Christian",
      relig %in% c("G6 - Sunni Muslim")        ~ "Sunni Muslim"
    )) %>%
    rename(gvar = religr) %>%
    filter(year == latestYear & country == mainCountry) %>%
    select(gvar, all_of(unlist(vars4plot, use.names = F))) %>%
    mutate(
      across(!gvar,
             ~if_else(.x == 1 | .x == 2, 1, 
                      if_else(!is.na(.x) & .x != 99, 0, 
                              NA_real_)))) %>%
    group_by(gvar) %>%
    summarise(across(everything(),
                     mean,
                     na.rm = T)) %>%
    filter(!is.na(gvar)) %>%
    pivot_longer(!gvar,
                 names_to   = "category",
                 values_to  = "value2plot") %>%
    mutate(
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
        category     == 'q46h_G2' ~ 11,
      ),
      label = case_when(
        category == 'q46c_G2'       ~ paste("**People** can <br> express opinions<br>against the government"),
        category == 'q46f_G2'       ~ paste("**Civil society** <br>organizations can <br> express opinions",
                                            "against<br>the government"),
        category == 'q46g_G2'       ~ paste("**Political parties**<br>can express opinions<br>",
                                            "against the<br>government"),
        category == 'q46c_G1'       ~ paste("**The media**<br>can express opinions<br>",
                                            "against the<br>government"),
        category == 'q46e_G2'       ~ paste("The media<br>can **expose cases<br>of corruption**"),
        category == 'q46d_G2'       ~ paste("People can<br>**attend community<br>meetings**"),
        category == 'q46f_G1'       ~ paste("People can<br>**join any political<br>organization**"),
        category == 'q46a_G2'       ~ paste("People can<br>**organize around an<br>issue or petition**"),
        category == 'q46d_G1'       ~ paste("Local government<br>officials **are elected<br>through a clean<br>process**"),
        category == 'q46e_G1'       ~ paste("People can<br>**vote freely** without<br>feeling harassed<br>or pressured"),
        category == 'q46h_G2'       ~ paste("Religious minorities<br>can **observe their<br>holy days**"),
      ),
      across(label,
             ~paste0("<span style='color:#524F4C;font-size:3.514598mm;font-weight:bold'>",
                     label,
                     "</span>")),
      label = if_else(gvar != "Orthodox Christian", 
                      NA_character_, 
                      label),
      latestYear = "Orthodox Christian"
    ) %>%
    rename(year = gvar)
  
  # Defining color palette
  colors4plot <- c("Orthodox Christian" = "#a90099", 
                   "Sunni Muslim"       = "#3273ff")
  
  # Plotting chart
  chart <- LAC_radarChart(data          = data2plot,
                          axis_var      = "category",         
                          target_var    = "value2plot",     
                          label_var     = "label", 
                          order_var     = "order_value",
                          colors        = colors4plot)
  
  # Saving panels
  saveIT.fn(chart  = chart,
            n      = nchart,
            suffix = NULL,
            w      = 189.7883,
            h      = 183.1106)
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Perceptions of Accountability Over Time (PAOT)                                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


figure_PAOT1.fn <- function(nchart = 6, data = master_data.df) {

# Defining data frame for plot
data2plot <- data %>%
  select(country, year, q43_G2) %>%
  mutate(
    q43_G2 = if_else(q43_G2 == 3, 1,
                     if_else(!is.na(q43_G2) & q43_G2 != 99, 0,
                             NA_real_))
  ) %>%
    group_by(year, country) %>%
    summarise(value2plot = mean(q43_G2, na.rm = T)) %>%
    mutate(value2plot = value2plot*100,
           label = paste0(format(round(value2plot, 0),
                                 nsmall = 0),
                          "%"),
           label = if_else(country == mainCountry, label, NA_character_)) %>%
    filter(year >= 2014)

  # Pulling minimum and maximum available year
  minyear <- 2013
  maxyear <- 2023

  # Creating a vector for yearly axis
  x.axis.values <- seq(minyear, maxyear, by = 2)
  sec.ticks     <- seq(minyear, maxyear, by = 1)
  x.axis.labels <- paste0("'", str_sub(x.axis.values, start = -2))

  # Applying plotting function
  chart <- LAC_lineChart(data           = data2plot,
                         target_var     = "value2plot",
                         grouping_var   = "year",
                         ngroups        = data2plot$country,
                         labels_var     = "label",
                         colors_var     = "country",
                         colors         = mainCOLOR,
                         repel          = F,
                         transparency   = F,
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
            h      = 149.7219)
}


# figure_PAOT2.fn <- function(nchart = 6, data = master_data.df) {
#   
#   # Defining variables to include in plot
#   vars4plot <- "q43_G2"
#   
#   # Defining data frame for plot
#   data2plot <- data %>%
#     filter(country == mainCountry & year == latestYear) %>%
#     select(relig, 
#            unlist(vars4plot, 
#                   use.names = F)) %>%
#     mutate(
#       religr = case_when(
#         relig %in% c("C57 - Orthodox Christian") ~ "Orthodox Christian",
#         relig %in% c("G6 - Sunni Muslim")        ~ "Sunni Muslim"
#       ),
#       across(!c(relig, religr),
#              ~if_else(.x == 1 | .x == 2, 1,
#                       if_else(!is.na(.x) & .x != 99, 0, 
#                               NA_real_)))
#     ) %>%
#     group_by(religr) %>%
#     select(-relig) %>%
#     filter(!is.na(religr)) %>%
#     summarise(across(everything(),
#                      \(x) mean(x, na.rm = TRUE))) %>%
#     pivot_longer(!religr,
#                  names_to   = "category",
#                  values_to  = "value2plot") %>%
#     mutate(
#       value2plot  = value2plot*100,
#       highlighted = if_else(year == latestYear, 
#                             "Highlighted", 
#                             "Regular"),
#       labels      = to_percentage.fn(value2plot),
#       year        = factor(as.character(year), 
#                            levels = c("2017", "2023"))
#     )
#   
#   # Defining colors
#   colors4plot <- barsPalette
#   names(colors4plot) <- c("Highlighted", "Regular")
#   
#   # Plotting each panel of Figure 5
#   panelVector <- c("A" = vars4plot)
#   
#   imap(panelVector,
#        function(tvar, panelName) {
#          
#          # Filtering data2plot to leave the variable for each panel
#          data2plot_panel <- data2plot %>%
#            filter(category %in% tvar)
#          
#          # Applying plotting function
#          chart <- LAC_barsChart(data           = data2plot_panel,
#                                 target_var     = "value2plot",
#                                 grouping_var   = "year",
#                                 labels_var     = "labels",
#                                 colors_var     = "highlighted",
#                                 colors         = colors4plot,
#                                 direction      = "horizontal")
#          
#          # Saving panels
#          saveIT.fn(chart  = chart,
#                    n      = nchart,
#                    suffix = "B",
#                    w      = 86.81057,
#                    h      = 22.60219)
#          
#        })
# }
# 
