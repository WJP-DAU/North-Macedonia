mainCountry  <- "North Macedonia"
latestYear   <- "2023"

master_data.df <- master_data.df %>%
  filter(year == latestYear & country == mainCountry) %>%
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
  mutate(
    sources_help = case_when(
      q24 == 1  ~ 1,
      q24 == 0  ~ 0,
      q24 == 99 ~ NA_real_
    ),
    q34_merge       = if_else(q34_merge != 99, q34_merge, NA_real_),
    fully_resolved  = if_else(q34_merge == 4, 1, 0),
    problem_persist = if_else(q34_merge == 3, 1, 0),
    fair = case_when(
      q36a == 1 ~ 1,
      q36a == 0 ~ 0,
      q36a == 99 ~ NA_real_
    ),
    difficult_cost       = if_else(q37d == 3 | q37d == 4, 1, 
                                   if_else(q37d == 1 | q37d == 2, 0, NA_real_)),
    financial_difficulty = if_else(q37c == 1 & difficult_cost == 1, 1, 0),
    time                 = if_else(q37b >= 0 & q28 == 1, q37b, NA_real_),
    hardships =
      if_else(
        q42a == 1 |
          q42b == 1 |
          q42c == 1 |
          q42d == 1 ,
        1, 0
  ))

diferencia_medias_significativa <- function(datos, 
                                            demographics = "govSupp", 
                                            var_grupo, 
                                            vars_resultados, 
                                            mainCountry) {
  # Crear un data frame vacío para almacenar los resultados
  resultados <- data.frame(variable = character(),
                           diferencia = numeric(),
                           p_valor = numeric(),
                           significance = character(),
                           stringsAsFactors = FALSE)
  
  if(demographics == "govSupp"){
    
    # Filtrar los datos para el grupo 1 y el grupo 2
    grupo1 <- datos %>% filter({{ var_grupo }} == "Gov. Supporter")
    grupo2 <- datos %>% filter({{ var_grupo }} == "Non Gov. Supporter")
    
    
  } else if (demographics == "religion"){
    
    # Filtrar los datos para el grupo 1 y el grupo 2
    grupo1 <- datos %>% filter({{ var_grupo }} == "Orthodox Christian")
    grupo2 <- datos %>% filter({{ var_grupo }} == "Sunni Muslim")
    
  } else if (demographics == "gender") {
    
    # Filtrar los datos para el grupo 1 y el grupo 2
    grupo1 <- datos %>% filter({{ var_grupo }} == "Male")
    grupo2 <- datos %>% filter({{ var_grupo }} == "Female")
    
  } else if (demographics == "area") {
    
    # Filtrar los datos para el grupo 1 y el grupo 2
    grupo1 <- datos %>% filter({{ var_grupo }} == "Urban")
    grupo2 <- datos %>% filter({{ var_grupo }} == "Rural")
    
  } else if (demographics == "financial"){
    
    # Filtrar los datos para el grupo 1 y el grupo 2
    grupo1 <- datos %>% filter({{ var_grupo }} == "Secure")
    grupo2 <- datos %>% filter({{ var_grupo }} == "Insecure")
    
  } else if (demographics == "skin_tone"){
      
      # Filtrar los datos para el grupo 1 y el grupo 2
      grupo1 <- datos %>% filter({{ var_grupo }} == "Darker Skin")
      grupo2 <- datos %>% filter({{ var_grupo }} == "Lighter Skin")
  
  } else if (demographics == "ethnicity") {
    
    # Filtrar los datos para el grupo 1 y el grupo 2
    grupo1 <- datos %>% filter({{ var_grupo }} == "Macedonian")
    grupo2 <- datos %>% filter({{ var_grupo }} == "Albanian")
    
  } else {
    
    print("No hay un argumento valido")
    
  }
  
  # Iterar sobre las variables de resultados y calcular la diferencia de medias y p-valor para cada una
  for (var_resultado in vars_resultados) {
    
    
    # Calcular la media para cada grupo, ignorando los valores perdidos
    media_grupo1 <- mean(grupo1[[var_resultado]], na.rm = TRUE)
    media_grupo2 <- mean(grupo2[[var_resultado]], na.rm = TRUE)
    
    # Verificar que haya suficientes observaciones para realizar la prueba
    if (nrow(grupo1) < 3 || nrow(grupo2) < 3) {
      warning(paste0("No hay suficientes observaciones para la variable '", var_resultado, "'."))
      p_valor <- NA
      significativa <- "NA"
    } else {
      # Calcular la diferencia de medias y el p-valor usando una prueba t de Student
      resultado_prueba <- t.test(grupo1[[var_resultado]], grupo2[[var_resultado]], na.rm = TRUE)
      p_valor <- resultado_prueba$p.value
      significance <- ifelse(p_valor < 0.1, "YES", "NO")
    }
    
    # Agregar los resultados al data frame de resultados
    resultados <- rbind(resultados, data.frame(country = mainCountry,
                                               media_group1 = media_grupo1,
                                               media_group2 = media_grupo2,
                                               variable = var_resultado,
                                               diff = media_grupo1 - media_grupo2,
                                               p_valor = p_valor,
                                               significance = significance,
                                               stringsAsFactors = FALSE))
  }
  # Retornar el data frame de resultados
  return(resultados)
}
  
ttest_dem_gpp.fn <- function(data.df = master_data.df, 
                             section = "govSupp"){
  
  authoritarianism <- c("CAR_q67_G1", "CAR_q67_G2", "CAR_q68_G1", "CAR_q61_G1",
                        "CAR_q66_G1", "CAR_q65_G1", "CAR_q64_G1",
                        "CAR_q64_G2", "CAR_q60_G2", "CAR_q65_G2", "CAR_q60_G1")
  bribery <- c("q4a", "q4b", "q4c", "q4d", "q4e")
  bribery_attitudes <- c("CAR_q2c","CAR_q2b", "CAR_q2f", "CAR_q2g","CAR_q2a", "CAR_q2d", "CAR_q2e")
  constraints    <- c("q45a_G1", "q45b_G1", "q45c_G1")
  ffreedoms      <- c("q46c_G2", "q46f_G2", "q46g_G2", "q46c_G1", "q46e_G2",
                      "q46d_G2", "q46f_G1", "q46a_G2",
                      "q46d_G1", "q46e_G1", "q46h_G2")
  accountability <- c("q43_G2")
  corruption     <- c("q2a", "q2d", "q2b", "q2c", "q2e", "q2f", "q2g")
  trust          <- c("q1a", "q1d", "q1b", "q1c", "q1e", "q1f", "q1g")
  security       <- c("q9")
  cjustice       <- c("q49a", "q49b_G2", "q49e_G2", "q49c_G2", "q49e_G1", "q49c_G1", "q49b_G1")
  effectiveness  <- c("q48f_G2", "q48h_G1", "q48g_G2")
  police         <- c("q48c_G2",
                      "q48a_G2", "q48b_G2", "q48b_G1",
                      "q48a_G1", "q48c_G1", "q48d_G2",
                      "q48e_G2",
                      "q48d_G1")
  police_new     <- c("EXP_q22i_G2", "EXP_q22h_G2",
                      "EXP_q24e_G2",
                      "EXP_q22e_G1",
                      "EXP_q22k_G2", "EXP_q22j_G2",
                      "EXP_q22f_G1", "EXP_q22g_G1", "EXP_q22h_G1")
  police_discrimination <- c("q18a","q18c","q18d","q18e", "EXP_q17g")
  A2J            <- c("legal", "sources_help", "fully_resolved", "problem_persist", "fair", "difficult_cost", "financial_difficulty", "time", "hardships")
  victimization <- c("victim", "prop_crimes", "life_crimes", "corr_crimes")
  VIP_vars <- c(ffreedoms, accountability,corruption, trust, security, cjustice, A2J, effectiveness, 
                police, police_new, police_discrimination, constraints, bribery, bribery_attitudes, authoritarianism)
  
  # Subsetting for variables of interest
  data_subset.df <- data.df %>%
    select(country, country_code, year,
           all_of(VIP_vars), 
           q50, q51, q52,
           age, fin, Urban, gend, edu, ethni, relig, paff3, COLOR,
           CAR_q59_G1, CAR_q59_G2) %>%
    mutate(
      q52 = case_when(
        q52 == 1 ~ 4,
        q52 == 2 ~ 3,
        q52 == 3 ~ 2,
        q52 == 4 ~ 1,
        q52 == 5 ~ 5,
        q52 == 99 ~ 99
      ),
      across(c(q50, q51, q52),
             ~if_else(.x < 3, 1, 0),
             .names = "{.col}_neg"),
      across(c(q50, q51, q52),
             ~if_else(.x == 3 | .x == 4, 1,
                      if_else(.x == 1 | .x == 2, 0, 
                              NA_real_)),
             .names = "{.col}_pos")) %>%
    mutate(
      across(all_of(c(constraints, authoritarianism)),
             ~ if_else(.x == 1 | .x == 2, 1,
                       if_else(.x == 3 | .x == 4, 0, 
                               NA_real_))),
      across(all_of(ffreedoms),
             ~ case_when(
               .x == 1 | .x == 2 ~ 1,
               .x == 3 | .x == 4 ~ 0
             )),
      across(all_of(accountability),
             ~if_else(.x == 3, 1, 
                      if_else(!is.na(.x) & .x != 99, 0, 
                              NA_real_))),
      across(all_of(c(corruption, bribery_attitudes)),
             ~if_else(.x == 3 | .x == 4, 1,
                      if_else(!is.na(.x)  & .x != 99, 0, 
                              NA_real_))),
      across(all_of(c(bribery, police_discrimination)),
             ~if_else(.x == 99, NA_real_, as.double(.x))),
      across(all_of(trust),
             ~if_else(.x == 1 | .x == 2, 1,
                      if_else(!is.na(.x) & .x != 99, 0, 
                              NA_real_))),
      across(all_of(security),
             ~case_when(
               .x == 1 | .x == 2    ~ 1,
               .x == 3 | .x == 4    ~ 0
             )),
      across(all_of(cjustice),
             ~if_else(.x == 1 | .x == 2, 1,
                      if_else(!is.na(.x) & .x != 99, 0, NA_real_))),
      across(all_of(effectiveness),
             ~if_else(.x == 1 | .x == 2, 1,
                      if_else(!is.na(.x) & .x != 99, 0, 
                              NA_real_))),
      across(all_of(c(police, police_new)),
             ~case_when(
               .x == 1  ~ 1,
               .x == 2  ~ 1,
               .x == 3  ~ 0,
               .x == 4  ~ 0))
    ) %>%
    mutate(
      govSupp       = 
        case_when(
          !is.na(CAR_q59_G1) & !is.na(CAR_q59_G2) ~ NA_character_,
          CAR_q59_G1 == 1   | CAR_q59_G2 == 1     ~ "Gov. Supporter",
          CAR_q59_G1 == 2   | CAR_q59_G2 == 2     ~ "Non Gov. Supporter",
          CAR_q59_G1 == 99  | CAR_q59_G2 == 99    ~ NA_character_,
          is.na(CAR_q59_G1) & is.na(CAR_q59_G2)   ~ NA_character_),
      religr        = 
        case_when(
          relig %in% c("C57 - Orthodox Christian") ~ "Orthodox Christian",
          relig %in% c("G6 - Sunni Muslim")        ~ "Sunni Muslim"),
      gender        = 
        if_else(
          gend == 1, "Male", "Female", NA_character_),
      area          =  
        if_else(Urban == 1, "Urban", "Rural", NA_character_),
      financial     = 
        case_when(
          fin == 1 | fin == 2 ~ "Insecure",
          fin == 3 | fin == 4 | fin == 5 ~ "Secure"
        ),
      skin_tone    =
        case_when(
          COLOR == 1 | COLOR == 2 | COLOR == 3 | COLOR == 4 ~ "Lighter Skin",
          COLOR == 5 | COLOR == 6 | COLOR == 7 | COLOR == 8 | COLOR == 9 | COLOR == 10 ~ "Darker Skin"
        ),
      ethnicity = 
        case_when(
          ethni == "Macedonian" ~ "Macedonian",
          ethni == "Albanian"   ~ "Albanian",
          TRUE ~ "Other"
          )
    )
  
  # Test for constant columns excluding NA values
  constant_cols <- sapply(data_subset.df, function(col) {
    unique_vals <- unique(col[!is.na(col)])
    length(unique_vals) == 1
  })
  constant_cols <- names(constant_cols[constant_cols])
  
  # Print constant columns excluding NA (for testing purposes)
  if (length(constant_cols) > 0) {
    cat("Constant columns found (excluding NA values): ", paste(constant_cols, collapse = ", "), "\n")
  } else {
    cat("No constant columns found (excluding NA values).\n")
  }
  
  if(section == "govSupp"){
    
    # Filtrar los datos para el grupo 1 y el grupo 2
    
    data2table <- data_subset.df %>%
      filter(govSupp %in% c("Gov. Supporter", "Non Gov. Supporter")) 
    
    diffmeans.df <- diferencia_medias_significativa(datos = data2table, 
                                                    demographics = "govSupp",
                                                    var_grupo = govSupp, 
                                                    vars_resultados = VIP_vars,
                                                    mainCountry = mainCountry)  

  } else if (section == "religion"){
    
    # Filtrar los datos para el grupo 1 y el grupo 2
    data2table <- data_subset.df %>%
      filter(religr %in% c("Orthodox Christian", "Sunni Muslim")) 
    
    diffmeans.df <- diferencia_medias_significativa(datos = data2table, 
                                                    demographics = "religion",
                                                    var_grupo = religr, 
                                                    vars_resultados = VIP_vars,
                                                    mainCountry = mainCountry)  
    
  } else if (section == "gender") {
    
    # Filtrar los datos para el grupo 1 y el grupo 2
    data2table <- data_subset.df %>%
      filter(gender %in% c("Male", "Female")) 
    
    diffmeans.df <- diferencia_medias_significativa(datos = data2table, 
                                                    demographics = "gender",
                                                    var_grupo = gender, 
                                                    vars_resultados = VIP_vars,
                                                    mainCountry = mainCountry)  
    
  } else if (section == "area") {
    
    # Filtrar los datos para el grupo 1 y el grupo 2
    data2table <- data_subset.df %>%
      filter(area %in% c("Urban", "Rural")) 
    
    diffmeans.df <- diferencia_medias_significativa(datos = data2table,
                                                    demographics = "area",
                                                    var_grupo = area, 
                                                    vars_resultados = VIP_vars,
                                                    mainCountry = mainCountry)  
    
  } else if (section == "financial"){
    
    # Filtrar los datos para el grupo 1 y el grupo 2
    data2table <- data_subset.df %>%
      filter(financial %in% c("Insecure", "Secure")) 
    
    diffmeans.df <- diferencia_medias_significativa(datos = data2table, 
                                                    demographics = "financial", 
                                                    var_grupo = financial, 
                                                    vars_resultados = VIP_vars,
                                                    mainCountry = mainCountry)  
    
  } else if (section == "skin_tone"){
    
    # Filtrar los datos para el grupo 1 y el grupo 2
    data2table <- data_subset.df %>%
      filter(skin_tone %in% c("Darker Skin", "Lighter Skin")) 
    
    diffmeans.df <- diferencia_medias_significativa(datos = data2table, 
                                                    demographics = "skin_tone", 
                                                    var_grupo = skin_tone, 
                                                    vars_resultados = VIP_vars,
                                                    mainCountry = mainCountry)  
    } else if (section == "ethnicity"){
      
      # Filtrar los datos para el grupo 1 y el grupo 2
      data2table <- data_subset.df %>%
        filter(ethnicity %in% c("Macedonian", "Albanian")) 
      
      diffmeans.df <- diferencia_medias_significativa(datos = data2table, 
                                                      demographics = "ethnicity", 
                                                      var_grupo = ethnicity, 
                                                      vars_resultados = VIP_vars,
                                                      mainCountry = mainCountry)  
    
  } else {
    
    print("No se insertó un argumento válido")
    
  }
  
  diffmeans.df <- diffmeans.df %>%
    mutate(label = case_when(
      variable == "q46c_G2" ~ "Fundamental Freedoms - People can express opinion against the government",
      variable == "q46f_G2" ~ "Fundamental Freedoms - Civil society organizations can express opinions against the government",
      variable == "q46g_G2" ~ "Fundamental Freedoms - Political parties can express opinions against the government",
      variable == "q46c_G1" ~ "Fundamental Freedoms - The media can express opinions against the government without fear of retaliation",
      variable == "q46e_G2" ~ "Fundamental Freedoms - The media can expose cases of corruption",
      variable == "q46d_G2" ~ "Fundamental Freedoms - People can attend community meetings",
      variable == "q46f_G1" ~ "Fundamental Freedoms - People can join any political organizations",
      variable == "q46a_G2" ~ "Fundamental Freedoms - People can organize around an issue or petition",
      variable == "q46d_G1" ~ "Fundamental Freedoms - Local government officials are elected through a clean process",
      variable == "q46e_G1" ~ "Fundamental Freedoms - People can vote freely without feeling harassed or pressured",
      variable == "q46h_G2" ~ "Fundamental Freedoms - Religious minorities can observe their holy days",
      variable == "q2a" ~ "Corruption - Members of the national assembly",
      variable == "q2d" ~ "Corruption - Police officers",
      variable == "q2b" ~ "Corruption - Local government officers",
      variable == "q2c" ~ "Corruption - National government officers",
      variable == "q2e" ~ "Corruption - Prosecutors",
      variable == "q2f" ~ "Corruption - Judges & magistrates",
      variable == "q2g" ~ "Corruption - Public Defense Attorneys",
      variable == "q1a" ~ "Trust - People living in their country",
      variable == "q1d" ~ "Trust - Police officers",
      variable == "q1b" ~ "Trust - Local government officers",
      variable == "q1c" ~ "Trust - National government officers",
      variable == "q1e" ~ "Trust - Prosecutors",
      variable == "q1f" ~ "Trust - Judges & magistrates",
      variable == "q1g" ~ "Trust - Public Defense Attorneys",
      variable == "q43_G2" ~ "Accountability - Government officials would be held accountable for breaking the law",
      variable == "q9" ~ "Security - Perception of security: feeling safe walking in their neighborhood at night",
      variable == "q49a" ~ "CJ - The criminal justice system is effective in bringing people who commit crimes to justice",
      variable == "q49b_G2" ~ "CJ - The criminal justice system allows all victims of crime to seek justice regardless of who they are",
      variable == "q49e_G2" ~ "CJ - The criminal justice system treats those accused of crime as 'innocent until proven guilty'",
      variable == "q49c_G2" ~ "CJ - The criminal justice system allows all those accused of crimes to get a fair trial regardless of who they are",
      variable == "q49e_G1" ~ "CJ - The criminal justice system gives punishments which fit the crime",
      variable == "q49c_G1" ~ "CJ - The criminal justice system makes sure everyone has access to the justice system if they need it",
      variable == "q49b_G1" ~ "CJ - The criminal justice system deals with cases promptly and efficiently",
      variable == "q48f_G2" ~ "Institutional Perfomance - Prosecutors prosecute crimes committed in an independent manner",
      variable == "q48h_G1" ~ "Institutional Perfomance - The public defenders do everything they can to defend poor people that are accused of committing a crime",
      variable == "q48g_G2" ~ "Institutional Perfomance - The judges decide cases in an independent manner and are not subject to any sort of pressure",
      variable == "q48c_G2" ~ "Police - Are available to help when needed",
      variable == "EXP_q22i_G2" ~ "Police - Serve the interests of the community",
      variable == "EXP_q22h_G2" ~ "Police - Serve the interests of regular citizens",
      variable == "q48b_G2"     ~ "Police - Help them feel safe",
      variable == "q48a_G2"     ~ "Police - Resolve security problems in  the community",
      variable == "q48b_G1"     ~ "Police - Perform effective and lawful investigations",
      variable == "EXP_q24e_G2" ~ "Police - Respond to crime reports",
      variable == "q48a_G1"     ~ "Police - Act lawfully",
      variable == "EXP_q22e_G1" ~ "Police - Do not use excessive force",
      variable == "q48c_G1"     ~ "Police - Respect the rights of suspects",
      variable == "q48d_G2"     ~ "Police - Treat all people with respect",
      variable == "q18a"     ~ "Police - Economic status",
      variable == "EXP_q17g" ~ "Police - Skin color",
      variable == "q18c"     ~ "Police - Ethnic background",
      variable == "q18d"     ~ "Police - Religion",
      variable == "q18e"     ~ "Police - Foreigner status",
      variable == "q2d"         ~ "Police - Are not involved in corrupt practices",
      variable == "q48e_G2"     ~ "Police - Investigate crimes in an independent manner",
      variable == "EXP_q22k_G2" ~ "Police - Do not serve the interests of gangs",
      variable == "EXP_q22j_G2" ~ "Police - Do not serve the interests of politicians",
      variable == "q1d"     ~ "Police - Trust the police",
      variable == "EXP_q8d" ~ "Police - Report a crime when they are a victim",
      variable == "q9"      ~ "Police - Feel safe in their neighborhoods",
      variable == "q48d_G1"         ~ "Police - Are held accountable for violating laws",
      variable == "EXP_q22f_G1"     ~ "Police - Are held accountable for seeking bribes",
      variable == "EXP_q22g_G1"     ~ "Police - Are held accountable for accepting bribes",
      variable == "EXP_q22h_G1"     ~ "Police - Are investigated for misconduct",
      variable == "q50_neg" ~ "Authoritarianism - Government efficiency is more important than citizen influence",
      variable == "q50_pos" ~ "Authoritarianism - It is important that citizens have a say in government matters, even at the expense of efficiency",
      variable == "q51_neg" ~ "Authoritarianism - The president should not be bound by the laws or courts",
      variable == "q51_pos" ~ "Authoritarianism - The president must always obey the law and the courts",
      variable == "q52_neg" ~ "Authoritarianism - It is not necessary to obey the laws of a government that you did not vote for",
      variable == "q52_pos" ~ "Authoritarianism - It is important to obey the government in power, no matter who you voted for",
      variable == "CAR_q60_G1" ~ "Authoritarianism - Censor information that comes \nfrom abroad",
      variable == "CAR_q61_G1" ~ "Authoritarianism - Censor opinions from opposition \ngroups",
      variable == "CAR_q60_G2" ~ "Authoritarianism - Resort to misinformation to shape \npublic opinion in their favor",
      variable == "CAR_q64_G2" ~ "Authoritarianism - Attack or attempt to discredit the \nmedia and civil society organizations\nthat criticize them",
      variable == "CAR_q67_G1" ~ "Authoritarianism - Attack or attempt to discredit \nopposition parties",
      variable == "CAR_q67_G2" ~ "Authoritarianism - Attack or attempt to discredit the \nelectoral system and other \nsupervisory organs", 
      variable == "CAR_q64_G1" ~ "Authoritarianism - Seek to limit the courts' competencies \nand freedom to interpret the law",
      variable == "CAR_q66_G1" ~ "Authoritarianism - Seek to influence the promotion and \nremoval of judges",
      variable == "CAR_q65_G2" ~ "Authoritarianism - Prosecute and convict journalists and \nleaders of civil society organizations     ",
      variable == "CAR_q68_G1" ~ "Authoritarianism - Prosecute and convict members of\nopposition parties                                   ",
      variable == "CAR_q65_G1" ~ "Authoritarianism - Refuse to comply with court rulings \nthat are not in their favor",
      variable == "q45a_G1" ~ "Contrainst - Congress could hypothetically stop a Head of State'sillegal actions",
      variable == "q45b_G1" ~ "Contrainst - Courts could hypothetically stop a Head of State'sillegal actions",
      variable == "q45c_G1" ~ "Contrainst - Citizens could hypothetically stop a Head of State'sillegal actions",
      variable == "q4a" ~ "Bribery - Request a government permit",
      variable == "q4b" ~ "Bribery - Request public benefits",
      variable == "q4c" ~ "Bribery - Obtain a bith certificate",
      variable == "q4d" ~ "Bribery - Secure a place at a public school",
      variable == "q4e" ~ "Bribery - Use a public health service",
      variable == "victim" ~ "Security - Victims in the last 12 months",
      variable == "prop_crimes" ~ "Security - Property crimes",
      variable == "life_crimes" ~ "Security - Crime against life",
      variable == "corr_crimes" ~ "Security - Corruption, financial, and commercial crimes",
      variable == "q8d" ~ "Security - Victims that reported crimes",
      variable == "q8f" ~ "Security - Cases ended in prosecution",
      variable == "q17_1" ~ "Discrimination - Ancestry or national \norigin",
      variable == "q17_2" ~ "Discrimination - Gender",
      variable == "q17_3" ~ "Discrimination - Race",
      variable == "q17_4" ~ "Discrimination - Age",
      variable == "q17_5" ~ "Discrimination - Religion",
      variable == "q17_6" ~ "Discrimination - Height",
      variable == "q17_7" ~ "Discrimination - Weight",
      variable == "q17_8" ~ "Discrimination - Physical\nappearence",
      variable == "q17_9" ~ "Discrimination - Physical or mental \ndisability",
      variable == "q17_10" ~ "Discrimination - Sexual orientation",
      variable == "q17_11" ~ "Discrimination - Education or\nincome level",
      variable == "q17_12" ~ "Discrimination - Nationality or inmigration \nstatus",
      variable == "q17_13" ~ "Discrimination - Shade of \nskin color",
      variable == "q17_14" ~ "Discrimination - Tribe",
      variable == "q17_15" ~ "Discrimination - Clothing or\nhairstyle",
      variable == "q16a" ~ "Discrimination - You are treated with less courtesy or respect than other people",
      variable == "q16b" ~ "Discrimination - You receive poorer service than other people at restaurants or stores",
      variable == "q16c" ~ "Discrimination - People act as if they think you are not smart",
      variable == "q16d" ~ "Discrimination - People act as if they are afraid of you",
      variable == "q16e" ~ "Discrimination - You are threatened or harassed",
      variable == "legal" ~ "A2J - Problem prevalence", 
      variable == "sources_help" ~ "A2J - Sources of help", 
      variable == "fully_resolved" ~ "A2J - Problem solved", 
      variable == "problem_persist" ~ "A2J - Problem persist", 
      variable == "fair" ~ "A2J - Fair process", 
      variable == "difficult_cost" ~ "A2J - Hard to afford the process", 
      variable == "financial_difficulty" ~ "A2J - Financial probles", 
      variable == "time" ~ "A2J - Time of process", 
      variable == "hardships" ~ "A2J - Hardships",
      variable == "CAR_q2b" ~ "Bribery Attitudes - A public officer asking for a bribe to \nspeed up administrative procedures",
      variable == "CAR_q2f" ~ "Bribery Attitudes - A law enforcement officer (police, \ncustoms, immigration, civil guard, \nmilitary police) asking for a bribe",
      variable == "CAR_q2g" ~ "Bribery Attitudes - A company official asking for a bribe\nfrom a job applicant",
      variable == "CAR_q2c" ~ "Bribery Attitudes - A private citizen offering a bribe \nto a public official to speed up \nadministrative procedures                 ",
      variable == "CAR_q2a" ~ "Bribery Attitudes - A public officer being recruited on \nthe basis of family ties and \nfriendship networks",
      variable == "CAR_q2d" ~ "Bribery Attitudes - An elected official taking public funds\nfor private use",
      variable == "CAR_q2e" ~ "Bribery Attitudes - An elected official using stolen public \nfunds to assist his or her community"
    ))
  
  
}

govSupp.df   <- ttest_dem_gpp.fn()
religion.df  <- ttest_dem_gpp.fn(section = "religion")
gender.df    <- ttest_dem_gpp.fn(section = "gender")
area.df      <- ttest_dem_gpp.fn(section = "area")
financial.df <- ttest_dem_gpp.fn(section = "financial")
skin_tone.df <- ttest_dem_gpp.fn(section = "skin_tone")
ethnicity.df <- ttest_dem_gpp.fn(section = "ethnicity")

diffmeans.list <- list(
  "GovSupp"    = govSupp.df,
  "Religion"   = religion.df,
  "Gender"     = gender.df,
  "Area"       = area.df,
  "Income"     = financial.df,
  "Skin Tone"  = skin_tone.df,
  "Ethnicity"  = ethnicity.df
)

openxlsx::write.xlsx(diffmeans.list, file = paste0("Outputs/ttest_differences_",latestYear,"_.xlsx"))
  