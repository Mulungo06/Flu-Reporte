# Cleaning flu data set
clean_flu_data <- function(file1, file2) {
  # Install and Load Necessary Packages
  pacman::p_load(dplyr,
                 openxlsx,
                 janitor,
                 lubridate,
                 stringr,
                 tidyverse)
  
  # Load and clean the first dataset
  data1 <- openxlsx::read.xlsx(file1, sheet = 'Resultados da Análise', startRow = 4) %>%
    janitor::clean_names()
  
  # Function to convert the date
  convert_date <- function(date_str) {
    # Replace month abbreviations with numbers
    month_replacements <- c(
      "JAN" = "01", "FEV" = "02", "MAR" = "03", "ABR" = "04",
      "MAI" = "05", "JUN" = "06", "JUL" = "07", "AGO" = "08",
      "SET" = "09", "OUT" = "10", "NOV" = "11", "DEZ" = "12"
    )
    for (abbr in names(month_replacements)) {
      date_str <- gsub(abbr, month_replacements[[abbr]], date_str)
    }
    # Convert to Date format
    as_date(dmy(date_str))
  }
  
  # Convert the date
  data1 <- data1 %>%
    mutate(across(contains("data"), ~ convert_date(.)))
  
  # Select important columns
  columns_keep <- c("numero_s_de_referencia", "laboratorio", "unidade_de_saude", "provincia_2", "data", "hora", "idade",
                    "sexo", "endereco_do_paciente", "data_2", "data_7", "data_8", "data_9", "data_10",
                    "data_11", "data_12", "result_2", "flu_a", "flu_b", "subtipo", "rsv_result")
  
  # Filter the columns
  filtered_columns <- names(data1)[sapply(names(data1), 
                                          function(col) any(sapply(columns_keep, function(keep) grepl(keep, col, ignore.case = TRUE))))]
  
  data1_filtered <- data1 %>% select(all_of(filtered_columns))
  
  # Remove unnecessary columns
  unnecessary_columns <- c("unidade_saude", "unidade_de_saude_2","hora_2", "hora_3", "hora_4", "hora_5", "hora_6", "hora_7", "hora_8", 
                           "hora_9", 'data_6', "hora_10", "hora_11", "hora_12", "hora_13", "hora_14", "data_5", "data_3", 
                           "data_4")
  
  unnecessary_columns <- intersect(names(data1_filtered), unnecessary_columns)
  data1_final <- data1_filtered %>% select(-all_of(unnecessary_columns))
  
  # Rename columns
  data1_final <- data1_final %>%
    rename(
      codigo_do_site = numero_s_de_referencia,
      codigo_do_lab = laboratorio,
      provincia = provincia_2, 
      local_de_colheita = unidade_de_saude,
      data_da_colheita = data,
      hora_de_colheita = hora,
      residencia_bairro = endereco_do_paciente,
      data_de_entrada = data_2,
      data_da_testagem_flu = data_9,
      data_de_validacao_flu = data_10,
      data_da_testagem_rsv = data_11,
      data_de_validacao_rsv = data_12,
      data_de_testagem_sars = data_7,
      data_de_validacao_sars = data_8,
      resultado_sars = result_2,
      inf_a = flu_a,
      inf_b = flu_b,
      resultado_rsv = rsv_result
    )
  
  # Create empty columns
  data1_final <- data1_final %>%
    mutate(
      data_de_nascimento = NA,
      apdm = NA,
      h1pdm = NA,
      h3 = NA,
      h5 = NA,
      h5a = NA,
      h5b = NA,
      h7 = NA,
      vic = NA,
      yam = NA,
      rsv_a = NA,
      rsvb = NA,
      trl_real_flu = NA,
      trl_sars_co_v_2 = NA,
      trl_real_rsv = NA
    )
  
  # Calculate TRL for all viruses
  data1_final <- data1_final %>% 
    mutate(trl_sars_co_v_2 = as.numeric(data_de_validacao_sars - data_de_entrada),
           trl_real_flu = as.numeric(data_de_validacao_flu - data_de_entrada),
           trl_real_rsv = as.numeric(data_de_validacao_rsv - data_de_entrada))
  
  # Create column called resultados flu
  data1_final <- data1_final %>%
    mutate(resultado_flu = ifelse(inf_a == 'Positivo' | inf_b == 'Positivo', 'Positivo', 'Negativo'))
  
  # Fill the subtype columns based on the result if negative or positive and for which subtype
  data1_final <- data1_final %>%
    mutate(
      h3 = 'Negativo',
      h1pdm = 'Negativo',
      apdm = 'Negativo',
      vic = 'Negativo'
    ) %>%
    mutate(
      h3 = ifelse(subtipo == 'Sazonal H3N2', 'Positivo', h3),
      h1pdm = ifelse(subtipo == 'Sazonal H1N1' | subtipo == 'A pdm(H1N1)09', 'Positivo', h1pdm),
      apdm = ifelse(subtipo == 'Sazonal H1N1' | subtipo == 'A pdm(H1N1)09', 'Positivo', apdm),
      vic = ifelse(subtipo == 'B-VITORIA Parainfluenza-PCR', 'Positivo', vic),
      resultado_sars = ifelse(resultado_sars == 'SARS-CoV-2 Positivo', 'Positivo', ifelse(resultado_sars == 'Negativo para SARS-CoV-2' | resultado_sars == "Negativo para SARS-CoV-2....", 'Negativo', resultado_sars)),
      resultado_rsv = ifelse(resultado_rsv == 'RSV Positivo', 'Positivo', ifelse(resultado_rsv == 'RSV Negativo', 'Negativo', resultado_rsv))
    ) %>%
    select(-c(subtipo))
  
  # Function to standardize the format
  standardize_format <- function(x) {
    # Get the current year
    current_year <- format(Sys.Date(), "%Y")
    # Remove hyphens
    clean_id <- gsub("-", "", x)
    # Check if the string starts with "IRA" and has less than the expected length
    ifelse(grepl("^IRA", clean_id) & nchar(clean_id) < 14, paste0(substr(clean_id, 1, 8), current_year), clean_id)
  }
  # Apply the function to the relevant column
  data1_final <- data1_final %>%
    mutate(codigo_do_site = standardize_format(codigo_do_site))
  
  
  # Load and clean the second dataset
  data2 <- openxlsx::read.xlsx(file2, sheet = '2024') %>%
    janitor::clean_names()
  
  data2 <- data2 %>%
    mutate(across(starts_with("data"), ~ as.Date(as.numeric(.x), origin = "1899-12-30")))
  
  # Ensure both datasets have columns of the same type before combining
  common_cols <- intersect(names(data1_final), names(data2))
  
  data1_final <- data1_final %>%
    mutate(across(all_of(common_cols), as.character))
  
  data2 <- data2 %>%
    mutate(across(all_of(common_cols), as.character))
  
  # Append rows from data1_final to data2
  data_combined <- bind_rows(data2, data1_final)
  
  # Remove unnecessary rows from the original dataset
  data_combined <- data_combined[grepl('^PMB0', data_combined$codigo_do_lab), ]
  
  return(data_combined)
}
