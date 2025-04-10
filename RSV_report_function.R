# Function to clean RSV data set
clean_rsv_data <- function(file1, file2) {
  pacman::p_load(dplyr, openxlsx, janitor, lubridate, stringr, tidyverse)
  
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
  columns_keep <- c("numero_s_de_referencia", "laboratorio", "unidade_de_saude", "provincia_2", "data", "idade",
                    "sexo", "data_2", "rsv_result", "subtipo")
  
  # Filter the columns
  filtered_columns <- names(data1)[sapply(names(data1), 
                                          function(col) any(sapply(columns_keep, function(keep) grepl(keep, col, ignore.case = TRUE))))]
  
  data1_filtered <- data1 %>% select(all_of(filtered_columns))
  
  # Remove unnecessary columns
  unnecessary_columns <- c("unidade_saude", "unidade_de_saude_2", "hora", "hora_2", "hora_3", "hora_4", "hora_5", "hora_6", "hora_7", "hora_8", 
                           "hora_9", 'data_6', "hora_10", "hora_11", "hora_12", "hora_13", "hora_14", "data_5", "data_3", 
                           "data_4", "endereco_do_paciente", "data_7", "data_8", "data_9", "data_10",
                           "data_11", "data_12", "result_2", "subtipo", 'provincia_2', 'unidade_de_saude')
  
  unnecessary_columns <- intersect(names(data1_filtered), unnecessary_columns)
  data1_final <- data1_filtered %>% select(-all_of(unnecessary_columns))
  
  # Assuming 'data1_final' is your dataframe and 'idade' is a character column
  data1_final$idade <- as.character(data1_final$idade)
  
  # Extract years and months from the 'idade' column
  years <- as.numeric(sub("a.*", "", data1_final$idade))
  months <- as.numeric(sub(".*m", "", data1_final$idade))
  data1_final$idade <- ifelse(!is.na(years), round(years * 12, 0), months)
  
  # Adjust the 'sexo' column
  data1_final <- data1_final %>%
    mutate(sexo = ifelse(sexo == 'M', 1, ifelse(sexo == 'F', 0, sexo)))
  
  # Rename columns
  data1_final <- data1_final %>%
    rename(
      codigo_do_site = numero_s_de_referencia,
      lab_id = laboratorio,
      'spec_dt Data de colheita' = data,
      'spec_recd_dt Data recebida' = data_2,
      rsv_result = rsv_result,
      "sex 1M /2F" = sexo,
      age_mo = idade
    )
  
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
  
  # Create the 'hosp_id' and 'pt_id' columns
  data1_final <- data1_final %>%
    mutate(
      hosp_id = substr(codigo_do_site, 4, 5),  
      pt_id = substr(codigo_do_site, 6, 8)
    )
  
  # Create empty columns
  data1_final <- data1_final %>%
    mutate(
      country = "Mozambique",
      birth_dt = NA,
      wt_kg = NA,
      ward_icu = NA,
      onset_dt = NA,
      admit_dt = NA,
      spec_dt = NA,
      spec_type = NA,
      temperature = NA,
      resp_rate = NA,
      sp_o2 = NA,
      fever_reported = NA,
      chest_indraw = NA,
      wheeze = NA,
      icmi_sign = NA,
      apnea = NA,
      resp_support = NA,
      premature = NA,
      cong_heart = NA,
      chronic_lung = NA,
      immunocompromised = NA,
      rsv_A_ct = NA,
      rsv_B_ct = NA,
      rsv_POS_ct = NA,
      flu_result = NA, 
      flu_ct = NA
    )
  
  # Create 'flu_result' column and adjust values
  data1_final <- data1_final %>%
    mutate(
      flu_result = ifelse(flu_result == 'Sazonal H3N2', 2, 
                          ifelse(flu_result == 'A pdm(H1N1)09', 1, 
                                 ifelse(flu_result == 'B-VITORIA Parainfluenza-PCR', 4,
                                        ifelse(is.na(flu_result), 0, flu_result)))),
      rsv_result = ifelse(rsv_result == 'RSV Positivo', 3, 
                          ifelse(rsv_result == 'RSV Negativo', 0, rsv_result))
    )
  data1_final <- data1_final %>% janitor::clean_names()
  
  # Load and clean the second dataset
  data2 <- openxlsx::read.xlsx(file2) %>%
    janitor::clean_names()
  
  data2$birth_dt <- as.Date(data2$birth_dt, format="%m/%d/%Y")
  data2$spec_dt_data_de_colheita <- as.Date(data2$spec_dt_data_de_colheita, format="%m/%d/%Y")
  data2$spec_recd_dt_data_recebida <- as.Date(data2$spec_recd_dt_data_recebida, format="%m/%d/%Y")
  
  # Ensure both datasets have columns of the same type before combining
  common_cols <- intersect(names(data1_final), names(data2))
  
  data1_final <- data1_final %>%
    mutate(across(all_of(common_cols), as.character))
  
  data2 <- data2 %>%
    mutate(across(all_of(common_cols), as.character))
  
  # Append rows from data1_final to data2
  data_combined <- bind_rows(data2, data1_final)
  
  # Remove unnecessary rows from the original dataset
  data_combined <- data_combined[grepl('^PMB0', data_combined$lab_id), ]
  
  data_combined <- data_combined %>% select(-c(codigo_do_site, fever_reported, ward_icu, onset_dt,spec_dt, spec_type))
  
  # Return the combined data
  return(data_combined)
}