#general recoding

response <- 
  response %>% 
  #Dates: converting dates from excel format
  dplyr::mutate(today = as_date(today, origin = "1899-12-30"),
                left_aoo = as_date(left_aoo, origin = "1899-12-30"),
                arrived_current = as_date(arrived_current, origin = "1899-12-30"),
                diff_today_aoo = as.period(today - left_aoo) %/% months(1)) %>%
  dplyr::rename(diff_today_current = difference_arrived_today_days,
                diff_current_aoo = difference_arrived_left_days) %>%
  dplyr::mutate(diff_today_current = as.period(today - arrived_current) %/% months(1),
                diff_current_aoo = as.period(arrived_current - left_aoo) %/% months(1)) %>%
  dplyr::rename(diff_today_aoo_months = diff_today_aoo,
                diff_today_current_months = diff_today_current,
                diff_current_aoo_months = diff_current_aoo)

response <-
  response %>%
  ### income_middle_point
  new_recoding(target = income_middle, source = average_income) %>%
  recode_to(to = 200, where.selected.exactly = "200more") %>%
  recode_to(to = 175, where.selected.exactly = "151_200") %>%
  recode_to(to = 125, where.selected.exactly = "101_150") %>%
  recode_to(to = 80, where.selected.exactly = "61_100") %>%
  recode_to(to = 45, where.selected.exactly = "31_60") %>%
  recode_to(to = 15, where.selected.exactly = "less30") %>%
  recode_to(to = 10, where.selected.exactly = "none") %>%
  ### education
  new_recoding(target = spent_education_middle) %>%
  recode_to(to = 5, where.selected.exactly = "usd_0_9", source = cash_bracket_education) %>%
  recode_to(to = 30, where.selected.exactly = "usd_10_50", source = cash_bracket_education) %>%
  recode_to(to = 75, where.selected.exactly = "usd_50_100", source = cash_bracket_education) %>%
  recode_to(to = 100, where.selected.exactly = "usd_more_100", source = cash_bracket_education) %>%
  # recode_to(to = 0, where.selected.exactly =  "no", source = pay_education) %>%
  # recode_to(to = "dd", where.selected.exactly = "dnk") %>%
  ### health
  new_recoding(target = spent_health_middle, source = cash_bracket_treatment) %>%
  recode_to(to = 5, where.selected.exactly = "usd_0_9") %>%
  recode_to(to = 30, where.selected.exactly = "usd_10_50") %>%
  recode_to(to = 75, where.selected.exactly = "usd_50_100") %>%
  recode_to(to = 100, where.selected.exactly = "usd_more_100") %>%
  recode_to(to = 0, where.selected.exactly =  "no", source = pay_health) %>%
  # recode_to(to = "dd", where.selected.exactly = "dnk") %>%
  ### water
  new_recoding(target = spent_water_middle, source = how_much_pay_water) %>%
  recode_to(to = 30, where.selected.exactly = "less_than_10") %>%
  recode_to(to = 45, where.selected.exactly = "11_20_month") %>%
  recode_to(to = 75, where.selected.exactly = "21_30_month") %>%
  recode_to(to = 105, where.selected.exactly = "31_40_month") %>%
  recode_to(to = 135, where.selected.exactly = "41_50_month") %>%
  recode_to(to = 150, where.selected.exactly = "50_more_month") %>%
  # recode_to(to = "dd", where.selected.exactly = "dnk") %>%
  ### food
  new_recoding(target = spent_food_middle, source = spent_food) %>%
  recode_to(to = 30, where.selected.exactly = "less10") %>%
  recode_to(to = 45, where.selected.exactly = "11_20") %>%
  recode_to(to = 75, where.selected.exactly = "21_30") %>%
  recode_to(to = 105, where.selected.exactly = "31_40") %>%
  recode_to(to = 135, where.selected.exactly = "41_50") %>%
  recode_to(to = 165, where.selected.exactly = "51_60") %>%
  recode_to(to = 195, where.selected.exactly = "61_70") %>%
  recode_to(to = 225, where.selected.exactly = "71_80") %>%
  recode_to(to = 255, where.selected.exactly = "81_90") %>%
  recode_to(to = 285, where.selected.exactly = "91_100") %>%
  recode_to(to = 300, where.selected.exactly = "100more") %>%
  # recode_to(to = "dd", where.selected.exactly = "dnk") %>%
  end_recoding()


y_n_to_zero <- function(dataset, y_n_col) {
  y_n_col_sym <- sym(y_n_col)
  score_col <- sym(sub(pattern = "_y_n", x = y_n_col, replacement = ""))
  dataset_r <- dataset %>%
    dplyr::mutate(!!score_col := ifelse(!!y_n_col_sym == "no", 0, !!score_col))
  return(dataset_r[[score_col]])
}

fcs_y_n_names <- grep("y_n", names(response), value = T)
new_fcs <- lapply(fcs_y_n_names, y_n_to_zero, dataset = response) %>% 
  do.call(cbind, .) %>% as.data.frame()
fcs_names <- sub(pattern = "_y_n", x = fcs_y_n_names, replacement = "")
names(new_fcs) <- fcs_names

response[, fcs_names] <- new_fcs
