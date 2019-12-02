#edited by Moh
pin_calculation <- function(data) {
  lookup_table <- 
    data.frame("1" = c(1,1,2,2,3),
               "2" = c(2,2,2,3,3),
               "3" = c(2,3,3,3,4),
               "4" = c(3,3,4,4,4),
               "5" = c(3,4,4,5,5))
  
  data_r <- data %>%
    new_recoding(target = humanitarian_condition_score) %>%
    recode_directly(ifelse(phywell_median > lvgs_median | phywell_median > coping_median, 
                           phywell_median,
                           lookup_table[lvgs_median, coping_median])) %>%
    end_recoding()
  return(data_r)
}


dd <- un_r[1:10,] %>%
  select(wash_lvgs_median, wash_coping_median, wash_phywell_median)

names(dd) <- c("lvgs_median", "coping_median", "phywell_median")

un_response <- 
  un_response %>%
  new_recoding(target = wash_humanitarian_condition) %>%
  recode_directly(ifelse(wash_phywell_median > wash_lvgs_median | wash_phywell_median > wash_coping_median, 
                         wash_phywell_median,
                         lookup_table[wash_lvgs_median, wash_coping_median])) %>%
  new_recoding(target = wash_pin) %>%
  recode_directly(ifelse(wash_humanitarian_condition >= 3, "in need", "not inneed")) %>%
  new_recoding(target = wash_severe_pin, source = wash_humanitarian_condition) %>%
  recode_to(to = "severe pin", where.num.larger.equal = 4) %>%
  recode_to(to = "pin", where.num.equal = 3) %>%
  recode_to(to = "not pin", where.num.smaller.equal = 2) %>%
  end_recoding()

un_response$wash_humanitarian_condition <- as.character(un_response$wash_humanitarian_condition)
short_df <- un_response  %>%
  select(region, settlement, yes_no_host, strata, wash_humanitarian_condition, wash_pin, wash_severe_pin)

analysisplan<-make_analysisplan_all_vars(short_df,
                                         questionnaire,
                                         independent.variable = "yes_no_host",
                                         repeat.for.variable = "region",
                                         hypothesis.type = "group_difference") 
analysisplan <- analysisplan[c(5:7), ]
analysisplan

strata_weight_fun <- map_to_weighting(sampling.frame = samplingframe,
                                      sampling.frame.population.column = "Population",
                                      sampling.frame.stratum.column = "strata",
                                      data.stratum.column = "strata")

short_df$general_weights <- strata_weight_fun(short_df)

results <- from_analysisplan_map_to_output(short_df, 
                                           analysisplan = analysisplan,
                                           weighting = strata_weight_fun,
                                           cluster_variable_name = "settlement",
                                           questionnaire)


hypegrammaR:::map_to_generic_hierarchical_html(results,
                                               render_result_with = hypegrammaR:::from_result_map_to_md_table,
                                               by_analysisplan_columns = c("dependent.var","repeat.var.value"),
                                               by_prefix =  c("",""),
                                               level = 2,
                                               questionnaire = questionnaire,
                                               label_varnames = TRUE,
                                               dir = "./output",
                                               filename = "summary_by_dependent_var_then_by_repeat_var.html"
)
browseURL("summary_by_dependent_var_then_by_repeat_var.html")

big_table <- results$results %>% lapply(function(x) x[["summary.statistic"]]) %>% do.call(rbind, .)

#pin disagregation geographic area / affected group
pin_table <- big_table %>%
  select(-c(se, min, max)) %>%
  filter(dependent.var == "wash_pin", 
         dependent.var.value == "in need") %>%
  spread(independent.var.value, numbers) %>%
  select(region = repeat.var.value,
         hc = yes,
         idp = no)

#severity phases
## creating table
severity_phase <- big_table %>%
  select(-c(se, min, max)) %>%
  filter(dependent.var == "wash_humanitarian_condition") %>%
  spread(dependent.var.value, numbers) %>%
  mutate(independent.var.value = as.character(independent.var.value)) %>%
  select(-c(dependent.var, independent.var, repeat.var)) %>%
  rename(region = repeat.var.value,
         population_group = independent.var.value) %>%
  arrange(region)

## renaming population group
severity_phase$population_group[severity_phase$population_group == "yes"] <- "hc"
severity_phase$population_group[severity_phase$population_group == "no"] <- "idp"


## adding missing severity phase
if(sum(c(1:5) %in% names(severity_phase)) != 5) {
  missing_severity_class <- c(1:5)[!(c(1:5) %in% names(severity_phase))]
  missing_severity_class <- as.character(missing_severity_class)
  severity_phase[, missing_severity_class] <- 0
}
severity_phase[is.na(severity_phase)] <- 0

## adding "s" to the severity score names to avoid confusion
names(severity_phase)[names(severity_phase) == "1"] <- "s1"
names(severity_phase)[names(severity_phase) == "2"] <- "s2"
names(severity_phase)[names(severity_phase) == "3"] <- "s3"
names(severity_phase)[names(severity_phase) == "4"] <- "s4"
names(severity_phase)[names(severity_phase) == "5"] <- "s5"

severity_phase
##selection of the phase
severity_phase <- severity_phase %>%
  new_recoding(target = education_severity_phase) %>%
  recode_directly(ifelse(s5 > .2, 5,
                         ifelse((s4 + s5) > 0.2, 4, 
                                ifelse((s3 + s4 + s5) > 0.2, 3,
                                       ifelse((s2 + s3 + s4 + s5) > 0.2, 2,
                                              1))))) %>%
  end_recoding()
severity_phase
data_frame_to_write <- list(un_response, pin_table, severity_phase, big_table)
names_data_frame_to_write <- c("dataset.csv", "pin_table.csv", "severity_phase.csv", "big_table.csv")
names_data_frame_to_write <- paste0("output/unicef/wash/", names_data_frame_to_write)

mapply(write.csv, x = data_frame_to_write, file = names_data_frame_to_write, row.names = F)
file.copy(from = "output/summary_by_dependent_var_then_by_repeat_var.html", to = "output/unicef/wash/")
