dfall <-
  import(here('data', 'patients.csv')) %>%
  clean_names() %>%
  select(-c(nap_id, cid, kp:kp_other_nm, start_arv_date, enter_date:updated_date)) %>%
  mutate(
    age = as.numeric(age),
    sex = factor(sex,
                 levels = c('Male','Female')),
    nation = factor(nation,
                    levels = c('Thai','Non-Thai'))
  ) %>% 
  left_join(import(here('data', 'followup.csv')) %>%
              clean_names() %>%
              select(-c(hn, hcode, enterdate:update_date)),
              by = 'pat_id') %>% 
  set_variable_labels(
    h_code = 'Hospital',
    sex = 'Sex',
    age = 'Age',
    nation = 'Nationality'
  )
