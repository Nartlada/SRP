df <-
  import(here('data', 'patients.csv')) %>%
  clean_names() %>%
  filter(h_code %in% c('10682', '10947', '11194', '11201', '11539')) %>%
  select(-c(
    nap_id,
    cid,
    kp:kp_other_nm,
    start_arv_date,
    enter_date:updated_date
  )) %>%
  left_join(
    import(here('data', 'hospitals.csv')) %>%
      clean_names() %>%
      select(h_code = code, province_en, h_type2),
    by = 'h_code') %>%
  left_join(
    import(here('data', 'followup.csv')) %>%
      clean_names() %>%
      group_by(pat_id) %>%
      slice_min(order_by = visitdate, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(-c(hn, hcode, enterdate:update_date)),
    by = 'pat_id'
  ) %>%
  mutate(
    across(
      c(ss_break_off_relations:ss_no_visit_hc,
        ss_post_break_off_relations:ss_post_no_visit_hc,
        age,
        de_little_interest:de_hurting_yourself,
        du_tobacco:du_other,
        tl_sameday_result, 
        tl_hi_vvs_aids_result,
        tl_cd4result,
        tl_vl_result,
        tl_uu_result,
        tl_emotion_result,
        tl_stigma_result,
        tl_nutrition_result,
        tl_idu_result,
        tl_gov_health_serv_result,
      ),
      ~ as.numeric(.)
    ),
    na_if(visitdate, '0000-00-00'),
    hiv_diag_date = if_else(
      hiv_diag_date == '0000-00-00' | as.Date(hiv_diag_date) < as.Date('1987-01-01'), 
      NA_character_, 
      hiv_diag_date
    ),
    start_arv_date = if_else(
      as.Date(start_arv_date) < as.Date('1987-01-01'), 
      NA_character_, 
      start_arv_date
    ),
    vl_result = case_when(
      vl_result %in% c('<20', '< 20', 'Not Detected') ~ 0,
      !is.na(vl_result) ~ as.numeric(str_extract(vl_result, "^\\d+")),
      TRUE ~ NA_real_
    ),
    last_cd4result = parse_number(last_cd4result),
    last_cd4result = if_else(last_cd4result > 10000, NA_real_, last_cd4result),
    province_en = fct_relevel(province_en, "Chiang Rai"),
    h_type2 = as.factor(h_type2),
    ss_score_pre = if_else(
      if_all(ss_break_off_relations:ss_no_visit_hc, is.na),
      NA_real_,
      rowSums(across(ss_break_off_relations:ss_no_visit_hc), na.rm = TRUE)
    ),
    ss_pre = factor(
      if_else(ss_score_pre < 16, 0, 1), 
      levels = c(0, 1), 
      labels = c('No', 'Yes')
    ),
    ss_score_post = if_else(
      if_all(ss_post_break_off_relations:ss_post_no_visit_hc, is.na),
      NA_real_,
      rowSums(across(ss_post_break_off_relations:ss_post_no_visit_hc), na.rm = TRUE)
    ),
    ss_post = factor(
      if_else(ss_score_post < 16, 0, 1), 
      levels = c(0, 1), 
      labels = c('No', 'Yes')
    ),
    across(
      c(ss_break_off_relations:ss_no_visit_hc,
        ss_post_break_off_relations:ss_post_no_visit_hc),
      ~ factor(
        .,
        levels = 4:1,
        labels = c('Strongly agree', 'Agree', 'Disagree', 'Strongly disagree')
      )
    ),
    sex = factor(
      sex,
      levels = c('Male', 'Female')
    ),
    agegrp = cut(
      age,
      breaks = c(-Inf, 14, 24, 34, 44, 54, Inf),
      labels = c('<15', '15-24', '25-34', '35-44', '45-54', '>= 55')
    ),
    agegrp2 = cut(
      age,
      breaks = c(-Inf, 24, 34, 44, 54, Inf),
      labels = c('<25', '25-34', '35-44', '45-54', '>= 55')
    ),
    hiv_diag_date = if_else(
      hiv_diag_date %in% c("0000-00-00", "NULL"),
      NA_character_,
      hiv_diag_date
    ),
    hivyears = time_length(interval(hiv_diag_date, visitdate), unit = "years"),
    hivmonths = time_length(interval(hiv_diag_date, visitdate), unit = "months"),
    hivyear = cut(
      hivyears,
      breaks = c(-Inf, 1, 5, 10, Inf),
      labels = c('<= 1 year', '> 1 - 5 years', '> 5 - 10 years', '> 10 years'),
      right = TRUE
    ),
    hiv1year = cut(
      hivyears,
      breaks = c(-Inf, 1, Inf),
      labels = c('<= 1 year', '> 1 year'),
      right = TRUE
    ),
    hiv3month = cut(
      hivmonths,
      breaks = c(-Inf, 3, Inf),
      labels = c('<= 3 months', '> 3 months'),
      right = TRUE
    ),
    hiv6month = cut(
      hivmonths,
      breaks = c(-Inf, 6, Inf),
      labels = c('<= 6 months', '> 6 months'),
      right = TRUE
    ),
    arvyears = case_when(
      start_arv_date == '0000-00-00' ~ 0,
      is.na(start_arv_date) ~ NA_real_,
      TRUE ~ time_length(interval(start_arv_date, visitdate), unit = "years")
    ),
    arvmonths = case_when(
      start_arv_date == '0000-00-00' ~ 0,
      is.na(start_arv_date) ~ NA_real_,
      TRUE ~ time_length(interval(start_arv_date, visitdate), unit = "months")
    ),
    arvyear = cut(
      arvyears,
      breaks = c(-Inf, 0, 1, 5, 10, Inf),
      labels = c('Not on ARV', '<= 1 year', '> 1 - 5 years', '> 5 - 10 years', '> 10 years'),
      right = TRUE
    ),
    arv1year = cut(
      arvyears,
      breaks = c(-Inf, 1, Inf),
      labels = c('<= 1 year', '> 1 year'),
      right = TRUE
    ),
    arv3month = cut(
      arvmonths,
      breaks = c(-Inf, 3, Inf),
      labels = c('<= 3 months', '> 3 months'),
      right = TRUE
    ),
    arv6month = cut(
      arvmonths,
      breaks = c(-Inf, 6, Inf),
      labels = c('<= 6 months', '> 6 months'),
      right = TRUE
    ),
    vlgrp = factor(
      case_when(
        vl_result < 200 ~ 1,
        between(vl_result, 200, 1000) ~ 2,
        vl_result > 1000 ~ 3,
        TRUE ~ NA_integer_
      ),
      levels = 1:3,
      labels = c('<200', '200-1000', '>1000')
    ),
    vlgrp2 = factor(
      case_when(
        vl_result < 200 ~ 1,
        vl_result >= 200 ~ 2,
        TRUE ~ NA_integer_
      ),
      levels = 1:2,
      labels = c('< 200', '>= 200')
    ),
    cd4grp = factor(
      case_when(
        last_cd4result < 200 ~ 1,
        between(last_cd4result, 200, 349) ~ 2,
        last_cd4result > 349 ~ 3,
        TRUE ~ NA_integer_
      ),
      levels = 1:3,
      labels = c('<200', '200-349', '>=350')
    ),
    cd4grp2 = factor(
      case_when(
        last_cd4result < 200 ~ 1,
        last_cd4result >= 200 ~ 2,
        TRUE ~ NA_integer_
      ),
      levels = 1:2,
      labels = c('< 200', '>= 200')
    ),
    arvformula = factor(
      case_when(
        regimen == 'TLD' ~ 1,
        regimen == 'TAF+FTC+DTG' ~ 2,
        regimen == '3TC+DTG' ~ 3,
        regimen == 'NULL' ~ NA_integer_,
        !is.na(regimen) ~ 4,
        TRUE ~ NA_integer_
      ),
      levels = 1:4,
      labels = c('TLD', 'TAF+FTC+DTG', '3TC+DTG', 'Other')
    ),
    depress_score = if_else(
      if_all(de_little_interest:de_hurting_yourself, is.na),
      NA_real_,
      rowSums(across(de_little_interest:de_hurting_yourself), na.rm = TRUE)
    ),
    depress = factor(
      case_when(
        depress_score < 7 ~ 1,
        between(depress_score,7,12) ~ 2,
        between(depress_score,13,18) ~ 3,
        depress_score > 18 ~ 4,
        TRUE ~ NA_integer_
      ),
      levels = 1:4,
      labels = c('No depression', 'Little depression', 'Moderate depression', 'Severe depression')
    ),
    drug = factor(
      if_else(if_any(du_tobacco:du_other, ~ . == 1), "Yes", "No"),
      levels = c("No", "Yes")
    ),
    across(
      c(du_liquor, du_tobacco), 
      ~ factor(
        .,
        levels = c(0, 1),
        labels = c('No', 'Yes')
      )
    ),
    drug_o = factor(
      if_else(if_any(du_idu:du_other, ~ . == 1), "Yes", "No"),
      levels = c("No", "Yes")
    ),
    know_mean = if_else(
      if_all(
        c(tl_sameday_result,
          tl_hi_vvs_aids_result,
          tl_cd4result,
          tl_vl_result,
          tl_uu_result,
          tl_emotion_result,
          tl_stigma_result,
          tl_nutrition_result,
          tl_idu_result,
          tl_gov_health_serv_result), is.na), 
      NA_real_, 
      rowMeans(
        across(
          c(tl_sameday_result,
            tl_hi_vvs_aids_result,
            tl_cd4result,
            tl_vl_result,
            tl_uu_result,
            tl_emotion_result,
            tl_stigma_result,
            tl_nutrition_result,
            tl_idu_result,
            tl_gov_health_serv_result)), 
        na.rm = TRUE
      )
    ),     
    knowledge = factor(
      case_when(
        know_mean <= 1.5 ~ 1,
        know_mean > 1.5 ~ 0,
        TRUE ~ NA_integer_
      ),
      levels = 1:0,
      labels = c('Yes', 'No')
    ),
    adh = factor(
      adh_this_visit,
      levels = 1:3,
      labels = c('>= 95%', '81 - 94%', '< 80%')
    ),
    ltfu = factor(
      lostfu_last_visit,
      levels = 1:2,
      labels = c('Yes', 'No')
    ),
    missmed = factor(
      miss_medicine,
      levels = 1:2,
      labels = c('Yes', 'No')
    ),
  ) %>% 
  select(
    ss_break_off_relations:ss_no_visit_hc, ss_score_pre, ss_pre, 
    ss_post_break_off_relations:ss_post_no_visit_hc, ss_score_post, ss_post,
    h_code, province_en, h_type2,
    sex,
    age, agegrp, agegrp2,
    visitdate,
    hiv_diag_date, hivyears, hivmonths, hivyear, hiv1year, hiv3month, hiv6month,
    start_arv_date, arvyears, arvmonths, arvyear, arv1year, arv3month, arv6month,
    vl_result, vlgrp, vlgrp2,
    last_cd4result, cd4grp, cd4grp2,
    regimen, regimen_other, arvformula,
    type_of_service,
    de_little_interest:de_hurting_yourself, depress,
    du_tobacco:du_other, drug, drug_o,
    tl_sameday_result, 
    tl_hi_vvs_aids_result,
    tl_cd4result,
    tl_vl_result,
    tl_uu_result,
    tl_emotion_result,
    tl_stigma_result,
    tl_nutrition_result,
    tl_idu_result,
    tl_gov_health_serv_result, know_mean, knowledge,
    adh_this_visit, adh,
    lostfu_last_visit, ltfu,
    miss_medicine, missmed,
  ) %>% 
  set_variable_labels(
    ss_pre = 'Self-stigma (Baseline)',
    ss_post = 'Self-stigma (Post immediate)',
    h_code = 'Hospital',
    province_en = 'Province',
    h_type2 = 'Hospital type',
    sex = 'Sex',
    age = 'Age (years)',
    agegrp = 'Age (years)', 
    agegrp2 = 'Age (years)', 
    hivyears = 'Years of HIV infection',
    hivmonths = 'Months of HIV infection',
    hivyear = 'Years of HIV infection',
    hiv1year = 'Years of HIV infection',
    hiv3month = 'Years of HIV infection',
    hiv6month = 'Years of HIV infection',
    arvyears = 'Years on ARV',
    arvmonths = 'Months on ARV',
    arvyear = 'Years on ARV',
    arv1year = 'Years on ARV',
    arv3month = 'Years on ARV',
    arv6month = 'Years on ARV',
    vl_result = 'Last VL (copies/ml)',
    vlgrp = 'Last VL (copies/ml)', 
    vlgrp2 = 'Last VL (copies/ml)', 
    last_cd4result = 'Last CD4 (cells/µL)',
    cd4grp = 'Last CD4 (cells/µL)',
    cd4grp2 = 'Last CD4 (cells/µL)',
    arvformula = 'ARV Formula',
    type_of_service = 'Type of service',
    depress = 'Depression',
    drug = 'Drug use',
    du_tobacco = 'Tobacco',
    du_liquor = 'Alcohol',
    drug_o = 'Other drug',
    knowledge = 'HIV knowledge',
    adh = 'Adherence',
    ltfu = 'Lost to follow up',
    missmed = 'Miss medicine',
    ss_score_pre = 'Self-stigma score (Baseline)',
    ss_score_post = 'Self-stigma score (Post immediate)',
    ss_break_off_relations = ' - ก่อนร่วมกิจกรรม', 
    ss_post_break_off_relations = ' - หลังร่วมกิจกรรม',
    ss_make_it_worse = ' - ก่อนร่วมกิจกรรม', 
    ss_post_make_it_worse = ' - หลังร่วมกิจกรรม',
    ss_ashamed = ' - ก่อนร่วมกิจกรรม', 
    ss_post_ashamed = ' - หลังร่วมกิจกรรม',
    ss_karma = ' - ก่อนร่วมกิจกรรม',
    ss_post_karma = ' - หลังร่วมกิจกรรม',
    ss_hopeless = ' - ก่อนร่วมกิจกรรม',
    ss_post_hopeless = ' - หลังร่วมกิจกรรม',
    ss_futureless = ' - ก่อนร่วมกิจกรรม',
    ss_post_futureless = ' - หลังร่วมกิจกรรม',
    ss_family_ashamed = ' - ก่อนร่วมกิจกรรม',
    ss_post_family_ashamed = ' - หลังร่วมกิจกรรม',
    ss_no_visit_hc = ' - ก่อนร่วมกิจกรรม',
    ss_post_no_visit_hc = ' - หลังร่วมกิจกรรม',
  )
