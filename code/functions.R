produce_table_2 <- function(df) {

t0 <- df %>%
  select(sex, age, agegrp, province_en, h_type2, hivyears, hivyear, arvyears, arvyear, vl_result, vl, last_cd4result, cd4, arvformula) %>%
  tbl_summary(
    type = c(age, hivyears, arvyears, vl_result, last_cd4result) ~ "continuous2",
    statistic = c(age, hivyears, arvyears, vl_result, last_cd4result) ~ c("{mean}, {sd}", "{min}, {max}", "{median} ({p25}, {p75})"),
    digits = list(
      c(age, hivyears, arvyears, vl_result, last_cd4result) ~ c(1, 1, 0, 0, 1, 1, 1),
      all_categorical() ~ c(0, 1)
    ),
    missing = 'ifany',
  ) %>% 
  bold_labels()

t1 <- df %>%
  select(ss_pre, sex, age, agegrp, province_en, h_type2, hivyears, hivyear, arvyears, arvyear, vl_result, vl, last_cd4result, cd4, arvformula) %>%
  mutate(ss_pre = fct_explicit_na(ss_pre, na_level = "Unknown")) %>% 
  tbl_summary(
    by = ss_pre,
    type = c(age, hivyears, arvyears, vl_result, last_cd4result) ~ "continuous2",
    statistic = c(age, hivyears, arvyears, vl_result, last_cd4result) ~ c("{mean}, {sd}", "{min}, {max}", "{median} ({p25}, {p75})"),
    digits = list(
      c(age, hivyears, arvyears, vl_result, last_cd4result) ~ c(1, 1, 0, 0, 1, 1, 1),
      all_categorical() ~ c(0, 1)
    ),
    missing = 'ifany',
  )

t2 <- df %>%
  select(ss_post, sex, age, agegrp, province_en, h_type2, hivyears, hivyear, arvyears, arvyear, vl_result, vl, last_cd4result, cd4, arvformula) %>%
  mutate(ss_post = fct_explicit_na(ss_post, na_level = "Unknown")) %>% 
  tbl_summary(
    by = ss_post,
    type = c(age, hivyears, arvyears, vl_result, last_cd4result) ~ "continuous2",
    statistic = c(age, hivyears, arvyears, vl_result, last_cd4result) ~ c("{mean}, {sd}", "{min}, {max}", "{median} ({p25}, {p75})"),
    digits = list(
      c(age, hivyears, arvyears, vl_result, last_cd4result) ~ c(1, 1, 0, 0, 1, 1, 1),
      all_categorical() ~ c(0, 1)
    ),
    missing = 'ifany',
  )

tbl_merge(
  list(t0, t1, t2),
  tab_spanner = c("**Total**", "**Self-stigma before SRP**", "**Self-stigma after SRP**")
)

}