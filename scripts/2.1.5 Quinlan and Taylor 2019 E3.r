#read sheet and transform data
qt_e6_studydat = read.csv('data/Quinlan and Taylor/QT-2019-E3.csv')
qt_e6_studydat <- tibble::rowid_to_column(qt_e6_studydat, "participant")
qt_e6long <- gather(qt_e6_studydat, condition, m, Proportion_SingHits:Proportion_SilentFA, factor_key=TRUE)
qt_e6long <- qt_e6long[order(qt_e6long$participant, qt_e6long$condition), ]
qt_e6long$is_old_factor <- rep(c("1","0"),129)
levels(qt_e6long$condition)[levels(qt_e6long$condition) == "Proportion_SingHits" | levels(qt_e6long$condition) == "Proportion_SingFA"] <- "sing"
levels(qt_e6long$condition)[levels(qt_e6long$condition) == "Proportion_AloudHits" | levels(qt_e6long$condition) == "Proportion_AloudFA"] <- "speak"
levels(qt_e6long$condition)[levels(qt_e6long$condition) == "Proportion_SilentHits" | levels(qt_e6long$condition) == "Proportion_SilentFA"] <- "read"

# Test phase summary
qt_e6long %>%
  group_by(participant, condition, is_old_factor) %>%
  summarize(n=n(), mean=m) -> qt_e6_test_sum

#d' calculations 
qt_e6_test_sum %>%
  select(participant, condition, is_old_factor, mean) %>%
  mutate(instruction = paste0(condition, is_old_factor)) %>%
  select(-condition, -is_old_factor) %>%
  pivot_wider(id_cols = participant, names_from = 'instruction', values_from = 'mean') %>%
  mutate(read_d = calc_d(read1, read0, cor = .013), 
         sing_d = calc_d(sing1, sing0, cor = .013), 
         speak_d = calc_d(speak1, speak0, cor = .013)) %>%
  select(participant, read_d:speak_d) %>%
  pivot_longer(cols = c(read_d:speak_d), names_to = 'condition', values_to = 'd') -> qt_e6_d_sum

#d cor
cor(qt_e6_d_sum[qt_e6_d_sum$condition=='sing_d',]$d, qt_e6_d_sum[qt_e6_d_sum$condition=='speak_d',]$d)

cor(qt_e6_d_sum[qt_e6_d_sum$condition=='read_d',]$d, qt_e6_d_sum[qt_e6_d_sum$condition=='speak_d',]$d)

cor(qt_e6_d_sum[qt_e6_d_sum$condition=='sing_d',]$d, qt_e6_d_sum[qt_e6_d_sum$condition=='read_d',]$d)