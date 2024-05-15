#read sheet and transform data
qt_e7_studydat = read.csv('data/Quinlan and Taylor/QT-2019-E4.csv')

# Test phase summary
qt_e7_studydat %>%
  group_by(participant, condition, is_old) %>%
  summarize(n=n(), mean=hits) -> qt_e7_test_sum

#d' calculations 
qt_e7_test_sum %>%
  select(participant, condition, is_old, mean) %>%
  mutate(instruction = paste0(condition, is_old)) %>%
  select(-condition, -is_old) %>%
  pivot_wider(id_cols = participant, names_from = 'instruction', values_from = 'mean') %>%
  mutate(read_d = calc_d_between(read1, read0, cor = .004), 
         sing_d = calc_d_between(sing1, sing0, cor = .004), 
         speak_d = calc_d_between(speak1, speak0, cor = .004)) %>%
  select(participant, read_d:speak_d) %>%
  pivot_longer(cols = c(read_d:speak_d), names_to = 'condition', values_to = 'd') %>%
  drop_na(d) -> qt_e7_d_sum

#d cor
cor(qt_e7_d_sum[qt_e7_d_sum$condition=='sing_d',]$d, qt_e7_d_sum[qt_e7_d_sum$condition=='speak_d',]$d)