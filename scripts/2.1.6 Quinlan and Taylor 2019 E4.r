#read sheet and transform data
qt_e7_studydat = read.csv('data/Quinlan and Taylor/QT-2019-E4.csv')

# Test phase summary
qt_e7_studydat %>%
  group_by(participant, condition, is_old) %>%
  summarize(n=n(), mean=hits) -> qt_e7_test_sum

#d' calculations 
qt_e7_test_sum %>%
  select(participant, condition, is_old, mean) %>%
  pivot_wider(id_cols = c(participant, condition), names_from = 'is_old', values_from = 'mean') %>%
  mutate(d = calc_d(`1`, `0`, cor = .004)) %>%
  select(participant, condition, d) -> qt_e7_d_sum