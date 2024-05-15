#participant 34 seems to have been excluded
#read sheet and transform data
qt_e5_studydat = read.csv('data/Quinlan and Taylor/QT-2019-E2.csv')
qt_e5_studydat <- tibble::rowid_to_column(qt_e5_studydat, "participant")
qt_e5long <- gather(qt_e5_studydat, condition, m, Proportion_SingHits:Proportion_SilentFA, factor_key=TRUE)
qt_e5long <- qt_e5long[order(qt_e5long$participant, qt_e5long$condition), ]
qt_e5long$is_old_factor <- rep(c("1","0"),108)
levels(qt_e5long$condition)[levels(qt_e5long$condition) == "Proportion_SingHits" | levels(qt_e5long$condition) == "Proportion_SingFA"] <- "sing"
levels(qt_e5long$condition)[levels(qt_e5long$condition) == "Proportion_AloudHits" | levels(qt_e5long$condition) == "Proportion_AloudFA"] <- "speak"
levels(qt_e5long$condition)[levels(qt_e5long$condition) == "Proportion_SilentHits" | levels(qt_e5long$condition) == "Proportion_SilentFA"] <- "read"

# Test phase summary
qt_e5long %>%
  group_by(participant, condition, is_old_factor) %>%
  summarize(n=n(), mean=m) -> qt_e5_test_sum

qt_e5_test_sum <- qt_e5_test_sum %>%
  filter(!participant=='34')

#d' calculations 
qt_e5_test_sum %>%
  select(participant, condition, is_old_factor, mean) %>%
  mutate(instruction = paste0(condition, is_old_factor)) %>%
  select(-condition, -is_old_factor) %>%
  pivot_wider(id_cols = participant, names_from = 'instruction', values_from = 'mean') %>%
  mutate(read_d = calc_d(read1, read0, cor = .013), 
         sing_d = calc_d(sing1, sing0, cor = .013), 
         speak_d = calc_d(speak1, speak0, cor = .013)) %>%
  select(participant, read_d:speak_d) %>%
  pivot_longer(cols = c(read_d:speak_d), names_to = 'condition', values_to = 'd') -> qt_e5_d_sum

#d cor
cor(qt_e5_d_sum[qt_e5_d_sum$condition=='sing_d',]$d, qt_e5_d_sum[qt_e5_d_sum$condition=='speak_d',]$d)

cor(qt_e5_d_sum[qt_e5_d_sum$condition=='read_d',]$d, qt_e5_d_sum[qt_e5_d_sum$condition=='speak_d',]$d)

cor(qt_e5_d_sum[qt_e5_d_sum$condition=='sing_d',]$d, qt_e5_d_sum[qt_e5_d_sum$condition=='read_d',]$d)

#descriptives for d' (mean and sd)
qt_e5_d_sum_desc <- data.frame(condition=c("Sing d", "Speak d", "read d"),
                               average = c(mean(qt_e5_d_sum[qt_e5_d_sum$condition == 'sing_d',]$d), 
                                           mean(qt_e5_d_sum[qt_e5_d_sum$condition == 'speak_d',]$d),
                                           mean(qt_e5_d_sum[qt_e5_d_sum$condition == 'read_d',]$d)),
                               sd = c(sd(qt_e5_d_sum[qt_e5_d_sum$condition == 'sing_d',]$d), 
                                      sd(qt_e5_d_sum[qt_e5_d_sum$condition == 'speak_d',]$d),
                                      sd(qt_e5_d_sum[qt_e5_d_sum$condition == 'read_d',]$d)
                               ))
#descriptives for a' (mean and sd)
qt_e5_a_sum_desc <- data.frame(condition=c("Sing a", "Speak a", "read a"),
                               average = c(mean(qt_e5_a_sum[qt_e5_a_sum$condition == 'sing_a',]$a), 
                                           mean(qt_e5_a_sum[qt_e5_a_sum$condition == 'speak_a',]$a),
                                           mean(qt_e5_a_sum[qt_e5_a_sum$condition == 'read_a',]$a)),
                               sd = c(sd(qt_e5_a_sum[qt_e5_a_sum$condition == 'sing_a',]$a), 
                                      sd(qt_e5_a_sum[qt_e5_a_sum$condition == 'speak_a',]$a),
                                      sd(qt_e5_a_sum[qt_e5_a_sum$condition == 'read_a',]$a)
                               ))

#sing vs. speak
t.test(corrh~condition, data = filter(qt_e5_corrh_sum, condition != "corrh_read"), paired = TRUE, var.equal = FALSE)

#d' t-tests
#sing vs. speak
t.test(d~condition, data = filter(qt_e5_d_sum, condition != "read_d"), paired = TRUE, var.equal = FALSE)
#sing vs. speak a'
t.test(a~condition, data = filter(qt_e5_a_sum, condition != "read_a"), paired = TRUE, var.equal = FALSE)
#sing vs. speak
t.test(corrh~condition, data = filter(qt_e5_corrh_sum, condition != "corrh_read"), paired = TRUE, var.equal = FALSE)

qt_e5_d_sum %>%
  select(participant, condition, d) %>%
  pivot_wider(id_cols = participant, names_from = 'condition', values_from = 'd') %>%
  filter(sing_d > speak_d) %>%
  select(participant, read_d:speak_d) %>%
  pivot_longer(cols=c(read_d:speak_d), names_to = 'condition', values_to = 'd') -> qt_e5_sse_sum

qt_e5_d_sum %>%
  select(participant, condition, d) %>%
  pivot_wider(id_cols = participant, names_from = 'condition', values_from = 'd') %>%
  filter(sing_d <= speak_d) %>%
  select(participant, read_d:speak_d) %>%
  pivot_longer(cols=c(read_d:speak_d), names_to = 'condition', values_to = 'd') -> qt_e5_ase_sum

by(qt_e5_sse_sum$d, list(qt_e5_sse_sum$condition), mean)
by(qt_e5_ase_sum$d, list(qt_e5_ase_sum$condition), mean)
