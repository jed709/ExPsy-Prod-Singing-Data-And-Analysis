# load data

temp1 = read.xlsx('data/Zhang/Zhang2024.xlsx', sheet=1, cols = 1:5) %>%
  rename(PID = Remember) %>%
  select(-`Hit.number`)

temp2 = read.xlsx('data/Zhang/Zhang2024.xlsx', sheet=1, cols = 6:11) %>%
  rename(sing2 = sing,
         silent2 = silent,
         aloud2 = aloud) %>%
  select(-Know)

# calculate hit rates as proportion

dat = cbind(temp1,temp2) %>%
  mutate(sing = (sing + sing2)/80,
         silent = (silent + silent2)/80,
         aloud = (aloud + aloud2)/80,
         FA = FA/240) %>%
  select(-c(sing2:aloud2))

# calculate descriptives/d'

dat %>%
  group_by(PID) %>%
  pivot_longer(cols = c(sing:FA), names_to = 'condition', values_to = 'hits') %>%
  group_by(condition) %>%
  summarise(m = mean(hits), sd = sd(hits))

dat %>%
  group_by(PID) %>%
  mutate(sing_d = calc_d(sing, FA, cor = .006),
         silent_d = calc_d(silent, FA, cor = .006),
         aloud_d = calc_d(aloud, FA, cor = .006)) %>%
  select(-c(sing:FA)) %>%
  pivot_longer(cols = sing_d:aloud_d, names_to = 'condition', values_to = 'd') -> d_sum


# calculate averages 

d_sum %>%
  group_by(condition) %>%
  summarise(m = mean(d), sd = sd(d))

# calculate d cors

cor(d_sum[d_sum$condition=='sing_d',]$d, d_sum[d_sum$condition=='aloud_d',]$d)

cor(d_sum[d_sum$condition=='sing_d',]$d, d_sum[d_sum$condition=='silent_d',]$d)

cor(d_sum[d_sum$condition=='aloud_d',]$d, d_sum[d_sum$condition=='silent_d',]$d)

