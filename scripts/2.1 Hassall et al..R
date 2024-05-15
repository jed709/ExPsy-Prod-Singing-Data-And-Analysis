
# Header key for Hassall data files
# 
# Behavioural File Columns (Column number, variable name in script, description, type, levels)
# 
# memory1_XX.mat: training data (data1)
# - Column 1: trial_counter, trial number, int, (1-240)
# - Column 2: word_type, word condition number, int, (1 = sing, 2 = read, 3 = silent)
# - Column 3: choose_colours, colour-condition mapping for this id, int, (1 = RYB, 2 = YBR, 3 = BRY)
# - Column 4: trial_OK, flag to indicate a valid encoding trial, boolean, (0 = spoke/sang too early, 1 = good trial)
# 
# memory2_XX.mat: testing data (data2)
# - Column 1: trial_counter, trial number, int, (1-480)
# - Column 2: word_type, word condition number, int, (1 = sing, 2 = read, 3 = silent)
# - Column 3: rt, time in seconds from word presentation to yes/no response, float
# - Column 4: accuracy, word recall performance, int, (1 = hit, 2 = false alarm, 3 = correct rejection, 4 = miss)

# Get all test phase files
test_phase_files = list.files('data/Hassall et al/Behavioural/', pattern='memory2', full.names = TRUE)

# Function to read-in hassall data (perhaps move to functions script)
read_hassall_data = function(x)
{
  dat = readMat(x) %>%
    data.frame() %>%
    rename('trial' = "data2.1", "prod_code" = "data2.2", "rt" = "data2.3", "resp_type" = "data2.4") %>%
    mutate(
      production = case_when(
        prod_code==1 ~ "sing",
        prod_code==2 ~ "speak",
        prod_code==3 ~ "read",
        TRUE ~ "ERROR"),
      resp_type = case_when(
        resp_type==1 ~ "h",
        resp_type==2 ~ "fa",
        resp_type==3 ~ "cr",
        resp_type==4 ~ "m",
        TRUE ~ "ERROR"
      ),
      is_old = case_when(
        resp_type %in% c('h', 'm') ~ 1,
        resp_type %in% c('fa', 'cr') ~ 0,
        TRUE ~ -9999
      ), said_old = case_when(
        resp_type %in% c('h', 'fa') ~ 1,
        resp_type %in% c('m', 'cr') ~ 0,
        TRUE ~ -9999
      ), id = x) %>%
    select(id, trial:said_old)
  
  return(dat)
}

# Read in test phase data
hassall_dat = purrr::map_df(test_phase_files, read_hassall_data) %>%
  mutate(id = as.numeric(factor(id)))

hassall_dat %>%
  group_by(id, production, is_old) %>%
  summarize(m = mean(said_old)) -> hassall_test_sum

#d' calculations 
hassall_test_sum %>%
  select(id, production, is_old, m) %>%
  mutate(instruction = paste0(production, is_old)) %>%
  select(-production, -is_old) %>%
  pivot_wider(id_cols = id, names_from = 'instruction', values_from = 'm') %>%
  mutate(read_d = calc_d(read1, read0, cor = .0063), 
         sing_d = calc_d(sing1, sing0, cor = .0063), 
         speak_d = calc_d(speak1, speak0, cor = .0063)) %>%
  select(id, read_d:speak_d) %>%
  pivot_longer(cols = c(read_d:speak_d), names_to = 'production', values_to = 'd') -> hassall_d_sum

#d cor
cor(hassall_d_sum[hassall_d_sum$production=='sing_d',]$d, hassall_d_sum[hassall_d_sum$production=='speak_d',]$d)

cor(hassall_d_sum[hassall_d_sum$production=='read_d',]$d, hassall_d_sum[hassall_d_sum$production=='speak_d',]$d)

cor(hassall_d_sum[hassall_d_sum$production=='sing_d',]$d, hassall_d_sum[hassall_d_sum$production=='read_d',]$d)