library(tidyverse)

input <- read_file('~/Desktop/advent-of-code-20/day-4/input.txt')

# how many passwords valid (missing cid field only)?
input %>%
    str_split('\n\n') %>%
    magrittr::extract2(1) %>%
    as_tibble() %>%
    mutate_at(vars(value), ~ str_replace_all(., '\n', ' ')) %>%
    mutate(num.fields = str_count(value, ':')) %>%
    mutate(cid = str_detect(value, 'cid')) %>%
    mutate(valid = case_when(num.fields==7 & !cid ~ TRUE,
                             num.fields==7 & cid ~ FALSE,
                             num.fields==8 ~ TRUE)) %>%
    filter(valid) %>%
    nrow()

# new rules
data <- input %>%
    str_split('\n\n') %>%
    magrittr::extract2(1) %>%
    as_tibble() %>%
    mutate_at(vars(value), ~ str_replace_all(., '\n', ' ')) %>%
    mutate(num.fields = str_count(value, ':')) %>%
    filter(num.fields >= 7) %>%
    select(-num.fields) %>%
    separate(value, ' ', into = paste0('X', seq_len(8))) %>%
    add_column(ID = paste0('E', seq_len(nrow(.))), .before = 1) %>%
    gather('var', 'value', -ID) %>%
    filter(!is.na(value)) %>%
    arrange(ID, value) %>%
    select(-var) %>% 
    separate(value, c('field.name', 'value'), '[:]') %>%
    spread('field.name','value') 

data %>%
    mutate(byr = ifelse((byr >= 1920 & byr <= 2002 &!is.na(byr)) , TRUE, FALSE)) %>%
    mutate(iyr = ifelse(iyr >= 2010 & iyr <= 2020 &!is.na(iyr), TRUE, FALSE)) %>%
    mutate(eyr = ifelse(eyr >= 2020 & eyr <= 2030 &!is.na(eyr), TRUE, FALSE)) %>%
    mutate(unit = str_extract(hgt,'[a-z]')) %>%
    mutate(hgt = str_remove_all(hgt, '[a-z]')) %>%
    mutate(hgt = case_when(is.na(unit) ~ FALSE,
                           unit=='c' & hgt>=150 & hgt<=193 ~ TRUE,
                           unit=='i' & hgt>=59 & hgt<=76 ~ TRUE, 
                           (unit=='c' & hgt<150) | (unit=='c' & hgt>193) ~ FALSE,
                           (unit=='i' & hgt<59) | (unit=='i' & hgt>76) ~ FALSE)) %>%
    select(-unit) %>%
    filter(!is.na(hcl) & !is.na(ecl) & !is.na(pid)) %>%
    mutate(hcl = str_detect(hcl, '[#][0-9a-zA-Z]')) %>%
    mutate(ecl = ifelse(ecl %in% c('amb','blu','brn','gry','grn','hzl','oth'),
                        TRUE, FALSE)) %>%
    mutate(pid = str_detect(pid, '[0-9]{9}')) %>%
    select(-cid) %>%
    mutate(valid = rowSums(.[,2:8])) %>%
    filter(valid == 7)

# first answer 117 = wrong
# still wrong ... 
