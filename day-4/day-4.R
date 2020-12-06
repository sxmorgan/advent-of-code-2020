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
    add_column(ID = paste0('E', seq_len(nrow(.))), .before = 1) %>%
    mutate_at(vars(value), ~ str_replace_all(., '\n', ' ')) %>%
    # mutate(num.fields = str_count(value, ':')) %>%
    # filter(num.fields >= 7) %>%
    # select(-num.fields) %>%
    separate(value, ' ', into = paste0('X', seq_len(8))) %>%
    gather('var', 'value', -ID) %>%
    select(-var) %>%
    filter(!is.na(value)) %>%
    arrange(ID, value) %>%
    separate(value, c('field.name', 'value'), '[:]') %>%
    filter(!str_detect(field.name, 'cid')) %>%
    spread('field.name','value') %>%
    select(-V1) #where from?

# data is 230 x 8
check <- data %>%
    mutate(byr = (byr >= 1920 & byr <= 2002 & !is.na(byr))) %>%
    mutate(iyr = (iyr >= 2010 & iyr <= 2020 & !is.na(iyr))) %>%
    mutate(eyr = (eyr >= 2020 & eyr <= 2030 & !is.na(eyr))) %>%
    mutate(unit = str_extract(hgt,'[a-z]')) %>%
    mutate(hgt = str_remove_all(hgt, '[cmin]')) %>%
    mutate(hgt = case_when(is.na(unit) ~ FALSE,
                           unit=='c' & hgt>=150 & hgt<=193 ~ TRUE,
                           unit=='i' & hgt>=59 & hgt<=76 ~ TRUE, 
                           (unit=='c' & hgt<150) | (unit=='c' & hgt>193) ~ FALSE,
                           (unit=='i' & hgt<59) | (unit=='i' & hgt>76) ~ FALSE)) %>%
    select(-unit) %>%
    filter(!is.na(hcl) & !is.na(ecl) & !is.na(pid)) %>%
    mutate(hcl = str_detect(hcl, '[#][0-9a-f]{6}')) %>%
    mutate(ecl = ecl %in% c('amb','blu','brn','gry','grn','hzl','oth')) %>%
    mutate(pid = str_detect(pid, '[0-9]{9}')) %>%
    mutate(valid = rowSums(.[,2:8])) %>%
    filter(valid == 7)

# from https://colinfay.me/aoc-2020-04/
ipt <- readLines("~/Desktop/advent-of-code-20/day-4/input.txt" ) %>% 
    paste(collapse = "\n") %>% 
    strsplit("\n\n") %>% 
    .[[1]] %>%
    gsub("\n", " ", .)

library(purrr, warn.conflicts = FALSE)

is_north_pole_valid <- function(x, patt = c("byr","iyr","eyr", 
                                            "hgt", "hcl", "ecl", "pid")) {
    map_lgl(patt, ~ grepl(.x, x)) %>% all()
}

ipt %>%
    discard(~ !is_north_pole_valid(.x)) %>%
    strsplit(" ") %>%
    map_dbl(~{
        vals <- map_chr(.x, ~ gsub("([^:]*):(.*)", "\\2", .x)) 
        names(vals) <- map_chr(.x, ~ gsub("([^:]*):(.*)", "\\1", .x)) 
        
        if (! dplyr::between(vals["byr"], 1920, 2002) ) return(0)
        if (! dplyr::between(vals["iyr"], 2010, 2020) ) return(0)
        if (! dplyr::between(vals["eyr"], 2020, 2030) ) return(0)
        if ( grepl("in", vals["hgt"]) ) {
            if (! dplyr::between(gsub("in", "", vals["hgt"]), 59, 76)) return(0)
        } else if (grepl("cm", vals["hgt"])) {
            if (! dplyr::between(gsub("cm", "", vals["hgt"]), 150, 193)) return(0)
        } else {
            return(0) }
        if (! grepl("^#[a-f0-9]{6}$", vals["hcl"])) return(0)
        if (! vals["ecl"] %in% c("amb","blu", "brn", "gry", "grn","hzl", "oth")) return(0)
        if (! grepl("^[0-9]{9}$", vals["pid"])) return(0)
        return(1) }
    ) %>% sum()
