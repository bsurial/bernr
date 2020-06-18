library(tidyverse)
library(ICD10gm)

# Less detailed version, but is useful in case detailed version doesn't match
labs <- icd_meta_codes %>%
  as_tibble() %>%
  filter(year == 2018) %>% # Has different versions categorized per year, arbitrarily chose 2018
  mutate(code = str_sub(icd_sub, 1, 3)) %>%
  select(icd_block_first, code, label_icd3, icd_normcode) %>%
  distinct()

# Detailed version
labs_det <- icd_meta_codes %>%
  as_tibble() %>%
  filter(year == 2018) %>% # Has different versions categorized per year, arbitrarily chose 2018
  select(icd_block_first, icd_normcode, label) %>%
  distinct()

# Blocks of ICD
blocks <- icd_meta_blocks %>%
  as_tibble() %>%
  filter(year == 2018)

# Chapters of ICD
chapter <- icd_meta_chapters %>%
  as_tibble() %>%
  filter(year == 2018) %>%
  select(chapter, chapter_label)

icd_codes <- labs_det %>%
  left_join(labs, by = c("icd_block_first", "icd_normcode")) %>%
  left_join(blocks, by = "icd_block_first") %>%
  left_join(chapter, by = "chapter") %>%
  select(icd_normcode, label, icd_code_short = code, label_short = label_icd3,
         icd_block_first, block_label, chapter_label)

write_csv(icd_codes, "data-raw/icd_codes.csv")
usethis::use_data(icd_codes, overwrite = TRUE)
