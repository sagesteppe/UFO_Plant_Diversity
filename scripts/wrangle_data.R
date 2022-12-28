library(tidyverse)

setwd('/media/sagesteppe/ExternalHD/UFO_Plant_Diversity/scripts')
praw <- '../data/raw'
ppro <- '../data/processed'
f <- list.files(praw, pattern = 'csv')


###############################################################################
# USDA download

usda <- read.csv(file.path(praw, f[grep('USDA', f)])) %>% 
  na_if("") %>% 
  select(-State.Common.Name, FULL_NAME = Scientific.Name.with.Author,
         SYNONYM_SYMBOL = Synonym.Symbol) %>% 
  separate(FULL_NAME, into = c('GENUS', 'SPECIES'),
           extra = 'drop', remove = F,) %>% 
  mutate(JOIN_SYMBOL = if_else(is.na(SYNONYM_SYMBOL), Symbol, SYNONYM_SYMBOL), .before = FULL_NAME) %>% 
  rename_with( toupper) 


# check to see if we can mark genera as such... 

usda %>% 
  mutate(SPECIES = case_when(
    str_detect(toupper(GENUS), SYMBOL) ~ NA,
    str_detect(FULL_NAME, 'Asplenium platyneuron') ~ 'platyneuron',
    str_detect(FULL_NAME, 'Boerhavia erecta') ~ 'erecta'
  ))

?str_detect

write.csv(usda, file.path(ppro, f[grep('USDA', f)]))


###############################################################################
# CNHP download and subset

cnhp <- read.csv(file.path(praw, f[grep('CNHP', f)]))  %>% 
  filter(MAJORGROUP == 'Vascular Plants') %>% 
  mutate(across(.cols = everything(), ~ na_if(., '-')),
         BLM_RANK = str_detect(OTHERSTATUS, 'BLM'),
         BLM_RANK = replace_na(BLM_RANK, F)
         )  %>% 
select(-MAJORGROUP, -SELEMENTID, -CNHPSENS:-TRACK, -OTHERSTATUS, -COSTATUS)


# add column for USDA look up symbol. 


usda

