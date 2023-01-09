library(tidyverse)

setwd('/media/sagesteppe/ExternalHD/UFO_Plant_Diversity/scripts')
praw <- '../data/raw'
ppro <- '../data/processed'
f <- list.files(praw, pattern = 'csv')
files <- list.files(ppro, pattern = 'csv')

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

usda <- usda %>% 
  mutate(SPECIES = case_when(
    str_detect(toupper(GENUS), SYMBOL) ~ NA,
    str_detect(FULL_NAME, 'Asplenium platyneuron') ~ 'platyneuron',
    str_detect(FULL_NAME, 'Boerhavia erecta') ~ 'erecta'
  ))

write.csv(usda, file.path(ppro, f[grep('USDA', f)]), row.names = F)
rm(usda)

###############################################################################
# CNHP Rare Species and Vegetation Types download and subset

cnhp <- read.csv(file.path(praw, f[grep('CNHP', f)]))  %>% 
  filter(MAJORGROUP == 'Vascular Plants') %>% 
  mutate(across(.cols = everything(), ~ na_if(., '-')),
         BLM_RANK = str_detect(OTHERSTATUS, 'BLM'),
         BLM_RANK = replace_na(BLM_RANK, F)
         )  %>% 
select(-MAJORGROUP, -SELEMENTID, -CNHPSENS:-TRACK, -OTHERSTATUS, -COSTATUS)

###############################################################################
# Access CNHP FQA Metrics
# these are in a microsoft table, we just ripped them out using OPEN SOURCE
# SOFTWARE. We should really have a discussion on why non-military branches of 
# the gov't use software which cost a taxpayer money to use. 

# In terminal the following is run: 
# sudo apt install mdbtools
# change the directory to be inside the folder with the DB
# mdb-tables FQA_calculator_2022_07_12.accdb # find all tables in the DB
# mdb-count FQA_calculator_2022_07_12.accdb tFQASpeciesList_lu # how many records are there?
# mdb-export FQA_calculator_2022_07_12.accdb tFQASpeciesList_lu > ../C-Values_export.csv # write out the results

cvals <- read_csv(file.path(praw, f[grep('export', f)]), show_col_types = F) %>% 
  select(National_USDASymbol, starts_with('FQA'), National_SciName_noAuthority, 
         Ack_SciName_noAuthority, USDA_Duration, -'FQA_C-Value2006') %>% 
  drop_na(FQA_Species)

write.csv(cvals, file.path(ppro, 'C-Values_Table.csv'), row.names = F)

################################################################################
# finish processing the CNHP rare plants here. 

usda_look <- cvals %>%
  select(National_USDASymbol, FQA_SciName_noAuthority, National_SciName_noAuthority)

# add column for USDA look up symbol to the rare plants

snames <- left_join(cnhp, usda_look, by = c('SNAME' = 'National_SciName_noAuthority')) %>% 
  drop_na(National_USDASymbol)
gnames <- left_join(cnhp, usda_look, by = c('GNAME' = 'National_SciName_noAuthority'))%>% 
  drop_na(National_USDASymbol)

inner_names <- inner_join(snames, gnames)
missedN <- anti_join(gnames, snames, by = 'National_USDASymbol')
missedS <- anti_join(snames, gnames, by = 'National_USDASymbol')
retrieved_names <- bind_rows(inner_names, missedS, missedN)

missed <- cnhp %>% 
  filter(!GNAME %in% c(retrieved_names$GNAME) & BLM_RANK == T) %>% # these not BLM addressed 
  left_join(., usda_look, by = c(SNAME = 'FQA_SciName_noAuthority')) %>% 
  mutate(National_USDASymbol = case_when(
    SNAME == 'Boechera crandallii' ~' ARCR5',
    SNAME == 'Aquilegia chrysantha var. rydbergii' ~ 'AQCHR',
    SNAME == 'Asclepias uncialis' ~ 'ASUN4',
    SNAME == 'Gilia stenothyrsa' ~ 'ALST12' 
    # we miss a couple species but they are local endemics not known to the UFO
  ))

cnhp <- bind_rows(retrieved_names, missed) 

write.csv(cnhp, file.path(ppro, 'Rare_Plants_BLM_CO.csv'), row.names = F)

rm(snames, gnames, missedN, missedS, inner_names, cvals, missed, retrieved_names, 
   usda_look, cnhp)


################################################################################
# Nativity dataset

nativity <- read_csv(file.path(praw, f[grep('export', f)]), show_col_types = F) %>% 
  select(SYMBOL = National_USDASymbol, BINOMIAL_NAT =  National_SciName_noAuthority, 
         BINOMIAL_ACKER = Ack_SciName_noAuthority, DURATION = USDA_Duration, NATIVITY = National_NativeStatus) %>% 
  drop_na() 

write.csv(nativity, file.path(ppro, 'Native_Status.csv'), row.names = F)

################################################################################
# Develop a template for Shrub Sprout not sprout lookup


s <- read_csv(file.path(praw, f[grep('export', f)]), show_col_types = F) %>%
  mutate(USDA_GrowthHabitSimple = if_else(National_USDASymbol == 'GUSA2', 'Shrub', USDA_GrowthHabitSimple)) %>% 
  filter(USDA_GrowthHabitSimple == 'Shrub') %>% 
  select(SYMBOL = National_USDASymbol, BINOMIAL_NAT =  National_SciName_noAuthority,
         BINOMIAL_ACKER = Ack_SciName_noAuthority ) %>% 
  drop_na() 

shrub_template <- data.frame(SYMBOL = c(
  'AMAL2', 'AMUT',  # Amelanchier
  'ARPA6', 'ARUV', # arcostaphylos
  'ARAR8', 'ARARA', 'ARARL', 'ARCA13', 'ARFR4', 'ARNO4', 'PIDE4', 'ARTRT',
  'ARTRV', 'ARTRW8', 'ARTRT2', # Artemisia,
  'ATCA2', 'ATCO', 'ATGA', 'ATCO4', # Atriplex
  'BEFE', 'MAFR3', 'MARE11', # Berberis
  'BRCA3', 'BRLO', 'BRMIS',  # brickellia,
  'CEFE', # ceanothus
  'CELE3', 'CEMO2', # cercocarpus
  'CHDE2', 'CHGR6', 'CHLI3', 'CHVA2', 'CHVI8', #chrysothamnus
  'CORA', # coleagyne
  'COSES', # cornus
  'EPTO', 'EPVI', #EPHEDRA
  'ERDID', 'ERNA10', 'ERPA30', # Ericameria
  'ERCO14', 'ERMI4',   # eriogonum
  'FERU', # fendlera
  'FOPU2', # forestiera
  'FRAN2', #fraxinus
  'GRSP', # grayia
  'GUMI', 'GUSA2', # Guiterrhiza
  'HEMUM', # heliomeris 
  'HEVIN', # heterotheca
  'HODU', # holodiscus
  'BAAM4', # bassia 
  'KRLA2', # kraschennikovia
  'LOIN5', # lonicera involucrata
  'MILI3', # mirabilis linearis
  'DAFR6', # dasiphora
  'PUST', 'PUTR2', # purshia
  'QUGA', # Quercus, 
  'RHTR', # rhus
  'ROWO', # rosa
  'SAVE4', # Sarcobatus
  'SYLO', 'SYRO', # symphoricarpos
  'TECA2', 'TENU2', 'TESP2', # tetradymia
  'TORY', # toxico
  'MACO13' # macheranthera ... 
  ))

shrub_template <- left_join(shrub_template, s) %>% 
  mutate(RESPROUT = "", .after = 'SYMBOL')

write.csv(shrub_template, file.path(praw, 'Shrub_Resprout.csv'), row.names = F)

rm(s, shrub_template)

rm(f, praw, ppro)

#################################################################################
# Look up additional resprout status of shrubs

shrub_sprout <- read.csv(file.path(ppro, files[grep('Shrub', files)])) %>% 
  select(SYMBOL, RESPROUT) %>% 
  mutate(RESPROUT = if_else(RESPROUT == 'Yes', 1, 0))

richness <- read.csv(file.path(praw, f[grep('Associations', f)])) %>% 
  filter(PHASE == '1.1', str_length(SYMBOL) >= 4)  

richness <- richness %>%  
  left_join(., shrub_sprout, by = 'SYMBOL')

shrubs_need_sprouts2 <- richness %>% 
  filter(FUNCTIONAL == 'SHRUB', is.na(RESPROUT)) %>% 
  distinct(SYMBOL) 

# write.csv(shrubs_need_sprouts2, file.path(praw, 'shrubs_resprout2.csv'),
#          row.names = F)

################################################################################
# ADD Some grass infraspecies to the C3/C4 lookup, also fill out graminoids which
# are not Poaceae

richness <- read.csv(file.path(praw, f[grep('Associations', f)])) %>% 
  filter(PHASE == '1.1', str_length(SYMBOL) >= 4)  

photosynthetic <- read.csv(file.path(praw, f[grep('Photo.*2022[.]csv', f)])) %>% 
  rename(SYMBOL = USDA.Species.Code, PHOTOSYNTHETIC = Photosynthetic.Group) %>% 
  mutate(PHOTOSYNTHETIC = if_else(str_detect(PHOTOSYNTHETIC, 'C3'), 'PG-C3', 'PG-C4' )) 

t <- richness %>%  
  left_join(., photosynthetic, by = 'SYMBOL')  %>% 
  filter(FUNCTIONAL == 'GRASS' & is.na(PHOTOSYNTHETIC)) %>% 
  distinct(SYMBOL, PHOTOSYNTHETIC) # these taxa missing designations

photo2write <- bind_rows(photosynthetic, t)

write.csv(photo2write, row.names = F,
          file.path(praw, 'PhotoSyntheticPathway-Naumanetal2022-RCB.csv'))

rm(richness, photosynthetic, t, photo2write)


###############################################################################
# Many of the forbs are missing life cycles, fix it

richness <- read.csv(file.path(praw, f[grep('Associations', f)])) %>% 
  filter(PHASE == '1.1', str_length(SYMBOL) >= 4)  

life_cycle <- read_csv(file.path(praw, f[grep('export', f)]), show_col_types = F) %>%
  filter(USDA_GrowthHabitSimple == 'Forb', FQA_USDASymbol != 'GUSA2') %>% 
  select(SYMBOL = FQA_USDASymbol, NATIVITY = FQA_NativeStatus, LIFECYCLE = USDA_Duration,
         BINOMIAL_ACK = Ack_SciName_noAuthority, BINOMIAL_NAT = FQA_SciName_noAuthority)

richness <- richness %>% 
  left_join(., life_cycle, by = 'SYMBOL')  %>% 
  filter(FUNCTIONAL == 'FORB', is.na(LIFECYCLE)) %>% 
  select(SYMBOL, LIFECYCLE, NATIVITY, BINOMIAL_NAT) %>% 
  distinct(.keep_all = T)

#write.csv(richness, row.names = F, file.path(praw, 'forbsNeedLifeCycles.csv'))

more_lifeforms <- read.csv( file.path(praw, 'forbsNeedLifeCycles.csv')) %>% 
  mutate(across(.cols = everything(), ~ na_if(., '')),
         across(.cols = LIFECYCLE:NATIVITY, ~ str_to_upper(.))) 

write.csv(more_lifeforms, file.path(ppro, 'UtahForbsLifeCycles.csv'), row.names = F)

################################################################################
# Append functional group and nativity information to the expected species lists


richness <- read.csv(file.path(praw, f[grep('Associations', f)])) %>% 
  filter(PHASE == '1.1', str_length(SYMBOL) >= 4)  

photosynthetic <- read.csv(file.path(praw, f[grep('Photo.*RCB', f)])) %>% 
  mutate(PHOTOSYNTHETIC = if_else(str_detect(PHOTOSYNTHETIC, 'C3'), 'C3', 'C4' )) 

shrub_sprout <- read.csv(file.path(ppro, files[grep('Shrub', files)])) %>% 
  bind_rows(., read.csv(file.path(praw, f[grep('shrubs_resprout', f)]))) %>% 
  select(SYMBOL, RESPROUT) %>% 
  mutate(RESPROUT = if_else(RESPROUT == 'Yes', 'RESPROUT', 'NON-RESPROUT'))

richness <- richness %>%  
  left_join(., photosynthetic, by = 'SYMBOL') %>% 
  left_join(., shrub_sprout, by = 'SYMBOL')

rm(photosynthetic, shrub_sprout)

life_cycle <- read_csv(file.path(praw, f[grep('export', f)]), show_col_types = F) %>%
  filter(USDA_GrowthHabitSimple == 'Forb', FQA_USDASymbol != 'GUSA2') %>% 
  select(SYMBOL = FQA_USDASymbol, NATIVITY = FQA_NativeStatus, LIFECYCLE = USDA_Duration) %>% 
  mutate(LIFECYCLE = case_when(
    LIFECYCLE == 'Biennial, Perennial' ~ 'Perennial', 
    LIFECYCLE == 'Annual, Biennial' ~ 'Annual', 
    LIFECYCLE == 'Biennial' ~ 'Annual', 
    LIFECYCLE == 'Annual, Perennial' ~ 'Any',
    LIFECYCLE == 'Annual, Biennial, Perennial' ~ 'Any',
    LIFECYCLE == 'Annual-Perennial' ~ 'ANY',
    TRUE ~ as.character(LIFECYCLE)
  ))

utah_life_cycles <- read.csv(file.path(ppro, 'UtahForbsLifeCycles.csv')) %>% 
  select(-BINOMIAL_NAT)

richnessF <- richness %>% 
  left_join(., life_cycle, by = 'SYMBOL')  %>% 
  filter(FUNCTIONAL == 'FORB', !is.na(LIFECYCLE)) 

richnessF <- richness %>% 
  left_join(., life_cycle, by = 'SYMBOL')  %>% 
  filter(FUNCTIONAL == 'FORB', is.na(LIFECYCLE)) %>% 
  select(-NATIVITY, -LIFECYCLE) %>% 
  left_join(., utah_life_cycles, by = 'SYMBOL')  %>% 
  bind_rows(., richnessF)

richness <- richness %>% 
  left_join(., life_cycle, by = 'SYMBOL') %>% 
  filter(FUNCTIONAL != 'FORB') %>% 
  bind_rows(., richnessF)  %>% 
  mutate(NATIVITY = ifelse(FUNCTIONAL == 'SHRUB', NA, NATIVITY),
         LIFECYCLE = ifelse(FUNCTIONAL == 'SHRUB', NA, LIFECYCLE),
         across(.cols = NATIVITY:LIFECYCLE, ~ str_to_upper(.x)))

rm(richnessF, utah_life_cycles)

richness <- richness %>% 
  relocate(any_of(c("PHOTOSYNTHETIC", "RESPROUT", "LIFECYCLE", "FUNCTIONAL")), 
           .after = last_col()) %>% 
  unite(col = FUNCTIONAL_FINE, PHOTOSYNTHETIC:FUNCTIONAL, 
        sep = "-", na.rm = T, remove = F) %>% 
  select(-PHOTOSYNTHETIC, -RESPROUT)

nativity <- read.csv(file.path(ppro, files[grep('Native', files)])) %>% 
  select(SYMBOL, LIFECYCLE = DURATION, NATIVITY) %>% 
  mutate(across(.cols = everything(), ~ str_to_upper(.))) %>% 
  mutate(LIFECYCLE = case_when(
    LIFECYCLE == 'BIENNIAL, PERENNIAL' ~ 'PERENNIAL', 
    LIFECYCLE == 'ANNUAL, BIENNIAL' ~ 'ANNUAL',
    LIFECYCLE == 'BIENNIAL' ~ 'ANNUAL', 
    LIFECYCLE == 'ANNUAL, PERENNIAL' ~ 'ANY',
    LIFECYCLE == 'ANNUAL-PERENNIAL' ~ 'ANY',
    LIFECYCLE == 'ANNUAL, BIENNIAL, PERENNIAL' ~ 'ANY',
    TRUE ~ as.character(LIFECYCLE)
  )) 

richness <- left_join(richness, nativity, by = 'SYMBOL') %>% 
  unite(LIFECYCLE, c(LIFECYCLE.x, LIFECYCLE.y), na.rm = T) %>% 
  unite(NATIVITY, c(NATIVITY.x, NATIVITY.y), na.rm = T) %>% 
  mutate(across(.cols = c(LIFECYCLE, NATIVITY), ~ str_remove_all(.x, '_.*')))

richness <- richness %>% # THESE ALL FROM USDA PLANTS
  mutate(LIFECYCLE = if_else(FUNCTIONAL == 'SHRUB', 'PERENNIAL', LIFECYCLE)) %>% 
  mutate(LIFECYCLE = if_else(SYMBOL %in% c('ELELE', 'HECOC8', 'LESA4', 
                                           'PUCCI', 'SPWR2', 'ARIST', 'PLMU3', 
                                           'SEVU2', 'PSSPI', 'EQUIS', 'SCIRP',
                                           'CAREX',
                                           # the next two both contain ANNUALS in 
                                           # the clades, but the amonut of production
                                           # should make them referrable to as
                                           # PERENNIALS,
                                           'JUNCU', 'GRAM'
  ), 'PERENNIAL', LIFECYCLE))  %>% 
  
  mutate(NATIVITY = if_else(SYMBOL %in% c('ELELE', 'HECOC8', 'LESA4', 'SYOR2', 'TEGL',
                                          'ATCU', 'EPHED', 'YUAN2', 'PUME', 'EPNE',
                                          'ERNAN5', 'QUHAT', 'ERWR', 'DAFRF', 
                                          'CHVIS5', 'PEPU7', 'MAGR2', 'BRMI', 'SEVU2',
                                          'SCWH', 'PSSPI', 'PLMU3', 'BACCH', 'KOSP', 
                                          'BACCH', 'RHUS', 'BRICK', 'SHRO', 'ATRIP', 
                                          'QUHA3', 'ATTR3', 'LEFR2', 'ERCOA', 
                                          'ALOC2', 'SPWR2', 'PSFR', 'ARIST', 'PUCCI',
                                          # THESE MUCH MORE VARIOUS BUT NOT REALIZING
                                          # SINCE IN REFERENCE (ALL OF THESE) NATIVE. OOOF
                                          'CAREX', 'SCIRP', 'JUNCU', 'GRAM'
  ), 
  'NATIVE', NATIVITY)) 

richness <- richness %>%
  mutate(FUNCTIONAL_FINE = case_when(
    FUNCTIONAL_FINE == 'C3-PERENNIAL-GRASS' ~ 'C3-GRASS',
    FUNCTIONAL_FINE == 'ANNUAL-PERENNIAL-FORB' ~'ANY-FORB',
    FUNCTIONAL_FINE == 'NON-RESPROUT-PERENNIAL-FORB' ~'PERENNIAL-FORB',
    TRUE ~ as.character(FUNCTIONAL_FINE)
    )) %>% 
  mutate(LIFECYCLE = if_else(LIFECYCLE =='ANNUAL-PERENNIAL', 'ANY', LIFECYCLE))

write.csv(richness,  row.names = F,
          file.path(ppro,
                    'ESD_Vegetation_Associations_StateTransition_Production-FUNCTIONAL_NATIVE.csv'))

rm(richness, nativity, life_cycle)
################################################################################
#