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
# Clean AIM Species attribute table


### Import AIM LPI Data
praw <- '../data/raw'
ppro <-  '../data/processed'
f <- list.files(praw, pattern = 'csv')
files <- list.files(ppro, pattern = 'csv')

## Extract the Main Species Attributes from AIM database

plot_char <- read.csv( file.path(praw, f[grep('Characterization', f)]) ) %>% 
  select(PrimaryKey, Easting, Northing) %>% 
  drop_na()  %>% 
  st_as_sf(coords = c('Easting', 'Northing'), crs = 4269) %>% 
  st_transform(26913)

pts <- st_read(
  '/media/sagesteppe/ExternalHD/aimDB/data/raw/AIM_Sample_Design/AIM_Design_Stratification.shp',
  quiet = T) %>% 
  st_transform(26913) %>% 
  st_buffer(55) %>% 
  select(PLOTID, STRATUM) 

plot_char <- plot_char %>% 
  st_transform(26913)

spp_attri <- read.csv( file.path(praw, 'TerrestrialSpeciesAttributes.csv')) %>% 
  distinct(Species, .keep_all = T) %>% 
  select(SYMBOL = Species,  PrimaryKey, GrowthHabit:Duration) %>% 
  mutate(across(.cols = everything(), ~ na_if(.x, ""))) %>% # there are a number
  drop_na() # of records without attributes, but these codes appear in error

plots <- st_intersection(pts, plot_char) 
spp_attri <- filter(spp_attri, PrimaryKey %in% plots$PrimaryKey) %>% 
  select(-PrimaryKey)

rm(plot_char, pts, plots)


### lookup grass and shrub status
photosynthetic <- read.csv(file.path(praw, f[grep('Photo.*RCB', f)])) %>% 
  mutate(PHOTOSYNTHETIC = if_else(str_detect(PHOTOSYNTHETIC, 'C3'), 'C3', 'C4' )) 

shrub_sprout <- read.csv(file.path(ppro, files[grep('Shrub', files)])) %>% 
  bind_rows(., read.csv(file.path(praw, f[grep('shrubs_resprout', f)]))) %>% 
  select(SYMBOL, RESPROUT) %>% 
  mutate(RESPROUT = if_else(RESPROUT == 'Yes', 'RESPROUT', 'NON-RESPROUT'))

spp_attri <- spp_attri %>% 
  mutate(SYMBOL = case_when(
    SYMBOL == 'KOMY' ~ 'KOMA',
    SYMBOL == 'PHPRN' ~ 'PHPR3',
    SYMBOL == 'CHER4' ~ 'BOER4',
    SYMBOL == 'BRPO5' ~ 'BRPO2',
    SYMBOL == 'STCO4' ~ 'HECO26',
    SYMBOL == 'AGSM' ~ 'PASM',
    SYMBOL == 'AGIN2' ~ 'THIN6',
    SYMBOL == 'ERRA' ~ 'SARA3',
    TRUE ~ as.character(SYMBOL)
  )) %>% 
  left_join(., photosynthetic, by = 'SYMBOL') %>% 
  left_join(., shrub_sprout, by = 'SYMBOL')

rm(photosynthetic, shrub_sprout)

### append nativity information

nativity <- read.csv(file.path(ppro, files[grep('Native', files)])) %>% 
  select(SYMBOL:BINOMIAL_ACKER, NATIVITY) 

spp_attri <- spp_attri %>% 
  mutate(SYMBOL_NEW = case_when(
    SYMBOL == 'ABBI3' ~ 'ABLA',
    SYMBOL == 'ACNE9' ~ 'ACNEN2',
    SYMBOL == 'AGPA14' ~ 'AGGLL',
    SYMBOL == 'ALAC' ~ 'ALAC4',
    SYMBOL == 'AMUTU' ~ 'AMUT',
    SYMBOL == 'AGHE2' ~ 'AGHEH',
    SYMBOL == 'ARHI' ~ 'ARHIP',
    SYMBOL == 'ARFEF' ~ str_remove('F$', SYMBOL), 
    SYMBOL == 'ASCH4' ~ 'ASCHC2',
    SYMBOL == 'ARSP5' ~ 'PIDE4',
    SYMBOL == 'ASAM5' ~ 'ASAMV',
    SYMBOL == 'AMDI5' ~ 'BADI',
    SYMBOL == 'ASMO10' ~ 'ASMOC2',
    SYMBOL == 'ASASC' ~ 'ASASA',
    SYMBOL == 'ASNU4' ~ 'ASNUM2',
    SYMBOL == 'AMUTU' ~ 'AMUT',
    SYMBOL == 'ASTH2' ~ 'ASMOT',
    SYMBOL == 'BAHI2' ~ 'BAHOH2',
    SYMBOL == 'BERE' ~ 'MARE11', 
    SYMBOL == 'BRAN' ~ 'BRPO2',
    SYMBOL == 'BRMA4' ~ 'BRCA5',
    SYMBOL == 'BRCO' ~ 'BRRA2', 
    SYMBOL == 'BRCO4' ~ 'BRRA2', 
    SYMBOL == 'BRTEG' ~ 'BRTE',
    SYMBOL == 'BRMI' ~ 'BRMIS', 
    SYMBOL == 'BOAR' ~ 'BOBA2',
    SYMBOL == 'ROBL' ~ 'BROBL',
    SYMBOL == 'BOFE' ~ 'ARFE',
    SYMBOL == 'CASCS3' ~ 'CASC18',
    SYMBOL == 'CHNA2' ~ 'ERNA10',
    SYMBOL == 'CHER' ~ 'CHERN', 
    SYMBOL == 'CHVIA4' ~ 'CHVIV2', 
    SYMBOL == 'CITR4' ~ 'CIUNT', 
    SYMBOL == 'CRCR3' ~ 'CRCRE',
    SYMBOL == 'COUM' ~ 'COUMP',
    SYMBOL == 'COMAN' ~ 'COUMP',
    SYMBOL == 'COSE16' ~ 'COSES',
    SYMBOL == 'COVI9' ~ 'ESVIV',
    SYMBOL == 'CRACA' ~ 'CRAC2',
    SYMBOL == 'CYFE' ~ 'CYACF',
    SYMBOL == 'CYLE6' ~ 'PSMO',
    SYMBOL == 'DIBI8' ~ 'MABIB',
    SYMBOL == 'DICA18' ~ 'MACA2',
    SYMBOL == 'DRPA' ~ 'DRPA2',
    SYMBOL == 'DRCUC' ~ 'DRCU',
    SYMBOL == 'ECCOC' ~ 'ECTR',
    SYMBOL == 'ELGL' ~ 'ELGLG',
    SYMBOL == 'ERAB3' ~ 'ERCO4',
    SYMBOL == 'ERFL4' ~ 'ERFLF',
    SYMBOL == 'ERDI13' ~ 'ERCO14',
    SYMBOL == 'ERMIL5' ~ 'ERMIL2',
    SYMBOL == 'ERGR9' ~ 'ERGRG3',
    SYMBOL == 'ERAR25' ~ 'ERJAF',
    SYMBOL == 'ERJA' ~ 'ERJAJ',
    SYMBOL == 'ERVI22' ~ 'CHVIV2', 
    SYMBOL == 'EREA' ~ 'EREAE',
    SYMBOL == 'ERINI4' ~ 'ERIN4',
    SYMBOL == 'EROVC2' ~ 'EROVP2',
    SYMBOL == 'ERCOC13' ~ 'ARCOC4',
    SYMBOL == 'ERCO24' ~ 'ARCOC4',
    SYMBOL == 'ERRO2' ~ 'ERCE2',
    SYMBOL == 'FEOC3' ~ 'VUOC',
    SYMBOL == 'FOME' ~ 'GLSPM',
    SYMBOL == 'JUAR2' ~ 'JUARL',
    SYMBOL == 'GAPIP2' ~ 'GAPI',
    SYMBOL == 'GAMUC' ~ 'GACO2',
    SYMBOL == 'GATR2' ~ 'GATRS2',
    SYMBOL == 'GLSP' ~ 'GLSPM',
    SYMBOL == 'GEMA4' ~ 'GEMAP',
    SYMBOL == 'HEFO9' ~ 'HEVIF',
    SYMBOL == 'JUARA4' ~ 'JUARL',
    SYMBOL == 'KOAM' ~ 'BAAM4',
    SYMBOL == 'LATA' ~ 'LATAP', 
    SYMBOL == 'LIIN' ~ 'LIIN2', 
    SYMBOL == 'LODI' ~ 'LODIM',
    SYMBOL == 'LOGR' ~ 'LOGRG2',
    SYMBOL == 'LUSE4' ~ 'LUSES2',
    SYMBOL == 'LUPOP5' ~ 'LUPR2',
    SYMBOL == 'LUARR2' ~ 'LUARR',
    SYMBOL == 'MATA' ~ 'MATA2', 
    SYMBOL == 'MARAR' ~ 'MARAA',
    SYMBOL == 'METH' ~ 'MEHUH',
    SYMBOL == 'MOFI' ~ 'MOFIM2',
    SYMBOL == 'ORBA4' ~ 'CRBA4',
    SYMBOL == 'ORLO' ~ 'CRLO6',
    SYMBOL == 'ORFL5' ~ 'CRFL5',
    SYMBOL == 'ORFL2' ~ 'CRFL5',
    SYMBOL == 'ORHU2' ~ 'CRHU2', 
    SYMBOL == 'OPFRF' ~ 'OPFR', 
    SYMBOL == 'PADI11' ~ 'PACR5',
    SYMBOL == 'PEDIO' ~ 'PESI',
    SYMBOL == 'PERA' ~ 'PERAA',
    SYMBOL == 'PHCR' ~ 'PHCRC',
    SYMBOL == 'PHRE3' ~ 'ILRI',
    SYMBOL == 'POPRP2' ~ 'POPR', 
    SYMBOL == 'PUPA5' ~ 'PUPAM', 
    SYMBOL == 'QUGAG' ~ 'QUGA',
    SYMBOL == 'RATE' ~ 'CETE5', 
    SYMBOL == 'RULA3' ~ 'RULAA',
    SYMBOL == 'SAMI15' ~ 'SARAR3',
    SYMBOL == 'SALA5' ~ 'SALUL',
    SYMBOL == 'SEBI2' ~ 'SEBIH',
    SYMBOL == 'SIAL11' ~ 'ALPE4',
    SYMBOL == 'SILI7' ~ 'SCLI12',
    SYMBOL == 'SILI5' ~ 'SCLI',
    SYMBOL == 'SPCOC' ~ 'SPCO', 
    SYMBOL == 'SOSI' ~ 'SOSIS',
    SYMBOL == 'SOSI3' ~ 'SOSIS',
    SYMBOL == 'SUNI' ~ 'SUMO',
    SYMBOL == 'SONE' ~ 'SONEL',
    SYMBOL == 'TARA' ~ 'TACH',
    SYMBOL == 'THPU3' ~ 'THEL', 
    SYMBOL == 'THSA2' ~ 'THSAS',
    SYMBOL == 'YUBAB' ~ 'YUBA',
    SYMBOL == 'YUBA2' ~ 'YUBA',
    SYMBOL == 'YUBAV' ~ 'YUBA',
    SYMBOL == 'YUHAH' ~ 'YUHA',
    SYMBOL == 'ZIEL2' ~ 'ZIELE',
    SYMBOL == 'ZIVE' ~ 'ZIVEG',
    SYMBOL == 'XASP' ~ 'XASP2',
    SYMBOL == 'LEAL' ~ 'LEALE', 
    SYMBOL == 'BRJA' ~ 'BRAR5', 
    TRUE ~ as.character(SYMBOL)
  ))

# MAYBE ADD TO CNHP ???
# SPCO. # BOST4 - this is stricta...
nativity <- nativity %>% 
  mutate(SYMBOL_NEW = case_when(
    SYMBOL == 'CALEL4' ~ 'CALE4',
    #   SYMBOL == 'YUBA' ~ 'YUBAB',
    SYMBOL == 'TACH2' ~ 'TACH',
    SYMBOL == 'ZIEL2' ~ 'ZIELE', 
    SYMBOL == 'LALAA' ~ 'LALA3',
    SYMBOL == 'ACNE9' ~ 'ACNEN2',
    TRUE ~ as.character(SYMBOL)
  ))


out <- left_join(spp_attri, nativity %>% 
                   select(-SYMBOL), by = 'SYMBOL_NEW') %>% 
  distinct(SYMBOL_NEW, .keep_all = T) 

# Append INVASIVE STATUS


invasive <- read.csv(file.path(praw, f[grep('Introduced', f)])) %>% 
  select(SYMBOL_NEW = National_USDASymbol, Invasive) 

out <- left_join(out, invasive, by = 'SYMBOL_NEW') %>% 
  mutate(Invasive = replace_na(Invasive, F))

rm(invasive)

## Create normal AIM Functional Groups
fnct_lkp <- read.csv( file.path(praw, f[grep('Functional', f)]) ) %>% 
  filter(INDICATOR_TYPE == 'SINGLE') %>% 
  mutate(INVASIVE = as.character(if_else(NOXIOUS == 1, T, F))) %>%
  select(FUNCTIONAL, GROWTHHABITSUB =  LIFEFORM, DURATION, INVASIVE)

### Create Finer Functional Groups
out <- out %>% 
  rename_with(toupper) %>% 
  relocate(any_of(c('GROWTHHABIT', 'GROWTHHABITSUB', 'DURATION', 'NATIVITY',
                    'INVASIVE',"PHOTOSYNTHETIC", "RESPROUT", "LIFECYCLE")), 
           .after = last_col()) %>% 
  mutate(across(.cols = GROWTHHABIT:RESPROUT, ~ str_to_upper(.)), 
         GROWTHHABITSUB = str_replace(GROWTHHABITSUB, 'SEDGE', 'GRAMINOID')) %>% 
  left_join(., fnct_lkp, by = c('GROWTHHABITSUB', 'DURATION', 'INVASIVE')) %>% 
  relocate('FUNCTIONAL', .before = 'GROWTHHABIT') %>% 
  rename(SYMBOL_AIM = SYMBOL , SYMBOL_USDA = SYMBOL_NEW) 

write.csv(out, file.path(ppro, 'SpeciesAttributes.csv'), row.names = F)

rm(fnct_lkp, nativity, spp_attri, out)

## Now detect which species are missing the MAIN attributes from all LPI data - 
# turns out the last table does not actually contain all species from LPI lines. 

spp_attribute_tbl <- read.csv(file.path(ppro, 'SpeciesAttributes.csv')) %>% 
  rename(BINOMIAL_ACK = BINOMIAL_ACKER)

c_vals <- read_csv(file.path(praw, f[grep('C-Values', f)]), show_col_types = FALSE) %>% 
  select(SYMBOL_USDA = National_USDASymbol,  BINOMIAL_NAT = National_SciName_noAuthority, 
         BINOMIAL_ACK = Ack_SciName_noAuthority, NATIVITY = National_NativeStatus,
         GROWTHHABITSUB = USDA_GrowthHabitSimple, DURATION = USDA_Duration)  %>% 
  mutate(SYMBOL_USDA = case_when(
    SYMBOL_USDA == 'TACH' ~ 'TACH2', 
    TRUE ~ as.character(SYMBOL_USDA)))

lpi <- read.csv( file.path(praw, 'LPIRAW.csv') ) %>% 
  select(PrimaryKey, PointLoc:SoilSurface) %>% 
  mutate(across(.cols = TopCanopy:SoilSurface, ~ na_if(.x, ""))) %>% 
  pivot_longer(TopCanopy:SoilSurface, values_to = 'SYMBOL_AIM') %>% 
  drop_na(SYMBOL_AIM) %>% 
  filter(SYMBOL_AIM != 'None', str_length(SYMBOL_AIM) >= 4) %>% 
  distinct(SYMBOL_AIM) 
  
lpi <- lpi[str_detect(lpi$SYMBOL_AIM, "^[A-Z]{2}\\d", negate = T),]

in_att_table <- left_join(lpi, spp_attribute_tbl, by = 'SYMBOL_AIM') %>% 
  drop_na(SYMBOL_USDA)
in_att_table1 <- left_join(lpi, spp_attribute_tbl, keep = T,
                           by = c('SYMBOL_AIM' = 'SYMBOL_USDA')) %>% 
  drop_na(SYMBOL_AIM.y) %>% 
  select(-SYMBOL_AIM.y)

attributes_found <- bind_rows(in_att_table, in_att_table1) %>% distinct()

rm(in_att_table1, in_att_table)

lpi1 <- lpi %>% 
  filter(!SYMBOL_AIM %in% 
           c(spp_attribute_tbl$SYMBOL_AIM, spp_attribute_tbl$SYMBOL_USDA))

# lpi1 all need looked up codes.... 

lpi1 <- left_join(lpi1, c_vals, 
                  by = c('SYMBOL_AIM' = 'SYMBOL_USDA'), keep = T) 

need <- lpi1 %>% 
  filter(is.na(GROWTHHABITSUB)) %>% 
  mutate(SYMBOL_USDA = case_when(
    SYMBOL_AIM == 'ABBI3' ~ 'ABLA', 
    SYMBOL_AIM == 'ACGL' ~ 'ACGLG2',
    SYMBOL_AIM == 'AGCRC' ~ 'AGCR',
    SYMBOL_AIM == 'AGSM' ~ 'PASM',
    SYMBOL_AIM == 'ASVE11' ~ 'XYVE',
    SYMBOL_AIM == 'ATCAC' ~ 'ATCA2',
    SYMBOL_AIM == 'BRCO4' ~ 'BRRA2',
    SYMBOL_AIM == 'BRPO5' ~ 'BRPO2',
    SYMBOL_AIM == 'CEST8' ~ 'CESTM',
    SYMBOL_AIM == 'DEPI' ~ 'DEPIB',
    SYMBOL_AIM == 'DEPII' ~ 'DEPIB',
    SYMBOL_AIM == 'ELTRT' ~ 'ELTR7',
    SYMBOL_AIM == 'ERCIC' ~ 'ERCI6',
    SYMBOL_AIM == 'ERDIJ' ~ 'ERDI2',
    SYMBOL_AIM == 'EREA3' ~ 'AREAE',
    SYMBOL_AIM == 'GLSP' ~ 'GLSPM',
    SYMBOL_AIM == 'GUSU2' ~ 'GUSA2',
    SYMBOL_AIM == 'HAAR2' ~ 'STARA',
    SYMBOL_AIM == 'HIJA' ~ 'PLJA',
    SYMBOL_AIM == 'JUAR2' ~ 'JUARL',
    SYMBOL_AIM == 'JUARA4' ~ 'JUARL',
    SYMBOL_AIM == 'LAMA9' ~'LAOCC',
    SYMBOL_AIM == 'LADE4' ~ 'LAPOP',
    SYMBOL_AIM == 'LESA2' ~ 'THAR5',
    SYMBOL_AIM == 'LESA4' ~ 'LESAS',
    SYMBOL_AIM == 'LOGR' ~ 'LOGRG2',
    SYMBOL_AIM == 'MAGR2' ~ 'MAGRG',
    SYMBOL_AIM == 'ORFL2' ~ 'CRFL5',
    SYMBOL_AIM == 'ORFL3' ~ 'CRFL6',
    SYMBOL_AIM == 'PELE2' ~ 'PELEL2',
    SYMBOL_AIM == 'PEPU7' ~ 'PEPUP',
    SYMBOL_AIM == 'PHCR' ~ 'PHCRC',
    SYMBOL_AIM == 'PIEN' ~ 'PIENE', 
    SYMBOL_AIM == 'PIPO' ~ 'PIPOS',
    SYMBOL_AIM == 'POSA17' ~ 'PODOJ2', 
    SYMBOL_AIM == 'PRVI' ~ 'PRVIM',
    SYMBOL_AIM == 'PSME' ~ 'PSMEG',
    SYMBOL_AIM == 'SEMU' ~ 'SEMUM', 
    SYMBOL_AIM == 'STAR10' ~ 'STARA',
    SYMBOL_AIM == 'STCO4' ~ 'HECO26',
    TRUE ~ as.character(SYMBOL_AIM)
  )) %>% 
  select(SYMBOL_AIM, SYMBOL_USDA)


need <- left_join(need, c_vals, by = 'SYMBOL_USDA' ) 
lpi1 <- lpi1 %>% 
  filter(!is.na(GROWTHHABITSUB))

got <- bind_rows(need, lpi1)

invasive <- read.csv(file.path(praw, f[grep('Introduced', f)])) %>% 
  select(SYMBOL_USDA = National_USDASymbol, INVASIVE = Invasive) 

photosynthetic <- read.csv(file.path(praw, f[grep('Photo.*RCB', f)])) %>% 
  mutate(PHOTOSYNTHETIC = if_else(str_detect(PHOTOSYNTHETIC, 'C3'), 'C3', 'C4' )) 

shrub_sprout <- read.csv(file.path(ppro, files[grep('Shrub', files)])) %>% 
  bind_rows(., read.csv(file.path(praw, f[grep('shrubs_resprout', f)]))) %>% 
  select(SYMBOL, RESPROUT) %>% 
  mutate(RESPROUT = if_else(RESPROUT == 'Yes', 'RESPROUT', 'NON-RESPROUT'))

got <- left_join(got, invasive, by = "SYMBOL_USDA") %>% 
  mutate(INVASIVE = replace_na(INVASIVE, F)) %>% 
  left_join(., photosynthetic, by = c('SYMBOL_USDA' = 'SYMBOL')) %>% 
  left_join(., shrub_sprout, by = c('SYMBOL_USDA' = 'SYMBOL'))

rm(need, lpi1, invasive, photosynthetic, shrub_sprout)

# need to add on photosynthetic, invasiveness and shrub resprout status !!! 

results_maybe <- bind_rows(attributes_found,  got) %>% 
  distinct(SYMBOL_AIM, .keep_all = T) %>% 
  mutate(across(.cols = GROWTHHABIT:RESPROUT, ~ str_to_upper(.)),
         INVASIVE = as.logical(INVASIVE)) %>% 
  select(-SYMBOL_AIM.x)

r  <- bind_rows(spp_attribute_tbl, results_maybe)

wtf <- r %>% 
  group_by(SYMBOL_AIM) %>% 
  filter(n() > 1) %>% 
  distinct()
k <- r %>% 
  group_by(SYMBOL_AIM) %>% 
  filter(n() == 1) %>% 
  filter(! SYMBOL_AIM %in% wtf$SYMBOL_AIM)

consensus <- bind_rows(wtf, k)  %>% 
  mutate(DURATION = case_when(
    DURATION == 'BIENNIAL, PERENNIAL' ~ 'PERENNIAL', 
    DURATION == 'PERENNIAL, BIENNIAL' ~ 'PERENNIAL', 
    DURATION == 'ANNUAL, PERENNIAL' ~ 'PERENNIAL', 
    DURATION == 'ANNUAL, BIENNIAL, PERENNIAL' ~ 'PERENNIAL',
    DURATION == 'ANNUAL, BIENNIAL' ~ 'ANNUAL',
    DURATION == 'BIENNIAL, ANNUAL' ~ 'ANNUAL',
    DURATION == 'BIENNIAL' ~ 'ANNUAL', 
    TRUE ~ as.character(DURATION)
  ), 
  GROWTHHABITSUB = str_replace(GROWTHHABITSUB, 'VINE', 'FORB')) 

rm(r, wtf, k)

fnct_lkp <- read.csv( file.path(praw, f[grep('Functional', f)]) ) %>% 
  filter(INDICATOR_TYPE == 'SINGLE') %>% 
  mutate(INVASIVE = if_else(NOXIOUS == 1, T, F)) %>%
  select(FUNCTIONAL, GROWTHHABITSUB =  LIFEFORM, DURATION, INVASIVE)

consensus <- consensus %>%
  select(-FUNCTIONAL) %>% 
  left_join(., fnct_lkp, by = c('GROWTHHABITSUB', 'DURATION', 'INVASIVE'))

consensus %>% 
  filter(is.na(FUNCTIONAL)) %>% 
  write.csv(. , file.path(praw, 'genera_need_chars.csv'), row.names = F)

consensus <- consensus %>% 
  filter(!is.na(FUNCTIONAL))  %>% 
  mutate(NATIVITY = as.logical(NATIVITY))

consensus <- read.csv( file.path(praw, 'genera_need_chars-RCB.csv')) %>% 
  select(-FUNCTIONAL) %>% 
  left_join(., fnct_lkp, by = c('GROWTHHABITSUB', 'DURATION', 'INVASIVE')) %>% 
  bind_rows(consensus, .)

consensus <- consensus %>% 
  mutate(SYMBOL_AIM = if_else(is.na(SYMBOL_AIM), SYMBOL_USDA, SYMBOL_AIM))


aim_symbs_need <- data.frame(
  SYMBOL_AIM = c('AMUT', 'SPCO', 'YUHA', 'OPFR', 'QUGA', 'VUOC', 'SCLI', 
                 'CETE5', 'ERNA10', 'YUBA', 'MARE11', 'SARAR3', 'PSMO', 
                 'DRCU', 'JUARL', 'ERCO14', 'BAAM4', 'MACA2', 'ALAC4', 
                 'ECTR', 'ERIN4', 'PESI', 'POPR')
  )

extra <- left_join(aim_symbs_need, ungroup(consensus) %>%  
            select(-SYMBOL_AIM), 
          by = c('SYMBOL_AIM' = "SYMBOL_USDA")) %>% 
  mutate(SYMBOL_USDA = SYMBOL_AIM) %>% 
  distinct()
extra <- extra[-15,]
consensus <- bind_rows(consensus, extra)

write.csv(consensus, file.path(ppro, 'SpeciesAttributeTable.csv'),  row.names = F)

rm(attributes_found, c_vals, consensus, fnct_lkp, genera, got, h, lpi, results_maybe, 
   spp_attribute_tbl, aim_symbs_need, extra)
