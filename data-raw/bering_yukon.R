
## AK coastal west and yukon ----
rm(list=ls(all=TRUE))
library(tidyverse)

wd <- "D:/bobby_adfg/backup_013122"
source(paste0(wd, "/R/gcl_functions.R"))

password <- readLines(paste0(wd, "/papahwork/loki_password.txt"))

yukon_loci380panel <- read_csv(paste0(wd, "/projects/imbalanced_baseline/ayk_bering_juv_king/baseline_yukon/loci380_panel.csv"))
yukon_loci177 <- yukon_loci380panel %>% filter(Panel == "Chinook_IDFG_299") %>% pull(loci380)

CreateLocusControl.GCL(locusnames=yukon_loci177, username="bhsu", password=password)

templin_pops211 <- read_csv(paste0(wd, "/projects/imbalanced_baseline/ayk_bering_juv_king/Baseline_Templin/baseline_Templin_pops_211_info.csv")) %>%
  mutate(popSILLY = case_when(popSILLY == "KCHUN09.KCHUN12plate1" ~ "KCHUN09.KCHUN12",
                              TRUE ~ popSILLY))
templin_sillys211 <- templin_pops211 %>% pull(popSILLY)
# templin_sillys211[87] <- "KCHUN09.KCHUN12" # I think there's a typo
templin_sillys_all <- str_split(templin_sillys211, pattern = "[.]") %>% unlist

yukon_pops50 <- read_csv(paste0(wd, "/projects/imbalanced_baseline/ayk_bering_juv_king/baseline_yukon/50pop_groupvecs.csv"))
yukon_sillys <- yukon_pops50 %>% pull(silly)
ty_sillys <- unique(c(templin_sillys_all, yukon_sillys))

LOKI2R.GCL(sillyvec=ty_sillys, username="bhsu", password=password)

# mixture ----
complete_grps <- ls()[grepl("[.]gcl", ls())] %>%
  gsub("[.gcl]","", .) # what's downloaded from LOKI

templin_silly393_ref <-
  templin_pops211 %>%
  separate(popSILLY, into = paste0("pop_silly", 1:7)) %>%
  pivot_longer(paste0("pop_silly", 1:7), values_to = "silly") %>%
  filter(!is.na(silly)) %>%
  select(-name)

yukon_silly117_ref <-
  yukon_pops50 %>%
  separate(Silly, into = paste0("pop_silly", 1:6)) %>%
  pivot_longer(paste0("pop_silly", 1:6), values_to = "silly") %>%
  filter(!is.na(silly)) %>%
  select(-name)

ty_silly_ref <-
  templin_silly393_ref %>%
  select(silly, repunit2) %>%
  rename(repunit = repunit2) %>%
  bind_rows({
    yukon_silly117_ref %>%
      select(silly, BroadNames2) %>%
      rename(repunit = BroadNames2)
  }) %>%
  unique()

complete_sillys <-
  tibble(silly = complete_grps) %>%
  left_join(ty_silly_ref, by = "silly") %>%
  filter(!is.na(repunit)) %>%
  mutate(grpvec = case_when(repunit == "Lower Yukon" ~ 1,
                            repunit == "Middle Yukon" ~ 2,
                            repunit == "Upper Yukon" ~ 3,
                            repunit == "Coastal West Alaska" ~ 4,
                            TRUE ~ 5),
         repunit = str_replace_all(repunit, " ", ""),
         group = case_when(grpvec == 5 ~ "Others",
                           TRUE ~ repunit)) %>%
  arrange(grpvec)

print(complete_sillys, n=Inf) # complete list of sillys (templin and yukon baselines) and associate repunit, groupvecs

sapply(unique(complete_sillys$group), function(grp) {
  PoolCollections.GCL(filter(complete_sillys, group == grp) %>% pull(silly), newname = grp)
})

all_gcl <- # 5 groups
  sapply(unique(complete_sillys$group), function(pop) {
    get(paste0(pop, ".gcl"), pos = 1)
  }, simplify = FALSE)

# set.seed(1984)
# n_sim <- rmultinom(1, 150, c(0.15, 0.1, 0.2, 0.1, 0.45))
# samps <-
#   sapply(1:5, function(i) {
#     sample(all_gcl[[i]]$SillySource, n_sim[i,], replace = FALSE)
#   }) %>% set_names(unique(complete_sillys$group))

# save(samps, file = "data-raw/samps.rda")
load("data-raw/samps.rda")

sapply(samps, length) #%>% sum
sapply(samps, length) %>% prop.table() # big group props

mix <-
  lapply(all_gcl, function(ag) {
    filter(ag, SillySource %in% unlist(samps))
  }) %>% bind_rows() %>%
  # rename(indiv = SillySource) %>%
  mutate(sample_type ="mixture",
         repunit = NA,
         collection = "Bering Sea",
         indiv = paste("fish", 1:150, sep = "_")) %>%
  select(!FK_FISH_ID:SillySource) %>%
  relocate(c(sample_type, repunit, collection, indiv), .before = everything())

# yukon baseline ----
yukon_sillys50 <- yukon_pops50 %>% pull(Silly) # pooled

for(si in yukon_sillys50) {
  PoolCollections.GCL(unlist(str_split(si, "[.]")))
}

all_yukon_gcl <-
  sapply(yukon_sillys50, function(pop) {
    get(paste0(pop, ".gcl"), pos = 1)
  }, simplify = FALSE)

base_yukon <-
  lapply(all_yukon_gcl, function(ag) {
    filter(ag, !SillySource %in% unlist(samps))
  }) %>% bind_rows() %>%
  left_join(select(yukon_pops50, Silly, BroadNames2), by = c("SILLY_CODE" = "Silly")) %>%
  mutate(collection = SILLY_CODE,
         indiv = SillySource,
         repunit = BroadNames2,
         sample_type = "reference") %>%
  select(!c(FK_FISH_ID:SillySource, BroadNames2)) %>%
  relocate(c(sample_type, repunit, collection, indiv), .before = everything())


usethis::use_data(mix, base_yukon, overwrite = TRUE)


# templin ----
rm(list=ls(all=TRUE))

wd <- "D:/bobby_adfg/backup_013122"
source(paste0(wd, "/R/gcl_functions.R"))
password <- readLines(paste0(wd, "/papahwork/loki_password.txt"))

yukon_loci177 <- read_csv(paste0(wd, "/projects/imbalanced_baseline/ayk_bering_juv_king/baseline_yukon/loci380_panel.csv"))  %>% filter(Panel == "Chinook_IDFG_299") %>% pull(loci380)

templin_loci42 <-
  read_csv(paste0(wd, "/projects/imbalanced_baseline/ayk_bering_juv_king/Baseline_Templin/baseline_Templin_loci_42.csv")) %>%
  pull(baseline_Templin_loci_42)
templin_loci28 <- templin_loci42[templin_loci42 %in% yukon_loci177] # all nalleles are 2

CreateLocusControl.GCL(locusnames=templin_loci28, username="bhsu", password=password)

templin_pops211 <- read_csv(paste0(wd, "/projects/imbalanced_baseline/ayk_bering_juv_king/Baseline_Templin/baseline_Templin_pops_211_info.csv"))
templin_sillys211 <- templin_pops211 %>% pull(popSILLY)
templin_sillys211[87] <- "KCHUN09.KCHUN12" # I think there's a typo
templin_sillys_all <- str_split(templin_sillys211, pattern = "[.]") %>% unlist

LOKI2R.GCL(sillyvec=templin_sillys_all, username="bhsu", password=password)

complete_grps <- ls()[grepl("[.]gcl", ls())] %>%
  gsub("[.gcl]","", .) # what's downloaded from LOKI

not_there <- templin_sillys_all[!templin_sillys_all %in% complete_grps]

pool_dis <-
  templin_sillys211 %>%
  str_split(pattern = "[.]")
pool_dis <- lapply(pool_dis, function(pd) pd[!pd %in% not_there])
pool_nms <- templin_sillys211 %>% as.list

sapply(seq(length(pool_dis)), function (i) {
  PoolCollections.GCL(pool_dis[[i]], newname = pool_nms[[i]])
})

load("data-raw/samps.rda")

all_templin_gcl <-
  sapply(templin_sillys211, function(pop) {
    get(paste0(pop, ".gcl"), pos = 1)
  }, simplify = FALSE)

base_templin <-
  lapply(all_templin_gcl, function(ag) {
    filter(ag, !SillySource %in% unlist(samps))
  }) %>% bind_rows() %>%
  left_join(select(templin_pops211, popSILLY, repunit2), by = c("SILLY_CODE" = "popSILLY")) %>%
  mutate(collection = SILLY_CODE,
         indiv = SillySource,
         repunit = repunit2,
         sample_type = "reference") %>%
  select(!c(FK_FISH_ID:SillySource, repunit2)) %>%
  relocate(c(sample_type, repunit, collection, indiv), .before = everything())


usethis::use_data(base_templin, overwrite = TRUE)


# pop info ----
templin_pops211 <- templin_pops211 %>%
  select(popSILLY, repunit2, groupvec2) %>%
  rename(collection = popSILLY, repunit = repunit2, grpvec = groupvec2)

yukon_pops50 <-  read_csv(paste0(wd, "/projects/imbalanced_baseline/ayk_bering_juv_king/baseline_yukon/50pop_groupvecs.csv")) %>%
  select(Silly, BroadGroup, BroadNames2) %>%
  rename(collection = Silly, repunit = BroadNames2, grpvec = BroadGroup)


usethis::use_data(templin_pops211, yukon_pops50, overwrite = TRUE)















