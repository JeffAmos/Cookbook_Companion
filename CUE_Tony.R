### Libraries ----

library(tidyverse)
library(readxl)

### Import ----

ind_fish <- read_rds("~/r_projects/testing/_data/netting.rds")
netting <- read_csv("Data/BsM_Netting.csv")
bsm_bathymetry <- read_xlsx("Data/BsM_Bathymetry_from_DB_July18.xlsx")

### Parameters ----

min_length<-1 #use to set min length that calc are performed on (eg recruits =>350 mmTlen)
fmz<-8 # use to set FMZ globally
spp<-131

### Effort ----

effort <- netting%>%
  filter(BsM_Cycle==1, FMZ==fmz, GearTypeCode==1, EffortStatusCode !=3, WbyName=="Horwood L.")%>%
  select(BsM_Cycle, FMZ, BsM_ProjectName, WbyName, TargetSpecies, LakeSelection, LakeSizeClass, DepthStratum,
         GearTypeCode, GearGangTotal, EffortSampleNum, EffortStatusCode, GearGangTotal)%>%
  group_by(BsM_ProjectName, WbyName, FMZ, DepthStratum, GearTypeCode)%>%
  summarize(
    event_count=sum(!is.na(EffortSampleNum)),
    N_gangs_sum=sum(GearGangTotal),
    N_gangs_mean=mean(GearGangTotal)
  )

### Catch ----

catch <- ind_fish%>%
  filter(BsM_Cycle==1, FMZ==fmz, SpecCode==spp, GearTypeCode==1, EffortStatusCode !=3, WbyName=="Horwood L.")%>%
  select(BsM_ProjectName, WbyName, LakeSizeClass, DepthStratum,
         GearGangTotal, EffortSampleNum, SpecCode, BestRoundWeight, BestTL)%>%
  group_by(BsM_ProjectName, WbyName, LakeSizeClass, DepthStratum, EffortSampleNum)%>%
  summarize(
    Catcnt_sum=sum(!is.na(BestTL)),
    Rwt_sum=sum(BestRoundWeight),
    Catcnt2_sum=Catcnt_sum^2,
    Rwt2_sum=Rwt_sum^2
  )%>%
  group_by(BsM_ProjectName, WbyName, LakeSizeClass, DepthStratum)%>%
  select(BsM_ProjectName, WbyName, DepthStratum, LakeSizeClass, Catcnt_sum, Rwt_sum, Catcnt2_sum, Rwt2_sum)%>%
  summarize(
    Catcnt_sum=sum(Catcnt_sum),
    Rwt_sum=sum(Rwt_sum),
    Catcnt2_sum=sum(Catcnt2_sum),
    Rwt2_sum=sum(Rwt2_sum)
  )

### Join ----

Cat_Eff_join<-left_join(effort, catch, by= "DepthStratum")%>%
  select(BsM_ProjectName=BsM_ProjectName.x, WbyName=WbyName.x, DepthStratum, event_count, N_gangs_mean, Catcnt_sum, Rwt_sum, Catcnt2_sum, Rwt2_sum)%>%
  mutate(Catcnt_mn=Catcnt_sum/event_count, Rwt_mn=Rwt_sum/event_count, var_Catcnt_mn=(Catcnt2_sum-(Catcnt_sum^2)/event_count)/((event_count-1)*event_count),
         var_Rwt_mn=(Rwt2_sum-(Rwt_sum^2)/event_count)/((event_count-1)*event_count), CUE=Catcnt_mn/N_gangs_mean, CUEW=Rwt_mn/N_gangs_mean,
         var_CUE=var_Catcnt_mn/N_gangs_mean^2, var_CUEW=var_Rwt_mn/N_gangs_mean^2, SE_CUE=sqrt(var_CUE), SE_CUEW=sqrt(var_CUEW))

### Bathymetry ----

strata <- c(0, 3, 6, 12, 20, 35, 50, 75)

bathy <- bsm_bathymetry %>%
  mutate_at(vars(Cycle), as.numeric) %>%
  filter(Cycle == 1, `Lake Name` == "Horwood L.") %>% # changed Lake_Name to `Lake Name`
  select(Cycle, FMZ, `Lake Name`, LID, Depth_Strata=`Contour Depth (m)`, `Surface Area Proportion`, -`Volume Proportion`) %>%
  filter(Depth_Strata %in% strata) %>%
  mutate(prop = `Surface Area Proportion` - lead(`Surface Area Proportion`)) %>%
  mutate(Depth_Strata = if_else(Depth_Strata == 0, 1, Depth_Strata)) #%>%
  # mutate(prop=1-`Surface Area Proportion`) %>%
  # mutate(strat=case_when(
  #   Depth_Strata<=3 ~ 1,
  #   Depth_Strata<=6 ~ 3,
  #   Depth_Strata<=12 ~ 6,
  #   Depth_Strata<=20 ~ 12,
  #   Depth_Strata<=35 ~ 20,
  #   Depth_Strata<=50 ~ 35,
  #   Depth_Strata<=75 ~ 50,
  #   Depth_Strata>75 ~ 50
  # ))#%>%
  # group_by()

# strata_areas<-data.frame("Lake_Name"=Cat_Eff_join$WbyName, "BsM_ProjectName"=Cat_Eff_join$BsM_ProjectName,"Depth_Strata"=Cat_Eff_join$DepthStratum)

bathy_final <- left_join(Cat_Eff_join, select(bathy, Depth_Strata, prop), by= c("DepthStratum" = "Depth_Strata"))

                       