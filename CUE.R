##calculating CUE and its variance at the lake level

##Lets start with Horwood Lake in FMZ 8 Cycle 1
##need to pull-in catch and effort separately and then join in the calc process
#first calc CUE by strata, need to also make use of RSC values which are not in ind_fish table
#then aply area weighting to get whole lake AW CUE

#There is a script in fmz_background report (_dataStage) to use for reference. Note Catch tables are currently commented out because it takes some processing time


library(data.table)
library(readxl)
library(tidyverse)
library(scales)
library(FSA)
library(rgdal)
library(broom)
library(knitr)
library(kableExtra)
library(flextable)
library(reshape)


ind_fish<-fread("C:\\Users\\amosje\\Everything R\\r_projects\\cookbook_companion\\Data\\effort_and_individualy_measured_fish_July24_2018.csv")
HAC<-fread("C:\\Users\\amosje\\Everything R\\r_projects\\cookbook_companion\\Data\\Lake_Synopsis_Exhibit_Generator_Data_Oct23_2017.csv")
netting<-fread("C:\\Users\\amosje\\Everything R\\r_projects\\cookbook_companion\\Data\\BsM_Netting.csv")
bsm_bathymetry<-fread("C:\\Users\\amosje\\Everything R\\r_projects\\cookbook_companion\\Data\\BsM_Bathymetry_from_DB_July18.csv")

min_length<-1 #use to set min length that calc are performed on (eg recruits =>350 mmTlen)
fmz<-8 # use to set FMZ globally
spp<-131




#**********Effort*********************


effort<-netting%>%
  filter(BsM_Cycle==1, FMZ==fmz, GearTypeCode==1, EffortStatusCode !=3, WbyName=="Horwood L.")%>%
  select(BsM_Cycle, FMZ, BsM_ProjectName, WbyName, TargetSpecies, LakeSelection, LakeSizeClass, DepthStratum,
         GearTypeCode, GearGangTotal, EffortSampleNum, EffortStatusCode, GearGangTotal)%>%
  group_by(BsM_ProjectName, WbyName, FMZ, DepthStratum, GearTypeCode)%>%
  summarize(
    event_count=sum(!is.na(EffortSampleNum)),
    N_gangs_sum=sum(GearGangTotal),
    N_gangs_mean=mean(GearGangTotal)
    )#%>%
  #left_join(bathy, effort, by="FMZ")
  
#**********catch**********************

#WbyName=="Horwood L."

catch<-ind_fish%>%
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
#********************Now need to join effort and catch********************

Cat_Eff_join<-left_join(effort, catch, by= "DepthStratum")%>%
  select(BsM_ProjectName=BsM_ProjectName.x, WbyName=WbyName.x, DepthStratum, event_count, N_gangs_mean, Catcnt_sum, Rwt_sum, Catcnt2_sum, Rwt2_sum)%>%
  mutate(Catcnt_mn=Catcnt_sum/event_count, Rwt_mn=Rwt_sum/event_count, var_Catcnt_mn=(Catcnt2_sum-(Catcnt_sum^2)/event_count)/((event_count-1)*event_count),
         var_Rwt_mn=(Rwt2_sum-(Rwt_sum^2)/event_count)/((event_count-1)*event_count), CUE=Catcnt_mn/N_gangs_mean, CUEW=Rwt_mn/N_gangs_mean,
         var_CUE=var_Catcnt_mn/N_gangs_mean^2, var_CUEW=var_Rwt_mn/N_gangs_mean^2, SE_CUE=sqrt(var_CUE), SE_CUEW=sqrt(var_CUEW))

##Now need to bring in bathy
#**********Bathy*********************
# HAC<-HAC%>%
#    filter(str_detect(Lake_Selection,regex("Trend", ignore_case = TRUE)), FMZ==fmz, str_detect(Lake_Name,regex("Horwood", ignore_case = TRUE)))%>%
#    select(Lake_Name, FMZ, Wby_LID, Lake_Selection, Lake_Selection_Target_Species, starts_with("pArea"), -pArea_epi)%>%
#   gather(key = "temp", value = "value", -c(Lake_Name, FMZ, Wby_LID, Lake_Selection, Lake_Selection_Target_Species))
  

bathy<-bsm_bathymetry%>%
  filter(Cycle == 1, Lake_Name == "Horwood L.")%>%
  select(Cycle, FMZ, Lake_Name, LID, Depth_Strata=Contour_Depth, Surface_Area_Proportion, -Volume_Proportion)%>%
  mutate(prop=1-Surface_Area_Proportion)%>%
  mutate(strat=case_when(
    Depth_Strata<=3 ~ 1,
    Depth_Strata<=6 ~ 3,
    Depth_Strata<=12 ~ 6,
    Depth_Strata<=20 ~ 12,
    Depth_Strata<=35 ~ 20,
    Depth_Strata<=50 ~ 35,
    Depth_Strata<=75 ~ 50,
    Depth_Strata>75 ~ 50
  ))%>%
  group_by()
  
strata_areas<-data.frame("Lake_Name"=Cat_Eff_join$WbyName, "BsM_ProjectName"=Cat_Eff_join$BsM_ProjectName,"Depth_Strata"=Cat_Eff_join$DepthStratum)

bathy_final<-left_join(strata_areas, select(bathy, Depth_Strata, prop), by= "Depth_Strata")%>%
  mutate(test=lead(prop))




#**garden lake for Greg Cull

Garden<-ind_fish%>%
  filter(GearTypeCode==1, WbyName=="Garden L.", SpecCode==81)%>%
  select(BsM_ProjectName, WbyName, TargetSpecies, LakeSelection, LakeSizeClass, DepthStratum,
         GearTypeCode, GearGangTotal, EffortSampleNum, BsM_Cycle, FMZ, SpecCode, CatchCount, ClipsObserved, BestTL, AssessedFishAge, FishIndivComment)%>%
  group_by(BsM_ProjectName, WbyName, FMZ, BsM_Cycle, DepthStratum, SpecCode, BestTL, ClipsObserved, AssessedFishAge, FishIndivComment)

write_csv(Garden,"Data/garden_lake.csv")



