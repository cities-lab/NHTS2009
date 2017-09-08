library(tidyverse)

poverty_df <- read_csv("inst/extdata/poverty_guideline.csv", comment = "#")

zip_url <- "http://nhts.ornl.gov/2009/download/Ascii.zip"
data_dir <- tempdir()
zip_file <- file.path(data_dir, "NHTS2009.zip")

download.file(zip_url, destfile=zip_file, method="libcurl")
unzip(zip_file, exdir = data_dir)

hh_file <- file.path(data_dir, "Ascii/HHV2PUB.CSV")
pp_file <- file.path(data_dir, "Ascii/PERV2PUB.CSV")
dd_file <- file.path(data_dir, "Ascii/DAYV2PUB.CSV")
vv_file <- file.path(data_dir, "Ascii/VEHV2PUB.CSV")

stopifnot(all(map_lgl(c(hh_file, pp_file, dd_file, vv_file), file.exists)))

hh_raw <- read_csv(hh_file, col_types = cols(HOUSEID=col_character()))
pp_raw <- read_csv(pp_file, col_types = cols(HOUSEID=col_character()))
dd_raw <- read_csv(dd_file, col_types = cols(HOUSEID=col_character()))
vv_raw <- read_csv(vv_file, col_types = cols(HOUSEID=col_character()))

#devtools::use_data(hh_raw)
#devtools::use_data(pp_raw)
#devtools::use_data(dd_raw)
#devtools::use_data(vv_raw)

Hh_df <- hh_raw %>%
  mutate_if(is.factor, as.character) %>%
  left_join(poverty_df, by=c("HHSIZE")) %>%
  transmute(
    HOUSEID=HOUSEID,
    HTPPOPDN=as.integer(HTPPOPDN),
    #Urban = ifelse(URBRUR=="01", 1L, 0L),

    HHFAMINCVAL=case_when(HHFAMINC %in% c("-9", "-8", "-7") ~ as.numeric(NA),
                          HHFAMINC == '01' ~ (0+5000)/2,
                          HHFAMINC == '02' ~ (5001+10000)/2,
                          HHFAMINC == '03' ~ (10001+15000)/2,
                          HHFAMINC == '04' ~ (15001+20000)/2,
                          HHFAMINC == '05' ~ (20001+25000)/2,
                          HHFAMINC == '06' ~ (25001+30000)/2,
                          HHFAMINC == '07' ~ (30001+35000)/2,
                          HHFAMINC == '08' ~ (35001+40000)/2,
                          HHFAMINC == '09' ~ (40001+45000)/2,
                          HHFAMINC == '10' ~ (45001+50000)/2,
                          HHFAMINC == '11' ~ (50001+55000)/2,
                          HHFAMINC == '12' ~ (55001+60000)/2,
                          HHFAMINC == '13' ~ (60001+65000)/2,
                          HHFAMINC == '14' ~ (65001+70000)/2,
                          HHFAMINC == '15' ~ (70001+75000)/2,
                          HHFAMINC == '16' ~ (75001+80000)/2,
                          HHFAMINC == '17' ~ (80001+100000)/2,
                          HHFAMINC == '18' ~ (100001+150000)/2
    ),

    HHFAMINC = ifelse(HHFAMINC %in% c("-9", "-8", "-7"), NA, HHFAMINC),
    HHFAMINC10k = case_when(
      #HHFAMINC %in% c("-9", "-8", "-7") ~ NA,
      HHFAMINC %in% c("01", "02") ~ "<$10k",
      HHFAMINC %in% c("03", "04") ~ "$10-20k",
      HHFAMINC %in% c("05", "06") ~ "$20-30k",
      HHFAMINC %in% c("07", "08") ~ "$30-40k",
      HHFAMINC %in% c("09", "10") ~ "$40-50k",
      HHFAMINC %in% c("11", "12") ~ "$50-60k",
      HHFAMINC %in% c("13", "14") ~ "$60-70k",
      HHFAMINC %in% c("15", "16") ~ "$70-80k",
      HHFAMINC == "17" ~ "$80-100k",
      HHFAMINC == "18" ~ ">$100k",
      TRUE ~ HHFAMINC
    ),

    HHFAMINC20k = case_when(
      HHFAMINC10k == "<$10k" ~ "<$10k",
      HHFAMINC10k %in% c("$10-20k", "$20-30k") ~ "$10-30k",
      HHFAMINC10k %in% c("$30-40k", "$40-50k") ~ "$30-50k",
      HHFAMINC10k %in% c("$50-60k", "$60-70k") ~ "$50-70k",
      HHFAMINC10k %in% c("$70-80k", "$80-100k") ~ "$70-100k",
      HHFAMINC10k == ">$100k" ~ ">$100k",
      TRUE ~ HHFAMINC10k
    ),

    HHFAMINC20k = factor(HHFAMINC20k, levels=c("<$10k", "$10-30k", "$30-50k", "$50-70k", "$70-100k", ">$100k")),

    #Whether households are below poverty line
    #poverty = ifelse(HHFAMINCVAL - 2500 < POVERTY_GUIDELINE, 1, 0),

    #CENSUS_R = as.character(CENSUS_R),
    CENSUS_R=recode(CENSUS_R, "01"="NE", "02"="MW", "03"="S", "04"="W"),
    CENSUS_D = as.character(CENSUS_D),
    CENSUS_D=recode(CENSUS_D, '01'='New England', '02'='Middle Atlantic',
                    '03'='East North Central', '04'='West North Central',
                    '05'='South Atlantic', '06'='East South Central',
                    '07'='West South Central', '08'='Mountain', '09'='Pacific'),

    #URBAN=as.character(URBAN),
    #URBAN=ifelse(URBAN=="01", "Urban Area", URBAN),
    #URBAN=ifelse(URBAN=="02", "Urban Cluster", URBAN),
    #URBAN=ifelse(URBAN %in% c("Urban Area", "Urban Cluster"), URBAN, "Non-Urban"),
    #Urban=ifelse(URBAN=="01", 1L, 0L),

    #LIF_CYC = as.character(
    LifeCycle = case_when(LIF_CYC=="01" ~ "Single",
                        LIF_CYC %in% c("02") ~ "Couple w/o children",
                        LIF_CYC %in% c("03", "04", "05", "06", "07", "08") ~ "Parents w/ children",
                        #LIF_CYC %in% c("03", "04") ~ "w/ children 0-5",
                        #LIF_CYC %in% c("05", "06") ~ "w/ children 6-15",
                        #LIF_CYC %in% c("07", "08") ~ "w/ children 16-21",
                        LIF_CYC %in% c("09", "10") ~ "Empty Nester"
    ),

    Drivers=DRVRCNT,
    HhSize=HHSIZE,
    NUMADLT,
    Workers=WRKCOUNT,
    Vehicles=HHVEHCNT,
    TRAVDAY, TDAYDATE,
    WEEKDAY = (as.character(TRAVDAY) %in% c("02", "03", "04", "05", "06")),
    WEEKEND = (as.character(TRAVDAY) %in% c("01", "07")),
    HBHTNRNT = as.character(HBHTNRNT),
    HBHTNRNT = ifelse(HBHTNRNT=="-9", NA, HBHTNRNT),
    HBHTNRNT = as.numeric(HBHTNRNT),
    HBHUR, #HBPPOPDN=as.numeric(as.character(HBPPOPDN)),  #home bg renter%, urban/rural, pop density, residential unit density
    #HBRESDN=as.numeric(as.character(HBRESDN)),
    HTHTNRNT = as.character(HTHTNRNT),
    HTHTNRNT = ifelse(HTHTNRNT=="-9", NA, HTHTNRNT),
    HTHTNRNT = as.numeric(HTHTNRNT),
    #HTPPOPDN, HTRESDN=as.numeric(as.character(HTRESDN)), #home tract renter%, emp density, pop density, residential unit density
    #HTEEMPDN=as.numeric(as.character(HTEEMPDN)), HTEEMPDN=ifelse(HTEEMPDN==-9, NA, HTEEMPDN),
    MSACAT,
    MSASIZE,
    RAIL=ifelse(RAIL=="01", 1, 0),         #MSA category, MSA size, and
    URBAN, URBANSIZE, URBRUR, WTHHFIN,     #home address in urbanized area, size of urbanize area, urban/rural,
    FLAG100
  ) %>%
  filter(FLAG100=="01")

pp_hh_df <- pp_raw %>%
  mutate_if(is.factor, as.character) %>%
  group_by(HOUSEID) %>%
  summarize(Age65Plus=sum(R_AGE>=65),
            Age0to14=sum(R_AGE<14),
            DrvAgePop= first(HHSIZE) - Age0to14
            #workers=sum(WORKER=="01"),  #=hh.WRKCOUNT
            #drivers=sum(DRIVER=="01")   #=hh.DRVRCNT
  )


Hh_df <- Hh_df %>% left_join(pp_hh_df, by="HOUSEID")

vv_df <- vv_raw %>%
  mutate_if(is.factor, as.character) %>%
  filter(VEHCOMM=="02") %>%  ## exclude vehicles w/ commercial license, not available in the public VV file
  mutate(ANNMILES=ifelse(ANNMILES<0, NA, ANNMILES),
         BESTMILE=ifelse(BESTMILE<0, NA, BESTMILE))

# aggregate vehicle vmt to household
hh_vv_df <- vv_df %>%
  group_by(HOUSEID) %>%
  dplyr::summarize(ANNMILES=sum(ANNMILES, na.rm=T),
                   BESTMILE=sum(BESTMILE, na.rm=T)) %>%
  ungroup()

Hh_df <- Hh_df %>%
  left_join(hh_vv_df)

dd_df <- dd_raw %>%
  #filter(as.character(TRIPTIME)=="01") %>% #only include those with correct trip time
  filter(TRPMILES >= 0, TRVL_MIN>=0,  as.character(TRIPPURP) != '-9') %>%
  #dplyr::select(HOUSEID, TRIPTIME, TRPMILES, VMT_MILE, TRPTRANS, TRPTRNOS, TRVL_MIN, TRVLCMIN,
  #TRVLHR, TRVLMIN, WAIT_HR, WAIT_MIN, WHYTRP90, WHYTRPSP)
  dplyr::select(HOUSEID, TRPTRANS, TRPMILES, VMT_MILE, TRVL_MIN, TRVLCMIN, TRIPPURP, VEHID) %>%
  mutate(HOUSEID=as.character(HOUSEID))

dd_df <- dd_df %>%
  mutate(TRPTRANS = as.character(TRPTRANS),
         mode = case_when(TRPTRANS %in% c('01', '02', '03', '04', '05', '06', '07') ~ 'Auto',
                          TRPTRANS %in% c('09', '10', '11', '12', '13', '14', '15', '16', '17', '18') ~ 'Transit',
                          TRPTRANS %in% c('22') ~ 'Bike',
                          TRPTRANS %in% c('23') ~ 'Walk',
                          TRUE ~ 'Other')
  )

trpmiles.cutoffs <- dd_df %>%
  left_join(vv_raw %>% dplyr::transmute(HOUSEID=as.character(HOUSEID), VEHID, VEHCOMM),
            by=c("HOUSEID", "VEHID")) %>%
  filter(is.na(VEHCOMM) | as.character(VEHCOMM)=="02") %>%
  group_by(mode) %>%
  summarize(cutoff = quantile(TRPMILES, probs=0.99))

dd_df <- dd_df %>%
  right_join(trpmiles.cutoffs) %>%
  filter(TRPMILES <= cutoff) %>%
  dplyr::select(-cutoff)

hh.x.mode_dd <- dd_df %>%
  filter(mode != "Other", TRPMILES>=0, TRVL_MIN>=0) %>%
  group_by(HOUSEID, mode) %>%
  summarize(td.miles=sum(TRPMILES, na.rm=T),
            tt.mins=sum(TRVL_MIN, na.rm=T),
            ntrips=n()) %>%
  ungroup() %>%
  mutate(atd.miles=td.miles/ntrips)

#compute %td by mode
hh.x.mode_dd <- hh.x.mode_dd %>%
  filter(td.miles > 0) %>% #not necessary as the minimum td.miles is larger than 0
  group_by(HOUSEID) %>%
  mutate(td.pct=td.miles/sum(td.miles),
         tt.pct=tt.mins/sum(tt.mins)) %>%
  ungroup()

## convert from
# HOUSEID, MODE, td.miles, tt.mins, td.pct, tt.pct
## to
# HOUSEID, td.miles.Auto, tt.mins.Auto, td.pct.Auto, tt.pct.Auto, ...

hh_dd_mode_df <- hh.x.mode_dd %>%
  gather(key="variable", value="value", td.miles:tt.pct) %>%
  unite(col="variable", variable, mode, sep=".") %>%
  spread(variable, value, fill=0)

Hh_df <- Hh_df %>% left_join(hh_dd_mode_df, by="HOUSEID")

# # confusing renaming to get sane column names
# hh_td.miles <- hh.x.mode_dd %>%
#   dplyr::select(HOUSEID, value=td.miles, td.miles=mode) %>%
#   tidyr::spread(key=td.miles, value=value, fill=0.0, sep=".")
#
# hh_td.pct <- hh.x.mode_dd %>%
#   dplyr::select(HOUSEID, value=td.pct, tdpct=mode) %>%
#   tidyr::spread(key=tdpct, value=value, fill=0.0, sep=".")
#
# hh_atd.miles <- hh.x.mode_dd %>%
#   dplyr::select(HOUSEID, value=atd.miles, atd.miles=mode) %>%
#   tidyr::spread(key=atd.miles, value=value, fill=0.0, sep=".")
#
# hh_ntrips <- hh.x.mode_dd %>%
#   dplyr::select(HOUSEID, value=ntrips, ntrips=mode) %>%
#   tidyr::spread(key=ntrips, value=value, fill=0.0, sep=".")
#
# Hh_df %<>% left_join(hh_td.miles, by="HOUSEID")
# Hh_df %<>% left_join(hh_td.pct, by="HOUSEID")
# Hh_df %<>% left_join(hh_atd.miles, by="HOUSEID")
# Hh_df %<>% left_join(hh_ntrips, by="HOUSEID")

# get DVMT from dd
dd_hh_df <- dd_df %>%
  mutate(VMT_MILE=ifelse(VMT_MILE<0, 0, VMT_MILE)) %>%
  filter(mode != "Other") %>%
  group_by(HOUSEID) %>%
  summarize(DVMT=sum(VMT_MILE, na.rm=T),
            td.miles=sum(TRPMILES, na.rm=T),
            tt.mins=sum(TRVL_MIN, na.rm=T),
            ntrips=n())

Hh_df <- Hh_df %>%
  left_join(dd_hh_df, by="HOUSEID") %>%
  mutate(DVMT=ifelse(is.na(DVMT) | is.null(DVMT), 0, DVMT),
         td.miles=ifelse(is.na(td.miles) | is.null(td.miles), 0, td.miles),
         tt.mins=ifelse(is.na(tt.mins) | is.null(tt.mins), 0, tt.mins)
  )

## compute variables derived from other variables

Hh_df <- Hh_df %>%
  mutate(
    BESTMILEcap=BESTMILE/HhSize,
    powBESTMILE=BESTMILE^0.38,
    AADVMT=BESTMILE/365,
    AADVMT.int=round(AADVMT, 0),
    lnBESTMILE=log1p(BESTMILE),

    DVMT.int=round(DVMT, 0),
    DVMTcap=DVMT/HhSize,
    powDVMT=DVMT^0.18,

    td.milescap=td.miles/HhSize,
    lntd.milescap=log(td.milescap),

    VehPerDrvAgePop = Vehicles/DrvAgePop,
    VehPerDriver = ifelse(Drivers!=0, Vehicles/Drivers, 0),
    LogIncome = log1p(HHFAMINCVAL),

    ZeroVeh=ifelse(Vehicles==0, 1, 0),
    ZeroDVMT=ifelse(DVMT==0, 1, 0),
    ZeroAADVMT=ifelse(AADVMT==0, 1, 0),

    #Tranmilescap=UZAAVRM/UZAPOP,
    #Fwylnmicap=UZAFWLM/UZAPOP,

    #TranRevMiP1k = 1000 * Tranmilescap,
    #FwyLaneMiP1k = 1000 * Fwylnmicap,

    #TRPOPDEN=TRPOP/TRAREA,
    #TRHUDEN=TRHU/TRAREA,
    #TREMPDEN=TREMP/TRAREA,
    #TRACTDEN=TRACT/TRAREA,
    #TRJOBPOP=TREMP/TRPOP,
    #TRJOBHH=TREMP/TRHU
  )

devtools::use_data(Hh_df, overwrite=TRUE)
