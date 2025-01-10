# ==========================================================================
# plots to check logger data and relevant meta-data in FIELD date time
# ==========================================================================


# PROCEDURE AND INFO 
  # run with field data and check printed output in part 'estimate first_egg and inc_start (and true/would be hatch date)'
  # enter acto_based data
  # run with acto_based extracted data and check printed output in part 'estimate first_egg and inc_start (and true/would be hatch date)'

  # end_field, end_state_field, end(field) - based on what people have entered in the googlesheets in the visits
  # end_ and, end_state final), end (final) - final estimated end
  # end of incubation is not indicated in the legend because it is clear from visits data within the actogram

# SETTINGS
  Sys.setenv(TZ="UTC") # set time to UTC to have always same time
  warn_off = TRUE#FALSE #TRUE#TRUE#FALSE #
  load_from = "comp" #  "google" #   load the data from comp (computer) or from google (googlesheets) 
  #load_from =   "google" #  load the data from comp (computer) or from google (googlesheets) 
  export = "d&a" #"acto" #"acto" #"data", "acto" "d&a"
  out_ = 'Data/to_extract/'# where to export
  out_a = 'Outputs/Actograms/' # 'inc_extract/acto_original_03/'# where to export
  bip = TRUE
  which_data = c('check') # c('to_db','mont','check_PS2','check_WE','mb','rm_check')#c('ff')#c('to_db','mont','check_PS2','check_WE','mb','rm_check')#c('check_PS2')#c('vis')#
  which_data_export = c('check')
  extracted = FALSE #extracted = TRUE  #FALSE #TFALSE#ALSE #TRUE  #limit data to those when nest was active
  # Tools (start R from project folder - Sublime: cmd + shift +t)
  specific = NULL #,'MPNR21BWST03' # NULL #,'SVAL19REPHN4')#NULL #c('PRCU22LESE01')# c('LHHMS002','SVAL19REPHN4')  #NULL#, 'c2016-SSMB03', 'c2016-SSMB07')#NULL#c('18MSR002','SN1801','SN1808','CH01FP19','LHHMS001','KNBA19FERZ1','KNBA19FERV6','MEYN19LSPL02','MEYN19LSPL04','MEYN19LSPL15','TAVA19WAJA02','REPH050_19','MPNR20GPSN01','WEST20RUFF01','PODL20GRSN11','BRPO_2','20CPPN021','12rnphwe41','DUSE21COSN01','OLNO01','CATH01','PODL22RUFF01')#NULL #c('MPNR20GPSN01')#c("IT04CS","IT35RU","IT58RU")#NULL #c("PODL21GRSN20")# c("11rnphee54")#c("11rnphee54","12rnphwe16","12rnphmh72") #NULL #"12rnphwe05"#"e2017-WRLW06"#NULL #"MPNR20GPSN01"#"PRCU20LESE01" #NULL #"LIPS20COSN04"#c('REPH100_18','REPH021_19','REPH123_18', 'REPH009_19','REPH272_18')#NULL #"DUSE20BLGO03"#c('DUSE20COSN01','DUSE20COSN02')# "KNBA19FERJ1" #NULL# 'KNBA19PHAV10' #NULL#c('MEYN19TEST11') #NULL#c('KNBA19MINZ2') #NULL#c('ZACK19SAND08') #c('ZACK19SAND10') #NULL#c('ZACK19SAND10') # 'TS02','TS04','TS06', 'TS09') #NULL #c('19TAH040')#
 
  adjust_ = TRUE #adjust visits for misinterpreted cases to generate proper hatching/laying

# PACKAGES & SUPPORTING DATA
  require(here) 
  source(here::here('R/utils.R')) # this code is for running the script utils.R, which is in directory. 
  #In the next line, JMZ has changed the package "maptools" by "sf", since "maptools" is no longer available for installation and sf containst the crepescule . 
  packages = c('anytime','data.table', 'DataEntry.validation', 'DT', 'foreach', 'ggplot2', 'ggthemes', 'glue','googledrive', 'googlesheets4', 'grid', 'htmlTable', 'lattice', 'lubridate', 'magrittr', 'terra', 'openxlsx','plyr','raster','readxl', 'sf', 'stringr','zoo')
  sapply(packages, function(x) suppressPackageStartupMessages(using(x)) )
  #devtools::install_github("mpio-be/DataEntry.validation", build_vignettes=TRUE)
  
  source(here::here('R/fun_lay_inc_hatch_est.R'))
  source(here::here('R/dat_actogram_map.R'))
  source(here::here('R/fun_actogram.R'))

  # video data
  # Fast read of the video datasheet
   vd = fread('Data/Data_NestBehaviour_Cams.csv')
  # change the column names of the the datasheet. The first vector (c) corresponds to old column names and the second vector to new names. 
   setnames(vd, c('date', 'time','beh'), new=c('day', 'start_time','what'))
   vd[,start_time := getime(as.POSIXct(start_time,format="%H:%M"))]
   vd[,end_date := day]
   vd[,end_time := start_time + 20/60/60]
   vd[what %in% c('Incubating','Preening','Changing','Panting','WingOpening','EggTurning','BodyShaking'), col := 'black']
   vd[is.na(col), col := 'grey']

   # file lists
    # MSR
    f = data.table(
      f = c(list.files(path = 'Data/MSR/', pattern = '.csv', recursive = TRUE, full.names = TRUE)),
      f2 = c(list.files(path = 'Data/MSR/', pattern = '.csv', recursive = TRUE, full.names = FALSE))
      )
    f[ ,file_name := substring(f2, 1, nchar(f2)-4)]
    #f[ ,nest := substring(f2, 30, nchar(f2)-4)]
    
    # RFID
    fr = data.table(
      f = c(list.files(path = 'Data/RFID/', pattern = '.TXT', recursive = TRUE, full.names = TRUE)),
      f2 = c(list.files(path = 'Data/RFID/', pattern = '.TXT', recursive = TRUE, full.names = FALSE))
      )
    fr[ ,file_name := substring(f2, 1, nchar(f2)-4)]

    # CAM
    fc = data.table(
      f = c(list.files(path = 'Data/CAM/', pattern = '.TXT', recursive = TRUE, full.names = TRUE)),
      f2 = c(list.files(path = 'Data/CAM/', pattern = '.TXT', recursive = TRUE, full.names = FALSE))
      )
    fc[ ,file_name := substring(f2, 1, nchar(f2)-4)]

  # correct logger time
    cort = data.table(
              file = c( 
                        'TT_2016_SSMB07',
                        'TT_2016_SSLS02',
                        'TT_2016_SSMB03',
                        'RF02_CRBA21TBPL06',
                        'MSR326614_211015_142643_NEPA_PABE21TBPL03',
                        'MSR326616_211022_140843_NEPA_BLBE21TBPL05',
                        'TT31_190622_131300_ZACK_S0219_edited',
                        'TT23_190627_130500_ZACK_S0819_edited',
                        'TT24_190629_181500_ZACK_S1019_edited',
                        'TT14_190702_153000_ZACK_S1019_edited',
                        'TT32_190627_123500_ZACK_S0719_edited',
                        'MSR324995_190409_101130_SAMI19EUWO01',
                        'MSR324999_190419_084114_SAJO19COSN01',
                        'MSR324993_190419_132539_SAJO19COSN02',
                        'MSR324996_190421_075739_SAJO19COSN03',
                        'MSR326744_190710_120715_SVAL19REPHN3',
                        'MSR326695_200617_061413_MEYN_TEST01',
                        'MSR326698_200617_083759_MEYN_LSPL02',
                        'MSR326694_200618_001258_MEYN_TEST03',
                        'MSR326697_200619_080508_MEYN_LSPL05',
                        'MSR326694_200625_010528_MEYN_LSPL06',
                        'MSR326695_200630_021030_MEYN_LSPL07',
                        'HAST20EUDO04_RFID11_008367014C9581E8',
                        'HB_2005_DUR03_a',
                        'HB_2005_DUR03_b',
                        'HB_2005_DUR03_c',
                        'HB_2002-07-01_RPA01',
                        'HB_2002-07-01_RPA02',
                        'TTn967099_VAPE22TEST11',
                        'TTn966071_VAPE22TEST12',
                        'TTn967415_VAPE22TEST13',
                        'TTn967468_VAPE22TEST14',
                        'TTn966072_VAPE22TEST15',
                        'TTn967434_VAPE22TEST16',
                        'TTn967092_VAPE22TEST17',
                        'TTn967086_VAPE22TEST18',
                        'TTn967102_VAPE22TEST19'
                        ),
              hours = c(
                        as.numeric(difftime(as.POSIXct('2016-06-24 11:58'),as.POSIXct('2016-06-21 03:26'), units = 'hours')),
                        #as.POSIXct('2016-06-21 06:49')+as.numeric(difftime(as.POSIXct('2016-06-24 11:58'),as.POSIXct('2016-06-21 03:26'), units = 'secs'))
                        as.numeric(difftime(as.POSIXct('2016-06-24 11:56'),as.POSIXct('2016-06-21 04:24'), units = 'hours')), #as.POSIXct('2016-06-21 09:29:27')+as.numeric(difftime(as.POSIXct('2016-06-24 11:56'),as.POSIXct('2016-06-21 04:24'), units = 'secs'))
                        as.numeric(difftime(as.POSIXct('2016-06-24 11:59'),as.POSIXct('2016-06-21 03:27'), units = 'hours')),
                        #as.POSIXct('2016-06-21 07:32')+as.numeric(difftime(as.POSIXct('2016-06-24 11:59'),as.POSIXct('2016-06-21 03:27'), units = 'secs'))
                        -1, 
                        1,
                        1,
                        -1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        -1,
                        9,
                        9,
                        9,
                        9,
                        9,
                        9,
                        442508.1666667,
                        -1,
                        -1,
                        -1,
                        48,
                        48,
                        2,
                        2,
                        2,
                        2,
                        2,
                        2,
                        2,
                        2,
                        2
                        ),
              comments = c(
                          'logger running on wrong time and date',
                          'logger running on wrong time and date',
                          'logger running on wrong time and date',
                           'run an hour ahead',
                           'computer on Virginian time which is -1 to Porta Madre',
                           'computer on Virginian time which is -1 to Porta Madre',
                           'computer on UTC+1 Dutch time',
                           'computer on UTC-1 Greenland time',
                           'computer on UTC-1 Greenland time',
                           'computer on UTC-1 Greenland time',
                           'computer on UTC-1 Greenland time',
                           'computer on UTC winter time',
                           'computer on UTC winter time',
                           'computer on UTC winter time',
                           'computer on UTC winter time',
                           'computer on UTC+1',
                           'computer on UTC+3 Moscow time',
                           'computer on UTC+3 Moscow time',
                           'computer on UTC+3 Moscow time',
                           'computer on UTC+3 Moscow time',
                           'computer on UTC+3 Moscow time',
                           'computer on UTC+3 Moscow time',
                           'RFID time not set',
                           'computer 1 hour off then usual at EABA',
                           'computer 1 hour off then usual at EABA',
                           'computer 1 hour off then usual at EABA',
                           '2 days off, not noted why',
                           '2 days off, not noted why',
                           'computer 2h begind the field time',
                           'computer 2h begind the field time',
                           'computer 2h begind the field time',
                           'computer 2h begind the field time',
                           'computer 2h begind the field time',
                           'computer 2h begind the field time',
                           'computer 2h begind the field time',
                           'computer 2h begind the field time',
                           'computer 2h begind the field time'
                           )
            )  

if(warn_off == TRUE) options(warn=0)
# get data, backup and load 
    if(load_from == 'google'){source(here::here('R/DB_backup.R'))}
    
    fd = c(list.files(here::here('Data'), pattern = 'DB_20', recursive = TRUE, full.names = TRUE))
    load(fd) 
    # summary(factor(n$MB_check))

if(warn_off == TRUE) options(warn=2) 

# TEMP CHECK - re-run and recheck these 309 nests - THEN DELETE
  unique(e$nest[!is.na(e$float_heigh) & e$float_height>-1]) 
  e[!is.na(float_height) & float_height>-1]
  length(unique(e[!is.na(float_height) & float_height>-1, nest]))

# Prepare metadata
  r[, correction:=as.numeric(difftime(datetime_real,datetime_, units='secs'))]
  parm = parm[!species %in% c('TEST')]
  s[, field_plus := hours_from_UTC_comp-hours_from_UTC_field]
  setnames(sp, c("abb"), c("species"))
  ile[, c("rowid","source", "esc_m", "comments") := NULL]
  ileg = ile[site == 'GENR']
  if(nrow(a)!=0){a = a[!(is.na(start_))]}
  
  if(length(specific)>0){
     n =n[nest %in% specific]
      } else {
        n = n[MB_check %in% which_data] #%>% nrow() # nests with data
      }
  #if(has_logger_data == TRUE){n = n[nest %in% g[!data %in% c('no',NA), nest]] }
  #n_no_dat = n[!nest %in% g[!data %in% c('no',NA), nest]]
  n[, first_egg := ifelse(first_egg == 'use_eggs_visits', NA, first_egg)]
  n[, first_egg := as.POSIXct(first_egg)] # MIGHT BE NEEDED
  n[, inc_start := ifelse(inc_start == 'use_eggs_visits', NA, inc_start)]
  n[, inc_start := as.POSIXct(inc_start)] # MIGHT BE NEEDED
  n[, hatch_date := as.POSIXct(NA)]
  n[, full_clutch_found := nest_found] # is adjusted for nests found during laying below in the visits script
  n[, laid_before_full := as.POSIXct(NA)] # datetime (closest to full_clutch_found) when maximum known clutch size smaller then full clutch was present in the nest (is adjusted for nests found during laying below in the visits script)
  n[, clutch_before_full := as.numeric(NA)] # number of eggs for laid_before_full is adjusted for nests found during laying below in the visits script
    #n$lay_interval = ile$lay_interval[match(paste(n$site,n$species), paste(ile$site,ile$species))]
    #n[ , layincint := (clutch - 1) * lay_interval]

   #n = n[site  %in% c('KNBA','PODL') & nest %in% g[!data %in% c('no',NA), nest]] #%>% nrow() # nests with data
  n = merge(n, sp, by = 'species', sort = FALSE, all.x = TRUE)
  n[, act_ID := formatC(rowid, width = 4, format = "d", flag = "0")]

  # add population/species specific info from 'ile'; GENR site gets species data, the rest, population specific data
   n[, sites := ifelse(paste(site,species) %in% ile[, paste(site,species)], site, 'GENR')]
   n = merge(n, ile, by.x = c('sites','species'),by.y = c('site','species'), sort = FALSE, all.x = TRUE)
   n[is.na(clutch), clutch := as.numeric(ileg$clutch_size[match(n[is.na(clutch), species], ileg$species)])]
   n[ , clutch := as.numeric(clutch)]

  # add time zone info
   n = merge(n, s[,list(abb, field_plus, time_zone)] , by.x = c('site'), by.y = 'abb', sort = FALSE, all.x = TRUE)
   n[site %in% 'PRBA' & year %in% 2019, field_plus := 2] # adjust for wrong datetime in 2019

   #summary(factor(nchar(n$end_)))
   #nn = n[nchar(end_)==19]
   #n[nchar(end_)==19, end_ := substring(end_,1, nchar(end_)-3)] 
   #as.POSIXct('2018-06-23 19:19', format="%Y-%m-%d %H:%M")
   #as.POSIXct(c('2018-06-21','2018-06-21 00:00','2018-06-23 19:19', '2018-06-23 19:19:00') , format="%Y-%m-%d %H:%M")
   n[nchar(end_)==10, end_ := paste(end_,"00:00")] 
   n[, end_field := as.POSIXct(end_, format="%Y-%m-%d %H:%M")]
   n[, end_ := as.POSIXct(end_, format="%Y-%m-%d %H:%M")]
   n[, end_state_field := end_state]

  ca = ca[nest %in% n$nest,]
  
  g = g[nest %in% n$nest,]
  g = g[getime(taken) %in% 0, taken := taken + 60*60*23]

  e = e[nest %in% n$nest,]
   #e = e[, float_height := as.character(float_height)]
   #e = e[nest %in% n[method == 'flotation',. (nest)]]
   e = e[!is.na(float_angle),]
   #e[ , float_angle := as.numeric(ifelse(float_angle<21,21, ifelse(float_angle>89,89, float_angle)))] # equation uses 20> angle < 90
   e = merge(e, n[,.(nest,clutch)], by = 'nest', sort = FALSE, all.x = TRUE)
   e[, temp:=as.POSIXct(datetime_processed)]
   e[nchar(datetime_processed)==16, temp:=as.POSIXct(datetime_processed)]
   e[, datetime_processed := temp]
   e[, site := ifelse(paste(site,species) %in% ile[, paste(site,species)], site, 'GENR')]
   e = merge(e, ile[,.(site,species,lay_interval, inc_period, clutch_size)], by = c('site','species'), sort = FALSE, all.x = TRUE)

  v = v[nest %in% n$nest,]
    if(adjust_ == TRUE){
      v[rowid == 798, eggs := "4d"] #N1101_17
    }
    v[ , egg := apply( str_extract_all(eggs, '\\d+', simplify = TRUE), 1, function(x) as.numeric(x) %>% sum(na.rm = TRUE)),] # gets number of eggs for each visit (if known)
    v[ , chick := apply( str_extract_all(chicks, '\\d+', simplify = TRUE), 1, function(x) as.numeric(x) %>% sum(na.rm = TRUE)),] # gets number of chicks for each visit (if known)
    # TRY WITHOUT THIS v = v[ !is.na(start_) & getime(start_) != 0] # use only visits with known start time
    
    #v = merge(v,a, sort = FALSE, all = TRUE) # add actogram based visits/information
    if(extracted == TRUE) {v = merge(v,a, sort = FALSE, all = TRUE)} # add actogram based visits/information
    v[is.na(end_) | start_ == end_ , end_ := start_ +10*60]  # make control visits with unknown end 1 min long and other visits with unknown end 10 min long
    v[, capture := str_detect(what,'a')] # was capture attempted during the visit?
    ###DELETE v[, hatch_start := ifelse((str_detect(eggs, c('o')) & chicks %in% c(NA,0) | str_detect(eggs, c('g'))) & chicks %in% c(NA,0) | what %in% 'm' & eggs %in% 'g' , TRUE, FALSE)] # did hatching start during the visits?
    v = v[order(site, nest, start_)]
    #v[,list(eggs,hatch_start)]
  
# adjust when full clutch found for nests found during laying, if known
     for (i in n[eggs_found<clutch, rowid]){
      # i = 1201
      ni = n[rowid == i]
      print(paste( i, ni$nest,'nest found at laying, adjust when full found'))
      vi = v[nest %in% ni$nest]
      if(nrow(vi[egg %in% ni$clutch])>0){
        n[rowid == i, full_clutch_found := vi[egg %in% ni$clutch, min(start_)]]
      } else {n[rowid == i, full_clutch_found := NA]}
      print(n[rowid == i, full_clutch_found])
    }       

# adjust laid and clutch before full for nests found during laying
    for (i in n[eggs_found<clutch, rowid]){
      # i = 1088
      ni = n[rowid == i]
      vi = v[nest %in% ni$nest]
      print(paste(i, ni$nest,'found during laying adjust laid and clutch before full'))

      if(is.na(ni$full_clutch_found)){
        n[rowid == i, laid_before_full := nest_found]
        n[rowid == i, clutch_before_full := eggs_found]
      } else {
        vi[,c("start_","egg")] 
        vii = vi[start_<ni$full_clutch_found]
        n[rowid == i, laid_before_full := vii[egg %in% max(egg,na.rm = TRUE), max(start_)]]
        n[rowid == i, clutch_before_full := max(vii$egg,na.rm = TRUE)]
      }
      }

# TEMP
  #n= n[rowid!=281]    

# update end_field date with datetime from visits
  for (i in n[getime(end_field) == 0, rowid]){
      # i = 1096
      ni = n[rowid == i]
      print(paste(i, ni$nest, ni$end_field))
      vi = v[nest %in% ni$nest]
      #vi[getDay(vi$start_)%in%as.Date(ni$end_field), max(start_)]
      n[rowid == i, end_field := vi[getDay(start_)%in%as.Date(ni$end_field), max(start_)]]
      }

# ESTIMATE first_egg and inc_start and true/would be hatch date
 for(i in n$rowid){#c(77:93, 148,149,150,151)){#n$rowid){
    # i = 1152
    ni = n[rowid == i]
    vi = v[nest %in% ni$nest]
    print(paste(ni$rowid, ni$nest, ni$method))

    # prepare likely hatching datetimes based on egg and chick states for each visit (note that warm nests that were subsequently found empty and determined as hatched are not included here, as their inc start and nest initiation is based on laying or flotation
      xi = data.table(end_ = ni[, end_field], end_state = ni[, end_state],
          g = vi[str_detect(eggs, 'g') & chicks %in% c(0, NA), suppressWarnings(min(start_))],
          w = vi[str_detect(chicks, 'w'), suppressWarnings(min(start_))],
          d = vi[str_detect(chicks, 'd') | str_detect(chicks, 'm'), suppressWarnings(min(start_)) - 12*60*60],
          l = vi[what %in% 'm' & str_detect(chicks, 'l'), suppressWarnings(min(start_)) - 12*60*60],
          n = vi[str_detect(chicks, 'n'), suppressWarnings(min(start_)) - 12*60*60],
          o = vi[str_detect(eggs, 'o') & chicks %in% c(0, NA), suppressWarnings(max(start_)) + 24*60*60],
          b = vi[str_detect(eggs, 'b') & !grepl('o',eggs) & chicks %in% c(0, NA), suppressWarnings(max(start_)) + 2*24*60*60],
          hatch_date_est = as.POSIXct(NA),
          based_on = NA
          )
      
      # if last end determining visit earlier than hatch estimated from 'holes', the nest hatched 6h before the last visit
      if(xi$o>max(vi$start_) & max(vi$start_)!= (xi$o-24*60*60)){
        o_ad = 'o, last vis -6h'
        xi[, o := max(vi$start_) -6*60*60] 
        } # later used to get correct 'based on' method
      

      # if last end determining visit earlier than hatch estimated from 'breaks', the nest hatched 6h before the last visit
      if(xi$b>max(vi$start_) & max(vi$start_)!= (xi$b-2*24*60*60)){
        b_ad = 'b, last vis -6h' # later used to get correct 'based on' method
        xi[, b := max(vi$start_) -6*60*60] 
        }
     
      #xi[!is.infinite(d), d := if(getime(d)>12) d+12*60*60 else d] # if dry chicks in the afternoon, nest hatched on that day (i.e. 12h ago)

    # estimate the hatching date for each nest based on above - hatching evidence from all visits (chicks have priority over egg states, except for 'g' state)
      xi[, hatch_date_est := if(!is.infinite(g)) g # signs of hatching from actograms
        else if(!is.infinite(w) & !is.infinite(d)) {if(w < d+12*60*60) w else d} # if both wet and dry chicks found, wet chicks have priority only if found earlier than dried ones (note that for wet dry daytime comparison we bring the dry chick record back to the datetime when found)
        else if(!is.infinite(w)) w else if (!is.infinite(d)) d else if(!is.infinite(l)) l else if(!is.infinite(n)) n  # if only wet, dry or near nest chicks found in separate visits or chicks left the nest based on acto, wet have priority over dry and dry over left and left over near
        else if(!is.infinite(o)) o else if (!is.infinite(b)) b else NA] # holes (pips) have priority over breaks

      xi[, based_on := if(!is.infinite(g)) 'g'
          else if(!is.infinite(w) & !is.infinite(d)){if(w < d+12*60*60) 'w' else 'd'} 
          else if(!is.infinite(w)) 'w' else if (!is.infinite(d)) 'd' else if(!is.infinite(l)) 'l' else if(!is.infinite(n)) 'n'  
          else if(!is.infinite(o)) 'o' else if (!is.infinite(b)) 'b' else NA]

      xi[based_on %in% 'o' & exists('o_ad'), based_on := o_ad ]
      xi[based_on %in% 'b' & exists('b_ad'), based_on := b_ad ]

      if(exists('o_ad')){rm('o_ad')}
      if(exists('b_ad')){rm('b_ad')}
      print(xi)


    # set laying, inc start, true/would be hatching
      # laying and inc start based on laying for nests found during laying; hatching estimated from  visits or laying (the later is "true hatching" only for warm nests subsequently found empty and determined as hatched)
        if(ni[,method] == 'camera'){
          n[rowid == i, hatch_date := inc_start+inc_period*24*60*60]
          n[rowid == i, hatch_based_on := 'laying']
          n[rowid == i, lay_inc_based_on := 'camera']

          print(paste('found            ', ni$nest_found, ni$eggs_found))
          print(paste('laid_before_full ', ni$laid_before_full, ni$clutch_before_full))
          print(paste('full             ', ni$full_clutch_found, ni$clutch))
          print(paste('inc_start:       ', ni$inc_start))

        }else if(ni[, method] == 'laying' | ni[, eggs_found] < ni[, clutch]){
          #i = 10
          #ni = n[rowid == i]
          es = lay_inc_hatch_est(method = 'laying', y = ni)
          n[rowid == i, method := 'laying']
          n[rowid == i, inc_start := es[, inc_start]]
          n[rowid == i, first_egg := es[, first_egg]]
          n[rowid == i, hatch_date := es[, hatch_date]]
          n[rowid == i, hatch_based_on := 'laying']
          n[rowid == i, lay_inc_based_on := 'laying']

          print(paste('found            ', ni$nest_found, ni$eggs_found))
          print(paste('laid_before_full ', ni$laid_before_full, ni$clutch_before_full))
          print(paste('full             ', ni$full_clutch_found, ni$clutch))
          print(paste('inc_start:       ', es$inc_start))

          if(!is.na(xi$hatch_date_est)){
              n[rowid == i, hatch_date := xi$hatch_date_est]
              n[rowid == i, hatch_based_on := paste('hatching', xi$based_on)]
            }

      # laying, inc start and hatching based on signs of hatching
        } else if(!is.na(xi$hatch_date_est)){
            n[rowid == i, method := 'hatching']
            n[rowid == i, hatch_date := xi$hatch_date_est]
            n[rowid == i, inc_start := xi$hatch_date_est - 24*60*60*inc_period]
            n[rowid == i & inc_start>full_clutch_found, inc_start := full_clutch_found - 12*60*60]
            n[rowid == i, first_egg := inc_start-((clutch-1)*lay_interval*24*60*60)]
            n[rowid == i, hatch_based_on := paste('hatching', xi$based_on)]
            n[rowid == i, lay_inc_based_on := paste('hatching', xi$based_on)]

      # laying, inc start and hatching based on flotation  (the later is 'true hatching' only for warm nests subsequently found empty and determined as hatched)
        } else if (nrow(e[nest %in% ni[ , nest]])>0){
            es = lay_inc_hatch_est(method = 'flotation', y = e[nest %in% ni[ , nest]])
            n[rowid == i, method := 'flotation']
            n[rowid == i, hatch_date := es[, hatch_date]]
            n[rowid == i, inc_start := es[, inc_start]]
            n[rowid == i, first_egg := inc_start-((clutch-1)*lay_interval*24*60*60)]
            n[rowid == i, hatch_based_on := 'flotation']
            n[rowid == i, lay_inc_based_on := 'flotation']
        # provided by collaborators (kept because visit or float data missing)
        # pop median (leave as is and adjust later)
        } else if (!ni[, method] %in% 'pop_median'){
            n[rowid == i, hatch_date := inc_start + 24*60*60*inc_period]
            n[rowid == i, hatch_based_on := paste0('provided inc_start (', method, ')')]
            n[rowid == i, lay_inc_based_on := paste0('provided (', method, ')')]
        }

    n[rowid == i, end_nest:=xi$l+12*60*60] # adds left datetime as an end of nest (where present) and in the following loop the failed datetime    
    nii = n[rowid == i]
    print(paste('method inc_start new  :', nii$method))
    print(paste('method inc_start field:', ni$method))
    print(paste('inc_start new  :', nii$inc_start))
    print(paste('inc_start field:', ni$inc_start))
    print(paste('lay_inc_method:', nii$lay_inc_based_on))
    print(paste('hatch_method  :', nii$hatch_based_on, nii$hatch_date))
  }

# adjust laying, inc start and hatching for unknown cases using population mean 
  #(the later is 'true hatching' only for warm nests subsequently found empty and determined as hatched);
  # TS08 - hatched, so not used here
  for( i in n[method %in% 'pop_median', rowid]){
    #i = 689
    ni = n[rowid == i]
    vi = v[nest == ni$nest]
    print(paste(ni$rowid, ni$nest, ni$method))
    nx = n[species %in%c(ni$species) & site %in%c(ni$site) & year %in%c(ni$year) & method != 'pop_median']
    
    if(round_date(mean(nx$inc_start), unit = 'days')>min(vi$start_)){
    n[rowid == i, inc_start := min(vi$start_)]
    n[rowid == i, first_egg := inc_start-((clutch-1)*lay_interval*24*60*60)]
    n[rowid == i, hatch_date := inc_start + 24*60*60*inc_period]
      }else{
    n[rowid == i, inc_start := round_date(mean(nx$inc_start), unit = 'days')]
    n[rowid == i, first_egg := inc_start-((clutch-1)*lay_interval*24*60*60)]
    n[rowid == i, hatch_date := inc_start + 24*60*60*inc_period]        
      }

    n[rowid == i, lay_inc_based_on := "pop_median"]
    n[rowid == i, hatch_based_on := "pop_median"]

    nii = n[rowid == i]
    print(paste('inc_start new  :', nii$inc_start))
    print(paste('inc_start field:', ni$inc_start))
    print(paste('lay_inc_method:',  nii$lay_inc_based_on))
  }

# check  
  # n[end_state %in% c('h') & hatch_based_on %in% c('laying','flotation','pop_med')] 
  # n[str_detect(hatch_based_on, 'hatching') & lay_inc_based_on %in% c('laying'), .(species, nest, first_egg, inc_start, hatch_date, inc_period, hatch_date-inc_start, as.numeric(hatch_date-inc_start) - inc_period,hatch_based_on)] 
  # n[species == 'REPH' & site == 'SVAL' & hatch_based_on=='pop_median',list(first_egg, inc_start, lay_inc_based_on)]
  # n[species == 'REPH' & site == 'SVAL' & hatch_based_on!='pop_median',list(rowid, nest, first_egg, inc_start, lay_inc_based_on)]
  # n[species == 'REPH' & site == 'SVAL' & hatch_based_on!='pop_median', median(inc_start, na.rm=TRUE)]
  # n[species == 'REPH' & site == 'SVAL' & hatch_based_on!='pop_median', median(first_egg, na.rm=TRUE)]

# CREATE final end_ and end_state based on actograms and visits (indicated as final within the actogram and shall be used for future server DB) 
   ## TO DO - check how it works for nest 12rnphwe41, 12rnphmh72, especially 11rnphmw32,  where multiple egg state or chicks state were present
   n$end_based_on = as.character(NA)
   for(i in n$rowid){#13:16){#
     # i = 45
    ni = n[rowid == i]
    vi = v[nest %in% ni$nest]
    print(paste(ni$rowid, ni$nest))

    # failed nests
       # based on actograms
         vm = vi[eggs %in% c('p', 'ps','a','u') & what %in% 'm']
         en = data.table(
           end_ = vm[,start_],
           end_state = vm[,eggs],
           method = if(nrow(vm) == 0) character() else 'acto'
         )
       # based on video
          vv = vi[eggs %in% c('p', 'ps','a','u') & what %in% 'v']
          if(nrow(vv) > 0){
            env = data.table(
             end_ = vv[,start_],
             end_state = vv[,eggs],
             method = if(nrow(vv) == 0) character() else 'cam'
             )
            en = rbind(en,env)
          }
   
       # based on visits
         vt = vi[!what %in% c('m','v') & (grepl(paste(c('p','a'),collapse="|"),eggs) | eggs %in% c('0u'))]
         if(nrow(vt) > 0){
           vt = vt[start_ == min(start_)]
           vf = data.table(end_ = vt[,start_], end_state = vt[,gsub("[0-9]+", "", eggs)], method = 'visits')
           en = rbind(en,vf)
         }
    # successful nests 
       # based on  chicks 
         if(grepl(paste(c('hatching g','hatching w','hatching d','hatching n','hatching l'),collapse="|"),ni$hatch_based_on) == TRUE){
           su = data.table(end_ = ni[, hatch_date], end_state = 'h', method = ni[,hatch_based_on])
           en = rbind(en,su)
         }
       # based on various methods if nest found empty and determined as hatched - uses hatching estimation from the previous for-loop (i.e. either based on signs of hatching - o, or b - or based on laying or flotation or pop_med); 
         ### likely does not hold anymore

       vi[, prev_egg := data.table::shift(eggs, type = 'lag')]
       vi[, prev_chick := data.table::shift(chicks, type = 'lag')]
       #vi[,.(start_, eggs, chicks,prev_egg, prev_chick )]
       if(nrow(vi[chick == 0 & grepl('h', eggs) & !grepl(paste(c('g','h'),collapse="|"), prev_egg) &  !grepl(paste(c('l','u','w','d','n'),collapse = "|"), prev_chick)]) > 0){
         vw = data.table(end_ = ni[,hatch_date], end_state = 'h', method = ni[,hatch_based_on])
         en = rbind(en,vw)
       }

       en = en[order(end_)]
       print(en)

     if(nrow(en)>0){

       #if(nrow(en["acto" %in% method])>0 & nrow(en[method%in%c('hatching g','hatching l','hatching w','hatching d','hatching n')])==0){ en = en[method %in% 'acto']; en = unique(en[ end_ == min(end_)])
       #} else {en = unique(en[ end_ == min(end_)])}

       en = unique(en[ end_ == min(end_)])
       
       if(nrow(en)>1 & nrow(en[duplicated(end_)])>0){ en = en[!duplicated(end_)]}


       en[ method %in% 'hatching g', method:= 'acto hatch']
       en[ method %in% 'hatching l', method:= 'acto left']
       en[ method %in% 'hatching w', method:= 'visit wet chick']
       en[ method %in% 'hatching d', method:= 'visit dry chick']
       en[ method %in% 'hatching n', method:= 'visit near chick']

       print(paste(ni$rowid, ni$nest, 'field', ni$end_field, ni$end_state))
       print(paste(ni$rowid, ni$nest, 'final', en$end_, en$end_state, en$method))

       n[ rowid == i , end_ := en[, end_]]
       n[ rowid == i , end_state := en[, end_state]]
       n[ rowid == i , end_based_on := en[, method]]
     }else{
        n[ rowid == i , end_based_on := 'provided']
       print('SAME end_ and end_field, end_state and end_state_field')
     }
   }

# update end_nest for nests ending between hatching and leaving and for failed ones
 # n[as.character(end_nest)=='Inf',end_nest := as.POSIXct(NA) ]
  for(i in n[end_nest%in%'Inf',rowid]){#13:16){#
     # i = 935
    ni = n[rowid == i]
    vi = v[nest %in% ni$nest]
    print(paste(ni$rowid, n[rowid == i, nest]))
    vim =  vi[what %in% c('m','v')]
    vimg = vim[eggs == 'g']
    if(nrow(vimg)>0){
      if(nrow(vim)>nrow(vimg) & nrow(vim[start_>vimg$start])>0){
          n[rowid == i, end_nest := max(vim$start_)]
          }else{n[rowid == i, end_nest := max(vi$start_[!vi$what%in%c('m','v')])]}
    }else if (ni$hatch_based_on%in%c('hatching d', 'hatching n')){n[rowid == i, end_full := end_+12*60*60]
    }else if (ni$hatch_based_on%in%c('hatching b, last vis -6h', 'hatching o, last vis -6h')){n[rowid == i, end_full := end_+6*60*60]
    }else{n[rowid == i, end_full := end_]}
  }

  #summary(factor(n$hatch_based_on[n$end_state =='h']))
  #n[end_state=='h' & hatch_based_on=='hatching d']
  #summary(factor(n$end_state))
  #nrow(n[end_state=='h' & hatch_based_on!='hatching g'])
  
if(warn_off == TRUE) options(warn=0)

if(bip == FALSE){n = n[!species%in%c('AMGP', 'AMOY','BADO','BBPL', 'BLGO','BWST','COSA', 'DUNL','EUCU', 'EUGP','KEPL','LEYE','LITE','LRPL','NZDO','PUSA','RUTU','SESA', 'SLRA','TBPL','TESA','WBST','WOSA')]}

if(export %in% c("d&a","data")) {
    n = n[nest%in%u[days>0, nest]]
    n = n[MB_check.x %in% which_data_export] #%
  }

# visualize 
 #install.packages("suntools")
 #library(suntools)
 for (i in n$rowid){
 #for (i in n$rowid[21:length(n$rowid)]){ #c(n$rowid[1:2],n$rowid[4:length(n$rowid)])){#2){ #17:25){# #n$rowid[4:length(n$rowid)]) [83:length(n$rowid)]
 #for (i in n$rowid[7:length(n$rowid)]){#2){ #17:25){# #n$rowid[62:length(n$rowid)]) [83:length(n$rowid)]
 #for (i in c(765)){
   # i = 22
   
   # limit data to one nest
     ni = n[ rowid %in% i]
     print(paste(ni$rowid, ni$nest))
     ni[, end_state :=ifelse(end_state == "l", "chicks out", ifelse(end_state=="w","eggs warm", ifelse(  end_state=="a", "abandoned", ifelse(end_state=="u", "unknown", ifelse(end_state%in% c("ps","pe",  "p"),   "depredated", ifelse(end_state%in%c("g","h"), "hatched",ifelse(end_state=="m", "messed up", ifelse(  end_state=="f","failed other",ifelse(end_state=="t","undetermined", end_state)))))))))]  
  
     ni[, end_state_field :=ifelse(end_state_field == "l", "chicks out", ifelse(end_state_field=="w","eggs warm", ifelse(end_state_field=="a", "abandoned", ifelse(end_state_field=="u", "unknown", ifelse(  end_state_field%in% c("ps","pe","p"), "depredated", ifelse(end_state_field%in%c("g","h"), "hatched",  ifelse(end_state_field=="m", "messed up", ifelse(end_state_field=="f","failed other",ifelse(  end_state_field=="t","undetermined", end_state_field)))))))))]  
  
     gi = g[ nest %in% ni[,nest] & logger_type == 'MSR' & !data %in% c('ext_only','no','await',NA)]
     gi_in = g[ nest %in% ni[,nest] & logger_type == 'MSRin' & !data %in% c('ext_only','no','await',NA)]
     gi_out = g[ nest %in% ni[,nest] & logger_type == 'MSRout' & !data %in% c('ext_only','no','await',NA)]
     rfi = g[ nest %in% ni[,nest] & logger_type == 'RFID' & !data %in% c('no','await',NA)]    
     vdi = vd[nest %in% ni[,nest]]
     vi = v[ nest %in% ni[,nest]]
     if(nrow(vi) == 0) {vi = data.table(start_ = as.POSIXct(NA), end_ = as.POSIXct(NA))}

     #if(end_off < ni[,end_]) {ni[, end_ := end_off]; ni[, end_state := 'logger off']}

     if(exists("b")){rm(b)}
     if(exists("b_in")){rm(b_in)}
     if(exists("b_out")){rm(b_out)}
     if(exists("bt")){rm(bt)}
     if(exists("brfid")){rm(brfid)}     
     if(exists("vid")){rm(vid)}  
   
   # get MSR logger data
     if(nrow(gi) > 0){
       b = foreach(j = 1:nrow(gi), .combine = rbind) %do% {
       #j =1
       ff = f[file_name == gi[j, file_name], f]
       if(ni$site == 'ANAD'){channel = names(fread(ff, skip = 18, nrows = 1))}else{channel = names(fread(ff, skip = 15, nrows = 1)) } # gives warning, but that is fine; if annoying run first options(datatable.verbose = TRUE) https://stackoverflow.com/questions/16132625/how-to-avoid-an-optimization-warning-in-data-table
       #sensors = read.table(ff, skip = 15, nrows = 1, stringsAsFactors = FALSE)
       if(paste(channel, collapse = ",") == "TIME,RH"){
        varnames = c("datetime_", "h_nest")
        colclass = c("POSIXct", "numeric")
       }

       if(paste(channel, collapse = ",") == "TIME,RH,T_RH,L1"){
        varnames = c("datetime_", "h_nest", "t_nest", "l_nest")
        colclass = c("POSIXct", "numeric", "numeric", "numeric")
       }

       if(paste(channel, collapse = ",") == "TIME,RH,T_RH,L1,BAT"){
        varnames = c("datetime_", "h_nest", "t_nest", "l_nest","bat")
        colclass = c("POSIXct", "numeric", "numeric", "numeric", "numeric")
       }

       if(paste(channel, collapse = ",") == "TIME,RH,T(RH),T,BAT"){
        varnames = c("datetime_", "h_surface", "t_surface", "t_nest", "bat")
        colclass = c("POSIXct", "numeric", "numeric", "numeric", "numeric")
       }

       if(paste(channel, collapse = ",") == "TIME,RH,T_RH,T,L1,BAT"){
        varnames = c("datetime_", "h_surface", "t_surface", "t_nest", "l_surface","bat")
        colclass = c("POSIXct", "numeric", "numeric", "numeric", "numeric", "numeric")
       }

       if(paste(channel, collapse = ",") == "TIME,RH,T_RH,RH2,T_RH2"){
           varnames = c("datetime_", "h_surface", "t_surface", "h_nest", "t_nest")
           colclass = c("POSIXct", "numeric", "numeric", "numeric", "numeric")
       } 

       if (paste(channel, collapse = ",") == "TIME,RH,T_RH,BAT,RH2,T_RH2"){
          varnames = c("datetime_", "h_surface", "t_surface", "bat", "h_nest", "t_nest")
          colclass = c("POSIXct", "numeric", "numeric", "numeric", "numeric", "numeric")
       }
       if(paste(channel, collapse = ",") == "TIME,RH,T_RH,T_RH2"){
          varnames = c("datetime_", "h_surface", "t_surface", "t_nest")
          colclass = c("POSIXct", "numeric", "numeric", "numeric")
       }
       if(paste(channel, collapse = ",") == "TIME,RH,T_RH,BAT,T_RH2"){
          varnames = c("datetime_", "h_surface", "t_surface", "bat", "t_nest")
          colclass = c("POSIXct", "numeric", "numeric", "numeric", "numeric")
       }
       if(paste(channel, collapse = ",") == "TIME,RH,T_RH,T,BAT"){
          varnames = c("datetime_", "h_surface", "t_surface", "t_nest", "bat")
          colclass = c("POSIXct", "numeric", "numeric", "numeric", "numeric")
       }
       if(paste(channel, collapse = ",") == "TIME,RH,T_RH,T,BAT"){
          varnames = c("datetime_", "h_surface", "t_surface", "t_nest", "bat")
          colclass = c("POSIXct", "numeric", "numeric", "numeric", "numeric")
       }
       if(paste(channel, collapse = ",") == "TIME,RH,T"){
          varnames = c("datetime_", "h_surface", "t_nest")
          colclass = c("POSIXct", "numeric", "numeric")
       }
       if(paste(channel, collapse = ",") == "TIME,RH,T_RH,T"){
          varnames = c("datetime_", "h_surface","t_surface", "t_nest")
          colclass = c("POSIXct", "numeric", "numeric", "numeric")
       }
       if(paste(channel, collapse = ",") == "TIME,T,BAT,T2"){
          varnames = c("datetime_", "t_surface", 'bat',"t_nest")
          colclass = c("POSIXct", "numeric", "numeric", "numeric")
       }
       if(paste(channel, collapse = ",") == "TIME,T,T2"){
          varnames = c("datetime_","t_surface", "t_nest")
          colclass = c("POSIXct", "numeric", "numeric")
       }
       if(paste(channel, collapse = ",") == "TIME,T,BAT"){
          varnames = c("datetime_","t_nest", "bat")
          colclass = c("POSIXct", "numeric", "numeric")
       }
       if(paste(channel, collapse = ",") == "TIME,RH,T_RH,BAT"){
          varnames = c("datetime_","h_surface","t_surface", "bat")
          colclass = c("POSIXct", "numeric", "numeric", "numeric")
       }
       if(paste(channel, collapse = ",") == "TIME,T"){
          varnames = c("datetime_","t_nest")
          colclass = c("POSIXct", "numeric")
       }
       
       if(ni$site == 'ANAD'){skip = 38}else{skip=27}
       b = fread(ff, skip = skip, col.names = varnames, colClasses = colclass, stringsAsFactors = FALSE)
      
       if("bat" %in% names(b)){b[,bat := NULL]}
       if(!"h_nest" %in% names(b)){b[,h_nest := NA]}
       if(!"h_surface" %in% names(b)){b[,h_surface := NA]}
       if(!"t_nest" %in% names(b)){b[,t_nest := NA]}
       if(!"t_surface" %in% names(b)){b[,t_surface := NA]}
       if(!"l_nest" %in% names(b)){b[,l_nest := NA]}
       if(!"l_surface" %in% names(b)){b[,l_surface := NA]}
  
       # T/H probes were swapped, so this corrects for it
         if(ni$nest %in%c('2022_A03','2022_A06', '2022_C02','2022_C06','2022_C09
          2022_C16','2024_C02')){ setnames(b,old = c("h_surface", "t_surface", "h_nest", "t_nest","l_nest","l_surface"), new = c("h_nest", "t_nest", "h_surface", "t_surface","l_nest","l_surface"))}  

         if(ni$site == 'AMVI' & ni$year %in% c(2022)){ setnames(b,old = c("h_surface", "t_surface", "h_nest", "t_nest","l_nest","l_surface"), new = c("h_nest", "t_nest", "h_surface", "t_surface","l_nest","l_surface")) }  
         
         if(ni$nest %in% c('VAPE22TEST04','BEVE22COSA3')){ setnames(b,old = c("h_surface", "t_surface", "h_nest", "t_nest","l_nest","l_surface"), new = c("h_nest", "t_nest", "h_surface", "t_surface","l_nest","l_surface")) }      
         if(ni$nest == 'PRCU21TBPL04'){ setnames(b,old = c("h_surface", "t_surface", "h_nest", "t_nest","l_nest","l_surface"), new = c("h_nest", "t_nest", "h_surface", "t_surface","l_nest","l_surface")) }      

         if(ni$site == 'SVAL' & ni$year %in% c(2019, 2020)){ setnames(b,old = c("h_surface", "t_surface", "h_nest", "t_nest","l_nest","l_surface"), new = c("h_nest", "t_nest", "h_surface", "t_surface","l_nest","l_surface")) }  
  
         if(ni$nest == 'TS08' & ni$year == 2019){ setnames(b,old = c("h_surface", "t_surface", "h_nest", "t_nest","l_nest","l_surface"), new = c("h_nest", "t_nest", "h_surface", "t_surface","l_nest","l_surface")) }  

         if(ni$nest =='LATE20BADO_CR022'){ setnames(b,old = c("h_surface", "t_surface", "h_nest", "t_nest","l_nest","l_surface"), new = c("h_nest", "t_nest", "h_surface", "t_surface","l_nest","l_surface")) }  

         if(ni$nest == 'MEYN21LSPL12'){ setnames(b,old = c("h_surface", "t_surface", "h_nest", "t_nest","l_nest","l_surface"), new = c("h_nest", "t_nest", "h_surface", "t_surface","l_nest","l_surface")) }  

         if(ni$nest == 'PODL21GRSN20'){ setnames(b,old = c("h_surface", "t_surface", "h_nest", "t_nest","l_nest","l_surface"), new = c("h_nest", "t_nest", "h_surface", "t_surface","l_nest","l_surface")) } 
  
       #b[, datetime_ := as.POSIXct(datetime_)]
       # adjust time for logger running in different time zone than field work
        if(ni[,field_plus] != 0 & !is.na(ni[,field_plus])){ b[, datetime_ := datetime_ - ni[,field_plus]*60*60] } 
       # adjust time for loggers that were set on computer with different than specified timezone or during   winter time, but placed during summer time  
       if(length(cort[file == gi[j, file_name], hours]) == 1){b[, datetime_ := datetime_ + 60*60*cort[file == gi[j, file_name], hours]]}   
       ### DELETE # adjust time in log  gers started during winter time, but placed during summer timeif(  ni$nest %in% c('SAJO19COSN03','  SAJO19COSN02','SAJO19COSN01','SAMI19EUWO01')){b[, datetime_ :=   datetime_ + 60*60]}  
  
       b = b[datetime_ > gi[j, placed]-4*60*60  & datetime_ < gi[j, taken]+(4)*60*60 ] # +/- 4h to see   whether time placed/taken are correct  
  
       b[,type := 'MSR']
       if(!c('t_surface')%in%names(b)){b[,t_surface := NA]}
       return(b)
      }
      }
 
   # get MSR logger data for nests with additional MSR loggers
      if(nrow(gi_out) > 0){
       b = foreach(j = 1:nrow(gi_out), .combine = rbind) %do% {
       #j =1
       ff = f[file_name == gi_out[j, file_name], f] 
       
        if(ni$site == 'ANAD'){channel = names(fread(ff, skip = 18, nrows = 1))}else{channel = names(fread(ff, skip = 15, nrows = 1)) } # gives warning, but that is fine; if annoying run first options(datatable.verbose = TRUE) https://stackoverflow.com/questions/16132625/how-to-avoid-an-optimization-warning-in-data-table
       #sensors = read.table(ff, skip = 15, nrows = 1, stringsAsFactors = FALSE)
        
        if(paste(channel, collapse = ",") == "TIME,RH,T(RH),T,BAT"){
        varnames = c("datetime_", "h_surface", "t_surface", "t_nest", "bat")
        colclass = c("POSIXct", "numeric", "numeric", "numeric", "numeric")
        }

        if(paste(channel, collapse = ",") == "TIME,RH,T_RH,L1,BAT"){
            varnames = c("datetime_", "h_surface", "t_surface", "l_surface","bat")
            colclass = c("POSIXct", "numeric", "numeric", "numeric", "numeric")
        }
        if(ni$site == 'ANAD'){skip = 38}else{skip=27}
        b_out = fread(ff, skip = skip, col.names = varnames, colClasses = colclass, stringsAsFactors = FALSE)
        #b_out[, datetime_ := as.POSIXct(datetime_)]
        
        # adjust time for logger running in different time zone than field work
        if(ni[,field_plus] != 0 & !is.na(ni[,field_plus])){ b_out[, datetime_ := datetime_ - ni[,field_plus]*60*60] } 

        # adjust time for loggers that were set on computer with different than specified timezone or  during winter time, but placed during summer time 
       if(length(cort[file == gi_out[j, file_name], hours]) == 1){ b_out[, datetime_ := datetime_ + 60*60 *cort[ file == gi_out[j, file_name], hours]]}  
 
        b_out = b_out[datetime_ > gi_out[j, placed]-4*60*60   & datetime_ < gi_out[j, taken]+(4)*60*60 ] #  +/- 4h to see whether ti me placed/taken are correct 
       b_out[,type := 'MSRout'] 
       if(exists("b") == TRUE){   
         b = rbind(b,b_out,fill  = TRUE)  
         }else{b  = b_out[, ':='  (h_nest  = NA, t_nest = NA, l_nest = NA)]} 
       return(b) 
       } 
       }
      if(nrow(gi_in) > 0){
       b_in= foreach(j = 1:nrow(gi_in), .combine = rbind) %do% {
       #j =1
       ff = f[file_name == gi_in[j, file_name], f]
       channel = names(fread(ff, skip = 15, nrows = 1)) # given warning, but that is fine; if annoying run  first options(datatable.verbose = TRUE) https://stackoverflow.com/ questions/16132625/how-to-avoid-an-optimization-warning-in-data-ta ble
       #sensors = read.table(ff, skip = 15, nrows = 1, stringsAsFactors =  FALSE)
        if(paste(channel, collapse = ",") == "TIME,RH,T(RH),T,BAT"){
        varnames = c("datetime_", "h_surface", "t_surface", "t_nest", "bat")
        colclass = c("POSIXct", "numeric", "numeric", "numeric", "numeric")
        }

        if(paste(channel, collapse = ",") == "TIME,RH,T_RH,L1,BAT"){
            varnames = c("datetime_", "h_nest", "t_nest", "l_nest","bat")
            colclass = c("POSIXct", "numeric", "numeric", "numeric", "numeric")
           }
        b_in = fread(ff, skip = 27, col.names = varnames, colClasses = colclass, stringsAsFactors = FALSE)
        #b_in[, datetime_ := as.POSIXct(datetime_)]
        
        # adjust time for logger running in different time zone than field work
        if(ni[,field_plus] != 0 & !is.na(ni[,field_plus])){ b_in[, datetime_ := datetime_ - ni[,field_plus]*60*60] } 

        # adjust time for loggers that were set on computer with different than specified timezone or  during winter time, but placed during summer time 
       if(length(cort[file == gi_in[j, file_name], hours ]) == 1){ b_in [, datetime_ := datetime_ + 60*60* cort[ file == gi_in[j, file_name], hours]]}  
 
        b_in = b_in[datetime_ > gi_in[j, placed]-4*60*60  & datetime_ < gi_in[j, taken]+(4)*60*60 ] # +/-  4h to see whether time placed/taken are correct 
        b_in[,type := 'MSRin'] 
        
        if(exists("b") == TRUE){  
          b = rbind(b,b_in,fill = TRUE)  
          }else{b = b_in[, ':=' (h_surface = NA, t_surface = NA, l_surface = NA)]} 
       return(b) 
       }
       }

   # get RFID data
     if(nrow(rfi)>0){
      cai = ca[nest %in% ni$nest]
      cai = cai[,c('sex','tag_ID')]
      cai[, tag_ID := toupper(tag_ID)]
      setnames(cai, 'sex', c("who"))
    
      brfid = foreach(j = 1:nrow(rfi), .combine = rbind) %do% {
          #j = 1

        if(ni$nest%in%'CRBA21TBPL06'){
          ffr = fr[file_name == rfi[j, file_name], f]
          varnames = c("datetime_", "t_nest")
          colclass = c("POSIXct", "numeric")
          d = read.table(ffr, stringsAsFactors = FALSE, skip =13)
          datetime_ = as.POSIXct(substr(d$V1,16,32), format = "%m/%d/%y %H:%M:%S")
          tag_ID = substr(d$V1, 1, 10)

        }else{
          ffr = fr[file_name == rfi[j, file_name], f]
              varnames = c("datetime_", "t_nest")
              colclass = c("POSIXct", "numeric")
          d = read.table(ffr, stringsAsFactors = FALSE)
          if(nrow(d) > 0) {
              datetime_ = ISOdatetime(substr(d$V1, 5, 8), substr(d$V1, 3, 4), substr(d$V1, 1, 2), substr(d$V1, 9, 10), substr(d$V1, 11, 12), substr(d$V1, 13, 14))  
              tag_ID = substr(d$V1, 15, 30)
                      #bv1 = as.numeric(sub  str(d$V1, 31, 32))/10  
                      #bv2 = as.numeric(sub  str(d$V1, 33, 34))/10  
                      #if (nchar(d$V1[1])==  36) boutID = substr(d  $V1, 35, 36) else boutID = NA  
            } 
        } 
        dr = data.table(datetime_, tag_ID)  
        dr = merge(dr,cai, by = 'tag_ID',   all.x = TRUE)  
        dr[is.na(who), who := 'fieldteam']  
        # adjust time for logger running in different time zone than field work
          if(ni[,field_plus] != 0 & !is.na(ni[,field_plus])){ dr[, datetime_ := datetime_ - ni[,field_plus]*60*60] } 
        # adjust time for loggers that were set on computer with different than specified timezone
          if(length(cort[file == rfi[j, file_name], hours]) == 1){dr[, datetime_ := datetime_ + 60*60*cort[file == rfi[j, file_name], hours]]}  
        # adjust time for rfids running on a wrong time
          dr[, datetime_:= datetime_+r[nest%in%ni$nest, correction]]
        # limit to time on nest
          dr = dr[datetime_ > rfi[j, placed]  -4*60*60  & datetime_ < rfi[j, taken]+(4)*60*60 ]  
        return  (dr[order(datetime_)])  
      }  
      }
  
   # get video data
    vid = copy(vdi)

   # get weather data from weather stations/loggers
         # nome seems better with the original data
         #if(ni$site == 'NOME' & exists("b") == TRUE){
          #nomei = nome[datetime_>min(b$datetime_) & datetime_<max(b$datetime_)]
          #b$t_surface = NA
          #b$t_surface = as.numeric(nomei$t_surface[match(lubridate::round_date(b$datetime_, "60 minutes"), nomei$datetime_)])
         #}

   # PLOT if data exist
     if(exists("b") == TRUE){ 
       ni[, logger_off:=max(c(gi[, (taken)],gi_in[, (taken)],gi_out[, (taken)]))]
       # estimate incubation
         test = round(as.numeric(difftime(b$datetime_[2], b$datetime_[1], units = 'sec')))
         if(test == 5){
          roll_t_int =17279 #whole day 17279
          roll_d_int = 24 # 2 minutes
         }
         if(test == 1){
          roll_t_int =17279*5 #whole day 17279
          roll_d_int = 24*5 # 2 minutes
         }
         if(test == 30){
          roll_t_int =floor(17279*5/30) #whole day 17279
          roll_d_int = 24*5/30 # 2 minutes
         }
         if(test == 60){
          roll_t_int =floor(17279*5/60) #whole day 17279
          roll_d_int = 24*5/60 # 2 minutes
         }
         if(test == 120){
          roll_t_int =round(17279*5/120) #whole day 17279
          roll_d_int = 24*5/120 # 2 minutes
         }


         b[!is.na(t_nest), t_difference := c(0, diff(t_nest))] 
         b[!is.na(t_nest), t_nest_run_med := rollmedian(t_nest, roll_t_int , fill="extend")] # 1/2 day 8639, whole day 17279
         b[!is.na(t_nest), t_diff_run := rollmean(t_difference, roll_d_int, fill="extend")] #2 minutes (for median neads to be odd)

         #zoo::rollmedian(b$t_nest[1:10000], 2550 , fill="extend") #2879
         #zoo::rollmedian(b$t_nest[1:10000], 2600 , fill="extend") #2879

         b[!is.na(t_nest), inc_t := ifelse(is.na(t_surface),
                  (ifelse(t_nest_run_med<20,
                    (ifelse(t_nest > t_nest_run_med+3, 1, 0)),#))
                    ifelse(t_nest > t_nest_run_med-3, 1,0))),#))
                  ifelse(t_nest_run_med<20,
                    (ifelse(t_nest > t_nest_run_med+3, 1,
                      ifelse(t_nest > t_surface+12.5, 1,0))),#))
                    ifelse(t_nest > t_nest_run_med-3, 1,
                      ifelse(t_nest > t_surface+12.5, 1,0))))#))
          ]
    
         b[!is.na(t_nest), inc := ifelse(is.na(t_surface),
                      (ifelse(abs(t_diff_run) >= 0.02 & t_diff_run < 0, 0,
                          ifelse(abs(t_diff_run) >= 0.02 & t_diff_run > 0,
                            1, inc_t))),
                    (ifelse(t_nest<20 &(t_surface-abs(t_nest))>(-3) & t_nest<t_nest_run_med+3, inc_t,
                        (ifelse(abs(t_diff_run) >= 0.02 & t_diff_run < 0, 0,
                        ifelse(abs(t_diff_run) >= 0.02 & t_diff_run > 0,1,
                                  inc_t)))))  )
              ]
       # check whether sex is unique and not multiple
                  # mf = ddply(dfr,.(tag, who),summarize, ntag = length(tag))
                    #mf = mf[order(mf$who, mf$ntag),]
                    #mm=mf$tag[mf$who=='m'][1]
                    #ff=mf$tag[mf$who=='f'][1]
                   # mm=dfr$tag[dfr$who=='male' & nchar(dfr$tag)>12][1]
                    #ff=dfr$tag[dfr$who=='female' & nchar(dfr$tag)>12][1]

       # add RFID data
          if(exists("brfid") == TRUE){
                # combine the datasets  
                b = rbind(b,brfid,fill = TRUE) #rbind.all.columns(b,bt) #bbt = merge(bt,b, all = TRUE)
                b[, who := ifelse(who%in%c("nest visit","visit","fieldteam","capture", "unknown", 'stranger','misread'), 'disturb',who)]  
          } else {b[, who := as.character(NA)]}  
  
          # taglist = unique(dfr$tag[which(!is.na(dfr$tag))])
          # trCol      = data.frame(tag = taglist, col2 = rainbow(length(taglist)),stringsAsFactors = FALSE)
          # dfr        = merge(dfr,trCol,all.x=TRUE)
          # dfr$cols[which(is.na(dfr$cols))] = dfr$col2[which(is.na(dfr$cols))]
          #  unknowns = trCol[which(trCol$tag %in% dfr$tag[which(dfr$who=='unknown')]),]
    
    
          #dfr$col_cv_nest=ifelse(is.na(dfr$inc), NA, ifelse(dfr$inc==1,signal_inc,signal_no_inc))
          #dfr$col_type=ifelse(dfr$type=='no',none_col, ifelse(dfr$type=='bip',bip_col, ifelse(  dfr$type=='uni',uni_col, NA)))  
       
       # limit data to those when nest was active
        if(extracted == TRUE) {
          # start limit
             b = b[datetime_ > min(c(gi[, (placed)],gi_in[, (placed)],gi_out[, (placed)]))]
          # end limit
            if(ni$nest%in%c('MPNR22BWST04','PABE21TBPL03')){
                b = b[ datetime_ < ni[, end_]+72*60*60 ]
              }else if(ni$hatch_based_on%in% c('hatching g','hatching w')){
              b = b[ datetime_ < ni[, end_]+60*60*60 ]
              }else if(ni[, end_]+36*60*60 < b[ , min(datetime_)]){
              b = b[datetime_ <  max(c(gi[, (taken)],gi_in[, (taken)],gi_out[, (taken)]))] 
              }else{b = b[ datetime_ < ni[, end_]+36*60*60 ]}      
          } 
      # export and plot 
        if(export == "data"){
           save(b, ni, vi, gi, vid, file = paste0(out_,ni$act_ID,"_",ni$sp,"_",ni$site,"_",ni$year,"_",ni$nest,'.RData'))
           print(paste(i, 'done')) #[13:length(n$rowid)]
          }else if(export == "acto"){
           source(here::here('R/fun_actogram.R'))
           RFID.temperature_actogram(where_ = out_a, dfr = b, meta = ni, vis = vi, 
           vid_ = if(exists("vid")==TRUE){TRUE}else{FALSE},extr = if(extracted == TRUE) TRUE else FALSE,   issues = if(nrow(gi[data %in% c('unit_hidden', 'nest_t_only', 'logger_issues', 'probe_out', 'probe_dislocated')])>0) toupper(paste(gi$data, colapse = ",")) else '')  
           print(paste(i, 'done')) #[13:length(n$rowid)]
          }else{
           save(b, ni, vi, file = paste0(out_,ni$act_ID,"_",ni$sp,"_",ni$site,"_",ni$year,"_",ni$nest,'.RData'))
           source(here::here('R/fun_actogram.R'))
           RFID.temperature_actogram(where_ = out_a, dfr = b, meta = ni, vis = vi, vid_ = if(exists("vid")==TRUE){TRUE}else{FALSE},extr = if(extracted == TRUE) TRUE else FALSE,   issues = if(nrow(gi[data %in% c('unit_hidden', 'nest_t_only', 'logger_issues', 'probe_out', 'probe_dislocated')])>0) toupper(paste(gi$data, colapse = ",")) else '')  
           print(paste(i, 'done')) #[13:length(n$rowid)]
          }
     
     }else{print(paste(i, ni$nest, 'no data'))}  
    
   if(exists("b")){rm(b)}
   if(exists("b_in")){rm(b_in)}
   if(exists("b_out")){rm(b_out)}
   if(exists("bt")){rm(bt)}
   if(exists("brfid")){rm(brfid)}     
   if(exists("vid")){rm(vid)}  
   }

#END 
