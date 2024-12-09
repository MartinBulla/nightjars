# PROCEDURE
  #1) run DB_checks and fix with collaborators
  #2) check whether all MSR files have csv file
  #3) run DB_visualise, fix bugs, extract end states and check with collaborators
  #4) upload to master DB

# SETTINGS
  load_from =  "google" # load the metadata from comp (computer) or from google (googlesheets) 
  #load_from = "comp" # 'google' # load the data from comp (computer) or from google (googlesheets) 
  data_select = "which_data" # "by_site_which_data" #"by_site", #
  #sites = c('EABA')#c('AMVI','ANCH','SAMI','SAJO','HANU','HALA','SVAL')
  sites = c('MURC')
  which_data = c("check") # c('check_24')

  Sys.setenv(TZ="UTC") # set time to UTC to have always same time
  
  # Tools (start R from project folder - Sublime: cmd + shift +t)
  require(here)

  POSIXct_validator <- function(x, ago = 7, reason = 'date-time wrong, in the future or older than a week') {
  o = meltall(x)

  o[, datetime_ := strp_date_or_time(value) ]

  o[, v := TRUE] # we are optimistic
  o[ !is.na(value) & is.na(datetime_), v := FALSE]
  o[ datetime_ > as.POSIXct(Sys.Date()+1) , v := FALSE]  # do not allow future dates
  o[ datetime_ < Sys.time() - 3600*24*ago , v:= FALSE ] # more than a week ago

  o = o[ (!v) , .(rowid, variable)]
  o[, reason := reason]
  o
  
  }

  source(here::here('R/utils.R'))
  source(here::here('R/inspectors_nightjars.R'))
  #source("/Users/martinbulla/Dropbox/Science/DATA_management/DataEntry.validation/R/validators.R")

  packages = c('anytime','data.table', 'DataEntry.validation', 'DT', 'foreach', 'glue', 'googledrive', 'googlesheets4', 'htmlTable', 'htmltools', 'lubridate', 'magrittr', 'plyr', 'readxl', 'stringr')
  sapply(packages, function(x) suppressPackageStartupMessages(using(x)) )
  #install.packages("devtools")
  #devtools::install_github("mpio-be/DataEntry.validation", force=TRUE)
  
# get data, backup and load all data
    if(load_from == 'google'){source(here::here('R/DB_backup.R'))}
    
    f = c(list.files(here::here('Data'), pattern = 'DB_20', recursive = TRUE, full.names = TRUE))
    load(f)   

# prepare data  
  au[ , author := abb]
  ileg = ile[site == 'GENR']
  d = l_IDs

  if(data_select == 'by_site_which_data'){
     n = n[site  %in% sites & MB_check %in% which_data]
  } else if(data_select == 'by_site'){
      n = n[site  %in% sites]
    } else {
      n = n[MB_check %in% which_data ,] #| is.na(MB_check) ##%>% nrow()
    }
     
  ca = ca[nest %in% n$nest,]
  r = r[paste(year,site) %in% paste(n$year,n$site)]
  e = e[nest %in% n$nest,]
  e[ , lengthMINwidth := length-width]
  
  # nests  
    n[ , siteSpecies := paste(site, species)]
    n$layint = ile$lay_interval[match(n$siteSpecies, paste(ile$site,ile$species))]
     xx = n[is.na(layint),unique(siteSpecies)]
     if(length(xx)>0){paste('no population data',xx)} # no population data
     xxx = xx[!unique(substring(xx,6,9))%in%ileg[ , species]] # lacking general species data - exclude for subsequent anal
    # add laying interval for species with only GENERAL sites
    n[siteSpecies%in%xx, layint := ileg$lay_interval[match(n[siteSpecies%in%xx, species],ileg$species)]] 
    # add laying interval for species with no population and no GENERAL and no SPECIES data
    n = n[siteSpecies%in%xxx, layint := 1] 
    # assign general clutch size to nests with NA (unknown) clutch size
        n[is.na(clutch), clutch := ileg$clutch_size[match(n[is.na(clutch), species], ileg$species)]]
    # assign  clutch size 4 to nests that still have  NA (unknown) clutch size (no species data in lay_inc_esc tab)
        n[is.na(clutch), clutch := 4]
    n[ , clutch := as.numeric(clutch)]
    n[ , layincint := (clutch - 1) * layint]
    #n[ ,nest_found := unlist(nest_found)]
    n[ , nest_found := anytime(nest_found)]
  
  # loggers
    fmsr = c(list.files(path = here::here('Data/MSR/'), pattern = '.msr', recursive = TRUE, full.names = FALSE))
    fmsr = paste(unique(substring(fmsr, 1,nchar(fmsr)-4)), collapse =", ")
    
    fmsr_csv = unique(list.files(path = here::here('Data/MSR/'), pattern = '.csv', recursive = TRUE, full.names = FALSE))
    #fmsr_csv = unique(substring(fmsr_csv, 1,nchar(fmsr_csv)-4))
    fmsr_csv = paste(unique(substring(fmsr_csv, 1,nchar(fmsr_csv)-4)), collapse =", ")

    fcam = unique(list.files(path = here::here('Data/Video/'), recursive = TRUE, full.names = FALSE))
    fcam = paste(fcam[!substring(fcam, nchar(fcam)-2)%in%c('jpg', 'jpeg', 'png')], collapse = ", ")
    frfid = paste(unique(list.files(path = here::here('Data/RFID/'), recursive = TRUE, full.names = FALSE)), collapse = ", ")
    
    #TT_file_present <- FALSE  #The previous line gives me an error, so I have to create the "TT_file_present" 
    #HOBO_file_present <- FALSE  # Idem than the previous line 
    #DHT_file_present <- FALSE #Idem. After run these three lines, then run again the og=evalTable(g, "LOGGERS")
    
    g = g[nest %in% n$nest,]
    g[ , MSR_file_present := str_detect(fmsr, file_name), by = rowid]
    g[ , MSRcsv_file_present := str_detect(fmsr_csv, file_name), by = rowid]
    g[ , CAM_file_present := str_detect(fcam, file_name), by = rowid]
    g[ , RFID_file_present := str_detect(frfid, file_name), by = rowid]

  # visits
    vi = v[nest %in% n$nest,]
    vi[ , egg := apply( str_extract_all(eggs, '\\d+', simplify = TRUE), 1, function(x) as.numeric(x) %>% sum(na.rm = TRUE)),] #v$egg = ifelse(is.na(v$eggs), NA, ifelse(v$eggs%in%c('0','p'), 0, ifelse(nchar(v$eggs) == 2, as.numeric(substring(v$eggs, 1, 1)), as.numeric(substring(v$eggs, 1, 1)) + as.numeric(substring(v$eggs,4,4)))))
    vi[ , chick := apply( str_extract_all(chicks, '\\d+', simplify = TRUE), 1, function(x) as.numeric(x) %>% sum(na.rm = TRUE)),]

    vi[ , eggss := gsub("[0-9]+", "", eggs)]
    vi[ , chickss := gsub("[0-9]+", "", chicks)] 
    #vi[ chickss == "", chickss := NA] # the validator will exlude NAs and will check only cases where chicks were present
    vi[ , eggPLUSchick := egg + chick] 
    #print(vi[,.(egg,chick, eggss,chickss,eggPLUSchick,chick_egg_state)], nrow = 140) 
    vi[ chick == 0, eggPLUSchick := NA] # the validator will exlude NAs and will check only cases where chicks were present
    vi[ , eggANDchick := egg + chick] 
    vi[ , chick_egg_state := (chickss=="" & eggss == "")]
    #vi[ , rowid := .I+1]
    
    
    vi[ , start_ := anytime(start_)]

    if(length(unique(vi$nest)) == 0){vi_nest = 'None'}else{vi_nest = unique(vi$nest)}
      #delete if anytime works
      #vi[nchar(start_)==10, start_ := paste(start_,'00:00')]
      #vi[,start_ := as.POSIXct(start_)]

# x =  n
# x[, rowid := .I+1]

# Evaluate
  oa=evalTable(x = au, Class = "AUTHORS")
  os=evalTable(s, "SITE")
  op=evalTable(sp, "SPECIES")
  oi=evalTable(ile, "INC_LAY_ESC")
  og=evalTable(g, "LOGGERS")
  oe=evalTable(e[!MB_check%in%'ok'], "EGGS")
  oc=evalTable(ca, "CAPTURES")
  or=evalTable(r, "RFIDS")

  ov=evalTable(vi, "VISITS")
  on=evalTable(n, "NESTS") # if datetime error, it is usually because of validators on interval differences ( because the date or time have wrong format; note that just date is ok)

# Visualize
   toFix = DT::datatable(width = '1100px',
                 rbindlist(list(oa, os, op, oi, on, og, ov, oe,  oc, or)),
                 caption = htmltools::tags$caption(
                   style = 'text-align: left;',
                   htmltools::strong('TABLE | Issues to fix in the Nightjar Google Sheets database')),
                 filter = 'top', 
                 options = list(
                   pageLength = 100, 
                   #autoWidth = TRUE, 
                   #scrollX = TRUE,
                   #aoColumn = list(list(sWidth = "5%", sWidth = "10%", sWidth = "25%", sWidth = "50%", sWidth = "10%")),

                   autoWidth = FALSE,
                   columnDefs = list(
                                 list(targets=c(0), visible=TRUE, width='2%'),
                                 list(targets=c(1), visible=TRUE, width='5%'),
                                 list(targets=c(2), visible=TRUE, width='10%'),
                                 list(targets=c(3), visible=TRUE, width='25%'),
                                 list(targets=c(4), visible=TRUE, width='58%'),
                                 list(targets='_all', visible=FALSE)
                                 ),
                   initComplete = JS( 
                       "function(settings, json) {", 
                         "$('body').css({'font-family': 'Helvetica'});",
                         "$(this.api().table().header()).css({'font-size': '75%'});",
                         "}"  
                     )
                   ), 
                 callback = JS('table.page(4).draw(false);'),
                 rownames = FALSE
                 ) %>% DT::formatStyle(columns = c(1, 2, 3, 4, 5), fontSize = '75%')

   toFix

   DT::saveWidget(toFix, here::here(glue('Reports/toFix_{format(Sys.time(),"%Y-%m-%d_%H%M")}_search.html')))
   unlink(here::here(glue('Reports/toFix_{format(Sys.time(),"%Y-%m-%d_%H%M")}_search_files')), recursive = TRUE, force = TRUE) 

### end