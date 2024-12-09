#' ==========================================================================
#' validators used unip_inc project.
#'
#' require(magrittr)
#' require(DataEntry.validation)
#' require(data.tables)
#' require(googlesheets)
#'
#' for_gs = gs_title("uniparental_incubation")
#' x = data.table(gs_read(for_gs, ws = 'nests'))
#' class(x) = c(class(x), 'NESTS')
#' x[, rowid := .I]
#'
#' ii = inspector(x)
#' evalidators(ii)
#' ==========================================================================

what_states = c('f','l','c','a','on','off','cn','cf','rn','rf','db','dc','p','v','ms','me')
egg_states = c('','c','w','f','m','b','o','g','h','p','a','u','i','n','d','l')
chick_states = c('','u','w','d','m','n','j','o','l','i','n')
logger_types = c('MSR','TTn','TTs','TTa','HOBO','CAM', 'RFID','DHT','MSRin','MSRout')
met_ = c("laying", "flotation","float_D","float_S","hatching","chick_age","logger", "pop_median","camera","lay_T")

inspector.AUTHORS <- function(x,...){
  list(
    x[, .(abb, rowid)]  %>%
    is.na_validator("Author abbreviation is required!") ,

    x[, .(abb, rowid)] %>%
    nchar_validator(v = data.table(variable = "abb", n = 2)),

    x[, .(abb, rowid)] %>%
    is.duplicate_validator(v = data.table(variable = "abb",
      set = list(unique(au[,.(name, surname, abb)])[, .N, by = abb][N>1, abb])  ) ),

    x[, .(email, rowid)]  %>%
    is.na_validator("email is required!")

    #, x[, .(name, surname, institution, street, postal_code , city, country, present_address, email, site, species, rowid)]  %>% is.na_validator("Missing mandatory entry!")

  )
}

inspector.SITE <- function(x,...){
  list(

    x[, .(author, site, abb, lat, lon, hours_from_UTC_field, hours_from_UTC_comp, time_changed, rowid)]  %>%
    is.na_validator("Missing mandatory entry!") ,

    x[ , .(author, rowid)] %>%
    is.element_validator(v = data.table(variable = "author",
    set = list(unique(au$abb))), "abbreviation mistyped or needs to be defined in AUTHORS")  ,

    x[, .(abb, rowid)] %>%
    is.duplicate_validator(v = data.table(variable = "abb",
      set = list(unique(x[duplicated(abb),(abb)]))) ) ,

    x[, .(abb, rowid)] %>%
    nchar_validator(v = data.table(variable = "abb", n = 4)) ,

    x[, .(time_changed, rowid)]  %>%
    is.element_validator(v = data.table(variable = "time-changed",
    set = list(c("y", "n"))  )),

    x[, .(time_zone, rowid)]  %>%
    is.element_validator(v = data.table(variable = "time_zone",
    set = list(OlsonNames())), "wrong time zone defintion, use from: https://en.wikipedia.org/wiki/List_of_tz_database_time_zones")

  )
}

inspector.SPECIES <- function(x,...){
  list(

    x[, .(abb, rowid)] %>%
    is.duplicate_validator(v = data.table(variable = "abb",
      set = list(unique(x[duplicated(abb),(abb)]))  ) ) ,

    x[, .(abb, rowid)] %>%
    nchar_validator(v = data.table(variable = "abb", n = 4))

  )
}

inspector.INC_LAY_ESC <- function(x,...){
  list(

    x[, .(author, site, species, lay_interval, inc_period, source, rowid)]  %>%
    is.na_validator("Missing mandatory entry!") ,

    x[ , .(author, rowid)] %>%
    is.element_validator(v = data.table(variable = "author",
    set = list(unique(au$abb))), "abbreviation not in AUTHORS tab; mistyped here or needs to be defined in AUTHORS")  ,

    x[ , .(site, rowid)] %>%
    is.element_validator(v = data.table(variable = "site",
    set = list(unique(s$abb))), "abbreviation not in SITES tab; mistyped here or needs to be defined in SITES")  ,

    x[ , .(species, rowid)] %>%
    is.element_validator(v = data.table(variable = "species",
    set = list(unique(sp$abb))), "abbreviation not in SPECIES tab; mistyped here or needs to be defined in SPECIES")  ,

    x[, .(ss = paste(site,species), rowid)] %>%  # or assign ss first: x[ ss := paste(site,species))]
    is.duplicate_validator(v = data.table(variable = "ss",
      set = list(unique(x[duplicated(paste(site,species)),(ss = paste(site,species))]))  ) ) , #list(unique(x[duplicated(ss),(ss)]))  ) ) ,

    x[, .(lay_interval, rowid)]  %>%
    interval_validator( v = data.table(variable = "lay_interval",  lq = 1, uq = 2), "laying interval <1 day or >2 days"),

    x[, .(inc_period, rowid)]  %>%
    interval_validator( v = data.table(variable = "inc_period",  lq = x[,as.numeric(quantile(inc_period, 0.025))] , uq = x[,as.numeric(quantile(inc_period, 0.9755))]), paste0("incubation period <", round(x[,as.numeric(quantile(inc_period, 0.025))],0), "days or >", round(x[,as.numeric(quantile(inc_period, 0.9755))], 0), "days"))

  )
}

inspector.LOGGERS <- function(x, ...){
  list(

    x[, .(author, site, species, nest, logger_type, logger_ID, placed, data, rowid)] %>%
    is.na_validator("Missing mandatory entry!") ,
    
    x[ data != 'no', .(file_name, taken, rowid)] %>%
    is.na_validator("Missing mandatory entry!") ,

    x[ , .(author, rowid)] %>%
    is.element_validator(v = data.table(variable = "author",
    set = list(unique(au$abb))), "author abbreviation missing in AUTHORS tab or mistyped here")  ,

    x[ , .(site, rowid)] %>%
    is.element_validator(v = data.table(variable = "site",
    set = list(unique(s$abb))), "site abbreviation missing in SITES tab or mistyped here")  ,

    x[ , .(species, rowid)] %>%
    is.element_validator(v = data.table(variable = "species",
    set = list(unique(sp$abb))), "species abbreviation missing in SPECIES tab or mistyped here")  ,

    x[ , .(nest, rowid)] %>%
    is.element_validator(v = data.table(variable = "nest",
    set = list(unique(n$nest))), "nest ID missing in NESTS tab or mistyped here")  ,

    x[ , .(logger_type, rowid)] %>%
    is.element_validator(v = data.table(variable = "logger_type",
    set = list(logger_types)))  ,

    x[ , .(logger_ID, rowid)] %>%
    is.element_validator(v = data.table(variable = "logger_ID",
    set = list(unique(d$ID))), "logger_ID not in logger_IDs tab; mistyped here or needs to be add to logger_IDs")  ,

    x[ , .(placed, rowid)] %>% POSIXct_validator (ago = 800, "date-time wrong, in the future or older than 2 years")      ,
    x[ , .(taken, rowid)] %>% POSIXct_validator (ago = 800, "date-time wrong, in the future or older than 2 years")     ,

    x[ , .(placed, taken, rowid)]  %>%
      datetime_order_validator( time1 = 'placed', time2 = 'taken', units = 'days',time_max = 30, 'invalid date order or logger on the nest > 30 days'),
    
    x[ , .(data, rowid)] %>%
    is.element_validator(v = data.table(variable = "data",
    set = list(c('yes','no', 'await','logger_issues', 'unit_hidden','nest_t_only','ext_only','probe_out', 'probe_dislocated')))) ,

    # do we have a file for a given file name when data yes or NA
    x[ data != 'no' & logger_type == 'MSR', .(MSR_file_present, rowid)] %>%
    is.element_validator(v = data.table(variable = "MSR_file_present",
    set = list(c('TRUE'))), "MSR file not found in DATA folder; mistyped or missing")   ,

    x[ data != 'no' & logger_type == 'MSR', .(MSRcsv_file_present, rowid)] %>%
    is.element_validator(v = data.table(variable = "MSRcsv_file_present",
    set = list(c('TRUE'))), "MSR csv file not found in DATA folder; mistyped or missing") ,  

    x[ data != 'no' & logger_type == 'CAM', .(CAM_file_present, rowid)] %>%
    is.element_validator(v = data.table(variable = "CAM_file_present",
    set = list(c('TRUE'))), "CAM file not found in DATA folder; mistyped or missing")  ,
    
    x[ data != 'no' & logger_type == 'RFID', .(RFID_file_present, rowid)] %>%
    is.element_validator(v = data.table(variable = "RFID_file_present",
    set = list(c('TRUE'))), "RFID file not found in DATA folder; mistyped or missing")
  )
}

inspector.EGGS <- function(x, ...){
  #xx = 
  list(

    x[, .(author, site, species, nest, datetime_processed, egg_ID, width, length, rowid)] %>%
    is.na_validator("Missing mandatory entry!") ,
    
    x[ nest %in% n[method != 'laying',.(nest)], .(float_angle, float_height, rowid)] %>%
    is.na_validator("Missing mandatory entry!") ,

    x[ , .(author, rowid)] %>%
    is.element_validator(v = data.table(variable = "author",
    set = list(unique(au$abb))), "author abbreviation missing in AUTHORS tab or mistyped here")  ,

    x[ , .(site, rowid)] %>%
    is.element_validator(v = data.table(variable = "site",
    set = list(unique(s$abb))), "site abbreviation missing in SITES tab or mistyped here")  ,

    x[ , .(species, rowid)] %>%
    is.element_validator(v = data.table(variable = "species",
    set = list(unique(sp$abb))), "species abbreviation missing in SPECIES tab or mistyped here")  ,

    x[ , .(nest, rowid)] %>%
    is.element_validator(v = data.table(variable = "nest",
    set = list(unique(n$nest))), "nest ID missing in NESTS tab or mistyped here")  ,

    x[ , .(datetime_processed, rowid)] %>% 
    POSIXct_validator(ago = 800, "date-time wrong, in the future or older than 2 years")     ,

    x[ , .(egg_ID, rowid)] %>%
    interval_validator( v = data.table(variable = "egg_ID",  lq = 1, uq = 4),
     "eggs ID  <1 or >4"),

    x[ , .(float_angle, rowid)] %>%
    interval_validator( v = data.table(variable = "float_angle",  lq = 0, uq = 90),
     "float_angle <0 or >90"),

    x[ , .(float_height, rowid)] %>%
    interval_validator( v = data.table(variable = "float_angle",  lq = -1, uq = 5),
     "float_height below -1 or >5mm"),

    x[ , .(lengthMINwidth, rowid)]  %>%
    interval_validator( v = data.table(variable = "lengthMINwidth",  lq = 0, uq = 20),
     "width longer than length or width by 20 mm smaller")
     #xx = x[ , lengthMINwidth := length-width, .(lengthMINwidth)]

  )
}

inspector.NESTS <- function(x, ...){
  list(

    x[, .(year, author, site, species, nest, lat, lon, nest_found, eggs_found, first_egg, inc_start, method, clutch, last_seen_alive, end_, end_state, sex, rowid)]  %>%
    is.na_validator("Missing mandatory entry!") ,

    x[, .(year, rowid)]  %>%
    interval_validator( v = data.table(variable = "year",  lq = 1990, uq = year(Sys.time())), "year <1990 or in the future"),

    x[ , .(author, rowid)] %>%
    is.element_validator(v = data.table(variable = "author",
    set = list(unique(au$abb))), "author abbreviation missing in AUTHORS tab or mistyped here")  ,

    x[ , .(site, rowid)] %>%
    is.element_validator(v = data.table(variable = "site",
    set = list(unique(s$abb))), "site abbreviation missing in SITES tab or mistyped here")  ,

    x[ , .(species, rowid)] %>%
    is.element_validator(v = data.table(variable = "species",
    set = list(unique(sp$abb))), "species abbreviation missing in SPECIES tab or mistyped here")  ,

    x[ , .(siteSpecies, rowid)] %>%
    is.element_validator(v = data.table(variable = "siteSpecies",
    set = list(unique(paste(ile$site, ile$species)))), "add info for this site-species into 'inc_lay_esc' tab")  ,

    x[, .(nest, rowid)] %>%
    is.duplicate_validator(v = data.table(variable = "nest",
    set = list(unique(n[duplicated(nest),(nest)]))  ) ),

    # check whether nest is within ~25km radius of the study site
    foreach(i = unique(n$site), .combine = rbind)  %do% {
      x[site == i , .(lat, rowid)]  %>%
      interval_validator(v = data.table(variable = "lat",
        lq = s[abb == i, lat] - 0.25, uq = s[abb == i, lat] + 0.25),
       "nest latitude >25km from the study site" )
     },


    foreach(i = unique(n$site), .combine = rbind)  %do% {
      x[site == i , .(lon, rowid)]  %>%
      interval_validator(v = data.table(variable = "lon",
        lq = s[abb == i, lon] - abs(25 / (cos(s[abb == i, lon] * pi / 180) * 111.321)),
        uq = s[abb == i, lon] + abs(25 / (cos(s[abb == i, lon] * pi / 180) * 111.321))),
       "nest longitude >25km from the study site" )

     },

    x[, .(eggs_found, rowid)] %>%
    interval_validator( v = data.table(variable = "eggs_found",  lq = 1, uq = 4),
     "number of found eggs below 1 or above 4" ),

    #x[ , .(nest_found, rowid)] %>% datetime_validatorSS      ,
    x[ , .(nest_found, rowid)] %>% 
    POSIXct_validator(ago = 800, "date-time wrong, in the future or older than 2 years")     ,

    x[ , .(method, rowid)] %>%
    is.element_validator( v = data.table(variable = "method",
    set = list(met_)  )),

    x[ grepl('egg', first_egg), .(first_egg, rowid)] %>%
    is.element_validator( v = data.table(variable = "first_egg",
    set = list(c("use_eggs_visits"))), reason = 'wrong spelling of use_eggs_visits'  ),

    x[ !grepl('egg', first_egg), .(first_egg, rowid)] %>% date_validator (reason = 'invalid date (should be: yyyy-mm-dd)')     , # consider posix validator

    x[ grepl('egg', inc_start), .(inc_start, rowid)] %>%
    is.element_validator( v = data.table(variable = "inc_start",
    set = list(c("use_eggs_visits"))), reason = 'wrong spelling of use_eggs_visits'),

    x[ !grepl('egg', inc_start), .(inc_start, rowid)] %>% date_validator (reason = 'invalid date (should be: yyyy-mm-dd)')     , # consider posix validator

    # are  VISITS present for a given nest
    x[ ,.(nest, rowid)] %>% 
        is.element_validator( v = data.table(variable = "nest",
            set = list(unique(vi$nest))), 
            reason = 'add VISITS for this nest'
    )
    ,
    # if laying than VISITS shall have entries with multiple clutch sizes
    if(nrow(x[nest%in%unique(vi$nest)]) > 0){
    x[method %in% 'laying' , .(nest, rowid)] %>% # if use_eggs_visits condition was there, but likely not needed: & grepl('egg', first_egg)
        is.element_validator(v = data.table(variable = "nest", set = list(data.table(ddply(setorderv(vi[!is.na(start_)],c('nest','start_')),.(nest), summarise, foundEgg = egg[1], maxEgg = max(eggANDchick, na.rm = TRUE)))[foundEgg < maxEgg, nest])),
        reason = "missing VISITS data to use 'laying' method for first_egg and start_inc"
        )
    }else{data.table(rowid = numeric(), variable = character(), reason = character())}
    ,

    ######  if flotation than EGGS shall have flotation info
    x[ method == 'flotation' , .(nest, rowid)] %>% # if use_egg_visits condition was there, but likely not needed: & grepl('egg', first_egg)
    is.element_validator(v = data.table(variable = "nest",
      set = list(e[!is.na(float_angle),(nest)])),
      reason = "missing flotation data for first_egg and start_inc calculation")
    ,

    ####### check whether laying interval reflects the one used in the given population
    foreach(i = x[ !(grepl('egg', first_egg) | grepl('egg', inc_start)) ,rowid], .combine = rbind)  %do% { # shall work without first_egg != 'use_eggs_visits'
     x[rowid == i, .(first_egg, inc_start, layincint, rowid)]  %>%
      datetime_order_validator( time1 = 'first_egg', time2 = 'inc_start', units = 'days',time_max = 'layincint', 'invalid date order or laying interval > than expected from lay_inc_esc tab')
     },


    x[ , .(clutch, rowid)] %>%
    interval_validator( v = data.table(variable = "clutch",  lq = 1, uq = 4),
     "final clutch size below 1 or above 4" ),

    #x[ last_seen_alive != 'use_logger', .(substr(last_seen_alive, 6,7), rowid)] %>% setnames(c('last_seen_alive','rowid')) %>%
    #is.element_validator( v = data.table(variable = "last_seen_alive",  set = list(c(paste0('0',seq(1,9,1)), 10,11,12))),
    # "month in a wrong position, likely exchanged with day" ), # likely this validator is not needed

    x[ as.character(last_seen_alive) != 'use_logger', .(last_seen_alive, rowid)] %>% datetime_validator   , # consider POSIXct_validator; works without !is.na(last_seen_alive) 
    
    x[ , .(end_, rowid)] %>% 
    POSIXct_validator(ago = 800, "date-time wrong, in the future or older than 2 years")     ,# works without !is.na(end_) ,

    x[ !grepl('egg', inc_start) & as.character(last_seen_alive) != 'use_logger', .(last_seen_alive, inc_start, rowid)] %>%
    datetime_order_validator( time1 = 'inc_start', time2 = 'last_seen_alive', units = 'days', time_max = 30, 'invalid date order or more than 30 days between inc_start and last_seen_alive')   , # works without !is.na(last_seen_alive) & 

    x[as.character(last_seen_alive) != 'use_logger', .(last_seen_alive, end_, rowid)] %>% 
    datetime_order_validator( time1 = 'last_seen_alive', time2 = 'end_', units = 'days', time_max = 15, 'invalid date order or more than 15 days between last_seen_alive and end_') , # works without !is.na(last_seen_alive) &

    x[ , .(end_state, rowid)] %>%
    is.element_validator(, v = data.table(variable = "end_state",
    set = list(c("a","p","h","m","f","u","t","x"))  )),

    x[, .(sex, rowid)] %>%
    is.element_validator(v = data.table(variable = "sex",
    set = list(c("m", "f","b","u"))  )),

    x[sex %in% c("m","f"), .(sex_method, rowid)] %>%
    is.element_validator(v = data.table(variable = "sex_method",
    set = list(c("DNA", "plumage", "morpho", "behavior","assumed","br_patch"))  )),

    # does each nest has logger entry?
    x[ , .(nest, rowid)] %>%
    is.element_validator(v = data.table(variable = "nest",
    set = list(unique(g$nest))),"entry for this nest missing in LOGGERS")

  )
}

inspector.VISITS <- function(x, ...){
  list(

    x[, .(author, site, species, nest, start_, what, eggs, chicks, rowid)]  %>%
    is.na_validator("Missing mandatory entry!") ,

    x[, .(end_, rowid)] %>%
    is.na_validator("Please remember to note the time you leave the nest!"),

    x[, .(esc_m, rowid)] %>%
    is.na_validator("Please remember to estimate the escape distance!"),

    x[, .(beh, rowid)] %>%
    is.na_validator("Please remember to note the behavior when parent escapes from the nest!"),

    x[ , .(author, rowid)] %>%
    is.element_validator(v = data.table(variable = "author",
    set = list(unique(au$abb))), "author abbreviation missing in AUTHORS tab or mistyped here")  ,

    x[ , .(site, rowid)] %>%
    is.element_validator(v = data.table(variable = "site",
    set = list(unique(s$abb))), "site abbreviation missing in SITES tab or mistyped here")  ,

    x[ , .(species, rowid)] %>%
    is.element_validator(v = data.table(variable = "species",
    set = list(unique(sp$abb))), "species abbreviation missing in SPECIES tab or mistyped here")  ,

    x[ , .(nest, rowid)] %>%
    is.element_validator(v = data.table(variable = "nest",
    set = list(unique(n$nest))), "nest ID missing in NESTS tab or mistyped here")  ,
    
    x[ , .(start_, end_, rowid)] %>% 
    POSIXct_validator(ago = 800, "date-time wrong, in the future or older than 2 years") ,

    x[ , .(start_, end_, rowid)]  %>%
      datetime_order_validator( time1 = 'start_', time2 = 'end_', units = 'hours',time_max = 1, 'invalid date order or visit took longer than 1h') ,

    x[ , .(what, rowid)] %>%
    is.element_validator(v = data.table(variable = "what",
    set = list(c(what_states, 
                apply(expand.grid(what_states, what_states), 1, paste, collapse = ","), 
                apply(expand.grid(list(what_states, what_states, what_states)), 1, paste, collapse = ","),
                apply(expand.grid(list(what_states, what_states, what_states, what_states)), 1, paste, collapse = ",")))), 'what not defined, space after comma, or > 3 what definitions')  
    ,

    x[ , .(eggss, rowid)] %>%
    is.element_validator(v = data.table(variable = "eggss",
    set = list(c(egg_states, 
                    apply(expand.grid(egg_states, egg_states), 1, paste, collapse = ","), 
                    apply(expand.grid(list(egg_states, egg_states, egg_states)), 1, paste, collapse = ",")))),"egg state in egg column not defined or space after comma")  ,

    x[ , .(chickss, rowid)] %>%
    is.element_validator(v = data.table(variable = "chickss",
    set = list(c(chick_states, apply(expand.grid(chick_states, chick_states), 1, paste, collapse = ","), apply(expand.grid(list(chick_states, chick_states, chick_states)), 1, paste, collapse = ",")))),'chick state in chicks column not defined or space after comma')  ,

    x[ , .(eggPLUSchick, rowid)] %>%
    interval_validator( v = data.table(variable = "eggPLUSchick",  lq = 1, uq = 4),
     "number of eggs plus chicks <1 or >4"),

    x[ , .(chick_egg_state, rowid)] %>%
    is.element_validator(v = data.table(variable = "chick_egg_state",
    set = list(FALSE)), "define state of 'eggs' or 'chicks' or both")
  )
}

inspector.CAPTURES <- function(x, ...){
  list(

    x[, .(capture_ID,author, site, species, nest, time_caught, time_released, recapture, cap_method, ring_num, tag_ID, tarsus, bill, toatalHead, wing, mass, sex,carries_egg, age, blood, rowid)]  %>%
    is.na_validator("Missing mandatory entry!") ,


    x[ , .(author, rowid)] %>%
    is.element_validator(v = data.table(variable = "author",
    set = list(unique(au$abb))), "author abbreviation missing in AUTHORS tab or mistyped here")  ,

    x[ , .(site, rowid)] %>%
    is.element_validator(v = data.table(variable = "site",
    set = list(unique(s$abb))), "site abbreviation missing in SITES tab or mistyped here")  ,

    x[ , .(species, rowid)] %>%
    is.element_validator(v = data.table(variable = "species",
    set = list(unique(sp$abb))), "species abbreviation missing in SPECIES tab or mistyped here")  ,

    x[ , .(nest, rowid)] %>%
    is.element_validator(v = data.table(variable = "nest",
    set = list(unique(n$nest))), "nest ID missing in NESTS tab or mistyped here")  ,

    x[ , .(datetime_start, time_1m, time_on, time_caught, time_released, rowid)] %>% 
    POSIXct_validator(ago = 800, "date-time wrong, in the future or older than 2 years")     ,#datetime_validatorSS      

    x[ , .(time_caught, time_released, rowid)]  %>%
      datetime_order_validator( time1 = 'time_caught', time2 = 'time_released', units = 'hours',time_max = 2, 'invalid date order or visit took longer than 1h') ,

    x[ , .(recapture, rowid)] %>%
    is.element_validator(v = data.table(variable = "recapture",
    set = list(c('y','n'))), 'recapture not "n" or "y"')  ,

    x[ , .(cap_method, rowid)] %>%
    is.element_validator(v = data.table(variable = "cap_method",
    set = list(c('clap trap us', 'clap trap bird', 'walk-in', 'mist net nest', 'mist net off', 'woosh net on', 'woosh net off'))), 'unknown capture method, please check or define in READ_ME tab')  ,

    if(nrow(x[cap_method%in%c('mist net off','woosh net off')]) > 0){
        x[cap_method%in%c('mist net off','woosh net off'), .(capture_lat, capture_lon, rowid)] %>%
        is.na_validator("capture_lat and lon missing when bird capture off the nest")
        },

    x[ , .(carries_egg, rowid)] %>%
    is.element_validator(v = data.table(variable = "carries_egg",
    set = list(c('y','n'))), 'carries_egg not "n" or "y"')  ,    
    
    x[ , .(age, rowid)] %>%
    is.element_validator(v = data.table(variable = "age",
    set = list(c('juv','hy','sy','ahy','asy'))), 'age not juv/hy/sy/ahy/asy')  ,    
    
    x[ , .(mass, rowid)] %>%
      interval_validator( v = data.table(variable = "mass",  lq = 70, uq = 120),
                          "mass <70g or >120g"),
    x[ , .(wing, rowid)] %>%
      interval_validator( v = data.table(variable = "wing",  lq = 200, uq = 230),
                          "wing <200mm or >230mm"),
    x[ , .(F8, rowid)] %>%
      interval_validator( v = data.table(variable = "F8",  lq = 140, uq = 160),
                          "F8 <140mm or >160mm"),
    x[ , .(F10, rowid)] %>%
      interval_validator( v = data.table(variable = "F10",  lq = 130, uq = 160),
                          "F10 <130mm or >160mm"),
    x[ , .(keel_lenght, rowid)] %>%
      interval_validator( v = data.table(variable = "keel_lenght",  lq = 28, uq = 37),
                          "keel_lenght <28mm or >37mm"),
        x[ , .(blood, rowid)] %>%
    is.element_validator(v = data.table(variable = "blood",
    set = list(c('y','n'))), 'blood not "n" or "y"')  , 

    x[ , .(brood_patch, rowid)] %>%
    is.element_validator(v = data.table(variable = "brood_patch",
    set = list(c('y','n'))),'brood_patch not "n" or "y"')   ,

    x[ , .(body_molt, rowid)] %>%
    is.element_validator(v = data.table(variable = "body_molt",
    set = list(c('0','1','2'))),'body_molt not 0, 1 or 2')  ,  

    x[ , .(fat, rowid)] %>%
    is.element_validator(v = data.table(variable = "fat",
    set = list(c('0','1','2','3','4','5','6','7'))),'fat not 0 - 7') ,   

    x[ , .(sperm, rowid)] %>%
    is.element_validator(v = data.table(variable = "sperm",
    set = list(c('n', 's', 'm', 'ms', 'm2', 'mcl', 'clf'))),'sperm not 0 - 7') 

  )
} 

inspector.RFIDS <- function(x, ...){
  list(

    x[, .(author, action, RFID_ID, datetime_, RFID_int_ID, rowid)] %>%
    is.na_validator("Missing mandatory entry!") ,

    x[ , .(author, rowid)] %>%
    is.element_validator(v = data.table(variable = "author",
    set = list(unique(au$abb))), "author abbreviation missing in AUTHORS tab or mistyped here")  ,

    x[ , .(RFID_ID, rowid)] %>%
    is.element_validator(v = data.table(variable = "RFID_ID",
    set = list(unique(d$ID))), "RFID_ID not in logger_IDs tab; mistyped here or needs to be add to logger_IDs")  ,

    x[ , .(datetime_, rowid)] %>% POSIXct_validator (ago = 800, "date-time wrong, in the future or older than 2 years")
  )
}
