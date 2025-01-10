### save googlesheets database as RData file and move the older version into freeze ###

  require(here)
  require(googlesheets4)
  require(googledrive)
  require(filesstrings)
  require(data.table)
  require(glue)
 
  gs4_auth_configure(api_key = "AIzaSyBthTLd1_tMO52sOV56EacVFji0IvKoc-A")
  gs4_deauth() 

  for_gs = "https://docs.google.com/spreadsheets/d/1h4S3FpTtyno_3E6xr9U77j8tv9ooRnSSAEeVJm1pHIM/edit?usp=drive_link"
  

  read_me = data.table(read_sheet(for_gs, sheet = 'READ_ME'))
  au = data.table(read_sheet(for_gs, sheet = 'authors', col_types = '????????c????c????'))

  sp  = data.table(read_sheet(for_gs, sheet = 'species'))
  s   = data.table(read_sheet(for_gs, sheet = 'site'))
  ile = data.table(read_sheet(for_gs, sheet = 'inc_lay_esc'))
  g = data.table(read_sheet(for_gs, sheet = 'loggers', col_types = '???????cTT???'))
  a = data.table(read_sheet(for_gs, sheet = 'acto_based'))
  n = data.table(read_sheet(for_gs, sheet = 'nests', col_types = '????????T?cc?icc??????' ))
  e = data.table(read_sheet(for_gs, sheet = 'eggs'))
  v = data.table(read_sheet(for_gs, sheet = 'visits', col_types = '????????????cc???' ))
  ca = data.table(read_sheet(for_gs, sheet = 'captures'))
  r = data.table(read_sheet(for_gs, sheet = 'RFID'))
  l_IDs = data.table(read_sheet(for_gs, sheet = 'logger_IDs' , col_types = '??c???????????' ))
  is = data.table(read_sheet(for_gs, sheet = 'data_issues'))
  parm = data.table(read_sheet(for_gs, sheet = 'flotation', col_types = '??nnnnnn?' ))
  u = data.table(read_sheet(for_gs, sheet = 'use'))

  fb = c(list.files(here::here('Data'), pattern = 'DB_20', recursive = TRUE, full.names = TRUE))
  file.move(fb, "freeze/googleDB")

  save(read_me,a,au,sp,s,ile,g,a,n,e,v,ca,r,u,l_IDs,is,parm, file = glue('Data/DB_{format(Sys.time(),"%Y-%m-%d_%H%M")}.RData')) #here::here(glue('Data/DB_{format(Sys.time(),"%Y-%m-%d_%H%M")}.Rdata')))

# END