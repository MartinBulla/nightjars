#' Loads packages and installs those that are not in the library
#' @param  vector of package names
#' @export

using<-function(...) {
    libs<-unlist(list(...))
    req<-unlist(lapply(libs,require,character.only=TRUE))
    need<-libs[req==FALSE]
    if(length(need)>0){ 
        install.packages(need)
        lapply(need,require,character.only=TRUE)
    }
}

#' Combines lists of validators and adds author responsible for the specific data entry
#' @param  x data frame with author and rowid
#' @param  L A list resulting from combining several validators
#' @param  collapase shall the issues be collapsed into one row with rowids in one column or not
#' @param  author shall author be added or not
#' @export

evalidatorsWITHauthor <- function (x ,L, collapse = TRUE, author = TRUE) {

    o = merge(x[,.(rowid, author)], try(rbindlist(L, fill = TRUE), silent = TRUE))

    if( all( c('rowid', 'author', 'variable', 'reason') %in% names(o) )  ) { 
        if(collapse == TRUE) {
          if(author == TRUE) {
            o = o[, .(rowid = paste(rowid, collapse = ",")), by = .(author, variable, reason)]
            } else
                o = o[, .(rowid = paste(rowid, collapse = ",")), by = .(variable, reason)]
        }} else
            o = data.frame(rowid = NA, variable = NA, 
                reason = 'Validators are not working at the moment!')

     o       

  }
#' Assign class to the data, runs the inspectors and prepares data for htmltable output
#' @param  x data frame to inspect
#' @param  Class (S3) name of the database table corresponding to the name of an inspector
#' @export

evalTable = function(x, Class) {
  class(x) = c(class(x), Class)
  if(!'rowid' %in% names(x)) {
        suppressWarnings(x[, rowid := .I+1])# accounts for row 1 being header in googlesheets
        }
  ii = inspector(x)
  o=evalidatorsWITHauthor(x, L = ii)[, tab_ := Class]
  setcolorder(o, c("author", "tab_", "variable","reason","rowid"))
  return(o)
  }

#' Assign transparency to colors
#' @param  col (color name/code)
#' @param  newalpha (transparency value in mcv units) 
#' @param  mcv (maximum color value) 
#' @export
transpcol = function (col = "red", newalpha = 100, mcv = 255) 
        {
          mycol = col2rgb(col)
          rgb(mycol[1, ], mycol[2, ], mycol[3, ], alpha = newalpha,  # to make it in %  alpha = (100-newalpha)*mcv/100,
            maxColorValue = mcv)
        }
  
#' Extract time as numeric from POSIXct
#' @param  x (POSIXct)
#' @export
getime = function (x) {ifelse(is.na(x), as.numeric(NA), as.numeric(difftime(x, trunc(x,"day"), units = "hours")))}

#' Extract DATE from POSIXct
#' @param  x (POSIXct)
#' @export
getDay = function (x) {as.Date(trunc(x, "day"))}

#' Split intervals crossing midnight into respective number of days
#' @param  x data.table containing 
#'            start_ (POSIXct) of an event
#'            end_ (POSIXct) of and event
#'            ... any other variables'
#' @export
#' @examples
#' x = data.table(
#' start_ = as.POSIXct(c("2018-05-17 16:50:00", "2018-05-21 22:50:00", "2018-05-22 22:55:00", "2018-05-29 23:50:00"), tz = "CET"),
#' end_ =   as.POSIXct(c("2018-05-17 16:55:00", "2018-05-22 02:50:00", "2018-05-25 03:50:00", "2018-06-05 04:50:00"), tz = "CET"), 
#' dummy = "dummy")
#' split_mid(x)
split_mid = function(x) {
 TZ = attributes(x[, start_])$tzone
 x[, pk := 1:nrow(x)]
 
 new = rbindlist(mapply(FUN = function(x1,x2, x3) {
    return(data.table(date_ = as.IDate(x1:x2), pk = rep(x3, length(x1:x2))))}, 
    x1 = x[, as.IDate(start_)], x2 = x[, as.IDate(end_)], x3 = x[, pk], SIMPLIFY = FALSE)
    )
 
 new = merge(new, x, by = 'pk')
 new[date_ != as.IDate(start_), start_ := as.POSIXct(paste0(date_, " 00:00:00"), tz = TZ)]
 new[date_ != as.IDate(end_), end_ := as.POSIXct(paste0(date_, " 23:59:59"), tz = TZ)]
 new[, ':=' (date_ = NULL, pk = NULL)]
}

#' New upload of a table to the database
#' @param  table_name 
#' @param  table_data 
#' @param  db_location 
#' @param  who_did 
#' @param  db_name
#' @param  upload_type
#' @param  upload_script
#' @param  upload_comments
#' @export
reuploadDB <-function(table_name = NULL, table_data = xd, db_location = db, who_did = who, db_name = 'unip_inc', upload_type = 'all new', upload_script = 'MasterDB_upload.R' , upload_comments = NA) {
    con = dbConnect(dbDriver("SQLite"),dbname = db_location)
    dbWriteTable(con, name = table_name, value = table_data, row.names = FALSE, overwrite = TRUE)
    dv = data.table(pk = NA, db = db_name, table = table_name, datetime_ = as.character(Sys.time()), 
                    author = who_did, type = upload_type, script = upload_script, comments = upload_comments, stringsAsFactors = FALSE)
    dbWriteTable(con, name = "DBLOG", value = dv, row.names = FALSE, append = TRUE)
    dbDisconnect(con)
    print(paste(table_name, upload_type))
    }

#' New upload of a table to the database
#' @param  table_name 
#' @param  table_data rows of data that are new or differ from those in the DB
#' @param  var_ primary key variable that is unique - same for rows in the masterDB and googlesheets shall ) 
#' @param  db_location 
#' @param  who_did 
#' @param  db_name
#' @param  upload_type
#' @param  upload_script
#' @param  upload_comments
#' @export
ins_repDB <-function (table_name = NULL, table_data = diff, var_ = "pk",
                      db_location = db, who_did = who, db_name = 'unip_inc', 
                      upload_type = 'added new or modified', upload_script = 'MasterDB_upload.R' , upload_comments = NA){
  if (nrow(table_data)>0){  
    # REPLACE or/and INSERT
      con = dbConnect(dbDriver("SQLite"),dbname = db_location)    
      dbExecute(con, "DROP TABLE IF EXISTS temp")
      dbWriteTable(con, name = "temp", value = table_data, row.names = FALSE, overwrite = TRUE)
      dbExecute(con, paste("DELETE FROM", table_name, "where", var_, "in (select", var_ , "from temp)"))
      dbWriteTable(con, name = table_name, value = table_data, row.names = FALSE, append = TRUE)
      
      dv = data.table(pk = NA, db = db_name, table = table_name, datetime_ = as.character(Sys.time()), 
                    author = who_did, type = upload_type, script = upload_script, comments = upload_comments, stringsAsFactors = FALSE)
      dbWriteTable(con, name = "DBLOG", value = dv, row.names = FALSE, append = TRUE)
      dbDisconnect(con)
      print(paste(table_name, upload_type))
    
    } else {print(paste(table_name,'no new data'))}

 }    