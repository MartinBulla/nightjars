#' to do - laying has priority, then hatching and then flotation - change accordingly

#' @title        Calculates the estimated first egg, start of incubation and hatch date
#' @name         lay_inc_hatch_est
#' @param        method a laying/incubation start estimation method ('flotation', 'laying', 'hatching')
#' @param        y a data.table conditional on method containing data for one nest
#'                  for 'flotation' method
#'                    datetime of floating  (POSIXct),
#'                    float_angle (angle with which the egg is floating in degree (must be >20 and <90) ),
#'                    float_height float height in mm (-1 if the egg is under the water surface)
#'                    clutch (final clutch size)'
#'                    lay_interval between two eggs of the species/population in days
#'                    inc_period of the species/population in days
#'                  for 'laying' method
#'                    laid_before_full (POSIXct datetime closest to full_clutch_found when maximum known clutch size smaller then full clutch was present in the nest),
#'                    clutch_before_full (numeric clutch size of laid before full),
#'                    full_cluth_found (POSIXct datetime when final clutch size was determined),
#'                    clutch (final clutch size),
#'                    lay_interval between two eggs of the species/population in days
#'                    inc_period of the species/population in days
#'                    clutch_size of the species/population in days
#'                  from the date of hatching
#' @param        floated a variable name with datetime the egg was floated
#' @description  This function estimates first egg and start of incubation based on laying interval of the species
#'               found in literature or based on egg flotation according to Liebezeit et al. (2007)
#'               "Assessing the development of shorebird eggs using the flotation method:
#'               Species-specific and generalized regression models." Condor 109(1): 32-47.
#'               - incubation start refers to the date the final egg of the clutch was laid
#'                    - for laying:  found date + number of eggs laid since nest found times laying interval of the species
#'                    - for flotation the youngest egg in the clutch, excluding extremes +/- 5 days from median egg
#'                      laying date of given clutch).'
#'               - first egg day refers to the date the first egg was laid estimated as
#'                   incubation start - (clutch size -1) * laying interval between two eggs of the species
#'               - hatch date refers to the date when the first chick is expected to hatch (calculated by adding
#'                 incubation period of the species to start of incubation; note that same as in Liebezeit incubation #'                 period is estimated by subtracting the date of incubation start from that of hatching
#' @return       an estimated dates (as POSIXct)
#' @export
#' @examples
#'  #----------------------------------------------------#
#' y1 = data.table(
#'   datetime = as.POSIXct(c('2017-06-01 10:00','2017-06-01 10:00','2017-06-01 10:00','2017-06-01 10:00')),
#'   float_angle = c(21, 80, 85,90),
#'   float_height = c(-1, -1, NA, 1),
#'   clutch = 4,
#'   lay_interval = 1,
#'   inc_period = 20
#'   )
#'
#' xx = lay_inc_hatch_est(method = 'flotation', y = y1, floated = 'datetime')
#' xx


lay_inc_hatch_est <- function(method = 'flotation', y,  floated = 'datetime_processed') { #

  logit = binomial()$linkfun

  if(method == 'flotation'){
   o = y 
   setnames(o, floated, c("floated"))
   #o = y[!is.na(float_angle),]
   #o[ ,float_height := as.numeric(float_height)]
   #o[ ,float_angle := as.numeric(float_angle)]
   o[,inc_start := as.POSIXct(NA)]
   # sinking eggs
      o[ is.na(float_height) | float_height < 0, float_angle := as.numeric(ifelse(float_angle<21,21, ifelse(float_angle>89,89, float_angle)))] # transform to avoid proportional angles of 0 and 1, which cannot be logit-transformed
      o[ is.na(float_height) | float_height < 0, inc_start := floated - 24*60*60*inc_period*abs(0.2159 + 0.0548 * logit((float_angle - 20) / 70))] # float_angle - 20) / 70 transforms the angle into proportional angle with 20° representing 0 and 90° representing 1
   
   # floating eggs  # o[ float_height => 0,]
     o[ !is.na(float_height) & !float_height < 0, inc_start := floated - 24*60*60*inc_period*abs(0.79522640272596 + 0.0669682399294409 * float_height -0.00420699750466937 * float_angle) ]
                                                                                           
   y = data.table(inc_start = o[,median(inc_start)], o[1, .(clutch, lay_interval, inc_period)]) #  the first egg shall be calculated in nests already
   y[ , first_egg := inc_start - 24*60*60*(clutch - 1)*lay_interval]
   y[ , hatch_date := inc_start + 24*60*60*inc_period]
   }

  if(method == 'laying'){
   y = y[eggs_found < clutch]
   if(nrow(y) > 0) {
      #y[, first_egg := nest_found -  days((eggs_found-1)*lay_interval)]
      #y[, inc_start := first_egg + days((clutch-1)*lay_interval)] 
      y[ , inc_start := laid_before_full + 24*60*60*((clutch-clutch_before_full)*lay_interval)]
      y[!is.na(full_clutch_found) &  full_clutch_found < inc_start, inc_start := full_clutch_found] # if full clutch found earlier then estimated, use datetime when found
      y[ , first_egg := nest_found - 24*60*60*((eggs_found-1)*lay_interval)]
      y[ , hatch_date := inc_start + 24*60*60*inc_period]
    }

   }

  return(y)

}