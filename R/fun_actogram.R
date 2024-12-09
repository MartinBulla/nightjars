#' @title        Actogram of temperature and RFID data and relevant metadata
#' @name         Actogram - temperature and RFID
#' @param        dfr a data.table containing the data for plotting
#'                  who (character)  indicates the owner of the registered tag (female, male, disturb)
#'                  datetime_ (POSIXct)  of the record,
#'         			h_surface (numeric; relative humidity [%] next to the nest)
#'                  t_surface (numeric; temperature [˚] next to the nest)
#'                  h_nest (numeric; relative humidity [%] in the nest)
#'                  t_nest (numeric; temperature [˚] in the nest)
#'                  inc (numeric; 0 - no incubation, 1 - incubation)
#'                  cols (character; colors associated with who)
#'                  col_t_nest (character; colors associated with T in the nest)
#'                  col_h_nest (character; colors associated with H in the nest)
#'                  act (numeric; height of the actogram record)  
#' @param        meta a data.table containing metadata necessary for plotting
#'                  act_ID (character; 4 digit unique ID of the plot - same as rowid in the database)
#'                  nest (character; unique nest ID)
#'                  species (character; unique four letter abbreviation of species common name)
#'                  common (character; species common name)
#'                  scientific (character; species scientific name)
#'                  inc_start (POSIXct; day when incubation started)
#'                  lat (latitude of the nest in decimal degrees)
#'                  lon (longitude of the nest in decimal degrees)
#'					local_plus (numeric; how many hours shall be added to datetime to get longitudinal time'
#'                  inc_period (numeric; length of incubation period of a given population/species:
#'                  subtracting the date the last egg was laid from the date of hatching)
#'                  end_ (datetime; visit during which end state of the nest was determined)
#'                  end_state (character; end state of the nest)
#'                  field_plus (numeric; difference between computer and field time;
#'					hours to subtract from the logger data to bring those to the field time)
#'                  time_zone (character; timezone of the study site from:
#'					https://en.wikipedia.org/wiki/List_of_tz_database_time_zones)
#'					lay_inc_based_on (character; to estimate first egg and inc start)
#'					hatch_based_on (character; indicates method used to estimate when nest hatched)
#'					end_based_on (character; indicates method used to estimated end date and state)
#' @param        vis a data.table containing visits and disturbances
#'                  day (Date; date of the visit in yyyy-mm-dd format)
#'                  hstart (numeric; time in hours when visit started)
#'                  hend (numeric; time in hours when visit finished > 'hstart')
#'                  capture (logi: TRUE/FALSE; has capture occurred during the given period?) 
#' @param        output (character; PNG - created, DEV - within R)
#' @param        UTC/UTC_met (TRUE/FALSE)  shall data/metadata be transformed to longitudinal sun based time
#' @param        map (logical; TRUE/FALSE) - if true, object "m" with ggplot type map needs to be provided
#' @param        extr (logical; TRUE/FALSE) - if true, "extr" is added in-front of the file name
#' @param        visit_labels (logical; TRUE/FALSE) - if true, egg and chick states are added to each visit
#' @param        issues (character) - issues with surface temperature - if none '', else 'ISSUES' will be pasted in legend
#' @param        day (character; type of panel labels: date,  day - day of incubation period and inc constancy,
#' 				 all- date, day of incubation period and inc constancy
#' @param        ylabR (character; right y axis label) 
#' @param        longest (character; longest figure legend item for all actograms in a batch
#' 					to ensure same legend placement)
#' @param        min_/max_ - limits of y-axis within the panel
#'				 if present dfr/meta needs 'local_plus' variable indicating how many hours to add to the datetime var
#' @param        guides_ (logical; TRUE/FALSE - shall guides for each hour be displayed)
#' @param        wr_col (character; color used for text)
#' @param        ln_col (character; color used for lines)
#' @param        wr_col_out (character; color used for lines)
#' @param        night (character; color to indicate night)
#' @param        twilight (character; color to indicate twilight)
#' @param        hath_st (character; color to indicate start of hatching)
#' @param        inc_end (character; color to indicate end of incubation)
#' @param        disturb (character; color indicating disturbance)
#' @param        female (character; color for female RFID readings)
#' @param        male (character; color for male RFID readings)
#' @param        Tnest_no (character; color indicating nest temperatures of unincubated nest)
#' @param        Tnest_inc (character; color indicating nest temperatures of incubated nest)
#' @param        Tsurf (character; color indicating surface temperatures next to the nest)
#' @param        Hnest_no (character; color indicating nest humidity of unincubated nest)
#' @param        Hnest_inc (character; color indicating nest humidity of incubated nest)
#' @param        Hsurf (character; color indicating  surface humidity next to the nest)
#' @description  This function plots temperature and RFID data together with their respective metadata
#' @return       plot (in open device, png, pdf or prepared Rdata)
#' @export
#' @examples
#'  #----------------------------------------------------#

RFID.temperature_actogram = function(dfr, meta, vis, vid_ = FALSE,
                                     output = "PNG", 
									 where_ = "Actograms/",
									 UTC = FALSE, UTC_met = FALSE, map = TRUE, 
									 visit_labels = TRUE,
									 extr = TRUE,
									 issues = '',
									 day = 'all', ylabR = 'Temperature [°C]   |   Humidity [relative]', 
									 longest = '0053 | 2019 | KNBA19MINZ2',
									 min_ = -3, max_ = 49, 
									 guides_ = TRUE,
									 wr_col = "grey50", wr_col_out = "grey70", ln_col = "grey80", tra = '#FFFFFFFF',
									 night = "grey45", twilight = "grey70", 
									 hatch_st =  "#ceb6d8", inc_end = "#9e6eb2",  inc_end_field = "#f0b2b2",
									 female = "#FCB42C", male = "#535F7C", 
									 disturb = "#5eab2b",##9097ab",#"#5eab2b", # visits transparent, captures solid
									 act_based_vis = "#44a6c6",
									 Tnest_no = "#fc4c2c",##71bbd4","#f0b2b2"
									 Tnest_inc = "#fdd280",#"#99c978",
									 Tsurf = "#FCB42C",#"#ADD8E6", ##adcae6",  ##44a6c6
									 #Tnest_no = "#fc4c2c", Tnest_inc = "#FCB42C", Tsurf = "#ADD8E6", ##adcae6",  ##44a6c6 
									 Hnest_no = "#535F7C",Hnest_inc = "#ADD8E6",Hsurf = "#4ba9c8",
									 Lnest_no = "#0A6358", #373737", 
									 Lnest_inc = "#D7F5F3",#E6FAF7"#7D7D7D", 
									 Lsurf = "#20BAA3",#5F5F5F",#3a9fbf"#"#72BCD4"#"#9097ab" #Hnest_inc = "#ADD8E6"
									 #Hnest_no = "grey80",Hnest_inc = "grey60",Hsurf = "grey90"
									 egg_chick = "grey14"#"#5eab2b"
									 ){
 
 # set up colors and line height for RFID readings
 act_c = data.table(stringsAsFactors = FALSE,
	   		who  = c('f','m','disturb'),
            cols = c(female,male,disturb),
            act  = c(15,15,10)
           )

# TO DO: adjust data to longitudinal time if UTC == TRUE
  if(UTC==TRUE){dfr[, datetime_ := datetime_ + local_plus*60*60]}# adjusts to longitudinal time if aksed for
  if(UTC_met==TRUE){ins=ins+meta$local_plus*60*60} # adjusts to longitudinal time if aksed for


# specific datasets from "meta"
	latlon = meta[, list(lat, lon)]	# 
	inp = meta[,inc_period]
	ins = meta[,inc_start] #+ meta[,field_plus]*60*60
	
	hatch = meta[,.(hatch_date)] # estimated/extracted hatch date
	hatch[getDay(hatch_date)>getDay(dfr$datetime_[nrow(dfr)]), hatch_date := NA] # use only if within data range
	hatch[, day := getDay(hatch_date)]
	hatch[, time := getime(hatch_date)]

	ine = meta[,list(end_, end_state)] # final end state
	#ine[, end_ := end_ + meta[,field_plus]*60*60]
	ine[, day := getDay(end_)]
	ine[, time := getime(end_)]

	inefield = meta[,list(end_field, end_state_field)] # database based end state
	inefield[, day := getDay(end_field)]
	inefield[, time := getime(end_field)]
	
	vis[, capture := ifelse(capture == TRUE, 1, 0.2)]
	vis[, col_ := ifelse(capture == TRUE, disturb, ifelse(what %in% c('m', 'v'), act_based_vis, transpcol(disturb, newalpha = 200)))]
	vis[, egg_chick := ifelse(is.na(eggs), paste0(" |", chicks), ifelse(is.na(chicks), paste0(eggs, "| "), paste0(eggs,"|",chicks)))]
    #vis[, start_ := start_ + meta[,field_plus]*60*60]
    #vis[, end_ := end_ + meta[,field_plus]*60*60]
	
	vis = split_mid(vis) # split visits spanning over midnight into two
	vis[ , day := getDay(start_)]
    vis[ , hstart := getime(start_)]
    vis[ , hend := getime(end_)]

#  if (type =='PDF') {
#      tf = paste0(outdir,'/',dfr$nest[1], ".pdf")
#     pdf(tf, paper = "a4", width = 8, height = 11.6929134)
#}
#dfr[, datetime_ := as.POSIXct(datetime_, tz="UTC")]
	
dfr = merge(dfr, act_c, all.x=TRUE) # add colors and size for actogram
dfr = dfr[order(datetime_)]
dfr[, col_t_nest := ifelse(is.na(inc), NA, ifelse(inc == 1,Tnest_inc, Tnest_no))] # color for nest temperatures
dfr[, col_h_nest := ifelse(is.na(inc), NA, ifelse(inc == 1, Hnest_inc, Hnest_no))] # color for nest humidity
dfr[, col_l_nest := ifelse(is.na(inc), NA, ifelse(inc == 1, Lnest_inc, Lnest_no))] # color for nest humidity

#if(meta[,field_plus] != 0 & !is.na(meta[,field_plus])){ dfr[, datetime_ := datetime_ - meta[,field_plus]*60*60] } # bring logger time to field time - done already earlier
dfr[, day := getDay(datetime_)]
dfr[, time := getime(datetime_)]

# prepare panels and their labels
	if(vid_==TRUE){ 
		sl1 = unique(unique(dfr$day), unique(vid$day))
		sl2a = dfr[, .(const = mean(inc,na.rm=TRUE)), by = day]
		sl2b = data.table(day = unique(vid$day))
		sl2 = merge(sl2a,sl2b, all= TRUE)
		}else{
		sl1=unique(dfr$day)
		sl2 = dfr[, .(const = mean(inc,na.rm=TRUE)), by = day]
	}
	 sl1=sl1[order(sl1)]

	
	 sl2[, day_j := formatC(as.numeric(format(sl2$day ,"%j")) - yday(ins), width = 2, format = "d", flag = "0")]
	 sl2[, day_inc_per := as.character(paste(day_j,"/",round(as.numeric(inp),0),"; ",round(const*100,0),'%',sep=""))] 
	 sl2[, date_dayInc_con := as.character(paste(format(sl2$day, "%b %d"), " | ", day_j,"/",round(as.numeric(inp),0)," | ",round(const*100,0),'%',sep=""))]
	 sl2[, date__con := as.character(paste(format(sl2$day, "%b %d"), " | ", round(const*100,0),'%',sep=""))]

# prepare polygons for night and twilight
	if(UTC == FALSE){tz_ = meta$time_zone; lon_ = latlon$lon} else {tz_ = "UTC"; lon_ = 0}
	nt = data.table(day = c(sl1[1]-1 ,sl1, sl1[length(sl1)]+1), stringsAsFactors =FALSE) # add day before and after time series to allow for sunrise  that happens after midnight
	nt[ , day_pos := as.POSIXct(as.character(day), tz = tz_)]

	dusk_ = data.table(datetime_ = crepuscule(crds = matrix(c(lon_,latlon$lat),nrow=1), nt$day_pos, solarDep=6, direction=c("dusk"),POSIXct.out=TRUE)$time)
	dusk_[, day := getDay(datetime_)]
	dusk_[, c_dusk := getime(datetime_)]
	dusk_ = dusk_[!is.na(datetime_)]
	dusk_[,datetime_ := NULL]

	sunrise_ = data.table(datetime_ = sunriset(matrix(c(lon_,latlon$lat),nrow=1), nt$day_pos, direction=c("sunrise"),POSIXct.out=TRUE)$time)
	sunrise_[, day := getDay(datetime_)]
	sunrise_[, sunrise := getime(datetime_)]
	sunrise_ = sunrise_[!is.na(datetime_)]
	sunrise_[,datetime_ := NULL]

	sunset_ = data.table(day = nt$day_pos, datetime_ = sunriset(matrix(c(lon_,latlon$lat),nrow=1), nt$day_pos, direction=c("sunset"),POSIXct.out=TRUE)$time)
	sunset_[, day := getDay(datetime_)]
	sunset_[, sunset := getime(datetime_)]
	sunset_ = sunset_[!is.na(datetime_)]
	sunset_[,datetime_ := NULL]


	dawn_ = data.table(datetime_ = crepuscule(matrix(c(lon_,latlon$lat),nrow=1), nt$day_pos, solarDep=6,direction=c("dawn"),POSIXct.out=TRUE)$time)
	dawn_[, day := getDay(datetime_)]
	dawn_[, c_dawn := getime(datetime_)]
	dawn_ = dawn_[!is.na(datetime_)]
	dawn_[,datetime_ := NULL]

	if((nrow(sunrise_)+nrow(sunset_)) == 0 ){
		nt = data.table(day = NA, sunrise = NA, sunset = NA, c_dawn = NA, c_dusk = NA, stringsAsFactors =FALSE)
		} else {
			nt = merge(sunrise_,sunset_,by = 'day', all = TRUE)
			if(nrow(dusk_)>0){
				nt = merge(nt, dusk_,by = 'day', all = TRUE)
			} else { nt[, c_dusk := NA]}
			if(nrow(dawn_)>0){
				nt = merge(nt, dawn_,by = 'day', all = TRUE)
			}else { nt[, c_dawn := NA]}
	}
	nt = nt[day %in% sl1]
	nt = nt[!duplicated(nt$day)] # removes duplicated dates (i.e. exceptional events where two sunsets at one day, i.e. when data very close to midnight)
	# dusk_ = data.table(datetime_ = crepuscule(matrix(c(-149.7516, 61.28376), nrow =1), as.POSIXct("2019-06-03", tz = tz_), solarDep=6,direction=c("dusk"),POSIXct.out=TRUE)$time), what = 'dusk'))

# scales, 
  if(day =='date'){ 
		strip.left1 = function(which.panel, ...) {
				LAB = format(sl1[which.panel], "%b-%d")
				grid.rect(gp = gpar(fill = "grey95", col=ln_col))
				ltext(0.5, 0.5, cex = 0.6, LAB, col=wr_col)
		 }
		ylab_=list('Date',cex=0.7, col=wr_col, vjust=1, hjust=0.5)
		rp_width = 2								
	 }
	 
  if(day == 'day'){
		 strip.left1 = function(which.panel, ...) {
			LAB = sl2$day_inc_per[which.panel]
			grid.rect(gp = gpar(fill = "grey95", col=ln_col))
			ltext(0.5, 0.5, cex = 0.6, LAB, col=wr_col)							
		  } 									
		 ylab_=list('Day of incubation / incubation period; incubation constancy %',cex=0.7, col=wr_col, vjust=1, hjust=0.5)	
		 rp_width = 3	
	 }

   if(day == 'Date & con'){
         strip.left1 = function(which.panel, ...) {
            LAB = sl2$date__con[which.panel]
            grid.rect(gp = gpar(fill = "grey95", col = ln_col))
            ltext(0.5, 0.5, cex = 0.6, LAB, col = wr_col)
          }
         ylab_=list('Date | Nest attendance', cex = 0.7, col = wr_col, vjust = 1, hjust = 0.5)   
         rp_width = 3       
     }	 
	   
  if(day == 'all'){
		 strip.left1 = function(which.panel, ...) {
			LAB = sl2$date_dayInc_con[which.panel]
			grid.rect(gp = gpar(fill = "grey95", col = ln_col))
			ltext(0.5, 0.5, cex = 0.6, LAB, col = wr_col)
		  }
		 ylab_=list('Date | Day of incubation / incubation period | incubation constancy %', cex = 0.7, col = wr_col, vjust = 1, hjust = 0.5)	
		 rp_width = 5		
	 }

  scales1 = list(x = list(at = c(0,6,12,18,24), labels = c('00:00','06:00','12:00','18:00','24:00'), cex = 0.6, tck = 				0.4, limits = c(0,24), col = wr_col, col.line = ln_col, alternating = 3), 
	 			 y = list(at = c(max_*10/max_, max_*30/max_), limits = c(min_, max_), draw = TRUE), col = wr_col, cex = 0.5, tck=0.04, alternating = 2, col.line = ln_col)
				
  ylab_right=list(ylabR, cex = 0.7, col = wr_col, vjust=-0.3)		#hjust=0

  #scales = list(x = list(at=c(0,6,12,18,24),labels=c('00:00','06:00','12:00','18:00','24:00') , #cex = 0.7, 
  #			limits = c(0,24),alternating=3), y = list(limits = c(0, 50),at =c(10,30), alternating=2, cex=0.6, tck=0.4))	

# legend 
  # caption
	clr_0=list(text=list(c(meta$scientific, meta$common, paste0(meta$act_ID, " | ", meta$year, " | ", meta$nest), paste0('laying method: ', meta$lay_inc_based_on), paste0('hatched method: ', meta$hatch_based_on),longest),cex=0.6, col=c(wr_col,wr_col,wr_col,wr_col,wr_col,tra), font=c(3,2,1,1,1,1)),
					points=list(pch=c(15),cex= c(0.8), col=c(tra))) 
  
  # caption compensation on the right of the legend
  	 clr_e=list(text=list(c(meta$scientific ,meta$common, meta$act_ID,longest),cex=0.6, col=tra))	
  
  # T/H caption 
	 clr_TH=list(text = list(c("nest (incubated) ","nest (un-incubated)","surface", issues)),cex=0.6, col=wr_col)
	
  # temperatures 
	 if(nrow(dfr[!is.na(t_surface)]) > 1) { col_t_hs = wr_col; col_p_hs = Tsurf } else { col_t_hs = wr_col_out; col_p_hs = tra } 
     if(nrow(dfr[!is.na(t_nest)]) > 1)  {col_t_nn = col_t_ni = wr_col; col_p_nn = Tnest_no; col_p_ni = Tnest_inc 
     	} else { col_t_nn = col_t_ni = wr_col_out; col_p_nn = col_p_ni = tra } 
    
	 clr_T=list(text = list(c("T [°C]","T [°C]","T [°C]"),cex=0.6, col=c(col_t_ni, col_t_nn, col_t_hs)),
							#lines = list(col=act_c$cols[act_c$who%in%c("nest temperature","surface temperature")],lwd=2,size=1))}			
							points = list(col=c(col_p_ni, col_p_nn, col_p_hs),pch=20,cex=0.5)
							)		
  
  # humidity
     if(nrow(dfr[!is.na(h_surface)]) > 1) { col_t_hs = wr_col; col_p_hs = Hsurf } else { col_t_hs = wr_col_out; col_p_hs = tra } 
     if(nrow(dfr[!is.na(h_nest)]) > 1)  { col_t_nn = col_t_ni = wr_col; col_p_nn = Hnest_no; col_p_ni = Hnest_inc 
     	} else { col_t_nn = col_t_ni = wr_col_out; col_p_nn = col_p_ni = tra } 
    
	 clr_H=list(text = list(c(" H [%]"," H [%]"," H [%]"),cex=0.6, col=c(col_t_ni, col_t_nn, col_t_hs)),
							#lines = list(col=act_c$cols[act_c$who%in%c("nest temperature","surface temperature")],lwd=2,size=1))}			
							points = list(col=c(col_p_ni, col_p_nn, col_p_hs),pch=20,cex=0.5)
							)		
  # light
     if(nrow(dfr[!is.na(l_surface)]) > 1) { col_t_hs = wr_col; col_p_hs = Lsurf } else { col_t_hs = wr_col_out; col_p_hs = tra } 
     if(nrow(dfr[!is.na(l_nest)]) > 1)  { col_t_nn = col_t_ni = wr_col; col_p_nn = Lnest_no; col_p_ni = Lnest_inc 
     	} else { col_t_nn = col_t_ni = wr_col_out; col_p_nn = col_p_ni = tra } 
    
	 clr_L=list(text = list(c(	" lux"," lux"," lux"),cex=0.6, col=c(col_t_ni, col_t_nn, col_t_hs)),
							#lines = list(col=act_c$cols[act_c$who%in%c("nest temperature","surface temperature")],lwd=2,size=1))}			
							points = list(col=c(col_p_ni, col_p_nn, col_p_hs),pch=20,cex=0.5)
							)					
  # incubates and RFID (+ dummy transparent line to add padding to the whole legend)
		r1=nrow(dfr[!is.na(who) & who=="f"])
		r2=nrow(dfr[!is.na(who) & who=="m"])
		r3= ifelse(meta$sex == 'f', paste("Sex: \u2640"), ifelse( meta$sex == 'm', paste("Sex: \u2642"), ifelse( meta$sex == 'u', paste("Sex: ?"),ifelse( meta$sex == 'b', paste("Sex: \u2640 & \u2642"), paste0("Sex: ", meta$sex)))))


		if(r1==0 & r2==0){ col_r_t= c(wr_col, wr_col_out,wr_col_out,tra)
						   col_r_p=c(tra,tra,tra,tra )
							}
		if(r1>0 & r2==0 ){ col_r_t= c(wr_col,wr_col,wr_col_out,tra)
							col_r_p=c(tra,female,tra,tra )
							}
		if(r1==0 & r2>0){	col_r_t= c(wr_col,wr_col_out,wr_col,tra)
							col_r_p=c(tra,tra, male,tra )
								}					
		if(r1>0 & r2>0){	col_r_t= c(wr_col,wr_col,wr_col,tra)
							col_r_p=c(tra,female, male,tra )
								}	

		clr_INC=list( text = list(c(r3, 'RFID \u2640','RFID \u2642'),col=col_r_t,cex=0.6),
								text = list(c("|","|","|"), cex=c(0.6,0.6,0.6), font="bold", col = col_r_p))
								  								   
  # disturbance, hatch start and end state
		if(nrow(vis) + nrow(dfr[who == 'disturb']) == 0){dis_t = wr_col_out; dis_p = tra}else{dis_t = wr_col; dis_p = disturb}
		if(nrow(vis[what %in%c('m','v')]) == 0){act_based_t = wr_col_out; act_based_p = tra}else{act_based_t = wr_col; act_based_p = act_based_vis}
		if(is.na(hatch$hatch_date)){hatch_t = wr_col_out; hatch_p = tra}else{hatch_t = wr_col; hatch_p = hatch_st}
		
		if(meta[, getDay(end_)] > dfr[nrow(dfr),getDay(datetime_)]){end_final_t = wr_col_out; end_final_p = tra}else{end_final_t = wr_col; end_final_p = inc_end}

		if(meta[, getDay(end_field)] > meta[,getDay(logger_off)] | meta[, getDay(end_field)] > dfr[nrow(dfr),getDay(datetime_)]){end_field_t = wr_col_out; end_field_p = tra}else{end_field_t = wr_col; end_field_p = inc_end_field}

	
		col_i_t=c(dis_t, act_based_t,hatch_t,end_final_t, end_field_t)
		col_i_p=c(dis_p,act_based_p, hatch_p, end_final_p, end_field_p)	
			  
		
		clr_HSED=list( text = list(c("visit", "acto based info", "hatching ", paste0("end (final): ",ine$end_state), paste0("end (field): ",inefield$end_state_field)),cex=0.6, col=col_i_t), text = list(c("|","|","|","|","|"),col=col_i_p,cex= c(0.6),font="bold"))
															   
  # twilight and night (sun elevation) + end_based_on
		nt1=nrow(nt[!is.na(sunrise)])
		nt2=nrow(nt[!is.na(c_dawn)])
		
		if(nt1==0 & nt2 ==0){	col_s_t=c(wr_col_out,wr_col_out)
							    col_s_p=c(tra,tra)}
		if(nt1>0 & nt2 ==0){	col_s_t=c(wr_col,wr_col_out)
								col_s_p=c(twilight,tra)}				
		if(nt1>0 & nt2 >0){		col_s_t=c(wr_col,wr_col)
								col_s_p=c(twilight, night)}
			
		clr_TN=list(text = list(c("twilight","night",'',meta$end_based_on),col=c(col_s_t, wr_col_out, wr_col), cex=0.6),
				   points = list(col=c(col_s_p, tra, tra),pch=c(15),cex= c(0.6)))

	
  # adds buffer around legend columns by creating fake legend columns
		clr_n=list(text = list(c("")))				
		
  # combine
	 key1 = c(# captions
					clr_0,
					clr_n,
				 # T/H
				 	clr_TH,	
				 # temperature
					clr_T,
					#clr_n,
				# humidity
					clr_H,
				# light
					clr_L,	
					clr_n,	
				 # rfid
					clr_INC,
					clr_n,
				# disturbance/ hatching / end
					clr_HSED,
					clr_n,
				 # sun
					clr_TN,
					clr_n,	
				# ending to compensate for captions
					clr_e,
				rep=FALSE, between=0.9 #just=-0.3#, 	padding.text=1.1,#, border=ln_col #columns
				)	
  	key_ =if(output == 'obj'){key_ = clr_T}else{key_=key1}

panel1 = function(...) {
	 
	# twilight and night 
		nti = nt[which(nt$day == sl1[panel.number()]),] 
		#nti = nt[which(nt$day == sl1[1]),] 
		if(nrow(nti)>0){
			# sunset present
			if(!is.na(nti$sunset)){
				if(nti$sunset>12){
					xr = ifelse(nti$sunrise>nti$sunset, nti$sunrise, 23.9999)
					panel.rect(xleft=nti$sunset, ybottom=min_, xright=xr, ytop=max_, col=twilight, border=0)
					}else{
						panel.rect(xleft=nti$sunset, ybottom=min_, xright=nti$sunrise, ytop=max_, col=twilight, border=0)	
				}
			}		
			# sunrise present
			if(!is.na(nti$sunrise)){
				if(nti$sunrise<12){
					if(is.na(nti$sunset)){
						panel.rect(xleft=0, ybottom=min_, xright=nti$sunrise, ytop=max_, col=twilight, border=0)
					}else{
					xl = ifelse(nti$sunrise>nti$sunset, nti$sunset, 0)
					panel.rect(xleft=xl, ybottom=min_, xright=nti$sunrise, ytop=max_, col=twilight, border=0)
					}
					}else{
					panel.rect(xleft=nti$sunset, ybottom=min_, xright=nti$sunrise, ytop=max_, col=twilight, border=0)	
					}
			}
			# dusk (evening twilight) present
			if(!is.na(nti$c_dusk)){
				if(nti$c_dusk>12){
					xr = ifelse(nti$c_dawn>nti$c_dusk, nti$sunrise, 23.9999)
					panel.rect(xleft=nti$c_dusk, ybottom=min_, xright=xr, ytop=max_, col=night, border=0)
					}else{
					panel.rect(xleft=nti$c_dusk, ybottom=min_, xright=nti$c_dawn, ytop=max_, col=night, border=0)	
					}
			}	
			# dawn (morning twilight) present
			if(!is.na(nti$c_dawn)){
				if(nti$c_dawn<12){
					xl = ifelse(nti$c_dawn>nti$c_dusk, nti$c_dusk, 0)
					panel.rect(xleft=xl, ybottom=min_, xright=nti$c_dawn, ytop=max_, col=night, border=0)
					}else{
					panel.rect(xleft=nti$c_dusk, ybottom=min_, xright=nti$c_dawn, ytop=max_, col=night, border=0)	
					}
			}
		  }					
	# disturbance and acto based information
		#vii = vis[which(day == "2018-06-05"),]
		#vii = vis[which(day == sl1[1]),]
		vii = vis[which(day == sl1[panel.number()]),] # exclude actogram data
	    if(nrow(vii)>0){
			panel.rect(xleft = vii$hstart, ybottom = min_, xright = vii$hend, ytop = max_ , col = vii$col_, border=0)
				}
	# guides
	 	if(guides_ == TRUE){
	 		panel.abline(v=c(1:5,7:11,13:17,19:23),col="grey90", lwd = 0.25)
     		panel.abline(v=c(6,12,18,24),col = "grey80")
	 	}	
	
	# hatching start and end state
		#vih = hatch[which(day == sl1[1]),]
		vih = hatch[day %in% sl1[panel.number()]]
		if (nrow(vih)>0){panel.xyplot(vih$time, max_, col = hatch_st, type="h",origin=min_, lwd=6)}				
		inei = ine[which(day == sl1[panel.number()] & !is.na(end_)),]  
		#inei = ine[which(day == sl1[1] & !is.na(end_)),]  
		if (nrow(inei)>0){ panel.xyplot(inei$time, max_ ,col = inc_end, origin = min_, type = "h",lwd = 3)}
    	
    	inefieldi = inefield[which(day == sl1[panel.number()] & !is.na(end_field)),]  
    	#inefieldi = inefield[which(day == sl1[1] & !is.na(end_field)),]  
		if (nrow(inefieldi)>0){ panel.xyplot(inefieldi$time, max_ ,col = inc_end_field, origin = min_, type = "h",lwd = 1.5)}
	
	# light, humnidity, rfid, temperature
		dfri = dfr[which(dfr$day == sl1[panel.number()]),]
		dfr[l_nest>1000, l_nest :=1000]
		dfr[l_surface>1000, l_surface :=1000]
		#dfri = dfr[which(dfr$day == sl1[1]),]
		
		panel.xyplot(dfri$time, dfri$act, col = dfri$cols, type = "h", origin=min_)
		
		if("l_nest" %in% names(dfri)){
			panel.xyplot(dfri$time, 0.95*max_*dfri$l_surface/1000, col = Lsurf, cex = 0.05)
			panel.xyplot(dfri$time, 0.95*max_*dfri$l_nest/1000, col = dfri$col_l_nest, cex = 0.05)
		   }

		if("h_nest" %in% names(dfri)){
			panel.xyplot(dfri$time, 0.95*max_*dfri$h_surface/100, col = Hsurf, cex = 0.05)
			panel.xyplot(dfri$time, 0.95*max_*dfri$h_nest/100, col = dfri$col_h_nest, cex = 0.05)
		   }

		
		panel.xyplot(dfri$time, dfri$t_nest, col = dfri$col_t_nest, cex = 0.1)	
	   			   								
	# surface temperature
	  panel.xyplot(...)

	# video
		if(vid_ ==TRUE){
		 vidi = vid[which(day == sl1[panel.number()])] # exclude actogram data
	     if(nrow(vidi)>0){
			panel.rect(xleft = vidi$start_time, xright = vidi$end_time,ybottom = min_ ,ytop = max_*0.45, lwd = 1, col = vidi$col, border=0)
				}
			}

	# state of the eggs/chicks and disturbance description
	  vii = vis[which(day == sl1[panel.number()]),] 
	  #vii = vis[which(day == sl1[1]),] 
	    if(nrow(vii)>0){
			panel.text(x = (vii$hstart+vii$hend)/2, y = max_*0.55 , col = egg_chick, labels=vii$egg_chick, cex = 0.6) 
			panel.text(x = (vii$hstart+vii$hend)/2, y = max_*0.32 , col = egg_chick, labels=vii$what, cex = 0.5)
				}		  

	}

# make the plot						
   rfidsplot = xyplot(t_surface ~ time | day, 
						data = dfr, 
						col = Tsurf,
						cex = 0.1, cex.title=0.5, main = NULL,
						layout = c(1,length(sl1)),# ifelse(length(sl1) > 30, 30, length(sl1))), 
						strip.left = strip.left1, 
						scales = scales1,
						panel=panel1, 
						key=key_,
						ylab=ylab_,
						ylab.right=ylab_right,
						xlab.top=list('Time [h]',cex=0.7,col=wr_col, vjust=1),
						xlab=NULL,
						par.settings=list(axis.components=list(left=list(tck=0)), layout.widths=list(right.padding=2),axis.line = list(col = ln_col)), #box.3d=list(col = wr_col)), #top=list(tck=0.4),
						as.table=TRUE,
						#aspect = "fill", 
						strip = FALSE, distribute.type = TRUE,    
						lattice.options = list(layout.widths = list(strip.left = list(x = rp_width))) # 3 for date
						)
						
	 #grid.rect(gp=gpar(lty="dashed", col="red"))
   
   # prepare the study sitelocation for map	
	  if(map == TRUE){
		  coordinates(latlon) = ~ lon+lat
		  proj4string(latlon) = p4s_latlon
		  loc = spTransform(latlon, CRS(p4s))
		  mm = m + geom_point(data=data.frame(loc@coords), aes(x=coords.x1, y=coords.x2),color='red', size=0.2)
			#vp <- viewport(x=0.89,y=0.94,width=0.11*1.5, height=0.11) 						
			#pushViewport(vp)
			#print(mm, newpage=FALSE)
			#plot(1:10,1:10)
		}

# export	
   if(output == 'DEV'){
       dev.new(width=8.26771654, height=11.6929134)
   	   par(pin = c(8.26771654, (57+8*length(sl1))/25.4)) #c(8.26771654, 11.6929134)#par(pin = c(8.26771654, 11.6929134)) 
	   print(rfidsplot)
	   
	   if(map == TRUE){
	      vp <- viewport(x=0.89,y=((57+8*length(sl1))-12.6225)/(57+8*length(sl1)),width=0.11*1.5, height=32.67/(57+8*length(sl1)))#vp <- viewport(x=0.89,y=0.9575,width=0.11*1.5, height=0.11) # creates area for map	 					
	     pushViewport(vp)
	     print(mm, newpage=FALSE) # prints the map	
	     }
   	 }

   if(output == 'PNG'){
		if(extr == TRUE){ext = "ex"}else{ext = ""}
		tf = paste0(here::here(where_), paste(ext, meta$act_ID,"_",meta$sp,"_",meta$site,"_",meta$year,"_",meta$nest,sep=""), ".png") 
		png(tf,width = 210, height = 57+8*length(sl1), units = "mm", res = 600)	#png(tf,width = 210, height = 297, units = "mm", res = 600)	
		par(pin = c(8.26771654, (57+8*length(sl1))/25.4)) #c(8.26771654, 11.6929134)#par(pin = c(8.26771654, 11.6929134)) 
		print(rfidsplot)
	    
	    if(map == TRUE){
		   vp <- viewport(x=0.89,y=((57+8*length(sl1))-12.6225)/(57+8*length(sl1)),width=0.11*1.5, height=32.67/(57+8*length(sl1)))#vp <- viewport(x=0.89,y=0.9575,width=0.11*1.5, height=0.11) # creates area for map	 					
		   pushViewport(vp)
		   print(mm, newpage=FALSE) # prints the map
		}
	    dev.off()
      }
	if(output == 'obj'){return(rfidsplot)}
}   

### END   	