# tools
{
  Sys.setenv(tz="UTC")
  #rm(list=ls())
  # load/install packages
  for (i in c("zoo","plyr","data.table","lme4","maptools","suncalc","openmeteo")){
    if(!i %in% installed.packages()[,"Package"])  
    { install.packages(i)}
    sapply(i, require, character.only = TRUE,quietly = TRUE)
  }
  # load functions
  bout_length=function(x){
    data=c(x,5)
    x=rep(NA,length(data))
    x=cumsum(c(0,abs(diff(data)))) 
    x1=cumsum(rep(1,times=length(data))) - 
      rep(c(0,which(!!abs(diff(data)))[-length(which(!!abs(diff(data))))]),
          times=c(which(!!abs(diff(data)))[1],diff(which(!!abs(diff(data))))))
    x=x[-length(x)];x1=x1[-length(x1)]
    tl=rep(as.numeric(tapply(x1,x,max)),
           times=as.numeric(tapply(x1,x,max)))
    return(tl)
  }
  
  bout_nr=function(x){
    data=c(x,5)
    x=rep(NA,length(data))
    if(!is.numeric(data)){
      data[is.na(data)]="NA"
      data=as.numeric(as.factor(data))
    }
    x=cumsum(c(0,abs(diff(data)))) 
    x=x[-length(x)]
    return(x)
  }
}


# load data
# individual nest-datasets with bouts and for each hour
{
  setwd("C:/Users/slava/OneDrive/Dokumenty/papery/nightjars/HM_Models_Sladecek/Data_cleaned/Final cleaned data/")
  
  file_list=list.files(pattern = "*.RData")
  
  dat_hour=NULL
  dat_bout=NULL
  dat_light=NULL
  
  # file=file_list[1]
  # datasets for nest attendance
  for(file in file_list){
    load(file)
    
    b=b[!is.na(b$prediction_final),]
    # create bout structure for final prediction
    b$bout_att=bout_nr(b$prediction_final)
    b$bout_length=bout_length(b$prediction_final)
    # numerical hours
    b$hr_num=as.numeric(strftime(b$datetime,format="%H"))
    b$hr_id=paste(b$nest,
                  trunc.POSIXt(b$datetime,units="hours"))
    
    
    # delete final gap (as it is after nest ending)
    if(b$prediction_final[nrow(b)]==0){
      b=b[-which(b$bout_att==b$bout_att[nrow(b)]),]
    }
    # delete bouts defined as visit (maybe rather delete hours with visit?)
    #b=b[b$visit!=1,]  
    xtabs(~b$sex)
    # assign whole bouts with RFID record to the corresponding sex
    {
      for (i in unique(b$bout_att[b$prediction_final==1])){
        if(length(unique(na.omit(b$sex[b$bout_att==i])))==1){
          b$sex[b$bout_att==i]=unique(na.omit(b$sex[b$bout_att==i]))[1]
        } else if (length(unique(na.omit(b$sex[b$bout_att==i])))==2){
          b$sex[b$bout_att==i][1]=na.omit(b$sex[b$bout_att==i])[1]
          j=2
          first_sex=na.omit(b$sex[b$bout_att==i])[1]
          while(j<length(b$sex[b$bout_att==i])){
            x=which(!is.na(b$sex[b$bout_att==i]) & 
                                 b$sex[b$bout_att==i]!=first_sex)
            if(max(x)<j){
            next_pos=length(b$sex[b$bout_att==i])
            } else {
            next_pos=min(x[x>j])
            }
            b$sex[b$bout_att==i][j:(next_pos-1)]=first_sex
            j=next_pos
            first_sex=b$sex[b$bout_att==i][next_pos]
          }
        } else {
          b$sex[b$bout_att==i]="U"
        }
      }
  
      b$prediction_final[!is.na(b$sex) & b$prediction_final==0]=1
      
    }
    
    
    # add sunlight/moonlight
    {
    # sunlight position
    b$sunlight_pos=getSunlightPosition(b$datetime_,37.88,-1.28)$altitude*(180/pi)
    b$sunlight_state=ifelse(b$sunlight_pos>=0,"daylight",
                            ifelse(b$sunlight_pos>-12,"twilight","night"))# TO DECIDE WHICH TWILIGHT TO USE
    # add moonlight
    # moonlight position 
    b$moonlight_pos=getMoonPosition(b$datetime_,37.88,-1.28)$altitude*(180/pi)
    # moonlight illumination
    b$moonlight_illum=getMoonIllumination(as.Date(b$datetime_))$fraction
    # cloudinness
    
    # weather_variables() # list of variables to choose from
    x=weather_history(location = c(37.88,-1.28),
                      start = as.Date(min(b$datetime_)),
                      end = as.Date(max(b$datetime_)),
                      hourly = "cloud_cover")
    
    b$cloudiness=x$hourly_cloud_cover[match(round.POSIXt(b$datetime,"hours"),x$datetime)]
    # for final moonlight metric 3 above metrics should be somehow combined
    
    }
    
    
    # aggregation by hours
    bh=b[,# here might be fittered something out, e.g. visits?
         .(start=min(datetime_),
           hr_num=hr_num[1],
           length_record=as.numeric(max(datetime_)-min(datetime_)),
           nest_att=mean(prediction_final),
           att_m=length(na.omit(sex[sex=="M"])),
           att_f=length(na.omit(sex[sex=="F"])),
           att_u=length(na.omit(sex[sex=="U"])),
           amb_temp=mean(t_surface[!is.na(t_surface)]),
           nest_temp=mean(t_nest[!is.na(t_nest)]),
           amb_hum=mean(h_surface[!is.na(h_surface)]),
           nest_hum=mean(h_nest[!is.na(h_nest)]),
           visit=mean(visit),
           night_prop=length(sunlight_state[sunlight_state=="night"])/length(sunlight_state),
           twilight_prop=length(sunlight_state[sunlight_state=="twilight"])/length(sunlight_state),
           daylight_prop=length(sunlight_state[sunlight_state=="daylight"])/length(sunlight_state),
           moonlight_pos=mean(moonlight_pos),
           moonlight_illum=mean(moonlight_illum),
           cloudiness=mean(cloudiness)),
         by=.(hr_id,nest)]
    dat_hour=rbind(dat_hour,bh)
    
    # aggregation by bouts
    bb=b[,# here might be fittered something out, e.g. visits?
         .(start=min(datetime_),
           bout_length=as.numeric(max(datetime_)-min(datetime_)),
           bout_what=prediction_final[1],
           who=sex[1],
           amb_hum=mean(h_surface[!is.na(h_surface)]),
           nest_hum=mean(h_nest[!is.na(h_nest)]),
           amb_temp=mean(t_surface[!is.na(t_surface)]),
           nest_temp=mean(t_nest[!is.na(t_nest)]),
           night_prop=length(sunlight_state[sunlight_state=="night"])/length(sunlight_state),
           twilight_prop=length(sunlight_state[sunlight_state=="twilight"])/length(sunlight_state),
           daylight_prop=length(sunlight_state[sunlight_state=="daylight"])/length(sunlight_state),
           moonlight_pos=mean(moonlight_pos),
           moonlight_illum=mean(moonlight_illum),
           cloudiness=mean(cloudiness),
           daylight_start=sunlight_state[1]
           ),
         by=.(bout_att,nest)]
    dat_bout=rbind(dat_bout,bb)
    # aggregation by "sunlight status"
    bs=b[,# here might be fittered something out, e.g. visits?
         .(start=min(datetime_),
           hr_num=hr_num[1],
           length_record=length(datetime_)*5/3600,
           nest_att=mean(prediction_final),
           att_m=length(na.omit(sex[sex=="M"])),
           att_f=length(na.omit(sex[sex=="F"])),
           att_u=length(na.omit(sex[sex=="U"]))),
         by=.(sunlight_state,nest)]
    dat_light=rbind(dat_light,bs)
    
    print(paste(file,Sys.time()))
   save(dat_hour,dat_bout,dat_light,file="data_nightjars_aggregated.RData") 
  }
  
}
