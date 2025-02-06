
### !!! After interrupting work!!!
# save the backup of the reports
# define the last completely done nest
last_nest=""
which(file_list==file)# next is 
which(file_list=="")# next is 154
### problems

# last done nest: 
# tools
setwd("C:/Users/josem/Desktop/Nightjars/HM_Models_Sladecek")
source('tools.R')

# working directories
###!!! decide where to take the reports!
{
  general = "C:\\Users\\josem\\Desktop\\Nightjars\\HM_Models_Sladecek"
  orig_data='C:\\Users\\josem\\Desktop\\Nightjars\\Data\\to_extract' # where the data are placed
  comp_data='C:\\Users\\josem\\Desktop\\Nightjars\\HM_Models_Sladecek\\Data_all_computed'#probably together with the postprocessing
  plots='C:\\Users\\josem\\Desktop\\Nightjars\\HM_Models_Sladecek\\Actograms_all'
  report={} #Alternatively, pour one from the past???
  #load(paste(general,"report.RData",sep="/"))
}


# parameter settings
{
  ### Add a smoothing window to the first parameter
  # MSR, detailed desription of all parameters is provided in the "protocol_hmm" file
  msr_par=list(lgap_length=60,# To find incubation breaks, here established as longer as 5 min. 
               #A 5min period  is comprised of 60 5-sec intervals, so this is the reason to establish the threshold in 60.   
               lgap_drop=-5,#To establish the minimum threshold for considering a temperature drop in -5°C
               sbout_length=36,#to establish the minimum incubation length in 3 min 
               sgap_drop=-.7,# 1°C, to delete temperature drops between consecutive readings "> -1º" 
               sstop_max_kvant=.25,# 25% quartile, median is power)
               ib_max=0.1, #this 
               init_smw=10)
}

# list of .RData file for which the extraction will be done
file_list=list.files(orig_data)

 # prepare list of a priori known data issues
{
#  di=read.csv(file=paste(general,"data_issues.csv",sep="/"),
#              sep=";",
#              stringsAsFactors = F)  
#  extr_nests=substr(file_list,21,nchar(file_list)-6)
#  di=di[di$nest %in% extr_nests,]# only nests which will be extracted
#  # only issues with some time record
#  di$start_[nchar(di$start_)==10]= paste(di$start_[nchar(di$start_)==10],
#                                         "00:00",
#                                         sep=" ")
#  di$start_=as.POSIXct(di$start_,
#                         format="%d.%m.%Y %H:%M") 
#  di$end_[nchar(di$end_)==10]= paste(di$end_[nchar(di$end_)==10],
#                                     "00:00",
#                                     sep=" ")
#  di$end_=as.POSIXct(di$end_,
#                     format="%d.%m.%Y %H:%M") 
  
  #di=di[-which(is.na(di$start_)&is.na(di$end_)),]
#  length(unique(di$nest))
  
}

# incubation extraction
{
  #file=file_list[1]
  for (file in file_list[1:length(file_list)]) {
    
    #setwd(orig_data) #This line was in the original script sent by Martin Sladecek 
    setwd(orig_data)
    load(file)
    
    # prepare data and process
    #{
      # sort b by datetime
      b=b[order(b$datetime_),]
      # shift data with only tag_ID and not t_nest, one row up
     b$tag_ID=c(NA,b$tag_ID[1:(nrow(b)-1)])
     b$who=c(NA,b$who[1:(nrow(b)-1)])  
     b=b[!is.na(b$t_nest),]
      
      # add info from nests
      b$nest=substr(file,1,nchar(file)-6)
      # define what should be included to HMM and what should be included to plot
      # ni$end_nest=max(b$datetime[!is.na(b$who)])
      if(!is.null(ni$end_nest)){
        b$tohmm=ifelse(b$datetime_<=ni$end_nest,TRUE,FALSE)
        b$toplot=ifelse(b$datetime_<=ni$end_nest+3*24*3600,TRUE,FALSE)
      } else {
        b$tohmm=TRUE
        b$toplot=TRUE
      }
      
      # add info from visits
      b$visit=0
      b$visit_what=NA
      if(!is.null(vi) & nrow(vi)>0){
        for(i in 1:nrow(vi)){
          b$visit[b$datetime_>=vi$start_[i] & b$datetime_<=vi$end_[i]]=1  
          b$visit_what[b$datetime_>=vi$start_[i] & b$datetime_<=vi$end_[i]]=vi$what[i]
        }
        
      }
      # add info from issues
      nest_id=substr(file,21,nchar(file)-6)
#      di_=di[di$nest==nest_id,]
#      b$issue=0
#      b$issue_what=NA
#      if(!is.null(di_) & nrow(di_)>0){
#        for(i in 1:nrow(di_)){
#          if(is.na(di_$start_[i])){
#            di_$start_[i]=min(b$datetime_)
#          }
#          if(is.na(di_$end_[i])){
#            di_$end_[i]=max(b$datetime_)
#          }
#          
#          b$issue[b$datetime_>=di_$start_[i] & b$datetime_<=di_$end_[i]]=1  
#          b$issue_what[b$datetime_>=di_$start_[i] & b$datetime_<=di_$end_[i]]=di_$what[i]
#        }
#      }
      
      # separate datasets by logger type
      # "fast" loggers - 1/s-5s
      b_m=b[b$type %in% c("MSR","MSRin","DHT") & b$toplot==TRUE,]
      b_m_=b_m[b_m$tohmm==F,]
      b_m=b_m[!is.na(b_m$t_nest) & b_m$tohmm==T,]
      
    #}
    
    #msr process
      
      #the sentences below this line have been included by  Jose to solve errors
      #install.packages("dplyr")
      #install.packages("zoo")
      #install.packages("mvnfast")
      #library(dplyr)
      #library(data.table)
      #library(zoo)
      #library(mvnfast)
      #b <- b %>% mutate(t_nest_prior = c(t_nest[1], t_nest[-nrow(b)]))
      
    
      if(nrow(b_m)>0){
      # prepare model predictions
      #{
        
        # calculate reference inc
        b_m$inc_ref=inc_ref(b=b_m)
        # stacionarization
        #t
        b_m$tdfn=dfr(b_m$t_nest,i=1)
        # To remove any logger outages (then -46°C)
        b_m$tdfn[abs(b_m$tdfn)>10]=0
        b_m$tdfn[b_m$tdfn>0]=0
        b_m$tdfn=ma(b_m$tdfn,msr_par$init_smw)
        
        # long_break_stops
        b_m$tdfp=dfr(b_m$t_nest,i=1)
        b_m$tdfp[abs(b_m$tdfp)>10]=0
        b_m$tdfp[b_m$tdfp<0]=0
        b_m$tdfp=ma(b_m$tdfp,msr_par$init_smw)
        b_m$tdfp[1:5]=0
        # models
        # is it next necesarry?
        day=c(length(unique(as.Date(b_m$datetime_))):1)
        b_m$day_order=day[match(as.Date(b_m$datetime),unique(as.Date(b_m$datetime)))]
        
        # models
        #t
        b_m$init_inc=ifelse(b_m$tdfn<quantile(b_m$tdfn[b_m$tdfn!=0&b_m$tohmm==T],.25),0,1)
        m=hmmf(formula=as.formula(paste0("init_inc~","tdfn")),
               data=as.data.frame(b_m[b_m$tohmm==TRUE,]),loo=F,em=T,sequences="day_order",
               nrep=30,full.output=T,
               independent.seq=b_m$day_order[b_m$tohmm==TRUE],
               tolerance=2)
        b_m$hmm_t[b_m$tohmm==T]=m$predstate-1#
        
        # t_ends
        b_m$init_inc=ifelse(b_m$tdfp>quantile(b_m$tdfp[b_m$tdfp!=0],.25),0,1)
        m=hmmf(formula=as.formula(paste0("init_inc~","tdfp")),
               data=as.data.frame(b_m),loo=F,em=T,sequences="day_order",
               nrep=30,full.output=T,
               independent.seq=b_m$day_order,
               tolerance=2)
        b_m$hmm_tl=m$predstate-1#
        #save(b_m,file=paste(comp_data,file,sep="/"))
      #}  
      
      # postprocessing
      {
        b_m$hmm_comb=b_m$hmm_t
        # Delete gaps within RFID-identified incubations
        b_m$hmm_comb[b_m$who %in% c("m","f")]=1
        # Delete short incubation bouts
        b_m$tdfn_bout=bout_nr(b_m$hmm_t)
        b_m$tdfn_bl=bout_length(b_m$hmm_t)
        b_m$hmm_comb[b_m$hmm_comb==1 & b_m$tdfn_bl<msr_par$sbout_length]=0
        # Delete incubation breaks with negligible temperature decrease
        b_m$tdfn_bout=bout_nr(b_m$hmm_comb)
        b_m$tdfn_bl=bout_length(b_m$hmm_comb)
        b_m$tdfn_drop=bout_drop(b_m$tdfn,b_m$tdfn_bout)
        b_m$hmm_comb[b_m$hmm_comb==0 & b_m$tdfn_drop> msr_par$sgap_drop ]=1
        # Identify ends of long gaps by the tdfp
        b_m$tdfn_bout=bout_nr(b_m$hmm_comb)
        b_m$tdfn_bl=bout_length(b_m$hmm_comb)
        b_m$tdfn_drop=bout_drop(b_m$tdfn,b_m$tdfn_bout)
        b_m$tdfp_bout=bout_nr(b_m$hmm_tl)
        b_m$tdfp_bl=bout_length(b_m$hmm_tl)
        b_m$tdfp_max=bout_summ(b_m$tdfp,b_m$tdfp_bout,max)
        
        # tdfp bouts to be used
        b_m$hmm_tl[b_m$hmm_tl==0 & b_m$tdfp_max<quantile(b_m$tdfp_max[b_m$hmm_tl==0],msr_par$sstop_max_kvant)]=1
        # gaps to be used
        x=unique(b_m$tdfn_bout[b_m$hmm_comb==0 &
                                 b_m$tdfn_bl>msr_par$lgap_length &
                                 b_m$tdfn_drop <msr_par$lgap_drop])
        x1=which(b_m$hmm_tl==0)
        for (i in x) {
          start=min(which(b_m$tdfn_bout==i))
          end=min(min(x1[x1>start+50]),nrow(b_m))
          b_m$hmm_comb[start:end]=0
        }
        
        # identify significant temperature increase as an incubation
        b_m$hmm_comb[b_m$tdfp>msr_par$ib_max]=1
        
        # Delete gaps within RFID-identified incubations
        b_m$hmm_comb[b_m$who %in% c("m","f")]=1
      }
      
    }
    
    # bind, save and prepare report
    {
     if(nrow(b_m)>0){
        b_m_[,setdiff(names(b_m),names(b_m_))]=NA
        b=rbind(b_m,b_m_)  
      } 
      
      save(b,file=paste(comp_data,file,sep="/"))
      load(paste(comp_data,file,sep="/"))
      
      # These below codes have been included by Jose to avoid warning and errors. 
      #comp_data <- "Data_all_computed"
      #file <- "my_data.RData"  # Asegúrate de que 'file' tenga un nombre válido
      
      if (is.na(file)) {
        stop("El nombre del archivo es NA. Por favor, proporciona un nombre de archivo válido.")
      }
      
      if (!dir.exists(comp_data)) {
        dir.create(comp_data, recursive = TRUE)
      }
      
      save(b, file = paste(comp_data, file, sep = "/")) # End of Jose’s lines
      
      report_=data.frame(nest=file,
                         process=c("MSR"),
                         type=c(ifelse(nrow(b_m)>0,b_m$type[1],NA)),
                         length=c(nrow(b_m)),
                         lgap_length=c(msr_par$lgap_length),
                         lgap_drop=c(msr_par$lgap_drop),
                         sbout_length=c(msr_par$sbout_length),
                         ib_max=c(msr_par$ib_max),
                         sgap_drop=c(msr_par$sgap_drop),
                         sstop_max_kvant=c(msr_par$sstop_max_kvant),
                         init_smw=c(msr_par$init_smw),
                         datetime=Sys.time()
      )
      report=rbind(report,report_)
      save(report,file=paste(general,"report.RData",sep="/"))
      
    }
    
    if(nrow(b)>0){
      # visualization
      {
        
        setwd(plots)
        b$t_nest[1]=min(b$t_surface)
        b$t_surface[1]=max(b$t_nest)
        b$t_nest[2]=max(b$t_surface)
        b$t_surface[2]=min(b$t_nest)
        
        # pridat do nadpisu typ loggeru!
   #     actogram_unip_hmm(x=b,sex=NULL,datetime="datetime_",hmm_lines = T,
  #                        t.in="t_nest",t.out="t_surface",h.in="h_nest",
  #                        h.out="h_surface",l.in="l_nest",l.out="l_surface",
  #                        issues=NULL,issue_what=NULL,
  #                        hmm_inc="hmm_comb",ref_inc="hmm_t",
  #                        visits="visit",visit_what="visit_what",
  #                        col.t.in="red",col.t.in_gap = "yellow",col.t.out="orange",
  #                        col.h.in="dodgerblue4",col.h.out="lightblue",
  #                        col.l.in=NULL,col.l.out=NULL,col.hmm.inc="green", col.ref.inc="black",
  #                        lwd=.5,type="PNG",nest=paste("hmm_t",
  #                                                     substr(file,1,nchar(file)-6),
  #                                                     sep = "_"))
        
        
        b$sex=toupper(b$who)
        b$sex[b$sex=="DISTURB"]=NA
        # numeric time of day
        b$time_num=as.numeric(substr(b$datetime_,12,13))+
          as.numeric(substr(b$datetime_,15,16))/60+
          as.numeric(substr(b$datetime_,18,19))/3600
        actogram_rwl_hmm(x=b,sex=NULL,datetime="datetime_",hmm_lines = T,
                         t.in="t_nest",t.out="t_surface",h.in="h_nest",
                         h.out="h_surface",
                         hmm_inc="hmm_comb",ref_inc="hmm_t",
                         visits="visit",visit_what="visit_what",bouts="who",
                         col.t.in="red",col.t.in_gap = "yellow",col.t.out="orange",
                         col.h.in="dodgerblue4",col.h.out="lightblue",col.m="dodgerblue4", col.f="firebrick3",
                         col.l.in=NULL,col.l.out=NULL,col.hmm.inc="green", col.ref.inc="black",
                         lwd=.5,type="PNG",nest=b$nest[1],
                         device="new")
        
        
      }
      
    }
    print(c(file,as.character(Sys.time()),nrow(b)))
    rm(b,vi,ni,di_)
  }
}

# after interruption
save(report,file=paste(general,paste0("report_",Sys.Date(),".RData"),sep="/"))



#actogram_unip_hmm(x=b,sex=NULL,datetime="datetime_",hmm_lines = T,
#                  t.in="t_nest",t.out="t_surface",h.in="h_nest",
#                  h.out="h_surface",l.in="l_nest",l.out="l_surface",
#                  issues="issue",issue_what="issue_what",
#                  hmm_inc="hmm_comb",ref_inc="hmm_t",
#                  visits="visit",visit_what="visit_what",
#                  col.t.in="red",col.t.in_gap = "yellow",col.t.out="orange",
#                  col.h.in="dodgerblue4",col.h.out="lightblue",
#                  col.l.in=NULL,col.l.out=NULL,col.hmm.inc="green", col.ref.inc="black",
#                  lwd=.5,type="plot",device="new",nest=paste("hmm_t",
#                                               substr(file,1,nchar(file)-6),
#                                               sep = "_"))


#x=b[b$datetime_>"2022-07-05 00:00:00",
#    c(1,2,9,13,24,25,28,30)]
