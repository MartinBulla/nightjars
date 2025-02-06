### NEW VERSION OF THE BOTH FIX. INCUB FUNCTION AND OVERALL SUGGESTED PROCESS!!!
# tools

{
  Sys.setenv(tz="UTC")
  
  #Before run this below code, set working directory in "tools.R". 
  setwd("C:/Users/josem/Desktop/Nightjars/HM_Models_Sladecek")
  source('tools.R')
  # fix.incub function - the same as in manual_edit.R
  # later (after debugging etc. will be moved to tools.R)
  
  fix.incub=function(x,nest,start_=min(x$datetime_),end_=max(x$datetime_),msr_par=msr_par,ttn_par=ttn_par){
    # from actogram_plotRWL, but here needed as well
    Sys.setenv(TZ="UTC")
    options(warn = -1)
    # define where change occurred
    x$change=0
    # add a day order
    day=c(length(unique(as.Date(x$datetime))):1)
    x$day_order=day[match(as.Date(x$datetime),unique(as.Date(x$datetime)))]
    x$time_num=as.numeric(x$datetime- 
                            as.POSIXct(trunc.POSIXt(x$datetime,"days")))/3600
    # final prediction
    x$prediction_final=x$hmm_comb
    
    # is the data quality good?
    x$include_data=T
    # initial visualization 
#    actogram_unip_hmm(x=x,sex=NULL,datetime="datetime_",hmm_lines = T,
#                      t.in="t_nest",t.out="t_surface",h.in="h_nest",
#                      h.out="h_surface",l.in="l_nest",l.out="l_surface",
#                      issues="issue",issue_what="issue_what",
#                      hmm_inc="prediction_final",ref_inc="hmm_t",changes="change",
#                      visits="visit",visit_what="visit_what",
#                      col.t.in="red",col.t.in_gap = "yellow",col.t.out="orange",
#                      col.h.in="dodgerblue4",col.h.out="lightblue",
#                      col.l.in=NULL,col.l.out=NULL,col.hmm.inc="green", col.ref.inc="black",
#                      lwd=1,type="plot",device="new",nest=nest)
    
    # SECTION - What do you want to do? In Czech, co chces? 
    fixed_list={}# object to store the list of changes
    fixed_cp={}# 
    zoomed=F
    repeat {
      wish=readline("Co chces?:")
      if(wish=="z"){
        print ("Select the area for zooming")
        loc=locator(2)
        start_=as.POSIXct(as.Date(x$datetime[x$day_order==trunc(loc$y[1])][1]))+trunc(loc$x[1]*3600)
        end_=as.POSIXct(as.Date(x$datetime[x$day_order==trunc(loc$y[2])][1]))+trunc(loc$x[2]*3600)
        print(c(start_,end_))  
        x2=x[x$datetime_>=start_ & x$datetime_<=end_,]
        actogram_unip_zoom(x=x2,sex=NULL,datetime="datetime_",hmm_lines = T,
                           t.in="t_nest",t.out="t_surface",h.in="h_nest",
                           h.out="h_surface",l.in="l_nest",l.out="l_surface",
                           hmm_inc="prediction_final",ref_inc="hmm_t",changes="change",
                           visits="visit",visit_what="visit_what",
                           col.t.in="red",col.t.in_gap = "yellow",col.t.out="orange",
                           col.h.in="dodgerblue4",col.h.out="lightblue",
                           col.l.in=NULL,col.l.out=NULL,col.hmm.inc="green",col.ref.inc="black",
                           lwd=2,type="plot",nest=nest)
        zoomed=T
      } # zoom the plot
      else if(wish=="f"){
        repeat{
        problem=readline("What is the problem?:")
          
        if(problem=="s"){
          
        print("select area for the edit") 
        loc=locator(1,type="p",pch=3)
        
        if(zoomed==T){
          end_=as.POSIXct(as.Date(x$datetime[x$day_order==x2$day_order][1]))+trunc(loc$x[2]*3600)
        } else {
          end_=as.POSIXct(as.Date(x$datetime[x$day_order==trunc(loc$y[2])][1]))+trunc(loc$x[2]*3600)
        }
        
          x$prediction_final[x$datetime_<=end_]=0# set start
          x$change[x$datetime_<=end_]=1
          
          fixed_list=rbind.data.frame(fixed_list,
                                      data.frame(nest=nest,
                                                 from=NA,
                                                 to=end_,
                                                 problem=problem,
                                                 timestamp=Sys.time()))
        } # define start of the record
        else if(problem=="e"){
          
        print("select area for the edit") 
        loc=locator(1,type="p",pch=3)
        
        if(zoomed==T){
          start_=as.POSIXct(as.Date(x2$datetime[x2$day_order==x2$day_order][1]))+trunc(loc$x[1]*3600)
        } else {
          start_=as.POSIXct(as.Date(x$datetime[x$day_order==trunc(loc$y[1])][1]))+trunc(loc$x[1]*3600)
        }
        
          x$prediction_final[x$datetime_>=start_]=0# set end
          x$change[x$datetime_>=start_]=1
          
          fixed_list=rbind.data.frame(fixed_list,
                                      data.frame(nest=nest,
                                                 from=start_,
                                                 to=NA,
                                                 problem=problem,
                                                 timestamp=Sys.time()))
        } # define end of the record
        else if(problem=="g"){
          repeat{
          loc=locator(2,type="p",pch=3)
          if(zoomed==T){
            start_=as.POSIXct(as.Date(x2$datetime[x2$day_order==x2$day_order][1]))+trunc(loc$x[1]*3600)
            end_=as.POSIXct(as.Date(x$datetime[x$day_order==x2$day_order][1]))+trunc(loc$x[2]*3600)
            if(any(loc$x<x2$time_num[x2$day_order==x2$day_order][1])){
              break
            }
          } else {
            start_=as.POSIXct(as.Date(x$datetime[x$day_order==trunc(loc$y[1])][1]))+trunc(loc$x[1]*3600)
            end_=as.POSIXct(as.Date(x$datetime[x$day_order==trunc(loc$y[2])][1]))+trunc(loc$x[2]*3600)
            if(any(loc$x<0)){
              break
            }
          }
          x$prediction_final[x$datetime_>=start_&x$datetime_<=end_]=0# set gap
          x$change[x$datetime_>=start_&x$datetime_<=end_]=1
          
          fixed_list=rbind.data.frame(fixed_list,
                                      data.frame(nest=nest,
                                                 from=start_,
                                                 to=end_,
                                                 problem=problem,
                                                 timestamp=Sys.time()))
          }
          
        } # define gap manually
        else if(problem=="i"){
          repeat{
            loc=locator(2,type="p",pch=3)
            if(zoomed==T){
              start_=as.POSIXct(as.Date(x2$datetime[x2$day_order==x2$day_order][1]))+trunc(loc$x[1]*3600)
              end_=as.POSIXct(as.Date(x$datetime[x$day_order==x2$day_order][1]))+trunc(loc$x[2]*3600)
            if(any(loc$x<x2$time_num[x2$day_order==x2$day_order][1])){
              break
            }
            } else {
              start_=as.POSIXct(as.Date(x$datetime[x$day_order==trunc(loc$y[1])][1]))+trunc(loc$x[1]*3600)
              end_=as.POSIXct(as.Date(x$datetime[x$day_order==trunc(loc$y[2])][1]))+trunc(loc$x[2]*3600)
            if(any(loc$x<0)){
              break
            }
            }
           x$prediction_final[x$datetime_>=start_&x$datetime_<=end_]=1# set incubation
           x$change[x$datetime_>=start_&x$datetime_<=end_]=1
           
           fixed_list=rbind.data.frame(fixed_list,
                                       data.frame(nest=nest,
                                                  from=start_,
                                                  to=end_,
                                                  problem=problem,
                                                  timestamp=Sys.time()))
          }
        } # define bout manually 
        else if(problem=="p"){
          repeat{
            loc=locator(2,type="p",pch=3)
            if(zoomed==T){
              start_=as.POSIXct(as.Date(x2$datetime[x2$day_order==x2$day_order][1]))+trunc(loc$x[1]*3600)
              end_=as.POSIXct(as.Date(x$datetime[x$day_order==x2$day_order][1]))+trunc(loc$x[2]*3600)
              if(any(loc$x<x2$time_num[x2$day_order==x2$day_order][1])){
                break
              }
            } else {
              start_=as.POSIXct(as.Date(x$datetime[x$day_order==trunc(loc$y[1])][1]))+trunc(loc$x[1]*3600)
              end_=as.POSIXct(as.Date(x$datetime[x$day_order==trunc(loc$y[2])][1]))+trunc(loc$x[2]*3600)
              if(any(loc$x<0)){
                break
              }
            }
            x$prediction_final[x$datetime_>=start_&x$datetime_<=end_]= x$hmm_t[x$datetime_>=start_&x$datetime_<=end_]
            x$change[x$datetime_>=start_&x$datetime_<=end_]= 1
            
            fixed_list=rbind.data.frame(fixed_list,
                                        data.frame(nest=nest,
                                                   from=start_,
                                                   to=end_,
                                                   problem=problem,
                                                   timestamp=Sys.time()))
          }  
        } # return to the raw extraction without postprocessing
        else if(problem=="d"){
          repeat{
            loc=locator(2,type="p",pch=3)
            if(zoomed==T){
              start_=as.POSIXct(as.Date(x2$datetime[x2$day_order==x2$day_order][1]))+trunc(loc$x[1]*3600)
              end_=as.POSIXct(as.Date(x$datetime[x$day_order==x2$day_order][1]))+trunc(loc$x[2]*3600)
              if(any(loc$x<x2$time_num[x2$day_order==x2$day_order][1])){
                break
              }
            } else {
              start_=as.POSIXct(as.Date(x$datetime[x$day_order==trunc(loc$y[1])][1]))+trunc(loc$x[1]*3600)
              end_=as.POSIXct(as.Date(x$datetime[x$day_order==trunc(loc$y[2])][1]))+trunc(loc$x[2]*3600)
              if(any(loc$x<0)){
                break
              }
            }
            x$include_data[x$datetime_>=start_&x$datetime_<=end_]=F
            x$change[x$datetime_>=start_&x$datetime_<=end_]= 1
            
            fixed_list=rbind.data.frame(fixed_list,
                                        data.frame(nest=nest,
                                                   from=start_,
                                                   to=end_,
                                                   problem=problem,
                                                   timestamp=Sys.time()))
          }  
        } # delete part as non-usable
        else if(problem=="h"){
          # selects parts for refitting
          pp=NULL
          repeat{
            loc=locator(2,type="p",pch=3)
            if(zoomed==T){
              start_=as.POSIXct(as.Date(x2$datetime[x2$day_order==x2$day_order][1]))+trunc(loc$x[1]*3600)
              end_=as.POSIXct(as.Date(x$datetime[x$day_order==x2$day_order][1]))+trunc(loc$x[2]*3600)
            } else {
              start_=as.POSIXct(as.Date(x$datetime[x$day_order==trunc(loc$y[1])][1]))+trunc(loc$x[1]*3600)
              end_=as.POSIXct(as.Date(x$datetime[x$day_order==trunc(loc$y[2])][1]))+trunc(loc$x[2]*3600)
            }
            
            if(any(loc$x<0)){
              break
            }
            else {
              pp=rbind.data.frame(pp,
                                  data.frame(start_=start_,
                                             end_=end_))
            }
          } 
  
          # refit the HMM separately for each selected part
            if(!is.null(pp)){
            print("Waiting for the HMM fitting...")
            for(p in 1:nrow(pp)){
            b_=x[x$datetime_>=pp$start_[p]&x$datetime_<=pp$end_[p],]
            xx=extract.incub(b=b_,msr_par=msr_par,ttn_par=ttn_par,cponly=F)$b
            
            x$hmm_t[x$datetime_>=pp$start_[p]&x$datetime_<=pp$end_[p]]=xx$hmm_t
            x$hmm_comb[x$datetime_>=pp$start_[p]&x$datetime_<=pp$end_[p]]=xx$hmm_comb
            x$prediction_final[x$datetime_>=pp$start_[p]&x$datetime_<=pp$end_[p]]=xx$hmm_comb
            x$change[x$datetime_>=pp$start_[p]&x$datetime_<=pp$end_[p]]= 1
            
            #report=rbind(report,xx$report)???
           
            fixed_list=rbind.data.frame(fixed_list,
                                      data.frame(nest=nest,
                                                 from=start_,
                                                 to=end_,
                                                 problem="refitted_hmm",
                                                 timestamp=Sys.time()))
            print(paste0(p,"'th part was fitted"))
            }
          
            # refresh the plot
            zoomed=F
            
           
            actogram_rwl_hmm(x=x,sex=NULL,datetime="datetime_",hmm_lines = T,
                             t.in="t_nest",t.out="t_surface",h.in="h_nest",
                             h.out="h_surface",
                             hmm_inc="prediction_final",ref_inc="hmm_t",
                             visits="visit",visit_what="visit_what",bouts="who",
                             col.t.in="red",col.t.in_gap = "yellow",col.t.out="orange",
                             col.h.in="dodgerblue4",col.h.out="lightblue",col.m="dodgerblue4", col.f="firebrick3",
                             col.l.in=NULL,col.l.out=NULL,col.hmm.inc="green", col.ref.inc="black",
                             lwd=.5,type="plot",nest=b$nest[1],
                             device="new")
            
            
            }
          
        } # refit the hmm only on selected part
        else if (problem=="cp"){ # so far only 1 time selection per 1 "cp" choice 
          
          # select range for postprocessing adjustments
          loc=locator(2,type="p",pch=3)
          if(zoomed==T){
            start_=as.POSIXct(as.Date(x2$datetime[x2$day_order==x2$day_order][1]))+trunc(loc$x[1]*3600)
            end_=as.POSIXct(as.Date(x$datetime[x$day_order==x2$day_order][1]))+trunc(loc$x[2]*3600)
          } 
          else {
            start_=as.POSIXct(as.Date(x$datetime[x$day_order==trunc(loc$y[1])][1]))+trunc(loc$x[1]*3600)
            end_=as.POSIXct(as.Date(x$datetime[x$day_order==trunc(loc$y[2])][1]))+trunc(loc$x[2]*3600)
          }
          if(any(loc$x<0)){
            next
          }
          
          # prepare data for changes and parameters
          b_=x[x$datetime_>=start_&x$datetime_<=end_,]
          type=ifelse(b_$type[1] %in% c("MSR","MSRin","DHT"),"msr","ttn")
          if(type=="msr"){pars=msr_par}else{pars=ttn_par}
          
          # iteratively search for the optimum settings
          repeat{
          # plot only the selected range
            actogram_rwl_hmm(x=x,sex=NULL,datetime="datetime_",hmm_lines = T,
                             t.in="t_nest",t.out="t_surface",h.in="h_nest",
                             h.out="h_surface",
                             hmm_inc="prediction_final",ref_inc="hmm_t",
                             visits="visit",visit_what="visit_what",bouts="who",
                             col.t.in="red",col.t.in_gap = "yellow",col.t.out="orange",
                             col.h.in="dodgerblue4",col.h.out="lightblue",col.m="dodgerblue4", col.f="firebrick3",
                             col.l.in=NULL,col.l.out=NULL,col.hmm.inc="green", col.ref.inc="black",
                             lwd=.5,type="plot",nest=b$nest[1],
                             device="new")
            
            
            print(data.frame(par=1:length(pars),
                             val=unlist(pars)
            ))
            wish=readline("Co chces?:")
            if(wish=="o"){# OK, move on 
              # prepare report
              changed=T
              break
            }# OK, move on
            else if(wish=="d"){
              changed=F
              break
              }# discard
            else if(nchar(wish)>2){# if needed, write more parameters separated by the ";"
              # change pars as suggested
              pp=unlist(strsplit(wish,";"))
              for(p in 1:length(pp)){
              pars[[as.numeric(substr(pp[p],1,1))]]=as.numeric(substr(pp[p],3,nchar(pp[p])))
              }
              
              # rerun the postprocessing with suggested pars
              if(type=="msr"){
                xx=extract.incub(b=b_,msr_par=pars,ttn_par=ttn_par,cponly=T)
              } 
              else {
                xx=extract.incub(b=b_,msr_par=msr_par,ttn_par=pars,cponly=T)
              }
              
              b_=x$b
              changed=T
             
            }
          }
          # save the report and refresh the plot
          if(changed==T){
            xxx=xx$b
            
            x$hmm_comb[x$datetime_>=start_&x$datetime_<=end_]=xxx$hmm_comb
            x$prediction_final[x$datetime_>=start_&x$datetime_<=end_]=xxx$hmm_comb
            x$change[x$datetime_>=start_&x$datetime_<=end_]= 1
            
            fixed_cp_=xx$report[xx$report$length>0,]
            fixed_cp_$from=start_
            fixed_cp_$to=end_
           
            #report=rbind(report,xx$report)???
            fixed_cp=rbind.data.frame(fixed_cp,
                                      fixed_cp_)    
            
            fixed_list=rbind.data.frame(fixed_list,
                                        data.frame(nest=nest,
                                                   from=start_,
                                                   to=end_,
                                                   problem="Changed postprocessing",
                                                   timestamp=Sys.time()))    
       
            # refresh the plot
            zoomed=F
          
            actogram_rwl_hmm(x=x,sex=NULL,datetime="datetime_",hmm_lines = T,
                             t.in="t_nest",t.out="t_surface",h.in="h_nest",
                             h.out="h_surface",
                             hmm_inc="prediction_final",ref_inc="hmm_t",
                             visits="visit",visit_what="visit_what",bouts="who",
                             col.t.in="red",col.t.in_gap = "yellow",col.t.out="orange",
                             col.h.in="dodgerblue4",col.h.out="lightblue",col.m="dodgerblue4", col.f="firebrick3",
                             col.l.in=NULL,col.l.out=NULL,col.hmm.inc="green", col.ref.inc="black",
                             lwd=.5,type="plot",nest=b$nest[1],
                             device="new")
            
            
          }
        } # change postprocessing
        else if (problem=="v"){
          zoomed=F
         
          actogram_rwl_hmm(x=x,sex=NULL,datetime="datetime_",hmm_lines = T,
                           t.in="t_nest",t.out="t_surface",h.in="h_nest",
                           h.out="h_surface",
                           hmm_inc="prediction_final",ref_inc="hmm_t",
                           visits="visit",visit_what="visit_what",bouts="who",
                           col.t.in="red",col.t.in_gap = "yellow",col.t.out="orange",
                           col.h.in="dodgerblue4",col.h.out="lightblue",col.m="dodgerblue4", col.f="firebrick3",
                           col.l.in=NULL,col.l.out=NULL,col.hmm.inc="green", col.ref.inc="black",
                           lwd=.5,type="plot",nest=b$nest[1],
                           device="current")
          
          
        }
        else if (problem=="b"){
          start_=fixed_list$from[nrow(fixed_list)]
          end_=fixed_list$to[nrow(fixed_list)]
          x$prediction_final[x$datetime_>=start_ & x$datetime_<=end_]=x$hmm_comb[x$datetime_>=start_ & x$datetime_<=end_]
          x$change[x$datetime_>=start_ & x$datetime_<=end_]=0
          fixed_list=fixed_list[-nrow(fixed_list),]
        }
        else if(problem=="x"){
          break 
        } # break out of the fixing loop
        else {
          print("problem must be one of s,e,g,i,p,d")
          next
        } # return due to the nonsense problem choice
  
      }  
        } # fix some particular problems
      else if (wish=="v") {
        zoomed=F
        
        actogram_rwl_hmm(x=x,sex=NULL,datetime="datetime_",hmm_lines = T,
                         t.in="t_nest",t.out="t_surface",h.in="h_nest",
                         h.out="h_surface",
                         hmm_inc="prediction_final",ref_inc="hmm_t",
                         visits="visit",visit_what="visit_what",bouts="who",
                         col.t.in="red",col.t.in_gap = "yellow",col.t.out="orange",
                         col.h.in="dodgerblue4",col.h.out="lightblue",col.m="dodgerblue4", col.f="firebrick3",
                         col.l.in=NULL,col.l.out=NULL,col.hmm.inc="green", col.ref.inc="black",
                         lwd=.5,type="plot",nest=b$nest[1],
                         device="current")
      } # visualize the plot after fixes
      else if (wish=="b"){
        start_=fixed_list$from[nrow(fixed_list)]
        end_=fixed_list$to[nrow(fixed_list)]
        x$prediction_final[x$datetime_>=start_ & x$datetime_<=end_]=x$hmm_comb[x$datetime_>=start_ & x$datetime_<=end_]
        x$change[x$datetime_>=start_ & x$datetime_<=end_]=0
        fixed_list=fixed_list[-nrow(fixed_list),]
      } # retract the last made change
      else if (wish=="r"){
        x$include_data=F
        fixed_list=data.frame(nest=nest,
                              from=min(x$datetime),
                              to=max(x$datetime),
                              problem="remove",
                              timestamp=Sys.time())
        break
      } # remove whole dataset
      else if (wish=="s"){
        break
      } # break and save corrected dataset
      else {
        print("wish must be one of z,f,b,r,s,cp")
        next
      }
    }
    
    if(length(fixed_list)==0){# if there were no problems to be solved
      fixed_list=data.frame(nest=nest,
                            from=min(x$datetime),
                            to=max(x$datetime),
                            problem="OK",
                            timestamp=Sys.time())
    }
    return(list(data=x,
                fixed_problems=fixed_list,
                fixed_cp=fixed_cp))
  }
  
  # Function to run the HMM process
  # set cponly=T, if you wish to proceed only the postprocessing (i.e. not the HMM extraction)
  extract.incub=function(b=b_,msr_par=msr_par,ttn_par=ttn_par,cponly=F){
    
    # separate datasets by logger type
    {
      # "fast" loggers - 1/s-5s
      b_m=b[b$type %in% c("MSR","MSRin","DHT") & b$toplot==TRUE,]
      b_m_=b_m[b_m$tohmm==F,]
      b_m=b_m[!is.na(b_m$t_nest) & b_m$tohmm==T,]
      #"slow" loggers - 1/min-2min
      b_t=b[b$type %in% c("TTn","HOBO") & b$toplot==TRUE,]
      b_t_=b_t[b_t$tohmm==F,]
      b_t=b_t[!is.na(b_t$t_nest) & b_t$tohmm==T,]
    }
    
    #msr proces
    if(nrow(b_m)>0){
      if(cponly==F){
        # prepare model predictions
        {
          
          # calculate reference inc
          b_m$inc_ref=inc_ref(b=b_m)
          # stacionarization
          #t
          b_m$tdfn=dfr(b_m$t_nest,i=1)
          # odstranit pripadny vypadky loggeru (pak -46°C)
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
        }  
      }
      
      # postprocessing
      {
        b_m$hmm_comb=b_m$hmm_t
        b_m$hmm_tl_=b_m$hmm_tl
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
        ###!!! prejmenovat!!!
        b_m$hmm_tl_[b_m$hmm_tl_==0 & b_m$tdfp_max<quantile(b_m$tdfp_max[b_m$hmm_tl_==0],msr_par$sstop_max_kvant)]=1
        # gaps to be used
        x=unique(b_m$tdfn_bout[b_m$hmm_comb==0 &
                                 b_m$tdfn_bl>msr_par$lgap_length &
                                 b_m$tdfn_drop <msr_par$lgap_drop])
        x1=which(b_m$hmm_tl_==0)
        for (i in x) {
          start=min(which(b_m$tdfn_bout==i))
          end=min(min(x1[x1>start+50]),nrow(b_m))
          b_m$hmm_comb[start:end]=0
        }
        
        # identify significant temperature increase as an incubation
        b_m$hmm_comb[b_m$tdfp>msr_par$ib_max]=1
        
      }
      
    }
    
    #ttn proces
    if(nrow(b_t)>0){
      if(cponly==F){
        # prepare model prediction
        {
          b_t=b_t[!is.na(b_t$t_nest) & b_t$tohmm==T,]
          
          # calculate reference inc
          b_t$inc_ref=inc_ref(b=b_t,roll_t_int =600 ,roll_d_int = 3)
          b_t$inc_ref[is.na(b_t$inc_ref)]=1
          # stacionarization
          #t
          b_t$tdfr=dfr(b_t$t_nest,i=1)
          b_t$tdfr[b_t$tdfr>0]=0
          b_t$tdfr=ma(b_t$tdfr,ttn_par$init_smw)
          
          b_t$tdfr=c(rep(0,2),b_t$tdfr[1:(nrow(b_t)-2)])
          b_t$tdfr[1:5]=0
          # long_break_stops
          b_t$tlbs=dfr(b_t$t_nest)
          b_t$tlbs=ma(b_t$tlbs,ttn_par$init_smw)
          b_t$tlbs=c(b_t$tlbs[1:(nrow(b_t)-1)],rep(0,1))
          b_t$tlbs[b_t$tlbs<0]=0
          #b_t$tlbs[b_t$tlbs>1]=1
          b_t$tlbs[1:5]=0
          
          # models
          # is it next necesarry?
          day=c(length(unique(as.Date(b_t$datetime_))):1)
          b_t$day_order=day[match(as.Date(b_t$datetime),unique(as.Date(b_t$datetime)))]
          
          # models
          #t
          b_t$init_inc=ifelse(b_t$tdfr<quantile(b_t$tdfr[b_t$tdfr!=0],.75),0,1)
          m=hmmf(formula=as.formula(paste0("init_inc~","tdfr")),
                 data=as.data.frame(b_t),loo=F,em=T,sequences="day_order",
                 nrep=30,full.output=T,
                 independent.seq=NULL,
                 tolerance=2)
          b_t$hmm_t=m$predstate-1#
          
          
          # t_ends
          b_t$init_inc=ifelse(b_t$tlbs>quantile(b_t$tlbs[b_t$tlbs!=0],.25),0,1)
          m=hmmf(formula=as.formula(paste0("init_inc~","tlbs")),
                 data=as.data.frame(b_t),loo=F,em=T,sequences="day_order",
                 nrep=30,full.output=T,
                 independent.seq=NULL,
                 tolerance=2)
          b_t$hmm_tl=m$predstate-1#
          
        }
      }
      
      # postprocessing
      {
        b_t$hmm_comb=b_t$hmm_t
        
        # kratky bouty
        b_t$tdfr_bout=bout_nr(b_t$hmm_t)
        b_t$tdfr_bl=bout_length(b_t$hmm_t)
        b_t$hmm_comb[b_t$hmm_comb==1 & b_t$tdfr_bl<ttn_par$sbout_length]=0
        # gapy s malym poklesem
        b_t$tdfr_bout=bout_nr(b_t$hmm_comb)
        b_t$tdfr_bl=bout_length(b_t$hmm_comb)
        b_t$tdfr_drop=bout_drop(b_t$tdfr,b_t$tdfr_bout)
        b_t$hmm_comb[b_t$hmm_comb==0 & b_t$tdfr_drop> ttn_par$sgap_drop ]=1
        # zarazky
        
        b_t$tdfr_bout=bout_nr(b_t$hmm_comb)
        b_t$tdfr_bl=bout_length(b_t$hmm_comb)
        b_t$tdfr_drop=bout_drop(b_t$tdfr,b_t$tdfr_bout)
        
        # b_t$tlbs_bout=bout_nr(b_t$hmm_tl)
        #  b_t$tlbs_bl=bout_length(b_t$hmm_tl)
        #  b_t$tlbs_max=bout_summ(b_t$tlbs,b_t$tlbs_bout,max)
        #  b_t$hmm_tl[b_t$hmm_tl==0 & b_t$tlbs_max<quantile(b_t$tlbs_max[b_t$hmm_tl==0],ttn_par$sstop_max_kvant)]=1
        x=unique(b_t$tdfr_bout[b_t$hmm_comb==0 &
                                 b_t$tdfr_bl>ttn_par$lgap_length&
                                 b_t$tdfr_drop <ttn_par$lgap_drop])
        x1=which(b_t$tlbs>quantile(b_t$tlbs[b_t$tlbs>0],ttn_par$sstop_max_kvant))
        for (i in x) {
          start=min(which(b_t$tdfr_bout==i))
          end=min(min(x1[x1>start]),nrow(b_t))
          b_t$hmm_comb[start:end]=0
        }
        
        # velkej prirust jako inkubace
        b_t$hmm_comb[b_t$tlbs>ttn_par$ib_max]=1
      }
    }
    
    # bind, save and prepare report
    {
      if(nrow(b_m)>0 & nrow(b_t)>0){
        b_t[,setdiff(names(b_m),names(b_t))]=NA
        b_m[,setdiff(names(b_t),names(b_m))]=NA
        b_m_[,setdiff(names(b_m),names(b_m_))]=NA
        b_t_[,setdiff(names(b_t),names(b_t_))]=NA
        b_t$tdfn=b_t$tdfr# vyresit systemoveji! - vsechny potencialni vsude a naopak zbytecnosti vyrezat vsude...
        b_t$tdfp=NA
        b=rbind(b_m,b_m_,
                b_t,b_t_)### zkontrolovat!!!
      } else if ((nrow(b_m) + nrow(b_m_))>0){
        b_m_[,setdiff(names(b_m),names(b_m_))]=NA
        b=rbind(b_m,b_m_)  
      } else {
        b_t_[,setdiff(names(b_t),names(b_t_))]=NA
        b=rbind(b_t,b_t_)  
      }
      
      report=data.frame(nest=file,
                        process=c("MSR","TTn"),
                        type=c(ifelse(nrow(b_m)>0,b_m$type[1],NA),
                               ifelse(nrow(b_t)>0,b_t$type[1],NA)),
                        length=c(nrow(b_m),nrow(b_t)),
                        lgap_length=c(msr_par$lgap_length,ttn_par$lgap_length),
                        lgap_drop=c(msr_par$lgap_drop,ttn_par$lgap_drop),
                        sbout_length=c(msr_par$sbout_length,ttn_par$sbout_length),
                        ib_max=c(msr_par$ib_max,ttn_par$ib_max),
                        sgap_drop=c(msr_par$sgap_drop,ttn_par$sgap_drop),
                        sstop_max_kvant=c(msr_par$sstop_max_kvant,ttn_par$sstop_max_kvant),
                        init_smw=c(msr_par$init_smw,ttn_par$init_smw),
                        datetime=Sys.time()
      )
    }
    return(list(report=report,
                b=b))
  }
  
}

### inputs
# Folder with extracted datasets (after "final_extraction_11_2022.R")
# Report from the extraction - to define the parameters of initial postprocessing
# Report from the "manual_edit.R" - to define which nests should be repeated?
# Default settings for the postprocessing parameters
# parameter settings
{
  ### add cooling windows to the parameter at the beginning
  # MSR
  msr_par=list(lgap_length=60,#5 min
               lgap_drop=-5,# 5°C
               sbout_length=36,# 3 min 
               sgap_drop=-.7,# 1°C
               sstop_max_kvant=.25,# 25% kvantil,. median je moc)
               ib_max=0.1,
               init_smw=10)
}

# workflow
# set working directories and run while loop for all the datasets which should be proceeded
  # after initial visualization answer "y", if some changes are needed a "fix.incub" function is called
    # there are subsequent choices on the "Co chces?:" question:
      # "z" - means "zoom the plot" and will call locator to select the area for zooming
      # "v" - means refresh the visualization on the whole dataset
      # "r" - means remove the whole dataset as unusable
      # "b" - will retract the last made change
      # "s" - will break the repeat loop and go to the return and jump to the saving of the fixed data
      # "f" - will enable to make various fixes, enables several answers on question "What is the problem?:"
    # namely:
          # "s" - by SINGLE CLICK set the start of the usable data (i.e. remove all the data before the selected point) 
          # "e" - by SINGLE CLICK set the end of the usable data (i.e. remove all the data after the selected point)  
          # "g" - by the SERIES OF PAIRS OF CLICKS (start-end) define manually incubation gaps
                # this can be repeated until some of the clicks in the pair will not be to the space before the 0:00
          # "i" - by the SERIES OF PAIRS OF CLICKS (start-end) define manually incubation bouts
                # this can be repeated until some of the clicks in the pair will not be to the space before the 0:00
          # "p" - by the SERIES OF PAIRS OF CLICKS (start-end) define manually parts, where you would like to 
                  # return back to the raw HMM prediction denoted by black polygons in the plot (i.e. avoid to use automated postprocessing)
                # this can be repeated until some of the clicks in the pair will not be to the space before the 0:00 
          # "d" - by the SERIES OF PAIRS OF CLICKS (start-end) define manually parts, which should be completely deleted
                # this can be repeated until some of the clicks in the pair will not be to the space before the 0:00 
          # "h" - by the SERIES OF PAIRS OF CLICKS (start-end) define manually parts, for which the HMM should be refitted separately
                # this can be repeated until some of the clicks in the pair will not be to the space before the 0:00
                # because HMM refitting may take some time, FIRST will be defined all parts with refitting requested
                # and after breaking all models will be calculated and plot automatically refreshed
          # "cp" - Change the postprocessing of the whole dataset, or its part
                # In the first step select ONE area by a PAIR OF CLICKS, where you would like to change the postprocessing parameters
                # Then parameters are printed out on a console and you are asked "Co chces?:"
                # Answers might include:
                    # one or more selected parameters to change in shape "parameter_suggested value"
                    # if change of more parameters is suggested in one step, these have to be separated by ";"
                    # After each suggested change, the plot with treated part of data will be refreshed automatically.
                    # "o" will confirm the changes of the postp. parameters will implement the last suggestions and will break out of the "cp" loop
                    # "d" will break the "cp" loop while discarding the suggested parameters as non-usefull
          # "x" - break the "fixing loop" and return back to the "co chces" level.  
# after the "s" choice and breaking out fromt the "fix.incub" function, you will be asked whether "Should be edits redone?(y/n):"
  # if your response will not be "y", fixed data, plot, and also the reports from the fixing process will be stored and other dataset will be loaded.



### outputs
# Final extraction (.RData file with data)
# Final actogram visualization
# Report about the changes made on the dataset (of same form as "fixed_data" and 
#"datasets_final" from "manual_edit.R" process)
# Report about the final automated postprocessing (of the same form as "report" from 
# "final_extraction_11_2022.R")


#### NEW VERSION
{# Folder with extracted datasets (after "final_extraction_11_2022.R")
data="C:/Users/josem/Desktop/Nightjars/HM_Models_Sladecek/Data_all_computed"
cleaned_data="C:/Users/josem/Desktop/Nightjars/HM_Models_Sladecek/Data_cleaned"
general_outputs="C:/Users/josem/Desktop/Nightjars/HM_Models_Sladecek"
actograms="C:/Users/josem/Desktop/Nightjars/HM_Models_Sladecek/Actograms_cleaned"
# report from the extraction - to define the parameters of initial postprocessing
# ??? report from the "manual_edit.R" - to define which nests should be repeated?
###
# loop for process particular datasets
fixed_data={}# 
#fixed_hmm={}# info about refitted HMM probably not needed - may be extracted from the fixed data
fixed_cp={}# 
datasets_final={}
#file=file_list[1]
file_list=list.files(data,pattern=".RData")

#This below line allows to set which exact nest you want to manually edit
i=26# set first i, which has not been done!!!

while (i <=length(file_list)) {
  file=file_list[i]
  setwd(data)
  load(file)# object b is universally the name for dataset
  nest=substr(file,1,nchar(file)-6)
  
  day=c(length(unique(as.Date(b$datetime))):1)
  b$day_order=day[match(as.Date(b$datetime_),unique(as.Date(b$datetime_)))]
  
  # 1) initially visualize the previously discarded extraction 
  # (the one after the "manual_edit.R" script) 
  ### !!! here should be added some indication of the section break!
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
                   lwd=.5,type="plot",nest=b$nest[1],
                   device="new")
  
  
 
  logger_sect=b[,.(start=min(datetime_),
                   end=max(datetime_)),
                by=type]
  print("Used loggers are as follows:")
  print(logger_sect)
 
 
  rl=readline("Should be some particular problems fixed?(y/n)?:")
  if(rl=="y"){
    # repair problems
    xf=fix.incub(x=b,nest=nest,msr_par=msr_par,ttn_par=ttn_par)
    
    b=xf$data
    
    # repaired dataset
    if(!is.null(xf$fixed_problems)){
    fixed_data=rbind(fixed_data,
                     xf$fixed_problems)
    }
    if(!is.null(xf$fixed_cp)){
    fixed_cp=rbind(fixed_cp,
                   xf$fixed_cp)
    }
    
  }
  
  # 5) create and save the report about the whole process 
  # 6) save final dataset and the script
  rl=readline("Should be edits redone?(y/n):")
  if(rl!="y"){
      
      setwd(actograms)
    actogram_rwl_hmm(x=b,sex=NULL,datetime="datetime_",hmm_lines = T,
                     t.in="t_nest",t.out="t_surface",h.in="h_nest",
                     h.out="h_surface",
                     hmm_inc="prediction_final",ref_inc="hmm_t",
                     visits="visit",visit_what="visit_what",bouts="who",
                     col.t.in="red",col.t.in_gap = "yellow",col.t.out="orange",
                     col.h.in="dodgerblue4",col.h.out="lightblue",col.m="dodgerblue4", col.f="firebrick3",
                     col.l.in=NULL,col.l.out=NULL,col.hmm.inc="green", col.ref.inc="black",
                     lwd=.5,type="PNG",nest=b$nest[1],
                     device="new")
    
      save(b,
           file=paste0(cleaned_data,
                       "/",nest,"_final.RData"))
    
    
    datasets_final=rbind(datasets_final,
                         data.frame(nest=nest,
                                    data_size=nrow(b),
                                    data_ok=length(b$include_data[b$include_data==T]),
                                    changes_made=nrow(xf$fixed_problems),
                                    change_postprocessing=ifelse(nrow(xf$fixed_problems[xf$problem=="change postprocessing",])>0,
                                                                 TRUE,FALSE),
                                    timestamp=Sys.time()
                         ))
    
    # maybe consolidate all the fixed_... into one?
    save(fixed_data,fixed_cp,datasets_final,#fixed_hmm,
         file=paste(general_outputs,paste0("summary_data",strftime(Sys.time(),
                                                                   format="_%Y_%d%_%m_%H_%M"),".RData"),sep="/"))
    
    rm(list=ls(pattern = nest))
    rm(b,xf)
    print(nest)
    i=i+1
    print(i)
  }
}
}

