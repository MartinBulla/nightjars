Sys.setenv(tz="UTC")
#rm(list=ls())
# load/install packages
for (i in c("zoo","plyr","tvR","data.table","Rcpp")){
  if(!i %in% installed.packages()[,"Package"])  
  { install.packages(i)}
  sapply(i, require, character.only = TRUE,quietly = TRUE)
}

require(zoo) 
require(plyr) 
require(tvR)
require(Rcpp)
require(data.table)
#load("dht_all.RData")#data with processed videos (only 5)
#setwd("C:/Users/josem/Desktop/Nightjars/HM_Models_Sladecek")
setwd("C:/Users/josem/Desktop/Nightjars/HM_Models_Sladecek/hmm_origin")
# funkce
source("hmm_dens.R")
source("hmm_prep.R")
source("em_predict.R")
sourceCpp("em_pred.cpp")
source("em4hmm.R")
source("viterbi4hmm.R")
source("ma.R")
source("hmmf.R")
source("plotdist.R")
source("incubation_summary.R")
# novy verze funkci (zatim stejny jmena)!
source("hmm_prep2.R")
source("hmmf2.R")

# visualization of HMM outputs for MB unip project
# derived from actogram_rwl function
# + relative input variables names
# + line-based visualization of HMM inputs instead of raw data
# + polygon-based visualization of HMM outputs + potentially referential extraction method
# + all t/h/l series without loess smoothing
# transparent color in plots
transpcol = function (col = "red", newalpha = 100, mcv = 255){
  mycol = col2rgb(col)
  rgb(mycol[1, ], mycol[2, ], mycol[3, ], alpha = newalpha, 
      maxColorValue = mcv)
}
actogram_rwl_hmm=function(x=b,sex=NULL,datetime="datetime_",hmm_lines=T,
                          t.in="t_nest",t.out="t_surface",h.in="h1",h.out="h2",bouts="sex_final",
                          hmm_inc="hmm_inc",ref_inc=NULL,changes=NULL,visits=NULL,visit_what=NULL,
                          col.m="dodgerblue4", col.f="firebrick3",
                          col.t.in="red",col.t.in_gap="yellow",col.t.out="orange",col.h.in="dodgerblue4",col.h.out="lightblue",
                          col.l.in=NULL,col.l.out=NULL,col.hmm.inc="green", col.ref.inc="black",col.chan="grey50",
                          day_height=20,legend_height=20,lwd=1,### pak vyresit, jak se bude urcovat
                          crop=35,type=c("plot","PNG","PDF"),device=c("new","current"),nest=NULL,legend=F
){
  #data_type ma rozlisit typy grafu dle dat, type zda se ma graf ukazat 
  Sys.setenv(TZ="UTC")
  options(warn = -1)
  
  x$datetime=x[,..datetime]
  if(!is.null(t.in)){x$t.in=x[,..t.in]}
  if(!is.null(t.out)){x$t.out=x[,..t.out]}
  if(!is.null(h.in)){x$h.in=x[,..h.in]}
  if(!is.null(h.out)){x$h.out=x[,..h.out]}
  if(!is.null(hmm_inc)){x$hmm_inc=x[,..hmm_inc]}
  if(!is.null(ref_inc)){x$ref_inc=x[,..ref_inc]}
  if(!is.null(changes)){x$changes=x[,..changes]}
  if(!is.null(visits)){x$visits=x[,..visits]}
  if(!is.null(visit_what)){x$visit_what=x[,..visit_what]}
  
  # add a day order
  day=c(length(unique(as.Date(x$datetime))):1)
  x$day_order=day[match(as.Date(x$datetime),unique(as.Date(x$datetime)))]
  x$time_num=as.numeric(x$datetime- 
                          as.POSIXct(trunc.POSIXt(x$datetime,"days")))/3600
  
  # bouts
  if(!is.null(bouts)){
    x$xs_=as.numeric(as.factor(x$sex))
    x$xs_[is.na(x$xs_)]=3
    x$xb_=bout_nr(as.numeric(as.factor(x$xs_)))
    xb=x[,.(datetime_start=min(datetime),
            datetime_end=max(datetime),
            start_num=min(time_num),
            end_num=max(time_num)),
         by=.(xb_,sex,day_order)]
    ### ??? jak filtrovat a priradit zde barvy???
    xb$col=ifelse(!is.na(xb$sex) & xb$sex=="M",transpcol(col.m),
                  ifelse(!is.na(xb$sex) & xb$sex=="F",transpcol(col.f),
                         "NA"))
    xb=xb[!is.na(xb$sex),]
    
    
  }  
  else {xb=NULL}
  
  
  
  # HMM
  if(!is.null(hmm_inc)){
    x$bout=c(0,cumsum(diff(x$hmm_inc)!=0)+cumsum(diff(x$day_order)))
    
    xhmm=x[,.(datetime_start=min(datetime),
              datetime_end=max(datetime),
              start_num=min(time_num),
              end_num=max(time_num)),
           by=.(bout,day_order,hmm_inc)] 
    xhmm=xhmm[xhmm$hmm_inc==1,]
  } 
  else {xhmm=NULL}
  
  # ref_inc
  if(!is.null(ref_inc)){
    x$bout=c(0,cumsum(diff(x$ref_inc)!=0)+cumsum(diff(x$day_order)))
    
    xref=x[,.(datetime_start=min(datetime),
              datetime_end=max(datetime),
              start_num=min(time_num),
              end_num=max(time_num)),
           by=.(bout,day_order,ref_inc)] 
    xref=xref[xref$ref_inc==1,]
  } 
  else {xref=NULL}
  
  # manual changes
  if(!is.null(changes)){
    x$bout=c(0,cumsum(diff(x$changes)!=0)+cumsum(diff(x$day_order)))
    
    xchan=x[,.(datetime_start=min(datetime),
               datetime_end=max(datetime),
               start_num=min(time_num),
               end_num=max(time_num)),
            by=.(bout,day_order,changes)] 
    xchan=xchan[xchan$changes==1,]
  } 
  else {xchan=NULL}
  
  # visits
  if(!is.null(visits)& sum(x$visits)>0){
    x$bout=c(0,cumsum(diff(x$visits)!=0)+cumsum(diff(x$day_order)))
    
    xvis=x[,.(datetime_start=min(datetime),
              datetime_end=max(datetime),
              start_num=min(time_num),
              end_num=max(time_num)),
           by=.(bout,day_order,visits,visit_what)] 
    xvis=xvis[xvis$visits==1,]
  } 
  else {xvis=NULL}
  
  # creating plot region
  {
    
    if(type=="PNG") {
      tiff(paste(getwd(),"/",nest,".tiff", sep=""), 
           width=180,height=ifelse(length(day)*day_height+legend_height>220,240,
                                   length(day)*day_height+legend_height+20),
           units="mm",res=600,compression = "lzw")
    }  
    else if (type=="PDF") {
      par(pin=c(170/25.4,(ifelse(length(day)*day_height+legend_height>220,240,
                                 length(day)*day_height+legend_height))/25.4))
    }  
    else {
      if(device=="new"){
        dev.new(width=180,height=ifelse(length(day)*day_height+legend_height>240,240,
                                        length(day)*day_height+legend_height),units="mm")
      } 
      else {
        dev.cur()
      }
    }
    par(yaxt="n",fg="grey40",mai=c(.3,0,.3,0),xpd=NA)
    
    plot(NA,xlim=c(-2,24),ylim=c(-2,max(day)+1),
         xlab="",ylab="",xaxt="n",bty="n")
    lines(x=c(24,24),y=c(1,max(day)+1))
    lines(x=c(-2,24),y=c(rep(max(day)+1,2)))
    
    
    axis(side=1,at=c(0,6,12,18,24),cex.axis=0.7,labels= c("","","","",""),
         mgp=c(0,0.1,0),pos=1)
    ### definice popisku osy pomoci mtext - lepsi synchronizace s popisem osy?
    ### asi spis jako text, y-coor pomoci fce vzhledem k celkove vysce gafu?
    #   mtext(side=1,at=c(0,6,12,18,24),text = c("00:00","06:00","12:00","18:00","24:00"),cex=0.5,
    #          pos=1)#line=0,
    text(x=c(0,6,12,18,24),y=rep(0.8,5),labels = c("00:00","06:00","12:00","18:00","24:00"),cex=0.5)
    text(x=12,y=0.5,'Time [h]', cex=0.8, col='grey30')#line=0.7,
    
    # mtext('Time [h]',side=1, pos=1, cex=0.8, las=1, col='grey30',at=12)#line=0.7,
    mtext('Date',side=2,line=0, cex=0.8, las=3, at=mean(c(1,day+1)),col='grey30')
    ### main
    mtext(nest,side=3,line=0, cex=1, las=1, col='grey30')
    
    # polygons with days
    for (i in length(day):1) {
      lines(x=c(-2,24),y=c(i,i))
      polygon(x=c(-2,0,0,-2),
              y=rep(c(day[i],day[i]+1),each=2),
              col = "grey90")
      text(x=-1,y=day[i]+0.5,labels = strftime(as.Date(unique(as.Date(x$datetime))[i], origin="1970-01-01"),
                                               format="%d.%m"),cex =0.7,col="grey30")
    }
    
    
    # polygons with bouts
    if(!is.null(xb)){
      for (i in 1:nrow(xb)) {
        
        polygon(x=c(xb$start_num[i],xb$end_num[i],xb$end_num[i],xb$start_num[i]),
                y=rep(c(xb$day_order[i],xb$day_order[i]+1),each=2),border = NA,
                col = xb$col[i])
      }
    }
    # lines with t/h/l
    {
      # relative scales
      {
        min_t.in=min(x$t.in[!is.na(x$t.in)]);max_t.in=max(x$t.in[!is.na(x$t.in)])
        coeff_t.in=1/(max_t.in-min_t.in)
        
        min_t.out=min(x$t.out[!is.na(x$t.out)]);max_t.out=max(x$t.out[!is.na(x$t.out)])
        coeff_t.out=1/(max_t.out-min_t.out)
        
        min_h.in=min(x$h.in[!is.na(x$h.in)]);max_h.in=max(x$h.in[!is.na(x$h.in)])
        coeff_h.in=1/(max_h.in-min_h.in)
        
        min_h.out=min(x$h.out[!is.na(x$h.out)]);max_h.out=max(x$h.out[!is.na(x$h.out)])
        coeff_h.out=1/(max_h.out-min_h.out)
        
        min_l.in=min(x$l.in[!is.na(x$l.in)]);max_l.in=max(x$l.in[!is.na(x$l.in)])
        coeff_l.in=1/(max_l.in-min_l.in)
        
        min_l.out=min(x$l.out[!is.na(x$l.out)]);max_l.out=max(x$l.out[!is.na(x$l.out)])
        coeff_l.out=1/(max_l.out-min_l.out)
      }
      
      # changes made
      if(!is.null(xchan)){
        for (i in 1:nrow(xchan)) {
          polygon(x=c(xchan$start_num[i],xchan$end_num[i],xchan$end_num[i],xchan$start_num[i]),
                  y=rep(c(xchan$day_order[i],xchan$day_order[i]+1),each=2),border = NA,
                  col =transpcol(col.chan,newalpha = 100))
        }
      }
      
      # visits
      if(!is.null(xvis)){;
        for (i in 1:nrow(xvis)) {
          polygon(x=c(xvis$start_num[i],xvis$end_num[i],xvis$end_num[i],xvis$start_num[i]),
                  y=rep(c(xvis$day_order[i],xvis$day_order[i]+1),each=2),border = NA,
                  col =transpcol("violetred4",200))
          text(xvis$end_num[i],y=xvis$day_order[i]+.7,
               labels = xvis$visit_what[i],cex=.8,pos=4,col="black")
        }
      }
      # polygons with predictions
      if(!is.null(xhmm)){
        for (i in 1:nrow(xhmm)) {
          xhmm=xhmm
          polygon(x=c(xhmm$start_num[i],xhmm$end_num[i],xhmm$end_num[i],xhmm$start_num[i]),
                  y=rep(c(xhmm$day_order[i]+.15,xhmm$day_order[i]+.25),each=2),border = NA,
                  col = col.hmm.inc)
        }
      }
      if(!is.null(xref)){
        for (i in 1:nrow(xref)) {
          polygon(x=c(xref$start_num[i],xref$end_num[i],xref$end_num[i],xref$start_num[i]),
                  y=rep(c(xref$day_order[i]+.35,xref$day_order[i]+.45),each=2),border = NA,
                  col =col.ref.inc,newalpha = 100)
        }
      }
      
      
      for (i in 1:max(unique(x$day_order))) {
        
        # h.out
        if (length(which(!is.na(x[x$day_order==i,"h.out"])))>0) {
          lines(x=x$time_num[x$day_order==i& !is.na(x$h.out)],# prasarna pres t.out - pak doresit!
                y=i+coeff_h.out*(x$h.out[x$day_order==i & !is.na(x$h.out)]-min_h.out),
                col=col.h.out,lwd=lwd)
        }
        
        # t.out
        if (length(which(!is.na(x[x$day_order==i,"t.out"])))>0) {
          lines(x=x$time_num[x$day_order==i & !is.na(x$t.out)],
                y=i+coeff_t.out*(x$t.out[x$day_order==i & !is.na(x$t.out)]-min_t.out),col=col.t.out,
                lwd=lwd)
        }
        # h.in
        if (length(which(!is.na(x[x$day_order==i,"h.in"])))>0) {
          lines(x=x$time_num[x$day_order==i& !is.na(x$h.in)],
                y=i+coeff_h.in*(x$h.in[x$day_order==i & !is.na(x$h.in)]-min_h.in),
                col=col.h.in,lwd=lwd)
        }
        
        # t.in
        if(length(which(!is.na(x[x$day_order==i,"t.in"])))>0){
          lines(x=x$time_num[x$day_order==i & !is.na(x$t.in)],
                y=i+coeff_t.in*(x$t.in[x$day_order==i & !is.na(x$t.in)]-min_t.in),col=col.t.in_gap,
                lwd=lwd)
          if(!is.null (xhmm) & hmm_lines==T){
            xxh=xhmm[xhmm$day_order==i,]
            for (j in 1:nrow(xxh)) {
              lines(x=x$time_num[x$day_order==i & !is.na(x$t.in) &
                                   x$time_num>=xxh$start_num[j]& x$time_num<=xxh$end_num[j]],
                    y=i+coeff_t.in*(x$t.in[x$day_order==i & !is.na(x$t.in) &
                                             x$time_num>=xxh$start_num[j]& x$time_num<=xxh$end_num[j]]-min_t.in),
                    col=col.t.in,
                    lwd=lwd)
            } 
          }
        }
        
        
      }
    }
    
    
    
  }
  
  if(type=="PNG") { 
    dev.off()
  }
}





actogram_unip_hmm=function(x=b,sex=NULL,datetime="datetime_",hmm_lines=T,
                           t.in="t_nest",t.out="t_surface",h.in="h1",h.out="h2",
                           l.in=NULL,l.out=NULL,issues=NULL,issue_what=NULL,
                           hmm_inc="hmm_inc",ref_inc=NULL,changes=NULL,visits=NULL,visit_what=NULL,
                           col.t.in="red",col.t.in_gap="yellow",col.t.out="orange",col.h.in="dodgerblue4",col.h.out="lightblue",
                           col.l.in=NULL,col.l.out=NULL,col.hmm.inc="green", col.ref.inc="black",col.chan="grey50",
                           day_height=20,legend_height=20,lwd=1,### pak vyresit, jak se bude urcovat
                           crop=35,type=c("plot","PNG","PDF"),device=c("new","current"),nest=NULL,legend=F
){
  #data_type ma rozlisit typy grafu dle dat, type zda se ma graf ukazat 
  Sys.setenv(TZ="UTC")
  options(warn = -1)
  
  x$datetime=x[,..datetime]
  if(!is.null(t.in)){x$t.in=x[,..t.in]}
  if(!is.null(t.out)){x$t.out=x[,..t.out]}
  if(!is.null(h.in)){x$h.in=x[,..h.in]}
  if(!is.null(h.out)){x$h.out=x[,..h.out]}
  if(!is.null(l.in)){x$l.in=x[,..l.in]}
  if(!is.null(l.out)){x$l.out=x[,..l.out]}
  if(!is.null(hmm_inc)){x$hmm_inc=x[,..hmm_inc]}
  if(!is.null(ref_inc)){x$ref_inc=x[,..ref_inc]}
  if(!is.null(changes)){x$changes=x[,..changes]}
  if(!is.null(visits)){x$visits=x[,..visits]}
  if(!is.null(visit_what)){x$visit_what=x[,..visit_what]}
  if(!is.null(issues)){x$issues=x[,..issues]}
  if(!is.null(issue_what)){x$issue_what=x[,..issue_what]}
  # add a day order
  day=c(length(unique(as.Date(x$datetime))):1)
  x$day_order=day[match(as.Date(x$datetime),unique(as.Date(x$datetime)))]
  x$time_num=as.numeric(difftime(x$datetime,
                                 as.POSIXct(trunc.POSIXt(x$datetime,"days")),
                                 units = "secs")/3600)
  
  # HMM
  if(!is.null(hmm_inc)){
    x$bout=c(0,cumsum(diff(x$hmm_inc)!=0)+cumsum(diff(x$day_order)))
    
    xhmm=x[,.(datetime_start=min(datetime),
              datetime_end=max(datetime),
              start_num=min(time_num),
              end_num=max(time_num)),
           by=.(bout,day_order,hmm_inc)] 
    xhmm=xhmm[xhmm$hmm_inc==1,]
  } else {xhmm=NULL}
  
  # ref_inc
  if(!is.null(ref_inc)){
    x$bout=c(0,cumsum(diff(x$ref_inc)!=0)+cumsum(diff(x$day_order)))
    
    xref=x[,.(datetime_start=min(datetime),
              datetime_end=max(datetime),
              start_num=min(time_num),
              end_num=max(time_num)),
           by=.(bout,day_order,ref_inc)] 
    xref=xref[xref$ref_inc==1,]
  } else {xref=NULL}
  
  # manual changes
  if(!is.null(changes)){
    x$bout=c(0,cumsum(diff(x$changes)!=0)+cumsum(diff(x$day_order)))
    
    xchan=x[,.(datetime_start=min(datetime),
               datetime_end=max(datetime),
               start_num=min(time_num),
               end_num=max(time_num)),
            by=.(bout,day_order,changes)] 
    xchan=xchan[xchan$changes==1,]
  } else {xchan=NULL}
  
  # visits
  if(!is.null(visits)& sum(x$visits)>0){
    x$bout=c(0,cumsum(diff(x$visits)!=0)+cumsum(diff(x$day_order)))
    
    xvis=x[,.(datetime_start=min(datetime),
              datetime_end=max(datetime),
              start_num=min(time_num),
              end_num=max(time_num)),
           by=.(bout,day_order,visits,visit_what)] 
    xvis=xvis[xvis$visits==1,]
  } else {xvis=NULL}
  # issues
  if(!is.null(issues)& sum(x$issues)>0){
    x$bout=c(0,cumsum(diff(x$issues)!=0)+cumsum(diff(x$day_order)))
    
    xiss=x[,.(datetime_start=min(datetime),
              datetime_end=max(datetime),
              start_num=min(time_num),
              end_num=max(time_num)),
           by=.(bout,day_order,issues,issue_what)] 
    xiss=xiss[xiss$issues==1,]
  } else {xiss=NULL}
  
  # creating plot region
  {
    
    if(type=="PNG") {
      tiff(paste(getwd(),"/",nest,".tiff", sep=""), 
           width=180,height=ifelse(length(day)*day_height+legend_height>220,240,
                                   length(day)*day_height+legend_height+20),
           units="mm",res=600,compression = "lzw")
    }  else if (type=="PDF") {
      par(pin=c(170/25.4,(ifelse(length(day)*day_height+legend_height>220,240,
                                 length(day)*day_height+legend_height))/25.4))
    }  else {
      if(device=="new"){
        dev.new(width=180,height=ifelse(length(day)*day_height+legend_height>240,240,
                                        length(day)*day_height+legend_height),units="mm")
      } else {
        dev.cur()
      }
    }
    par(yaxt="n",fg="grey40",mai=c(.3,0,.3,0),xpd=NA)
    
    plot(NA,xlim=c(-2,24),ylim=c(-2,max(day)+1),
         xlab="",ylab="",xaxt="n",bty="n")
    lines(x=c(24,24),y=c(1,max(day)+1))
    lines(x=c(-2,24),y=c(rep(max(day)+1,2)))
    
    
    axis(side=1,at=c(0,6,12,18,24),cex.axis=0.7,labels= c("","","","",""),
         mgp=c(0,0.1,0),pos=1)
    ### definice popisku osy pomoci mtext - lepsi synchronizace s popisem osy?
    ### asi spis jako text, y-coor pomoci fce vzhledem k celkove vysce gafu?
    #   mtext(side=1,at=c(0,6,12,18,24),text = c("00:00","06:00","12:00","18:00","24:00"),cex=0.5,
    #          pos=1)#line=0,
    text(x=c(0,6,12,18,24),y=rep(0.8,5),labels = c("00:00","06:00","12:00","18:00","24:00"),cex=0.5)
    text(x=12,y=0.5,'Time [h]', cex=0.8, col='grey30')#line=0.7,
    
    # mtext('Time [h]',side=1, pos=1, cex=0.8, las=1, col='grey30',at=12)#line=0.7,
    mtext('Date',side=2,line=0, cex=0.8, las=3, at=mean(c(1,day+1)),col='grey30')
    ### main
    mtext(nest,side=3,line=0, cex=1, las=1, col='grey30')
    
    # polygons with days
    for (i in length(day):1) {
      lines(x=c(-2,24),y=c(i,i))
      polygon(x=c(-2,0,0,-2),
              y=rep(c(day[i],day[i]+1),each=2),
              col = "grey90")
      text(x=-1,y=day[i]+0.5,labels = strftime(as.Date(unique(as.Date(x$datetime))[i], origin="1970-01-01"),
                                               format="%d.%m"),cex =0.7,col="grey30")
    }
    
    # lines with t/h/l
    {
      # relative scales
      {
        min_t.in=min(x$t.in[!is.na(x$t.in)]);max_t.in=max(x$t.in[!is.na(x$t.in)])
        coeff_t.in=1/(max_t.in-min_t.in)
        
        min_t.out=min(x$t.out[!is.na(x$t.out)]);max_t.out=max(x$t.out[!is.na(x$t.out)])
        coeff_t.out=1/(max_t.out-min_t.out)
        
        min_h.in=min(x$h.in[!is.na(x$h.in)]);max_h.in=max(x$h.in[!is.na(x$h.in)])
        coeff_h.in=1/(max_h.in-min_h.in)
        
        min_h.out=min(x$h.out[!is.na(x$h.out)]);max_h.out=max(x$h.out[!is.na(x$h.out)])
        coeff_h.out=1/(max_h.out-min_h.out)
        
        min_l.in=min(x$l.in[!is.na(x$l.in)]);max_l.in=max(x$l.in[!is.na(x$l.in)])
        coeff_l.in=1/(max_l.in-min_l.in)
        
        min_l.out=min(x$l.out[!is.na(x$l.out)]);max_l.out=max(x$l.out[!is.na(x$l.out)])
        coeff_l.out=1/(max_l.out-min_l.out)
      }
      
      # changes made
      if(!is.null(xchan)){
        for (i in 1:nrow(xchan)) {
          polygon(x=c(xchan$start_num[i],xchan$end_num[i],xchan$end_num[i],xchan$start_num[i]),
                  y=rep(c(xchan$day_order[i],xchan$day_order[i]+1),each=2),border = NA,
                  col =transpcol(col.chan,newalpha = 100))
        }
      }
      
      # issues
      if(!is.null(xiss)){;
        for (i in 1:nrow(xiss)) {
          polygon(x=c(xiss$start_num[i],xiss$end_num[i],xiss$end_num[i],xiss$start_num[i]),
                  y=rep(c(xiss$day_order[i],xiss$day_order[i]+1),each=2),border = NA,
                  col =transpcol("coral4",70))
          if(xiss$end_num[i]<23){
          text(xiss$end_num[i],y=xiss$day_order[i]+.85,
               labels = xiss$issue_what[i],cex=.8,pos=4,col="black")
          }
        }
      }
      
      # visits
      if(!is.null(xvis)){;
        for (i in 1:nrow(xvis)) {
          polygon(x=c(xvis$start_num[i],xvis$end_num[i],xvis$end_num[i],xvis$start_num[i]),
                  y=rep(c(xvis$day_order[i],xvis$day_order[i]+1),each=2),border = NA,
                  col =transpcol("violetred4",200))
          text(xvis$end_num[i],y=xvis$day_order[i]+.7,
               labels = xvis$visit_what[i],cex=.8,pos=4,col="black")
        }
      }
      # polygons with predictions
      if(!is.null(xhmm)){
        for (i in 1:nrow(xhmm)) {
          xhmm=xhmm
          polygon(x=c(xhmm$start_num[i],xhmm$end_num[i],xhmm$end_num[i],xhmm$start_num[i]),
                  y=rep(c(xhmm$day_order[i]+.15,xhmm$day_order[i]+.25),each=2),border = NA,
                  col = col.hmm.inc)
        }
      }
      if(!is.null(xref)){
        for (i in 1:nrow(xref)) {
          polygon(x=c(xref$start_num[i],xref$end_num[i],xref$end_num[i],xref$start_num[i]),
                  y=rep(c(xref$day_order[i]+.35,xref$day_order[i]+.45),each=2),border = NA,
                  col =col.ref.inc,newalpha = 100)
        }
      }
      
      
      for (i in 1:max(unique(x$day_order))) {
        
        # l.in
        if(length(which(!is.na(x[x$day_order==i,"l.in"])))>0){
          lines(x=x$time_num[x$day_order==i & !is.na(x$l.in)],
                y=i+coeff_l.in*(x$l.in[x$day_order==i & !is.na(x$l.in)]-min_l.in),col=col.l.in,
                lwd=lwd)
        }
        # l.out
        if (length(which(!is.na(x[x$day_order==i,"l.out"])))>0) {
          lines(x=x$time_num[x$day_order==i & !is.na(x$l.out)],
                y=i+coeff_l.out*(x$l.out[x$day_order==i & !is.na(x$l.out)]-min_l.out),col=col.l.out,
                lwd=lwd)
        }
        
        # h.out
        if (length(which(!is.na(x[x$day_order==i,"h.out"])))>0) {
          lines(x=x$time_num[x$day_order==i& !is.na(x$h.out)],# prasarna pres t.out - pak doresit!
                y=i+coeff_h.out*(x$h.out[x$day_order==i & !is.na(x$h.out)]-min_h.out),
                col=col.h.out,lwd=lwd)
        }
        
        # t.out
        if (length(which(!is.na(x[x$day_order==i,"t.out"])))>0) {
          lines(x=x$time_num[x$day_order==i & !is.na(x$t.out)],
                y=i+coeff_t.out*(x$t.out[x$day_order==i & !is.na(x$t.out)]-min_t.out),col=col.t.out,
                lwd=lwd)
        }
        # h.in
        if (length(which(!is.na(x[x$day_order==i,"h.in"])))>0) {
          lines(x=x$time_num[x$day_order==i& !is.na(x$h.in)],
                y=i+coeff_h.in*(x$h.in[x$day_order==i & !is.na(x$h.in)]-min_h.in),
                col=col.h.in,lwd=lwd)
        }
        
        # t.in
        if(length(which(!is.na(x[x$day_order==i,"t.in"])))>0){
          lines(x=x$time_num[x$day_order==i & !is.na(x$t.in)],
                y=i+coeff_t.in*(x$t.in[x$day_order==i & !is.na(x$t.in)]-min_t.in),col=col.t.in_gap,
                lwd=lwd)
          if(!is.null (xhmm) & hmm_lines==T){
            xxh=xhmm[xhmm$day_order==i,]
            for (j in 1:nrow(xxh)) {
              lines(x=x$time_num[x$day_order==i & !is.na(x$t.in) &
                                   x$time_num>=xxh$start_num[j]& x$time_num<=xxh$end_num[j]],
                    y=i+coeff_t.in*(x$t.in[x$day_order==i & !is.na(x$t.in) &
                                             x$time_num>=xxh$start_num[j]& x$time_num<=xxh$end_num[j]]-min_t.in),
                    col=col.t.in,
                    lwd=lwd)
            } 
          }
        }
        
        
      }
    }
    
    
    
  }
  
  if(type=="PNG") { 
    dev.off()
  }
}

# zjednodusena verze zoom_plotu
actogram_unip_zoom=function(x=b,sex=NULL,datetime="datetime_",coor=locator(2),hmm_lines=T,
                            t.in="t_nest",t.out="t_surface",h.in="h1",h.out="h2",l.in=NULL,l.out=NULL,
                            hmm_inc="hmm_inc",ref_inc=NULL,changes=NULL,visits=NULL,visit_what=NULL,
                            col.t.in="red",col.t.in_gap="yellow",col.t.out="orange",col.h.in="dodgerblue4",col.h.out="lightblue",
                            col.l.in=NULL,col.l.out=NULL,col.hmm.inc="green", col.ref.inc="black",col.chan="grey50",
                            day_height=20,legend_height=20,lwd=1,### pak vyresit, jak se bude urcovat
                            crop=35,type=c("plot","PNG","PDF"),nest=NULL,legend=F
){
  #data_type ma rozlisit typy grafu dle dat, type zda se ma graf ukazat 
  Sys.setenv(TZ="UTC")
  options(warn = -1)
  
  x$datetime=x[,..datetime]
  if(!is.null(t.in)){x$t.in=x[,..t.in]}
  if(!is.null(t.out)){x$t.out=x[,..t.out]}
  if(!is.null(h.in)){x$h.in=x[,..h.in]}
  if(!is.null(h.out)){x$h.out=x[,..h.out]}
  if(!is.null(l.in)){x$l.in=x[,..l.in]}
  if(!is.null(l.out)){x$l.out=x[,..l.out]}
  if(!is.null(hmm_inc)){x$hmm_inc=x[,..hmm_inc]}
  if(!is.null(ref_inc)){x$ref_inc=x[,..ref_inc]}
  if(!is.null(changes)){x$changes=x[,..changes]}
  if(!is.null(visits)){x$visits=x[,..visits]}
  if(!is.null(visit_what)){x$visit_what=x[,..visit_what]}
  # add a day order
  
 # x$time_num=as.numeric(x$datetime- 
#                          as.POSIXct(trunc.POSIXt(x$datetime,"days")))
  
  # HMM
  if(!is.null(hmm_inc)){
    x$bout=c(0,cumsum(diff(x$hmm_inc)!=0))
    
    xhmm=x[,.(datetime_start=min(datetime),
              datetime_end=max(datetime),
              start_num=min(time_num),
              end_num=max(time_num)),
           by=.(bout,hmm_inc)] 
    xhmm=xhmm[xhmm$hmm_inc==1,]
  } else {xhmm=NULL}
  # ref_inc
  if(!is.null(ref_inc)){
    x$bout=c(0,cumsum(diff(x$ref_inc)!=0))
    
    xref=x[,.(datetime_start=min(datetime),
              datetime_end=max(datetime),
              start_num=min(time_num),
              end_num=max(time_num)),
           by=.(bout,ref_inc)] 
    xref=xref[xref$ref_inc==1,]
  } else {xref=NULL}
  
  # changes
  if(!is.null(changes)){
    x$bout=c(0,cumsum(diff(x$changes)!=0))
    
    xchan=x[,.(datetime_start=min(datetime),
               datetime_end=max(datetime),
               start_num=min(time_num),
               end_num=max(time_num)),
            by=.(bout,changes)] 
    xchan=xchan[xchan$changes==1,]
  } else {xchan=NULL}
  # visits
  if(!is.null(visits)){
    x$bout=c(0,cumsum(diff(x$visits)!=0)+cumsum(diff(x$day_order)))
    
    xvis=x[,.(datetime_start=min(datetime),
              datetime_end=max(datetime),
              start_num=min(time_num),
              end_num=max(time_num)),
           by=.(bout,day_order,visits,visit_what)] 
    xvis=xvis[xvis$visits==1,]
  } else {xvis=NULL}
  # creating plot region
  {
    
    #    if(type=="PNG") {
    #      tiff(paste(getwd(),"/",nest,".tiff", sep=""), 
    #           width=180,height=ifelse(length(day)*day_height+legend_height>220,240,
    #                                   length(day)*day_height+legend_height+20),
    #           units="mm",res=600,compression = "lzw")
    #    }  else if (type=="PDF") {
    #      par(pin=c(170/25.4,(ifelse(length(day)*day_height+legend_height>220,240,
    #                                 length(day)*day_height+legend_height))/25.4))
    #    }  else {
    dev.cur()
    #    }
    par(yaxt="n",fg="grey40",mai=c(.3,0,.3,0),xpd=NA)
    
    plot(NA,ylim=c(0,110),
         xlab="",ylab="",xaxt="n",bty="n",xlim=c(min(x$time_num),max(x$time_num)))
    
    # polygons with changes made
    if(!is.null(xchan)& nrow(xchan)>0){
      for (i in 1:nrow(xchan)) {
        #xhmm=xhmm
        polygon(x=c(xchan$start_num[i],xchan$end_num[i],xchan$end_num[i],xchan$start_num[i]),
                y=rep(c(0,100),each=2),border = NA,
                col = transpcol(col.chan))
      }
    }
    # polygons with HMM predictions
    if(!is.null(xhmm) & nrow(xhmm)>0){
      for (i in 1:nrow(xhmm)) {
        xhmm=xhmm
        polygon(x=c(xhmm$start_num[i],xhmm$end_num[i],xhmm$end_num[i],xhmm$start_num[i]),
                y=rep(c(15,25),each=2),border = NA,
                col = col.hmm.inc)
      }
    }
    if(!is.null(xref)& nrow(xref)>0){
      for (i in 1:nrow(xref)) {
        polygon(x=c(xref$start_num[i],xref$end_num[i],xref$end_num[i],xref$start_num[i]),
                y=rep(c(35,45),each=2),border = NA,
                col = col.ref.inc)
      }
    }
    
    # visits
    if(!is.null(xvis)& nrow(xvis)>0){
      for (i in 1:nrow(xvis)) {
        polygon(x=c(xvis$start_num[i],xvis$end_num[i],xvis$end_num[i],xvis$start_num[i]),
                y=rep(c(0,100),each=2),border = NA,
                col =transpcol("violetred4",200))
        text(xvis$end_num[i],y=70,
             labels = xvis$visit_what[i],pos=4,col="black")
      }
    }
    # lines with t/h/l
    {
      # relative scales
      {
        min_t.in=min(x$t.in[!is.na(x$t.in)]);max_t.in=max(x$t.in[!is.na(x$t.in)])
        coeff_t.in=1/(max_t.in-min_t.in)
        
        min_t.out=min(x$t.out[!is.na(x$t.out)]);max_t.out=max(x$t.out[!is.na(x$t.out)])
        coeff_t.out=1/(max_t.out-min_t.out)
        
        min_h.in=min(x$h.in[!is.na(x$h.in)]);max_h.in=max(x$h.in[!is.na(x$h.in)])
        coeff_h.in=1/(max_h.in-min_h.in)
        
        min_h.out=min(x$h.out[!is.na(x$h.out)]);max_h.out=max(x$h.out[!is.na(x$h.out)])
        coeff_h.out=1/(max_h.out-min_h.out)
        
        min_l.in=min(x$l.in[!is.na(x$l.in)]);max_l.in=max(x$l.in[!is.na(x$l.in)])
        coeff_l.in=1/(max_l.in-min_l.in)
        
        min_l.out=min(x$l.out[!is.na(x$l.out)]);max_l.out=max(x$l.out[!is.na(x$l.out)])
        coeff_l.out=1/(max_l.out-min_l.out)
      }
      # l.in
      if(length(which(!is.na(x[,"l.in"])))>0){
        lines(x=x$time_num[!is.na(x$l.in)],
              y=i+coeff_l.in*(x$l.in[!is.na(x$l.in)]-min_l.in)*100,col=col.l.in,
              lwd=lwd)
      }
      # l.out
      if (length(which(!is.na(x[,"l.out"])))>0) {
        lines(x=x$time_num[!is.na(x$l.out)],
              y=coeff_l.out*(x$l.out[ !is.na(x$l.out)]-min_l.out)*100,col=col.l.out,
              lwd=lwd)
      }
      # h.in
      if (length(which(!is.na(x[,"h.in"])))>0) {
        lines(x=x$time_num[!is.na(x$h.in)],# prasarna pres t.out - pak doresit!
              y=coeff_h.in*(x$h.in[!is.na(x$h.in)]-min_h.in)*100,
              col=col.h.in,lwd=lwd)
      }
      # h.out
      if (length(which(!is.na(x[,"h.out"])))>0) {
        lines(x=x$time_num[!is.na(x$h.out)],# prasarna pres t.out - pak doresit!
              y=coeff_h.out*(x$h.out[!is.na(x$h.out)]-min_h.out)*100,
              col=col.h.out,lwd=lwd)
      }
      # t.out
      if (length(which(!is.na(x[,"t.out"])))>0) {
        lines(x=x$time_num[!is.na(x$t.out)],
              y=coeff_t.out*(x$t.out[!is.na(x$t.out)]-min_t.out)*100,col=col.t.out,
              lwd=lwd)
      }
      # t.in
      if(length(which(!is.na(x[,"t.in"])))>0){
        lines(x=x$time_num[ !is.na(x$t.in)],
              y=coeff_t.in*(x$t.in[!is.na(x$t.in)]-min_t.in)*100,col=col.t.in_gap,
              lwd=lwd)
        if(!is.null (xhmm) & hmm_lines==T){
          xxh=xhmm
          for (j in 1:nrow(xxh)) {
            lines(x=x$time_num[!is.na(x$t.in) &
                                 x$time_num>=xxh$start_num[j]& x$time_num<=xxh$end_num[j]],
                  y=coeff_t.in*(x$t.in[!is.na(x$t.in) &
                                         x$time_num>=xxh$start_num[j]& x$time_num<=xxh$end_num[j]]-min_t.in)*100,
                  col=col.t.in,
                  lwd=lwd)
          } 
        }
      }
    }
    
    
  }
  
}


# transformace
{
  
  fun=function(x){return(length(x[x>0]))}
  
  # nove s prehozenymi znamenky v pripade, ze okolni teplota je vetsi
  # taky nevim, jestli je obecne dobre zvolenej nazev t2 - mozna lepsi ta???
  negdiff=function(x,t2=NULL,wind=50,smth=10){
    pred=ma(x,smth)
    pred=c(0,diff(pred))
    
    if(!is.null(t2)){
      pred=ifelse(t2>x,
                  ifelse(pred>=0,pred-2*pred,abs(pred)),
                  pred)
    }
    
    pred=abs(ifelse(pred<0,pred,0))
    pred=c(rep(fun(pred[1:wind]),wind/2),
           rollapply(pred,wind,fun),
           rep(fun(pred[(length(x)-wind):length(x)]),wind/2-1))
    return(pred)
  }
  
  
  tvdifflo=function(x){
    n=1:length(x)
    x=abs(c(0,diff((denoise1(c(0,diff(x)))))))
    pred=summary(loess(x~n,span=60/length(x),
                       degree=1))$fitted
    return(pred)
  }
  
  difflo=function(x){
    n=1:length(x)
    x=abs(c(0,diff(x)))
    pred=summary(loess(x~n,span=60/length(x),
                       degree=1))$fitted
    return(pred)
  }
  
  dfr=function(x,i=1){
    pred=c(0,diff(x,i))
    return(pred)
  }
  
  tvd=function(x){
    pred=abs(dfr(denoise1(dfr(x))))
    return(pred)
  }
  
  tvdlo=function(x,abslt=F){
    n=1:length(x)
    if(abslt==F){
      x=tvd(x)
    } else {
      x=abs(tvd(x))
    }
    pred=summary(loess(x~n,span=60/length(x),
                       degree=1))$fitted
    return(pred)
  }
  
  negmax=function(x,wind=20,smth=10){
    pred=ma(x,smth)
    pred=c(0,diff(pred))
    pred=abs(ifelse(pred<0,pred,0))
    pred=c(rep(max(pred[1:wind]),wind/2),
           rollapply(pred,wind,max),
           rep(max(pred[(length(x)-wind):length(x)]),wind/2-1))
    return(pred)
  }
  
}

# calculate reference extraction by MB algorithm
inc_ref=function(b,roll_t_int =17279,roll_d_int = 24){
  
  b[, t_nest_prior := c(t_nest[1], t_nest[-nrow(b)])]
  b[, t_difference := t_nest - t_nest_prior]
  b[, t_nest_run_med := rollmedian(t_nest, roll_t_int , fill="extend")] # 1/2 day 8639, whole day 17279
  b[, t_diff_run := rollmean(t_difference, roll_d_int, fill="extend")] #2 minutes (for median neads to be odd)
  b[, inc_t := ifelse(is.na(t_surface),
                      (ifelse(t_nest_run_med<20,
                              (ifelse(t_nest > t_nest_run_med+3, 1, 0)),#))
                              ifelse(t_nest > t_nest_run_med-3, 1,0))),#))
                      ifelse(t_nest_run_med<20,
                             (ifelse(t_nest > t_nest_run_med+3, 1,
                                     ifelse(t_nest > t_surface+12.5, 1,0))),#))
                             ifelse(t_nest > t_nest_run_med-3, 1,
                                    ifelse(t_nest > t_surface+12.5, 1,0))))#))
    ]
  
  b[, inc := ifelse(is.na(t_surface),
                    (ifelse(abs(t_diff_run) >= 0.02 & t_diff_run < 0, 0,
                            ifelse(abs(t_diff_run) >= 0.02 & t_diff_run > 0,
                                   1, inc_t))),
                    (ifelse(t_nest<20 &(t_surface-abs(t_nest))>(-3) & t_nest<t_nest_run_med+3, inc_t,
                            (ifelse(abs(t_diff_run) >= 0.02 & t_diff_run < 0, 0,
                                    ifelse(abs(t_diff_run) >= 0.02 & t_diff_run > 0,1,
                                           inc_t)))))  )
    ]
  return(b$inc)
}

# bout-summarization functions
{
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
    x=cumsum(c(0,abs(diff(data)))) 
    x=x[-length(x)]
    return(x)
  }
  
  bout_drop=function(x,bouts){
    tl=rep(as.numeric(tapply(x,bouts,sum)),
           times=as.numeric(tapply(x,bouts,length)))
    return(tl)
  }
  
  bout_summ=function(x,bouts,fun){
    tl=rep(as.numeric(tapply(x,bouts,fun)),
           times=as.numeric(tapply(x,bouts,length)))
    return(tl)
  }
  
  
  n_within_bout=function(x){
    data=c(x,5)
    x=rep(NA,length(data))
    x=cumsum(c(0,abs(diff(data)))) 
    x1=cumsum(rep(1,times=length(data))) - 
      rep(c(0,which(!!abs(diff(data)))[-length(which(!!abs(diff(data))))]),
          times=c(which(!!abs(diff(data)))[1],diff(which(!!abs(diff(data))))))
    x1=x1[-length(x1)]
    return(x1)
  }
  
  bout_start_shift=function(x,shift=3){
    data=c(x,5)
    x1=cumsum(rep(1,times=length(data))) - 
      rep(c(0,which(!!abs(diff(data)))[-length(which(!!abs(diff(data))))]),
          times=c(which(!!abs(diff(data)))[1],diff(which(!!abs(diff(data))))))
    x1=x1[-length(x1)];data=data[-length(data)]
    data[data==0 & x1<=shift]=1
    return(data)
  }
}

