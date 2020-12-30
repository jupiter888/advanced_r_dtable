#download the file
#change secs to hours
#group by year
library(data.table)
library(ggplot2)
dir()
setwd("~/advanced_r/precip/out")
getwd

ddf = function(r, scen = 2070:2100, control = 1970:2000){
  yr = if (grepl('historical', r$forc[1])){
    control
  }else{
    scen
  }
  #- transform precip to mm by r[, p := p * 60 * 60]
  r[, p := p * 60 * 60]
  #- modify mr[year(DTM)!=1950, .(D1 = value, D2 = frollsum(value, 2), D3 = frollsum(value, 3), D6 = frollsum(value, 6), D12 = frollsum(value, 12), D24 = frollsum(value, 24)), by = year(DTM)] to be applicable for r
  r = r[year(date) %in% yr, .(D1=p, D2=frollsum(p, 2), D3=frollsum(p, 3), D6=frollsum(p, 6), D12=frollsum(p, 12), D24=frollsum(p, 24)), by=year(date)]
  #melting
  r = melt.data.table(r, id.vars = 'year')
  #- calculate annual maxima
  mx = r[, .(Depth = max(value, na.rm=TRUE)), by=.(year,variable)]
  #- calculate quantile corresponding to prob = 1 - 1/c(2, 5, 10, 20)
  mx = mx[, .(Depth=quantile(Depth, p=1-1/c(2,5,10,20)), Freq = c(2,5,10,20)), by=.(Duration=variable)]
  mx[, hour := as.numeric(gsub('D', '', Duration))]
  copy(mx)
}

#run this on every file in dir using tapply/lapply
dta<-lapply(dir(pattern="CZ_pr_EUR-11_"),readRDS)

DDF= lapply(dta,ddf)
#put all ddf together
names(DDF)=dir(pattern="CZ_pr_EUR-11_")
DDF=rbindlist(DDF, idcol = 'SID')
DDF = DDF[, .(Duration, Depth, Freq, hour, RCP=sapply(strsplit(SID, '_'), function(x)x[5]), GCM = sapply(strsplit(SID, '_'), function(x)x[4]), RCM = sapply(strsplit(SID, '_'), function(x)x[7]))]
DDF
#calculate historical climate data
hDDF=DDF[RCP=='historical', .(Duration, Freq, hour,hDepth=Depth,GCM,RCM)]
#setnames(hDDF,'Depth', 'hDepth')
DDF=DDF[RCP!='historical']
#calculate delta
cDDF=hDDF[DDF, on=c('Duration','Freq','hour','GCM','RCM')]
cDDF[,delta:=Depth/hDepth]
cDDF= cDDF[, .(delta=mean(delta)),by=.(GCM,RCM,RCP,Duration,Freq,hour)]
#plot the delta 
ggplot(cDDF)+
  geom_line(aes(x=hour,y=delta,col=RCP,lty=RCM))+
  facet_grid(factor(Freq)~GCM)

#climate models
unique(hDDF$GCM)
CNRM=cDDF[GCM == "CNRM-CERFACS-CNRM-CM5", .(hour, delta, Freq, RCP)]
ggplot(CNRM)+geom_line(aes(x=hour,y=delta,col=RCP,group=Freq))+facet_grid(.~RCP)

ICHEC=cDDF[GCM == "ICHEC-EC-EARTH", .(hour,delta,Freq,RCP)]
ggplot(ICHEC)+geom_line(aes(x=hour,y=delta,col=RCP,group=Freq))+facet_grid(.~RCP)

MIROC=cDDF[GCM == "MIROC", .(hour, delta, Freq, RCP)]
ggplot(MIROC)+geom_line(aes(x=hour,y=delta,col=RCP,group=Freq))+facet_grid(.~RCP)

MOHC=cDDF[GCM == "MOHC-HadGEM2-ES", .(hour, delta, Freq, RCP)]
ggplot(MOHC)+geom_line(aes(x=hour,y=delta,col=RCP,group=Freq))+facet_grid(.~RCP)

######################################################################

