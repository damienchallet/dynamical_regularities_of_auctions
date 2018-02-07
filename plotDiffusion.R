library(data.table)
library(ggplot2)
source('~/admin/R/lib/ggplot_theme_bw_latex.R')


plotDiffusion=function(price,ticker,auction,coarseFactor=coarseFactor,dirPlots="plots",toPDF=FALSE,
                       HHMMSS_open=as.POSIXct("2000-01-01 09:30:00 UTC",tz="UTC"),
                       HHMMSS_close=as.POSIXct("2000-01-01 16:00:00 UTC",tz="UTC"),minNumEvents=50
){
  numEventsPerDay=price[,N:=sum(!is.na(price)),by="day"]
  daysOK=numEventsPerDay[N>minNumEvents,day]
  price=price[day %in% daysOK]

    allHHMMSS=price[,sort(unique(HHMMSS))]
  if(auction=="open"){
    HHMMSS_auction=HHMMSS_open
  }
  if(auction=="close"){
    HHMMSS_auction=HHMMSS_close
  }
  allHHMMSS=c(allHHMMSS,HHMMSS_auction)

  alldays=data.table(day=price[,unique(day)])
  alldaysHHMMSS=alldays[,.(HHMMSS=allHHMMSS),by=day]
  setkeyv(alldaysHHMMSS,c("day","HHMMSS"))


  setkeyv(price,c("day","HHMMSS"))
  price=merge(alldaysHHMMSS,price,all.x=TRUE)
  price[,price:=na.locf(price,na.rm = FALSE)]
  price[,price:=na.locf(price,na.rm = FALSE,fromLast = TRUE)]

  price=na.omit(price,cols = "price")

  scaledFluct=price[,.(dP2=((log(price[.N])-log(price))^2),HHMMSS,price),by="day"]
  scaledFluct[,dP2:=dP2/var(diff(log(price))),by="day"]
  scaledFluct=scaledFluct[dP2>0]

  scaledFluctHHMMSSday=scaledFluct[,.(dP2scaled=head(dP2,1)),by="HHMMSS,day"]

  scaledFluctHHMMSS=scaledFluctHHMMSSday[,.(dP2med=median(dP2scaled)),by=HHMMSS]


  if(auction=="open"){
    scaledFluctHHMMSS[,seconds:=9.5*3600-(hour(HHMMSS)*3600+minute(HHMMSS)*60+second(HHMMSS))]
  }else{
    scaledFluctHHMMSS[,seconds:=16*3600-(hour(HHMMSS)*3600+minute(HHMMSS)*60+second(HHMMSS))]
  }

  scaledFluctHHMMSS=scaledFluctHHMMSS[dP2med>0]
  if(nrow(scaledFluctHHMMSS)<10){
    return(NULL)
  }

  setkey(scaledFluctHHMMSS,HHMMSS)
  mymain=paste0(ticker,", ",auction)
  qplot(data=scaledFluctHHMMSS,HHMMSS,dP2med,ylab=expression(paste("Median[",D[tau]^2,"]")),xlab="",main=mymain)+theme_bw_latex
  if(toPDF){
    dir.create(dirPlots,showWarnings = FALSE,recursive = TRUE)
    filename=paste0(dirPlots,"/diffusion_HHMMSS_",ticker,"_",auction,"_coarseFactor",coarseFactor,".pdf")
    ggsave(filename,width = 7, height= 7)
  }
  scaledFluctHHMMSS=scaledFluctHHMMSS[dP2med>0]
  if(nrow(scaledFluctHHMMSS)<10){
    return(NULL)
  }
 # browser()
  ntries=0
  mynls=NA
  while(ntries<100 && class(mynls)!="nls"){
    mynls=tryCatch(scaledFluctHHMMSS[dP2med<max(dP2med),nls(dP2med~a*seconds^b,start=c(a=1+0.6*runif(1),b=1+0.6*runif(1)))],error=function(e) return(NA))
    ntries=ntries+1
 #   print(summary(mynls))
  }
  if(class(mynls)!="nls"){
    return(NULL)
  }
  pval_b=summary(mynls)$coeff[,4]["b"]
  if(pval_b>0.001){
    return(NULL)
  }
  qplot(data=scaledFluctHHMMSS,seconds,dP2med,ylab=expression(paste("Median",( D[tau]),"")),main=mymain)+theme_bw_latex+geom_smooth(method = "nls",formula = y ~ a*x^b, method.args=list(start = c(a = 1, b = 1)),se=FALSE)
  if(toPDF){
    dir.create(dirPlots,showWarnings = FALSE,recursive = TRUE)
    filename=paste0(dirPlots,"/diffusion_seconds_",ticker,"_",auction,"_coarseFactor",coarseFactor,".pdf")
    ggsave(filename,width = 7, height= 7)
  }

  return(mynls)
}
