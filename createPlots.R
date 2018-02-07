library(data.table)
library(ggplot2)
library(xts)
library(nonnest2)
library(parallel)

options(error=recover)

source('~/admin/R/lib/ggplot_theme_bw_latex.R')
source('plotDiffusion.R')
source('plotConditionalDiffImbalance.R')




Sys.setenv(TZ='UTC')

createPlot=function(myticker,
                    HHMMSS_open=as.POSIXct("2000-01-01 09:30:00 UTC",tz="UTC"),
                    HHMMSS_close=as.POSIXct("2000-01-01 16:00:00 UTC",tz="UTC"),
                    dirDataAuctionsIB="/quant/trading/data/intraday/equity/stocks/auctions/",
                    dirPlots="plots/",
                    onlyARCA=TRUE,
                    minNumEvents=50,
                    coarseFactor=60*5, #in seconds
                    statType="doPlotCumNumEvents",
                    doPlotCumNumEvents =FALSE,
                    doPlotEventRates=FALSE,
                    doPlotIncreaseRates=FALSE,
                    doPlotResponseFunctions=TRUE,
                    doPlotMarketImpact=FALSE,
                    doPlotDiffusion=FALSE,
                    doPlotConditionalDiffImbalance=FALSE,
                    withRealPrices=FALSE,
                    toPDF=TRUE,
                    dirAuctions="/quant/trading/results/data/intraday/equity/stocks/auctions/IB/",
                    dirTimesSales="/quant/trading/results/data/intraday/equity/stocks//IB/times_and_sales/US/",
                    withoutTimesAndSales=FALSE){

  if(is.null(myticker)){
    return(NULL)
  }

  dir.create(dirPlots,recursive = TRUE,showWarnings = FALSE)

  fileDataAuctionsIB=paste0(dirAuctions,"/",myticker,".rds")
  if(!file.exists(fileDataAuctionsIB)){
    return(NULL)
  }
  dataAuctions=readRDS(fileDataAuctionsIB)

  if("symbol" %in% names(dataAuctions)){
    setnames(dataAuctions,"symbol","ticker")
  }
  dataAuctions[,date:=as.POSIXct(date,tz="UTC")]
  startDatePreAuction=dataAuctions[,min(as.Date(date))]

  if(!withoutTimesAndSales){
    fileDataTimesSales=paste0(dirTimesSales,"/",myticker,".rds")
    if(!file.exists(fileDataTimesSales)){
      return(NULL)
    }
    dataTimesSales=as.data.table(readRDS(fileDataTimesSales))
    dataTimesSales[,ticker:=myticker]

    openAuctions=dataTimesSales[,.(index,ticker,open_auction_price,open_auction_volume,open_auction_time)]
    closeAuctions=dataTimesSales[,.(index,ticker,close_auction_price,close_auction_volume,close_auction_time)]
    setnames(openAuctions,c("index","ticker","price","volume","time"))
    setnames(closeAuctions,c("index","ticker","price","volume","time"))

    # openAuctions[,index:=as.POSIXct(paste0(index," ",sprintf("%06d",time)),format="%Y-%m-%d %H%M%S")]
    # closeAuctions[,index:=as.POSIXct(paste0(index," ",sprintf("%06d",time)),format="%Y-%m-%d %H%M%S")]
    dataTimesSales=rbind(openAuctions,closeAuctions)
    dataTimesSales=na.omit(dataTimesSales)
    # dataTimesSales[,time:=NULL]
    setnames(dataTimesSales,"index","date")
    setkey(dataTimesSales,date)

  # if(myticker=="VXX"){
  #   browser()
  # }

    stopDateTRTH=dataTimesSales[,max(as.Date(date))]

    dataTimesSales=dataTimesSales[as.Date(date)>=startDatePreAuction]
    dataAuctions=dataAuctions[as.Date(date)<=stopDateTRTH]
    dataAuctions=rbind(dataAuctions,dataTimesSales,fill=TRUE)
  }


  AICs=data.table()

  t50=data.table()
  diffLM=data.table()
  allPsdII=data.table()

  setkey(dataAuctions,date)

  auctions=c("open","close")


  if(withRealPrices){
    if(!exists("bidaskPricesDay")){
      dirAuctionDataTicker=paste0(dirDataAuctionsIB,"/",myticker,"/")
      askFiles=list.files(dirAuctionDataTicker,pattern=".*askPrice.*",full.names = TRUE)
      bidFiles=list.files(dirAuctionDataTicker,pattern=".*bidPrice.*",full.names = TRUE)
      askPrices=lapply(askFiles,function(myfile){
        if(file.info(myfile)$size<100){
          return(data.table())
        }
        print(myfile)
        if(grepl("csv.gz$",myfile)){
          DT=fread(paste0("zcat ",myfile),select = c(1,7))
        }else if(grepl(".csv$",myfile)){
          DT=fread(myfile,select=c(1,7))
        }else{
          return(data.table())
        }
        setnames(DT,c("date","price"))
        DT[,date:=as.POSIXct(date,tz="America/New_York", format="%Y%m%d %H:%M:%OS %z"  )]
        return(DT)
      })
      askPrices=rbindlist(askPrices)
      bidPrices=lapply(bidFiles,function(myfile){
        if(file.info(myfile)$size<100){
          return(data.table())
        }
        if(grepl("csv.gz$",myfile)){
          DT=fread(paste0("zcat ",myfile),select = c(1,7))
        }else if(grepl(".csv$",myfile)){
          DT=fread(myfile,select=c(1,7))
        }else{
          return(data.table())
        }
        setnames(DT,c("date","price"))
        DT[,date:=as.POSIXct(date,tz="America/New_York", format="%Y%m%d %H:%M:%OS %z"  )]
        return(DT)
      })
      bidPrices=rbindlist(bidPrices)

      setkey(askPrices,"date")
      setnames(askPrices,"price","ask")
      setkey(bidPrices,"date")
      setnames(bidPrices,"price","bid")
      bidaskPricesDay=merge(bidPrices,askPrices,all=TRUE)
      rm(askPrices)
      rm(bidPrices)

      bidaskPricesDay[,`:=`(bid=na.locf(bid,na.rm = FALSE),
                            ask=na.locf(ask,na.rm = FALSE))]
      bidaskPricesDay[,`:=`(mid=(bid+ask)*0.5,spread=(ask-bid))]
      bidaskPricesDay=bidaskPrices[spread>0  & spread< mid]
      bidaskPricesDay=na.omit(bidaskPricesDay)
      bidaskPricesDay[,date:=as.POSIXct(date,tz="UTC")]
      bidaskPricesDay[,seconds:=second(date)+60*minute(date)+3600*hour(date)]

    }
  }

  for(myauction in auctions){
    DTticker=dataAuctions[ticker==myticker & myauction==myauction]
    DTticker[,seconds:=second(date)+60*minute(date)+3600*hour(date)]

    if(myauction=="open"){
      DT=DTticker[seconds>=8*3600 &seconds<=9.5*3600+45]
      if(nrow(DT)<10){
        next
      }
      DT[,day:=as.Date(date)]
      weirdDates=DT[,.(maxSec=max(seconds)),by=day][maxSec<9.25*3600][,day]    # no data after 9:15 that day, either missing data or wrong TZ
    }else{
      DT=DTticker[seconds>=15*3600 & seconds<=16*3600+45]
      if(nrow(DT)<10){
        next
      }
      DT[,day:=as.Date(date)]
      weirdDates=DT[,.(maxSec=max(seconds)),by=day][maxSec<15.75*3600][,day]    # no data after 15:45 that day, either early close, missing data, or wrong TZ
    }
    DT=DT[!(day %in% weirdDates)]
    DT[,coarseTime:=floor((hour(date)*60+minute(date)+second(date)/60)/coarseFactor)*coarseFactor]
    DT[,HHMMSS:=as.POSIXct(floor((hour(date)*3600+minute(date)*60+second(date))/coarseFactor)*coarseFactor, origin = "2000-01-01",tz="UTC")]
    if(myauction=="open"){
      DT[,TTA:=as.numeric(as.POSIXct(9.5*3600, origin = "2000-01-01",tz="UTC")-HHMMSS)]  # Time To Auction
      DT[,TTAx:=(as.POSIXct(9.5*3600, origin = "2000-01-01",tz="UTC")-HHMMSS)]
    }
    if(myauction=="close"){
      DT[,TTA:=as.numeric(as.POSIXct(16*3600, origin = "2000-01-01",tz="UTC")-HHMMSS)]
      DT[,TTAx:=(as.POSIXct(16*3600, origin = "2000-01-01",tz="UTC")-HHMMSS)]
    }
    imbalance=na.omit(unique(DT[,.(date,imbalance,HHMMSS,day)]))
    imbalance[,numEvents:=.N,by=day]
    imbalance=imbalance[numEvents>=minNumEvents]

    price=na.omit(unique(DT[,.(date,price,HHMMSS,day)]))
    price[,numEvents:=.N,by=day]
    price=price[numEvents>=minNumEvents]

    volume=na.omit(unique(DT[,.(date,volume,HHMMSS,day)]))
    volume[,numEvents:=.N,by=day]
    volume=volume[numEvents>=minNumEvents]
    # DT=DT[,.(imbalance=tail(imbalance,1),price=tail(price,1),volume=tail(volume,1),ticker=tail(ticker,1)),by="date"]   #data repeats itself sometimes for the same seconds
    imbalance[,day:=as.Date(date)]
    price[,day:=as.Date(date)]
    volume[,day:=as.Date(date)]

    setorder(imbalance,date)
    setorder(price,date)
    setorder(volume,date)

    volume=volume[volume>0]
    price=price[price>0]

    imbalance[,diffImbalance:=c(diff(imbalance),0),by=day]
    price[,diffPrice:=c(diff(log(price)),0),by=day]
    volume[,diffVolume:=c(diff(volume),0),by=day]

    # imbalance[,scaledNumevents:=(1:.N)/.N,by=day]
    # price[,scaledNumevents:=(1:.N)/.N,by=day]
    # price[,diffPriceAuction:=log(price[.N]/price),by=day]
    # volume[,scaledVolume:=volume/volume[.N],by=day]
    # volume[,scaledNumevents:=(1:.N)/.N,by=day]


    imbalance[,signIIlag:=as.factor(sign(imbalance*shift(imbalance,1,fill = 0)))]
    imbalance[,signI:=as.factor(sign(imbalance))]
    imbalance[,signIdI:=as.factor(sign(diffImbalance*imbalance))]
    imbalance[,signdIIx:=sign(diffImbalance*imbalance[.N]),by="day"]
    imbalance[,signIIx:=sign(imbalance)*sign(imbalance[.N]),by="day"]



    setkey(imbalance,date)
    setkey(price,date)
    setkey(volume,date)

    ################ end of initialization

    if(doPlotConditionalDiffImbalance){
      myPsdII=plotConditionalDiffImbalance(imbalance,myticker,myauction,toPDF=toPDF)
      allPsdII=rbind(allPsdII,myPsdII)
    }

    if(doPlotDiffusion){
      mylm=plotDiffusion(price,ticker=myticker,auction=myauction,coarseFactor=coarseFactor,dirPlots=dirPlots,toPDF=toPDF,HHMMSS_open = HHMMSS_open,HHMMSS_close = HHMMSS_close)
      if(is.null(mylm)){
        next
      }
      mycoef=coef(mylm)
      pval_b=summary(mylm)$coeff[2,4]
      newline=data.table(a=mycoef[1],b=mycoef[2],auction=myauction,ticker=myticker,pval=pval_b)
      diffLM=rbind(diffLM,newline)
    }

    if(doPlotCumNumEvents){
      numI=imbalance[,.N,by=date][,.(date,totI=cumsum(N))]
      numP=price[,.N,by=date][,.(date,totP=cumsum(N))]
      numV=volume[,.N,by=date][,.(date,totV=cumsum(N))]
      setkey(numI,date)
      setkey(numP,date)
      setkey(numV,date)
      m=numI[numP[numV]]
      setnames(m,c("totI","totP","totV"),c("imbalance","price","volume"))
      qplot(data=na.omit(melt(m,id.vars="date")),date,value,group=variable,col=variable,geom="step",size=I(1.2),ylab="cumulated number of updates",xlab="",main=myticker)+theme_bw_latex+theme(legend.title=element_blank())
      ggsave(paste0(dirPlots,"/cum_updates_",myticker,".pdf"),width = 7, height= 7)
      ggsave(paste0(dirPlots,"/cum_updates_",myticker,".png"),width = 7, height= 7)
    }



    if(doPlotMarketImpact){
      browser()

      volume_pos=volume[diffVolume>0]

      DT_dVposResponse=DT_dVpos[is.finite(diffPriceAuction) & imbalance!=0]

      impactEps_dVpos=DT_dVposResponse[,.(priceImpactAvg=mean(sign(diffImbalance)*diffPriceAuction*diffVolume/volume[.N],na.rm=TRUE),
                                          priceImpactMed=median(sign(diffImbalance)*diffPriceAuction*diffVolume/volume[.N],na.rm=TRUE),
                                          priceImpactSd=sd(sign(diffImbalance)*diffPriceAuction*diffVolume/volume[.N],na.rm=TRUE)/sqrt(.N)),
                                       by="HHMMSS"]

      impactEps_dVpos_signI=DT_dVposResponse[,.(priceImpactAvg=mean(sign(diffImbalance)*diffPriceAuction*diffVolume/volume[.N],na.rm = TRUE),
                                                priceImpactMed=median(sign(diffImbalance)*diffPriceAuction*diffVolume/volume[.N],na.rm=TRUE),
                                                priceImpactSd=sd(sign(diffImbalance)*diffPriceAuction*diffVolume/volume[.N],na.rm=TRUE)/sqrt(.N)),by="HHMMSS,signIdI"]


      impactEps_dVpos_signI=impactEps_dVpos_signI[signIdI!=0]

      main_message=paste0(myticker,", ",myauction," auction, new order")
      (qplot(data=impactEps_dVpos,HHMMSS,priceImpactAvg,geom =c('point',"smooth"),main=main_message,ylab="impact",fill=I(3),xlab="")+theme_bw_latex#+ #geom_errorbar(aes(ymin=priceImpactAvg-2*priceImpactSd, ymax=priceImpactAvg+2*priceImpactSd), width=.1)+geom_abline(aes(intercept=0,slope=0),linetype="dotted"))
      )
      file_plot=paste0(dirPlots,"/","impact_pos_",myticker,"_",myauction,"_coarseFactor",coarseFactor,".pdf")
      if(toPDF){
        ggsave(file_plot,width = 7, height= 7)
      }

      (qplot(data=impactEps_dVpos_signI,HHMMSS,priceImpactAvg,geom =c('point',"smooth"),main=main_message,ylab="conditional impact",group=signIdI,col=signIdI,fill=signIdI,xlab="")+theme_bw_latex
        #+ geom_errorbar(aes(min=priceImpactAvg-2*priceImpactSd, ymax=priceImpactAvg+2*priceImpactSd), width=.1)
        +geom_abline(aes(intercept=0,slope=0),linetype="dotted"))
      file_plot=paste0(dirPlots,"/","conditional_impact_pos_",myticker,"_",myauction,"_coarseFactor",coarseFactor,".pdf")
      if(toPDF){
        ggsave(file_plot,width = 7, height= 7)
      }


    }

    if(doPlotEventRates){

      imbalance[,`:=`(numEvents=.N,cumFracEvents=(1:.N)/.N),by=day]
      price[,`:=`(numEvents=.N,cumFracEvents=(1:.N)/.N),by=day]
      volume[,`:=`(numEvents=.N,cumFracEvents=(1:.N)/.N),by=day]

      allHHMMSS=imbalance[,sort(unique(HHMMSS))]
      if(myauction=="open"){
        HHMMSS_auction=HHMMSS_open
      }
      if(myauction=="close"){
        HHMMSS_auction=HHMMSS_close
      }
      allHHMMSS=c(allHHMMSS,HHMMSS_auction)

      alldays=data.table(day=imbalance[,unique(day)])
      alldaysHHMMSS=alldays[,.(HHMMSS=allHHMMSS),by=day]
      setkeyv(alldaysHHMMSS,c("day","HHMMSS"))

      setkeyv(imbalance,c("day","HHMMSS"))
      imbalance=merge(alldaysHHMMSS,imbalance,all.x=TRUE)
      imbalance[,cumFracEvents:=na.locf(cumFracEvents,na.rm=FALSE),by=day]
      imbalance[is.na(cumFracEvents),cumFracEvents:=0]
      imbalance[HHMMSS==HHMMSS_auction,cumFracEvents:=1]


      alldays=data.table(day=price[,unique(day)])
      alldaysHHMMSS=alldays[,.(HHMMSS=allHHMMSS),by=day]
      setkeyv(alldaysHHMMSS,c("day","HHMMSS"))

      setkeyv(price,c("day","HHMMSS"))
      price=merge(alldaysHHMMSS,price,all.x=TRUE)
      price[,cumFracEvents:=na.locf(cumFracEvents,na.rm=FALSE),by=day]
      price[is.na(cumFracEvents),cumFracEvents:=0]
      price[HHMMSS==HHMMSS_auction,cumFracEvents:=1]

      alldays=data.table(day=volume[,unique(day)])
      alldaysHHMMSS=alldays[,.(HHMMSS=allHHMMSS),by=day]
      setkeyv(alldaysHHMMSS,c("day","HHMMSS"))

      setkeyv(volume,c("day","HHMMSS"))
      volume=merge(alldaysHHMMSS,volume,all.x=TRUE)
      volume[,cumFracEvents:=na.locf(cumFracEvents,na.rm=FALSE),by=day]
      volume[is.na(cumFracEvents),cumFracEvents:=0]
      volume[HHMMSS==HHMMSS_auction,cumFracEvents:=1]

      # fractionEventsHHMMSS_imbalance=imbalance[,.(fraction=.N/numEvents[1]),by="day,HHMMSS"]
      # fractionEventsHHMMSS_price=price[,.(fraction=.N/numEvents[1]),by="day,HHMMSS"]
      # fractionEventsHHMMSS_volume=volume[,.(fraction=.N/numEvents[1]),by="day,HHMMSS"]

      meanCumFracEvents_imbalance=imbalance[,.(cumFracEvents=tail(cumFracEvents,1)),by="day,HHMMSS"][,.(imb=mean(cumFracEvents)),by=HHMMSS]
      meanCumFracEvents_price=price[,.(cumFracEvents=tail(cumFracEvents,1)),by="day,HHMMSS"][,.(pr=mean(cumFracEvents)),by=HHMMSS]
      meanCumFracEvents_volume=volume[,.(cumFracEvents=tail(cumFracEvents,1)),by="day,HHMMSS"][,.(vol=mean(cumFracEvents)),by=HHMMSS]
      setkey(meanCumFracEvents_imbalance,HHMMSS)
      setkey(meanCumFracEvents_price,HHMMSS)
      setkey(meanCumFracEvents_volume,HHMMSS)
      meanCumFracEvents=meanCumFracEvents_imbalance[meanCumFracEvents_price[meanCumFracEvents_volume]]
      setnames(meanCumFracEvents,c("imb","pr","vol"),c("imbalance","price","volume"))
      #  meanCumFracEvents[,variable:="mean"]

      medianCumFracEvents_imbalance=imbalance[,.(cumFracEvents=tail(cumFracEvents,1)),by="day,HHMMSS"][,.(imb=median(cumFracEvents)),by=HHMMSS]
      medianCumFracEvents_price=price[,.(cumFracEvents=tail(cumFracEvents,1)),by="day,HHMMSS"][,.(pr=median(cumFracEvents)),by=HHMMSS]
      medianCumFracEvents_volume=volume[,.(cumFracEvents=tail(cumFracEvents,1)),by="day,HHMMSS"][,.(vol=median(cumFracEvents)),by=HHMMSS]
      setkey(medianCumFracEvents_imbalance,HHMMSS)
      setkey(medianCumFracEvents_price,HHMMSS)
      setkey(medianCumFracEvents_volume,HHMMSS)
      medianCumFracEvents=medianCumFracEvents_imbalance[medianCumFracEvents_price[medianCumFracEvents_volume]]
      setnames(medianCumFracEvents,c("imb","pr","vol"),c("imbalance","price","volume"))
      #medianCumFracEvents[,variable:="median"]

      # setkeyv(medianCumFracEvents,c("HHMMSS","variable"))
      # setkeyv(meanCumFracEvents,c("HHMMSS","variable"))
      # statsCumFracEvents=rbind(meanCumFracEvents,medianCumFracEvents)

      if(toPDF){
        qplot(data=melt(meanCumFracEvents,id.vars="HHMMSS"),HHMMSS,value,geom="line",col=variable,group=variable,size=I(1.2),xlab="",ylab="average cumulated fraction of updates",main=paste0(myticker,", ",myauction," auction"))+theme_bw_latex+theme(legend.title=element_blank())
        filename=paste0(dirPlots,"/meanCumFracEvents_",myauction,"_",myticker,"_coarseFactor",coarseFactor,".pdf")
        ggsave(filename,width = 7, height= 7)

        qplot(data=melt(medianCumFracEvents,id.vars="HHMMSS"),HHMMSS,value,geom="line",col=variable,group=variable,size=I(1.2),xlab="",ylab="median cumulated fraction of updates",main=paste0(myticker,", ",myauction," auction"))+theme_bw_latex+theme(legend.title=element_blank())
        filename=paste0(dirPlots,"/medianCumFracEvents_",myauction,"_",myticker,"_coarseFactor",coarseFactor,".pdf")
        ggsave(filename,width = 7, height= 7)

      }
      newlines_AICs=data.table()
      newlines_AICs=rbind(newlines_AICs,tryCatch(meanCumFracEvents[imbalance>0 & imbalance<1,.(what="imbalance",variable="cumfracevent",type="linear",test="AIC",value=AIC(nls(formula=imbalance~b*.I+c,start=list(b=0.01,c=0))),ticker=myticker)], error=function(e) return(data.table())))
      newlines_AICs=rbind(newlines_AICs,tryCatch(meanCumFracEvents[imbalance>0 & imbalance<1,.(what="imbalance",variable="cumfracevent",type="linear",test="BIC",value=BIC(nls(formula=imbalance~b*.I+c,start=list(b=0.01,c=0))),ticker=myticker)], error=function(e) return(data.table())))
      newlines_AICs=rbind(newlines_AICs,tryCatch(meanCumFracEvents[imbalance>0 & imbalance<1,.(what="imbalance",variable="cumfracevent",type="square",test="AIC",value=AIC(nls(formula=imbalance~a*.I^2+b*.I+c,start=list(a=0,b=0.01,c=0))),ticker=myticker)], error=function(e) return(data.table())))
      newlines_AICs=rbind(newlines_AICs,tryCatch(meanCumFracEvents[imbalance>0 & imbalance<1,.(what="imbalance",variable="cumfracevent",type="square",test="BIC",value=BIC(nls(formula=imbalance~a*.I^2+b*.I+c,start=list(a=0,b=0.01,c=0))),ticker=myticker)], error=function(e) return(data.table())))

      newlines_AICs=rbind(newlines_AICs,tryCatch(meanCumFracEvents[price>0 & price<1,.(what="price",variable="cumfracevent",type="linear",test="AIC",value=AIC(nls(formula=price~b*.I+c,start=list(b=0.01,c=0))),ticker=myticker)], error=function(e) return(data.table())))
      newlines_AICs=rbind(newlines_AICs,tryCatch(meanCumFracEvents[price>0 & price<1,.(what="price",variable="cumfracevent",type="linear",test="BIC",value=BIC(nls(formula=price~b*.I+c,start=list(b=0.01,c=0))),ticker=myticker)], error=function(e) return(data.table())))
      newlines_AICs=rbind(newlines_AICs,tryCatch(meanCumFracEvents[price>0 & price<1,.(what="price",variable="cumfracevent",type="square",test="AIC",value=AIC(nls(formula=price~a*.I^2+b*.I+c,start=list(a=0,b=0.01,c=0))),ticker=myticker)], error=function(e) return(data.table())))
      newlines_AICs=rbind(newlines_AICs,tryCatch(meanCumFracEvents[price>0 & price<1,.(what="price",variable="cumfracevent",type="square",test="BIC",value=BIC(nls(formula=price~a*.I^2+b*.I+c,start=list(a=0,b=0.01,c=0))),ticker=myticker)], error=function(e) return(data.table())))

      newlines_AICs=rbind(newlines_AICs,tryCatch(meanCumFracEvents[volume>0 & volume<1,.(what="volume",variable="cumfracevent",type="linear",test="AIC",value=AIC(nls(formula=volume~b*.I+c,start=list(b=0.01,c=0))),ticker=myticker)], error=function(e) return(data.table())))
      newlines_AICs=rbind(newlines_AICs,tryCatch(meanCumFracEvents[volume>0 & volume<1,.(what="volume",variable="cumfracevent",type="linear",test="BIC",value=BIC(nls(formula=volume~b*.I+c,start=list(b=0.01,c=0))),ticker=myticker)], error=function(e) return(data.table())))
      newlines_AICs=rbind(newlines_AICs,tryCatch(meanCumFracEvents[volume>0 & volume<1,.(what="volume",variable="cumfracevent",type="square",test="AIC",value=AIC(nls(formula=volume~a*.I^2+b*.I+c,start=list(a=0,b=0.01,c=0))),ticker=myticker)], error=function(e) return(data.table())))
      newlines_AICs=rbind(newlines_AICs,tryCatch(meanCumFracEvents[volume>0 & volume<1,.(what="volume",variable="cumfracevent",type="square",test="BIC",value=BIC(nls(formula=volume~a*.I^2+b*.I+c,start=list(a=0,b=0.01,c=0))),ticker=myticker)], error=function(e) return(data.table())))



      if(nrow(newlines_AICs)>0){
        AICs=rbind(AICs,newlines_AICs)
      }


      #
      #       fractionMeanImbalance=fractionEventsImbalanceHHMMSS[,.(ratio=mean(fraction)),by=HHMMSS]
      #       fractionMeanPrice=fractionEventsPriceHHMMSS[,.(ratio=mean(fraction)),by=HHMMSS]
      #       fractionMeanVolume=fractionEventsVolumeHHMMSS[,.(ratio=mean(fraction)),by=HHMMSS]
      #
      #       setorder(fractionMeanImbalance,HHMMSS)
      #       setorder(fractionMeanPrice,HHMMSS)
      #       setorder(fractionMeanVolume,HHMMSS)
      #
      #       fractionMeanImbalance[,normCumSum:=cumsum(ratio)/sum(ratio)]
      #       fractionMeanPrice[,normCumSum:=cumsum(ratio)/sum(ratio)]
      #       fractionMeanVolume[,normCumSum:=cumsum(ratio)/sum(ratio)]
      #
      #       fractionMeanImbalance[,TTA:=as.numeric(HHMMSS[.N]-HHMMSS)/coarseFactor]
      #
      #       (qplot(data=fractionByDayImbalance[,.(ratio=mean(fraction)),by=HHMMSS],HHMMSS,ratio,geom="line",xlab="",ylab="fraction of events",main=paste0(myticker,", ",myauction," auction, imbalance changes"),log='')+theme_bw_latex)
      #       file_plot=paste0(dirPlots,"/","events_fraction_imbalance_",myticker,"_",myauction,"_coarseFactor",coarseFactor,".pdf")
      #       if(toPDF){
      #         ggsave(file_plot,width = 7, height= 7)
      #       }

      #   (qplot(data=fractionByDayPrice[,.(ratio=mean(fraction)),by=HHMMSS],HHMMSS,ratio,geom="line",ylab="fraction of events",main=paste0(myticker,", ",myauction," auction, match price changes"),log='')+theme_bw_latex)
      #   file_plot=paste0(dirPlots,"/","events_fraction_price_",myticker,"_",myauction,"_coarseFactor",coarseFactor,".pdf")
      #   if(toPDF) ggsave(file_plot,width = 7, height= 7)
      #
      #   (qplot(data=fractionByDayVolume[,.(ratio=mean(fraction)),by=HHMMSS],HHMMSS,ratio,geom="line",ylab="fraction of events",main=paste0(myticker,", ",myauction," auction, matched volume changes"),log='',xlab="")+theme_bw_latex)
      #   file_plot=paste0(dirPlots,"/","events_fraction_volume_",myticker,"_",myauction,"_coarseFactor",coarseFactor,".pdf")
      #   if(toPDF) ggsave(file_plot,width = 7, height= 7)
      # }
      #


    }


    ### increase rates, i.e. how quantities change as a function of time
    if(doPlotIncreaseRates){
      browser()
      volume[,scaledVolume:=volume/volume[.N],by=day]
      volume=na.omit(volume)

      allHHMMSS=volume[,sort(unique(HHMMSS))]
      if(myauction=="open"){
        HHMMSS_auction=HHMMSS_open
      }
      if(myauction=="close"){
        HHMMSS_auction=HHMMSS_close
      }
      allHHMMSS=c(allHHMMSS,HHMMSS_auction)


      alldays=data.table(day=volume[,unique(day)])
      alldaysHHMMSS=alldays[,.(HHMMSS=allHHMMSS),by=day]
      setkeyv(alldaysHHMMSS,c("day","HHMMSS"))

      setkeyv(volume,c("day","HHMMSS"))
      volume=merge(alldaysHHMMSS,volume,all.x=TRUE)
      volume[,scaledVolume:=na.locf(scaledVolume,na.rm=FALSE),by=day]
      volume[is.na(scaledVolume),scaledVolume:=0]
      volume[HHMMSS==HHMMSS_auction,scaledVolume:=1]

      meanScaledVolume=volume[,.(scaledVolume=tail(scaledVolume,1)),by="day,HHMMSS"][,.(scaledVolume=mean(scaledVolume),variable="mean"),by=HHMMSS]
      medianScaledVolume=volume[,.(scaledVolume=tail(scaledVolume,1)),by="day,HHMMSS"][,.(scaledVolume=median(scaledVolume),variable="median"),by=HHMMSS]

      statsScaledVolume=rbind(meanScaledVolume,medianScaledVolume)
      if(nrow(statsScaledVolume)<5){
        next
      }
      t50=rbind(t50,data.table(ticker=myticker,exchange=allTickerRes[ticker==myticker,exchange[1]],auction=myauction,t50_mean=meanScaledVolume[scaledVolume>=0.5][1,]$HHMMSS,t50_median=medianScaledVolume[scaledVolume>=0.5][1,]$HHMMSS))
      # qplot(data=meanScaledVolume,HHMMSS,scaledVolume,geom="line",xlab="",ylab="average volume(t)/volume(auction)",size=I(1.2),main=paste0(myticker,", ",myauction," auction"))+theme_bw_latex
      # if(toPDF){
      #   filename=paste0(dirPlots,"/meanScaledVolume_",myauction,"_",myticker,"_coarseFactor",coarseFactor,".pdf")
      #   ggsave(filename,width = 7, height= 7)
      # }
      #
      # qplot(data=medianScaledVolume,HHMMSS,scaledVolume,geom="line",xlab="",ylab="median volume(t)/volume(auction)",size=I(1.2),main=paste0(myticker,", ",myauction," auction"))+theme_bw_latex
      # if(toPDF){
      #   filename=paste0(dirPlots,"/medianScaledVolume_",myauction,"_",myticker,"_coarseFactor",coarseFactor,".pdf")
      #   ggsave(filename,width = 7, height= 7)
      # }

      qplot(data=statsScaledVolume,HHMMSS,scaledVolume,group=variable,col=variable,geom="line",xlab="",ylab=expression(paste({W^{x}} (t)/V(t^x))),size=I(1.2),main=paste0(myticker,", ",myauction," auction"))+theme_bw_latex+theme(legend.title=element_blank())
      if(toPDF){
        filename=paste0(dirPlots,"/statsScaledVolume_",myauction,"_",myticker,"_coarseFactor",coarseFactor,".pdf")
        ggsave(filename,width = 7, height= 7)
      }

      newlines_AICs=data.table()

      myNLS2=tryCatch(statsScaledVolume[variable=="median" & scaledVolume >0 & scaledVolume<1 ,nls(formula=scaledVolume~a*.I^2+b*.I+d,start = c(a=0.001,b=0.01,d=0.1))],error=function(e) return(NA))
      myNLS3=tryCatch(statsScaledVolume[variable=="median" & scaledVolume >0 & scaledVolume<1 ,nls(formula=scaledVolume~f*.I^3+a*.I^2+b*.I+d,start = c(a=0.001,b=0.01,d=0.1,f=0.00001 ))],error=function(e) return(NA))
      myLM=  tryCatch(statsScaledVolume[variable=="median" & scaledVolume >0 & scaledVolume<1 ,nls(formula=scaledVolume~b*.I+d,start = c(b=0.01,d=0.1))],error=function(e) return(NA))
      if(is.na(myLM) || is.na(myNLS2) || is.na(myNLS3)){
        next
      }

      myICCI_12=icci(myNLS2,myLM)
      myICCI_23=icci(myNLS2,myNLS3)
      newline=meanScaledVolume[,.(ticker=myticker,polyn3=AIC(myNLS3),parabola=AIC(myNLS2),
                                  linear=AIC(myLM),a=coef(myNLS2)["a"],
                                  lower_Delta_AIC_12=myICCI_12$AICci[1],lower_Delta_BIC_12=myICCI_12$BICci[1],
                                  upper_Delta_AIC_12=myICCI_12$AICci[2],upper_Delta_BIC_12=myICCI_12$BICci[2],
                                  lower_Delta_AIC_23=myICCI_23$AICci[1],lower_Delta_BIC_23=myICCI_23$BICci[1],
                                  upper_Delta_AIC_23=myICCI_23$AICci[2],upper_Delta_BIC_23=myICCI_23$BICci[2],
                                  auction=myauction,what="scaledVolume")]

      AICs=rbind(AICs,newline)

      if(nrow(AICs)>0){
        AICs=AICs[ticker %in% tickers_ARCA]
        saveRDS(AICs,file=paste0("AICs_increaseRates_linear_vs_square_ARCA_coarseFactor",coarseFactor,".rds"))
      }

      # file_plot=paste0(dirPlots,"/","fraction_volume_",myticker,"_",myauction,"_coarseFactor",coarseFactor,".pdf")
      # ggsave(file_plot,width = 7, height= 7)


      #         meanScaledQuantities=DT[,.(scaledVolume=mean(scaledVolume,na.rm=TRUE),
      #                            scaledImbalanceTime=mean(scaledImbalanceTime,na.rm=TRUE),
      #                            scaledPrice=mean(scaledPrice,na.rm=TRUE),
      #                            scaledDiffVolume=mean(scaledDiffVolume,na.rm=TRUE),
      #                            scaledDiffImbalanceTime=mean(scaledDiffImbalanceTime,na.rm=TRUE),
      #                            scaledAbsDiffPrice=mean(abs(scaledDiffPrice)),na.rm=TRUE),by="HHMMSS"]
      # medianScaledQuantities=DT[,.(scaledVolume=median(scaledVolume,na.rm=TRUE),
      #                              scaledImbalanceTime=median(scaledImbalanceTime,na.rm=TRUE),
      #                              scaledPrice=median(scaledPrice,na.rm=TRUE),
      #                              scaledDiffVolume=median(scaledDiffVolume,na.rm=TRUE),
      #                              scaledDiffImbalanceTime=median(scaledDiffImbalanceTime,na.rm=TRUE),
      #                              scaledAbsDiffPrice=median(abs(scaledDiffPrice)),na.rm=TRUE),by="HHMMSS"]
      # if(myauction=="open"){
      #   myxlab="seconds since 8:00"
      #   medianScaledQuantities[,mytime:=as.integer(HHMMSS)%%86400-(8)*3600]
      #   meanScaledQuantities[,mytime:=as.integer(HHMMSS)%%86400-8*3600]
      # }else{
      #   myxlab="seconds since 15:00"
      #   medianScaledQuantities[,mytime:=as.integer(HHMMSS)%%86400-(15)*3600]
      #   meanScaledQuantities[,mytime:=as.integer(HHMMSS)%%86400-15*3600]
      # }
      # if(nrow(meanScaledQuantities)<10){
      #   next
      # }
      # # print(qplot(data=meanScaledQuantities,HHMMSS,scaledDiffVolume,geom="line",main=paste0(myticker,", ",auction," auction"))+theme_bw_latex)
      # # print(qplot(data=meanScaledQuantities,HHMMSS,scaledAbsDiffPrice,geom="line",main=paste0(myticker,", ",auction," auction"))+theme_bw_latex)
      # print(qplot(data=na.omit(meanScaledQuantities),mytime,scaledVolume,geom="line",main=paste0(myticker,", ",myauction," auction"),xlab=myxlab)+theme_bw_latex+geom_smooth(method="nls",formula=y~a*x^2+b*x+d,method.args=list(start = c(a=0.001,b=0.01,d=0.1)),se=FALSE))



      # myNLS=meanScaledQuantities[,nls(formula=scaledVolume~a*mytime^2+b*mytime+d,start = c(a=0.001,b=0.01,d=0.1))]
      # myLM=meanScaledQuantities[,lm(scaledVolume~mytime)]
      # myICCI=icci(myNLS,myLM)
      # newline=meanScaledQuantities[,.(ticker=myticker,parabola=AIC(myNLS),
      #                                 linear=AIC(myLM),a=coef(myNLS)["a"],
      #                                 lower_Delta_AIC=myICCI$AICci[2],lower_Delta_BIC=myICCI$BICci[2],
      #                                 upper_Delta_AIC=myICCI$AICci[2],upper_Delta_BIC=myICCI$BICci[2],auction=myauction)]
      # AICs=rbind(AICs,newline)
      #
      # file_plot=paste0(dirPlots,"/","fraction_volume_",myticker,"_",myauction,"_coarseFactor",coarseFactor,".pdf")
      # ggsave(file_plot,width = 7, height= 7)


      # print(qplot(data=fractionByDayImbalance[,.(ratio=mean(fraction)),by=HHMMSS],HHMMSS,ratio,geom="line",xlab="time",ylab="fraction of events",main=paste0(myticker,", ",auction," auction, imbalance changes"),log='')+theme_bw_latex)
      # file_plot=paste0(dirPlots,"/","events_fraction_imbalance_",myticker,"_",auction,"_coarseFactor",coarseFactor,".pdf")
      # ggsave(file_plot,width = 7, height= 7)
      #
      # print(qplot(data=fractionByDayPrice[,.(ratio=mean(fraction)),by=HHMMSS],HHMMSS,ratio,geom="line",ylab="fraction of events",main=paste0(myticker,", ",auction," auction, match price changes"),log='')+theme_bw_latex)
      # file_plot=paste0(dirPlots,"/","events_fraction_price_",myticker,"_",auction,"_coarseFactor",coarseFactor,".pdf")
      # ggsave(file_plot,width = 7, height= 7)
      #
      # print(qplot(data=fractionByDayVolume[,.(ratio=mean(fraction)),by=HHMMSS],HHMMSS,ratio,geom="line",xlab="time",ylab="fraction of events",main=paste0(myticker,", ",auction," auction, matched volume changes"),log='',xlab="")+theme_bw_latex)
      # file_plot=paste0(dirPlots,"/","events_fraction_volume_",myticker,"_",auction,"_coarseFactor",coarseFactor,".pdf")
      # ggsave(file_plot)
    }



    #### now response functions
    if(doPlotResponseFunctions){
      if(DT[,exchange[1]]!="ARCA"){
        next
      }
      if(withRealPrices){
        if(myauction=="open"){
          bidaskPrices=bidaskPricesDay[seconds>=8*3600 & seconds<9.5*3600]
        }
        if(myauction=="close"){
          bidaskPrices=bidaskPricesDay[seconds>=15*3600 & seconds<16*3600]
        }
        bidaskPrices_seconds=bidaskPrices[,.(bid=bid[.N],ask=ask[.N],mid=mid[.N],spread=spread[.N]),by=date]
        setkey(bidaskPrices_seconds,date)
      }
      imbalance_second=imbalance[,.(imbalance=imbalance[.N]),by=date]   # takes the last quote for each second
      price_second=price[,.(price=price[.N]),by=date]
      volume_second=volume[,.(volume=volume[.N]),by=date]
      pvi=merge(merge(price_second,imbalance_second,all=TRUE),volume_second,all=TRUE)
      if(withRealPrices){
        pvi=merge(pvi,bidaskPrices_seconds,all=TRUE)
      }
      pvi[,`:=`(price=na.locf(price,na.rm = FALSE),
                imbalance=na.locf(imbalance,na.rm = FALSE),
                volume=na.locf(volume,na.rm = FALSE)) ]
      if(withRealPrices){
        pvi[,`:=`(bid=na.locf(bid,na.rm = FALSE),
                  ask=na.locf(ask,na.rm = FALSE),
                  mid=na.locf(mid,na.rm = FALSE),
                  spread=na.locf(spread,na.rm = FALSE))]
      }

      pvi[,day:=as.Date(date)]
      pvi[,`:=`(dP=log(price[.N])-log(price),
                dI=c(diff(imbalance),NA),
                dI_Ito=c(NA,diff(imbalance)),
                dV=c(diff(volume),NA),
                dV_scaled=c(diff(volume),NA)/volume[.N]),by="day"]

      if(withRealPrices){
        pvi[,dP_real:=log(price[.N])-log(mid),by="day"]
      }

      if(myauction=="close"){
        pvi=pvi[hour(date)>=15]
      }
      pvi[,HHMMSS:=as.POSIXct(floor((hour(date)*3600+minute(date)*60+second(date))/coarseFactor)*coarseFactor, origin = "2000-01-01",tz="UTC")]
      pvi[,signIdI:=as.factor(sign(imbalance)*sign(dI))]
      pvi[,signIdI_ito:=as.factor(sign(imbalance)*sign(dI_Ito))]
      pvi[sign(imbalance)==-sign(shift(imbalance,1)),signIIdI:=as.factor(sign(imbalance)*sign(dI))]
      pvi[sign(imbalance)==sign(shift(imbalance,1)),signIIdI_eq:=as.factor(sign(imbalance)*sign(dI))]

      #     pvi=na.omit(pvi)


      response_dVpos=pvi[dV>0 & dI !=0 & is.finite(dP) & imbalance!=0,.(priceResponseAvg=median(sign(dI)*dP,na.rm=TRUE),
                                                                        priceResponseSd=sd(sign(dI)*dP,na.rm=TRUE)/sqrt(.N),
                                                                        priceImpactAvg=median(sign(dI)*dP*dV_scaled,na.rm=TRUE),
                                                                        priceImpactSd=sd(sign(dI)*dP*dV_scaled,na.rm=TRUE)/sqrt(.N)),
                         ## avg impact over HHMMSS days
                         by="HHMMSS"]

      response_dVpos_signI=pvi[dV>0 &  dI !=0 & is.finite(dP) & imbalance!=0,.(priceResponseAvg=median(sign(dI)*dP,na.rm=TRUE),
                                                                               priceResponseSd=sd(sign(dI)*dP,na.rm=TRUE)/sqrt(.N),
                                                                               priceImpactAvg=median(sign(dI)*dP*dV_scaled,na.rm=TRUE),
                                                                               priceImpactSd=sd(sign(dI)*dP*dV_scaled,na.rm=TRUE)/sqrt(.N)),

                               by="HHMMSS,signIdI"]
      response_dVpos_signI=response_dVpos_signI[signIdI!=0]



      if(withRealPrices){
        response_dVposReal=pvi[dV>0 & is.finite(dP_real) & imbalance!=0,.(priceResponseAvg=median(sign(dI)*dP_real,na.rm=TRUE),
                                                                          priceResponseSd=sd(sign(dI)*dP_real,na.rm=TRUE)/sqrt(.N),
                                                                          priceImpactAvg=median(sign(dI)*dP_real*dV_scaled,na.rm=TRUE),
                                                                          priceImpactSd=sd(sign(dI)*dP_real*dV_scaled,na.rm=TRUE)/sqrt(.N)),
                               ## avg impact over HHMMSS days
                               by="HHMMSS"]

        response_dVposReal_signI=pvi[dV>0 & is.finite(dP_real) & imbalance!=0,.(priceResponseAvg=median(sign(dI)*dP_real,na.rm=TRUE),
                                                                                priceResponseSd=sd(sign(dI)*dP_real,na.rm=TRUE)/sqrt(.N),
                                                                                priceImpactAvg=median(sign(dI)*dP_real*dV_scaled,na.rm=TRUE),
                                                                                priceImpactSd=sd(sign(dI)*dP_real*dV_scaled,na.rm=TRUE)/sqrt(.N)),

                                     by="HHMMSS,signIdI"]
        response_dVposReal_signI=response_dVposReal_signI[signIdI!=0]

        response_dVposReal_signII=pvi[dV>0 & is.finite(dP_real) & imbalance!=0,.(priceResponseAvg=median(sign(dI)*dP_real,na.rm=TRUE),
                                                                                 priceResponseSd=sd(sign(dI)*dP_real,na.rm=TRUE)/sqrt(.N),
                                                                                 priceImpactAvg=median(sign(dI)*dP_real*dV_scaled,na.rm=TRUE),
                                                                                 priceImpactSd=sd(sign(dI)*dP_real*dV_scaled,na.rm=TRUE)/sqrt(.N)),

                                      by="HHMMSS,signIIdI"]

        response_dVposReal_signII_eq=pvi[dV>0 & is.finite(dP_real) & imbalance!=0,.(priceResponseAvg=median(sign(dI)*dP_real,na.rm=TRUE),
                                                                                    priceResponseSd=sd(sign(dI)*dP_real,na.rm=TRUE)/sqrt(.N),
                                                                                    priceImpactAvg=median(sign(dI)*dP_real*dV_scaled,na.rm=TRUE),
                                                                                    priceImpactSd=sd(sign(dI)*dP_real*dV_scaled,na.rm=TRUE)/sqrt(.N)),

                                         by="HHMMSS,signIIdI_eq"]


        response_dVposRealShift_signII=pvi[dV>0 & is.finite(dP_real) & imbalance!=0,.(priceResponseAvg=median(sign(dI)*shift(dP_real,1),na.rm=TRUE),
                                                                                      priceResponseSd=sd(sign(dI)*shift(dP_real,1),na.rm=TRUE)/sqrt(.N),
                                                                                      priceImpactAvg=median(sign(dI)*shift(dP_real,1)*dV_scaled,na.rm=TRUE),
                                                                                      priceImpactSd=sd(sign(dI)*shift(dP_real,1)*dV_scaled,na.rm=TRUE)/sqrt(.N)),

                                           by="HHMMSS,signIIdI"]


      }


      #    setnames(impactEps_dVpos_signI,"signIdI","imbalance increase")
      main_message=paste0(myticker,", ",myauction," auction, new order")
      qplot(data=na.omit(response_dVpos,cols = "priceResponseAvg"),HHMMSS,priceResponseAvg,geom =c('point',"smooth"),main=main_message,ylab="response function R",fill=I(32),xlab="")+theme_bw_latex+ geom_errorbar(aes(ymin=priceResponseAvg-2*priceResponseSd, ymax=priceResponseAvg+2*priceResponseSd), width=.1)+geom_abline(aes(intercept=0,slope=0),linetype="dotted")
      file_plot=paste0(dirPlots,"/","response_pos_",myticker,"_",myauction,"_coarseFactor",coarseFactor,".pdf")
      if(toPDF) ggsave(file_plot,width = 7, height= 7)

      (qplot(data=na.omit(response_dVpos,cols="priceImpactAvg"),HHMMSS,priceImpactAvg,geom =c('point',"smooth"),main=main_message,ylab="impact function R",fill=I(32),xlab="")+theme_bw_latex+geom_abline(aes(intercept=0,slope=0),linetype="dotted"))
      file_plot=paste0(dirPlots,"/","impact_pos_",myticker,"_",myauction,"_coarseFactor",coarseFactor,".pdf")
      if(toPDF) ggsave(file_plot,width = 7, height= 7)

      (qplot(data=response_dVpos_signI,HHMMSS,priceResponseAvg,geom =c('point',"smooth"),main=main_message,ylab="conditional response function R",group=signIdI,col=signIdI,fill=signIdI,xlab="")+theme_bw_latex+ geom_errorbar(aes(ymin=priceResponseAvg-2*priceResponseSd, ymax=priceResponseAvg+2*priceResponseSd), width=.1)+geom_abline(aes(intercept=0,slope=0),linetype="dotted"))
      file_plot=paste0(dirPlots,"/","conditional_response_pos_",myticker,"_",myauction,"_coarseFactor",coarseFactor,".pdf")
      if(toPDF) ggsave(file_plot,width = 7, height= 7)

      (qplot(data=na.omit(response_dVpos_signI,cols="priceImpactAvg"),HHMMSS,priceImpactAvg,geom =c('point',"smooth"),main=main_message,ylab="impact function R",fill=I(32),group=signIdI,col=signIdI,fill=signIdI,xlab="")+theme_bw_latex+geom_abline(aes(intercept=0,slope=0),linetype="dotted"))
      file_plot=paste0(dirPlots,"/","conditional_impact_pos_",myticker,"_",myauction,"_coarseFactor",coarseFactor,".pdf")
      if(toPDF) ggsave(file_plot,width = 7, height= 7)

      if(withRealPrices){
        (qplot(data=na.omit(response_dVposReal,cols="priceResponseAvg"),HHMMSS,priceResponseAvg,geom =c('point',"smooth"),main=main_message,ylab="mid response function R",fill=I(32),xlab="")+theme_bw_latex+ geom_errorbar(aes(ymin=priceResponseAvg-2*priceResponseSd, ymax=priceResponseAvg+2*priceResponseSd), width=.1)+geom_abline(aes(intercept=0,slope=0),linetype="dotted"))
        file_plot=paste0(dirPlots,"/","response_mid_pos_",myticker,"_",myauction,"_coarseFactor",coarseFactor,".pdf")
        if(toPDF) ggsave(file_plot,width = 7, height= 7)

        (qplot(data=na.omit(response_dVposReal),HHMMSS,priceImpactAvg,geom =c('point',"smooth"),main=main_message,ylab="mid impact function R",fill=I(32),xlab="")+theme_bw_latex+geom_abline(aes(intercept=0,slope=0),linetype="dotted"))
        file_plot=paste0(dirPlots,"/","impact_mid_pos_",myticker,"_",myauction,"_coarseFactor",coarseFactor,".pdf")
        if(toPDF) ggsave(file_plot,width = 7, height= 7)


        (qplot(data=response_dVposReal_signI,HHMMSS,priceResponseAvg,geom =c('point',"smooth"),main=main_message,ylab="conditional response function R",group=signIdI,col=signIdI,fill=signIdI,xlab="")+theme_bw_latex+ geom_errorbar(aes(ymin=priceResponseAvg-2*priceResponseSd, ymax=priceResponseAvg+2*priceResponseSd), width=.1)+geom_abline(aes(intercept=0,slope=0),linetype="dotted"))
        file_plot=paste0(dirPlots,"/","conditional_response_mid_pos_",myticker,"_",myauction,"_coarseFactor",coarseFactor,".pdf")
        if(toPDF) ggsave(file_plot,width = 7, height= 7)


        (qplot(data=na.omit(response_dVposReal_signI),HHMMSS,priceImpactAvg,geom =c('point',"smooth"),main=main_message,ylab="impact function R",fill=I(32),group=signIdI,col=signIdI,fill=signIdI,xlab="")+theme_bw_latex+geom_abline(aes(intercept=0,slope=0),linetype="dotted"))
        file_plot=paste0(dirPlots,"/","conditional_mid_impact_pos_",myticker,"_",myauction,"_coarseFactor",coarseFactor,".pdf")
        if(toPDF) ggsave(file_plot,width = 7, height= 7)



      }

      response_dVneg=pvi[dV<0 & is.finite(dP) & imbalance!=0,.(priceResponseAvg=median(sign(dI)*dP,na.rm=TRUE),
                                                               priceResponseSd=sd(sign(dI)*dP,na.rm=TRUE)/sqrt(.N),
                                                               priceImpactAvg=median(sign(dI)*dP*dV_scaled,na.rm=TRUE),
                                                               priceImpactSd=sd(sign(dI)*dP*dV_scaled,na.rm=TRUE)/sqrt(.N)),
                         ## avg impact over HHMMSS days
                         by="HHMMSS"]

      response_dVneg_signI=pvi[dV<0 & is.finite(dP) & imbalance!=0,.(priceResponseAvg=median(sign(dI)*dP,na.rm=TRUE),
                                                                     priceResponseSd=sd(sign(dI)*dP,na.rm=TRUE)/sqrt(.N),
                                                                     priceImpactAvg=median(sign(dI)*dP*dV_scaled,na.rm=TRUE),
                                                                     priceImpactSd=sd(sign(dI)*dP*dV_scaled,na.rm=TRUE)/sqrt(.N)),

                               by="HHMMSS,signIdI"]
      response_dVneg_signI=response_dVneg_signI[signIdI!=0]



      main_message=paste0(myticker,", ",myauction," auction, cancellation")

      (qplot(data=response_dVneg,HHMMSS,priceResponseAvg,geom =c('point',"smooth"),main=main_message,xlab="",ylab="response function R",fill=I(32))+theme_bw_latex+ geom_errorbar(aes(ymin=priceResponseAvg-2*priceResponseSd, ymax=priceResponseAvg+2*priceResponseSd), width=.1)+geom_abline(aes(intercept=0,slope=0),linetype="dotted"))
      file_plot=paste0(dirPlots,"/","response_neg_",myticker,"_",myauction,"_coarseFactor",coarseFactor,".pdf")
      if(toPDF) ggsave(file_plot,width = 7, height= 7)


      (qplot(data=na.omit(response_dVneg,cols="priceImpactAvg"),HHMMSS,priceImpactAvg,geom =c('point',"smooth"),main=main_message,ylab="impact function R",fill=I(32),xlab="")+theme_bw_latex+geom_abline(aes(intercept=0,slope=0),linetype="dotted"))
      file_plot=paste0(dirPlots,"/","impact_neg_",myticker,"_",myauction,"_coarseFactor",coarseFactor,".pdf")
      if(toPDF) ggsave(file_plot,width = 7, height= 7)


      (qplot(data=response_dVneg_signI,HHMMSS,priceResponseAvg,geom =c('point',"smooth"),main=main_message,ylab="conditional response function R",group=signIdI,col=signIdI,fill=signIdI,xlab="")+theme_bw_latex+ geom_errorbar(aes(ymin=priceResponseAvg-2*priceResponseSd, ymax=priceResponseAvg+2*priceResponseSd), width=.1)+geom_abline(aes(intercept=0,slope=0),linetype="dotted"))
      file_plot=paste0(dirPlots,"/","conditional_response_neg_",myticker,"_",myauction,"_coarseFactor",coarseFactor,".pdf")
      if(toPDF) ggsave(file_plot,width = 7, height= 7)



      (qplot(data=na.omit(response_dVneg_signI,cols="priceImpactAvg"),HHMMSS,priceImpactAvg,geom =c('point',"smooth"),main=main_message,ylab="conditional impact function R",fill=I(32),group=signIdI,col=signIdI,fill=signIdI,xlab="")+theme_bw_latex+geom_abline(aes(intercept=0,slope=0),linetype="dotted"))
      file_plot=paste0(dirPlots,"/","conditional_impact_neg_",myticker,"_",myauction,"_coarseFactor",coarseFactor,".pdf")
      if(toPDF) ggsave(file_plot,width = 7, height= 7)

    }



    # DT_V=DT[selDiffVolume==TRUE]
    #
    # DT_PV=DT[selDiffPrice & selDiffVolume]
    #
    # DT_PV[,coarseTime:=floor((hour(date)*60+minute(date)+second(date)/60)/coarseFactor)*coarseFactor]
    # DT_PV[,retScaledVolume:=exp(c(0,diff(scaledVolume)))-1]
    #
    # DT_PV[,scaledVolume:=volume/volume[.N],by=day]
    # DT_PV[,retScaledVolume:=exp(c(0,diff(scaledVolume)))-1]
    #
    #
    # meanRetScVol=DT_PV[,.(meanRet=median(retScaledVolume),medScVol=median(scaledVolume)),by="coarseTime"]
  }
  return(list(diffLM=diffLM,allPsdII=allPsdII,N=dataAuctions[,.N]))
}



alltickers=readRDS("tickers_ARCA.rds")


mylapply=mclapply
options(mc.cores=detectCores()/2)
allres=mylapply(alltickers,function(myticker){
  createPlot(myticker,doPlotResponseFunctions = TRUE,doPlotDiffusion = FALSE,withoutTimesAndSales=FALSE,coarseFactor = 60*5)
})
diffLM=lapply(allres,function(x) x$diffLM)
diffLM=rbindlist(diffLM)

if(nrow(diffLM)>0){
  qplot(data=diffLM[b>0 & pval<0.001 &  ticker %in% alltickers],b/2,geom="density",group=auction,col=auction,fill=auction,alpha=I(0.2),xlab="H",main="ARCA")+theme_bw_latex
  ggsave("H_pi_ARCA.pdf",width=7,height=7)
}

allPsdII=lapply(allres,function(x) x$allPsdII)
allPsdII=rbindlist(allPsdII)

if(nrow(allPsdII)>0){
  qplot(data=allPsdII[ticker %in% tickers_ARCA,median(prob_1),by="ticker,auction"],V1,geom="density",group=auction,col=auction,fill=auction,alpha=I(0.2),xlab=expression(paste("P(",delta,I[t+1],I[t],"=-1)")),main="ARCA")+theme_bw_latex
  ggsave("P(PsdII=-1).pdf",width=7,height=7)
}

sum(unlist(sapply(allres,function(x) x$N)))   # total number of updates
