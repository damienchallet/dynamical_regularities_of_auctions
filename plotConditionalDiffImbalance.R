library(data.table)

plotConditionalDiffImbalance=function(imbalance,ticker,auction,dirPlots="plots",toPDF=FALSE){
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
  imbalance[,imbalance:=na.locf(imbalance,na.rm = FALSE)]
  imbalance=na.omit(imbalance)

  qplot(data=imbalance[imbalance!=0,table(signIdI)[1]/.N,by="HHMMSS"],HHMMSS,V1,geom=c("line","smooth"),xlab="",ylab=expression(paste("P(",delta,I[t+1],I[t],"<0)")),main=paste0(ticker,", ",auction),fill=I("lightblue"))+theme_bw_latex

  if(toPDF){
    dir.create(dirPlots,showWarnings = FALSE,recursive = TRUE)
    filename=paste0(dirPlots,"/PsigndII_HHMMSS_",ticker,"_",auction,".pdf")
    ggsave(filename,width = 7, height= 7)
  }

  qplot(data=imbalance[imbalance!=0,table(signIIlag)[1]/.N,by="HHMMSS"],HHMMSS,V1,geom=c("line","smooth"),xlab="",ylab=expression(paste("P(",I[t+1],I[t],"<0)")),main=paste0(ticker,", ",auction),fill=I("lightblue"))+theme_bw_latex
  if(toPDF){
    dir.create(dirPlots,showWarnings = FALSE,recursive = TRUE)
    filename=paste0(dirPlots,"/PsignIIlag_HHMMSS_",ticker,"_",auction,".pdf")
    ggsave(filename,width = 7, height= 7)
  }

  qplot(data=imbalance[imbalance!=0 & signIdI!=0 ,table(signIIlag)[1]/.N,by="HHMMSS,signIdI"],HHMMSS,V1,geom=c("line","smooth"),xlab="",ylab=expression(paste("P(",I[t+1],I[t],"<0)")),main=paste0(ticker,", ",auction),fill=I("lightblue"),group=signIdI,col=signIdI)+theme_bw_latex
  if(toPDF){
    dir.create(dirPlots,showWarnings = FALSE,recursive = TRUE)
    filename=paste0(dirPlots,"/PsignIIlag_HHMMSS_cond_signIdI_",ticker,"_",auction,".pdf")
    ggsave(filename,width = 7, height= 7)
  }



  qplot(data=imbalance[imbalance!=0,table(signIdI)[1]/.N,by="HHMMSS,signI"],HHMMSS,V1,geom=c("line","smooth"),xlab="",ylab=expression(paste("P(",delta,I[t+1],I[t],"<0)")),main=paste0(ticker,", ",auction),group=signI,col=signI,fill=signI)+theme_bw_latex
  if(toPDF){
    dir.create(dirPlots,showWarnings = FALSE,recursive = TRUE)
    filename=paste0(dirPlots,"/PsigndII_condsignI_HHMMSS_",ticker,"_",auction,".pdf")
    ggsave(filename,width = 7, height= 7)
  }
  PsdII=imbalance[imbalance!=0,.(ticker,auction,prob_1=table(signIdI)[1]/.N),by="HHMMSS"]
  return(PsdII)

}
