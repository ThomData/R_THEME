.fun.THEME2017<-function(Xtot,Ctot,E,resE,W,s=.5,l=1,optEquiPondTau="Global",optEquiPondVarPhi="Theme"){
  nbcomp<-resE$nbcomp  
  res<-THEME:::.fun.initialisation(Ctot,W,E,nbcomp=nbcomp,Einfo=NULL)
  Ftot<-res$F
  Ttot<-res$T
  if(length(resE$rcov)>0){ 
    for(jj in 1:length(resE$rcov)){
      Ftot[[resE$rcov[jj]]]<-Xtot[[resE$rcov[jj]]]
      }
    }       
  
  aaarepeat<-0
  maxcor<-0
  mylimit<-.99999
  repeat{
    aaarepeat<-aaarepeat+1
    Ftotold<-Ftot
    for(myr in resE$rF){
      for(compk in 1:nbcomp[myr]){
        res<-THEME:::.fun.maxcrit(Ftot,Ttot,r=myr,E,Ctot=Ctot,Xr=Xtot[[myr]],Wr=W,compk=compk,nbcomp=nbcomp,s=s,l=l,optEquiPondTau=optEquiPondTau,optEquiPondVarPhi=optEquiPondVarPhi)
        Ttotnew<-res$Ttot
        Ftotnew<-res$Ftot
        Ttot<-Ttotnew
        Ftot<-Ftotnew
        }
      }
    
    mincor<-min(unlist(sapply(resE$rF,function(i)diag(abs(cor(Ftot[[i]],Ftotold[[i]]))))))
    oldw <- getOption("warn")
    options(warn = -1)
    meancorspace<-unlist(sapply(resE$rF,function(i)
      sapply(1:ncol(Ftot[[i]]),function(j)
          if(ncol(Ftot[[i]])==1){summary(lm(Ftot[[i]]~.,data = data.frame(Ftotold[[i]])))$r.squared
          }else{summary(lm(Ftot[[i]]~.,data = data.frame(Ftotold[[i]])))[[j]]$r.squared}
          )
      ))
    options(warn = oldw)
    
    meancorspace<-mean(meancorspace,na.rm=TRUE)
    if(mincor>maxcor){maxcor<-mincor}
    if(mincor>mylimit){
      #if(aaarepeat>=30){cat("n iteration higher than",aaarepeat)} 
      #cat("CONVvector. FTOT =",mincor,"\n") 
      #cat("CONVspace. ",aaarepeat," FTOT =",meancorspace,"\n")  
      break}
    if(aaarepeat>=5){if(meancorspace>.999){break}}
    if(aaarepeat>=30){
      if(aaarepeat==30){
        mylimit<-.99*maxcor} #*maxcor
      
      if(aaarepeat>=90){
        #browser()
        }
      if(aaarepeat>=100){
        warningconv<-"PB"
        #print dans un log cf sink cat("Maximimun number of iterations reached, before reaching convergence. Advise: Change number of components")
        #cat("CONVvector. FTOT =",mincor,"\n") 
        #cat("CONVspace. ",aaarepeat," FTOT =",meancorspace,"\n")
        break
        }
      }
    }  
  LogFileConv<-"Non Activated"
  return(list(Ftot=Ftot,Ttot=Ttot,LogFileConv=LogFileConv,resE=resE,E=E))
}
