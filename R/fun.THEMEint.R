.fun.THEMEint<-function(Xtot,Ctot,E,resE,W,s=.5,l=1,optEquiPondTau="Global",optEquiPondVarPhi="Theme",myEps=10^(-6),OutputDir=NULL){
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
  #mylimit<-1-10^-5
  repeat{
    aaarepeat<-aaarepeat+1
    #cat(aaarepeat,"...")
    Ftotold<-Ftot
    for(myr in resE$rF){
      for(compk in 1:nbcomp[myr]){
        #print(system.time(
          res<-THEME:::.fun.maxcrit(Ftot,Ttot,r=myr,E,Ctot=Ctot,Xr=Xtot[[myr]],Wr=W,compk=compk,nbcomp=nbcomp,s=s,l=l,optEquiPondTau=optEquiPondTau,optEquiPondVarPhi=optEquiPondVarPhi,epsconv=myEps,OutputDir=OutputDir)
        #  ))
        #Ttotnew<-res$Ttot
        #Ftotnew<-res$Ftot
        Ttot<-res$Ttot
        Ftot<-res$Ftot
        mycrit=res$mycrit
        }
      }

    optionbreak<-THEME:::.fun.convergence(resE,Ftot,Ftotold,aaarepeat,mycrit)$optionbreak
    if(optionbreak){break}
    }
  LogFileConv<-"Non Activated"
  return(list(Ftot=Ftot,Ttot=Ttot,LogFileConv=LogFileConv,resE=resE,E=E))
}
