fun.crossvalonestep<-function(i,nbsteps,listsamples,Xtot,resE,E,optEquiPondTau,optEquiPondVarPhi,myEps,s,l,LogfileCV=NULL){

  nbgroup<-resE$Rtot
  nbeq<-length(resE$rEq)
  Xcal<-lapply(1:nbgroup,function(k)Xtot[[k]][-listsamples[[i]],,drop=FALSE])
  Xval<-lapply(1:nbgroup,function(k)Xtot[[k]][listsamples[[i]],,drop=FALSE])
  Xcalorig<-Xcal

  ## Xcal Xval standardisation
  W<-as.matrix(1/nrow(Xcal[[1]])*diag(1,nrow(Xcal[[1]])))
  resscale<-THEME:::.fun.scale(Xcal,resE)
  Xcal<-resscale$Xcal
  Xcalsd<-resscale$Xcalsd
  Xcalmean<-resscale$Xcalmean

  res<-THEME:::.fun.XlisttoClist(Xcal,W,E,Einfo=resE)
  Clist<-res$Clist
  Vlist<-res$Vlist
  Mlist<-res$Mlist


  resTHEME<-THEME:::.fun.THEMEint(Xcal,Ctot=Clist,E,resE,W,s=s,l=l,optEquiPondTau=optEquiPondTau,optEquiPondVarPhi=optEquiPondVarPhi,myEps=myEps)
  Ftot<-resTHEME$Ftot
  Ttot<-resTHEME$Ttot

  mycoeff<-THEME:::.THEME.coeff(Vlist,Ftot,Ttot,Xcal,Xcalorig,Xcalmean,Xcalsd,resE)
  mypred<-THEME:::.THEME.Predict(mycoeff,Xnew=Xval)

  ## LOG FILE CONVERGENCE INDICATIONS
  vers<-paste(resE$nbcomp,collapse="_")
  if(resTHEME$LogFileConv=="PB"){
    LogfileCV<-c(LogfileCV,paste("Convergence failed for the model ",resTHEME$LogFileConv," during cross validation mode (step ",i,"/",nbsteps,")",sep=""))
  }else{LogfileCV<-c(LogfileCV,paste("Convergence OK (",resTHEME$LogFileConv,") for the model ",vers," during cross validation mode (step ",i,"/",nbsteps,")",sep=""))}

  Ypredtot<-mypred$Ypred
  R2CV<-mycoeff$reslmR2

  errorq<-lapply(1:nbeq,function(j)abs(Ypredtot[[j]]-(Xval[resE$rEq[[j]][1]])[[1]]))

  return(list(Ypredtot=Ypredtot,R2CV=R2CV,errorq=errorq,LogfileCV=LogfileCV))
}
