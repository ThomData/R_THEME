.THEME.CrossVal<-function(Xtot,E,resE,nbtest=1,optordersample=NULL,optEquiPondTau="Global",optEquiPondVarPhi="Theme",exps=1,expl=1,updateProgress=NULL,myEps=10^(-6)){
## ADD an help
## Initialisation

  nbcomp<-resE$nbcomp
  nbcomptot<-sum(nbcomp)
  blockcov<-resE$rcov

  LogfileCV<-NULL
	nind<-nrow(Xtot[[1]])
	nbgroup<-length(Xtot)
	nbeq<-nrow(E)

	rescvtot<-vector("list",nind)
	eq<-vector("list",nbeq)
	R2CV<-vector("list",nbeq)
	resCV<-vector("list",nbeq)
	Ypredtot<-vector("list",nbeq)
	Ypredvalidtot<-vector("list",nbeq)

	Xtotmi<-Xtot
	Xtotioptim<-Xtot

  ## Build k Samples for cross-validation
  # Plan to update for an intelligent function such as Kenstone

	if(is.null(optordersample)){ordersample<-sample(1:nind)}else{ordersample<-optordersample}
	listsamples<-split(ordersample, sort(ordersample%%round(nind/nbtest,0)))
	nbsteps<-length(listsamples)

## LOOP on the k samples	TODO decoupage X% cal (100-X)% val
	for(i in 1:nbsteps){ #foreach
	  if(is.function(updateProgress)){
	    text <- paste0("CV ", i,"/",nbsteps)
	    updateProgress(detail = text)
	    }
	  Xtotmi<-lapply(1:nbgroup,function(k)Xtot[[k]][-listsamples[[i]],,drop=FALSE])
	  Xtotioptim<-lapply(1:nbgroup,function(k)Xtot[[k]][listsamples[[i]],,drop=FALSE])
	  Xtotorig<-Xtotmi
    W<-as.matrix(1/nrow(Xtotorig[[1]])*diag(1,nrow(Xtotorig[[1]])))
    resscale<-THEME:::.fun.scale(Xtotmi,resE)
    Xtotmi<-resscale$Xcal
    Xtotmisd<-resscale$Xcalsd
    Xtotmimean<-resscale$Xcalmean

    res<-THEME:::.fun.XlisttoClist(Xtotmi,W,E,Einfo=resE)
    Clist<-res$Clist
    Vlist<-res$Vlist
    Mlist<-res$Mlist

    resTHEME<-THEME:::.fun.THEMEint(Xtotmi,Ctot=Clist,E,resE,W,s=exps,l=expl,optEquiPondTau=optEquiPondTau,optEquiPondVarPhi=optEquiPondVarPhi,myEps=myEps)
    Ftot<-resTHEME$Ftot
    Ttot<-resTHEME$Ttot

    mycoeff<-THEME:::.THEME.coeff(Vlist,Ftot,Ttot,Xtotmi,Xtotorig,Xtotmimean,Xtotmisd,resE)
    mypred<-THEME:::.THEME.Predict(mycoeff,Xnew=Xtotioptim)

		## LOG FILE CONVERGENCE INDICATIONS
    vers<-paste(resE$nbcomp,collapse="_")
    if(resTHEME$LogFileConv=="PB"){
      LogfileCV<-c(LogfileCV,paste("Convergence failed for the model ",resTHEME$LogFileConv," during cross validation mode (step ",i,"/",nbsteps,")",sep=""))
      }else{LogfileCV<-c(LogfileCV,paste("Convergence OK (",resTHEME$LogFileConv,") for the model ",vers," during cross validation mode (step ",i,"/",nbsteps,")",sep=""))}


		for(j in 1:nbeq){
			if(i==1){
				eq[[j]]<-0
				R2CV[[j]]<-0
			  }
		  if(length(listsamples[[i]])==1){
		    eq[[j]]<-eq[[j]]+abs(as.vector(mypred$Ypred[[j]])-unlist(Xtotioptim[resE$rEq[[j]][1]]))
		    }else{eq[[j]]<-eq[[j]]+apply(abs(mypred$Ypred[[j]]-(Xtotioptim[resE$rEq[[j]][1]])[[1]]),2,mean)}

      Ypredtot[[j]]<-rbind(Ypredtot[[j]],mypred$Ypred[[j]])
			R2CV[[j]]<-R2CV[[j]]+(1/nbsteps)*mycoeff$reslmR2[[j]]
			}
		rescvtot[[i]]<-mypred
		#return(list(eq,R2CV,Ypredtot,LogfileCV)) #LogfileCV sous forme de list
		}

	## LOOP END

	for(j in 1:nbeq){resCV[[j]]<-(eq[[j]]/nbsteps)/apply(abs(Xtot[resE$rEq[[j]][1]][[1]]),2,mean)}


	return(list(resCV=resCV,nbeq=nbeq,eq=eq,R2CV=R2CV,ordersample=ordersample,Ypredtot=Ypredtot,LogfileCV=LogfileCV,resE=resE))
	}
