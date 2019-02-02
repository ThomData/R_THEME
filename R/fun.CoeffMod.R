.THEME.coeff<-function(Vlist,Ftot,Ttot,Xtot,Xtotorig,Xtotmean,Xtotsd,resE){
  ## CREER UNE AIDE
  NbEq<-length(resE$rEq)
  rescoeff<-vector("list")
  resrsq<-vector("list")
  nbcomp<-unlist(sapply(1:length(Ftot),function(i)if(is.null(ncol(Ftot[[i]]))){0}else{ncol(Ftot[[i]])}))
  if(is.null(resE$rcov)==FALSE){Ftot[resE$rcov]<-Xtotorig[resE$rcov]}
  nbg<-length(nbcomp)
  Cstelist<-vector("list",NbEq)
  Coefflist<-vector("list",NbEq)
  Coeffinlist<-vector("list",NbEq)
  Cstinlist<-vector("list",NbEq)
  for(i in 1:NbEq){
        Coefflist[[i]]<-vector("list",nbg)
	      Coeffinlist[[i]]<-vector("list",nbg)
        Cstelist[[i]]<-vector("list",nbg)
        }

  for(i in 1:NbEq){
    myY<-resE$rEq[[i]][1]
    myX<-resE$rEq[[i]][-1]
    Freg<-data.frame(Reduce(cbind,Ftot[myX]))
    Ymat<-as.matrix(Xtotorig[[myY]])
    oldw <- getOption("warn")
    options(warn = -1)
    reslm<-lm(Ymat~.,data=Freg)
    options(warn = oldw)

    if(ncol(Ymat)==1){
      rescoeff[[i]]<-matrix(reslm$coeff)
      }else{rescoeff[[i]]<-reslm$coeff}

    if(ncol(Ymat)>1){
      resrsq[[i]]<-sapply(1:length(summary(reslm)),function(j)summary(reslm)[[j]]$r.squared)
      }else{resrsq[[i]]<-summary(reslm)$r.squared}

    aaa<-0
		Cstinlisttp<-0

		for(r in myX){
      aaa<-aaa+1
      if(r %in%  resE$rcov){
           ncXr<-length(Xtotmean[[r]])
  			   IdMeanSigma<-matrix(Xtotmean[[r]]/Xtotsd[[r]],byrow=TRUE,nrow=nrow(Xtot[[1]]),ncol=ncXr)
           Cstelist[[i]][[r]]<-matrix(0,byrow=TRUE,nrow=nrow(Xtot[[1]]),ncol=ncXr)#IdMeanSigma
           Coefflist[[i]][[r]]<-diag(1,ncXr)#diag(1/Xtotsd[[r]])
           }else{
        			Br<-Vlist[[r]]%*%Ttot[[r]]

              ncXr<-length(Xtotmean[[r]])
        			IdMeanSigma<-matrix(Xtotmean[[r]]/Xtotsd[[r]],byrow=TRUE,nrow=nrow(Xtot[[1]]),ncol=ncXr)
        			Cster <- -IdMeanSigma%*%Br

        			if(length(Xtotsd[[r]])>1){
        			  Coeffr<-diag(1/Xtotsd[[r]])%*%Br
        			  }else{Coeffr<-(1/Xtotsd[[r]])%*%Br}
        			Cstelist[[i]][[r]]<-Cster
        			Coefflist[[i]][[r]]<-Coeffr
              myselectcoeff<-rep(myX,nbcomp[myX])
              }

      Coeffinlisttemp<-Coefflist[[i]][[r]]%*%rescoeff[[i]][c(FALSE,myselectcoeff==r),]
      Coeffinlist[[i]][[r]]<-Coeffinlisttemp
      Cstinlisttp<-Cstinlisttp+Cstelist[[i]][[r]]%*%rescoeff[[i]][c(FALSE,myselectcoeff==r),]
      Cstinlisttemp<-Cstinlisttp+matrix(rescoeff[[i]][1,],ncol=ncol(Cstinlisttp),nrow=nrow(Cstinlisttp),byrow=TRUE)
      Cstinlist[[i]]<-matrix(Cstinlisttemp[1,],ncol=1)
      }
    }
  return(list(Coeffinlist=Coeffinlist,Cstinlist=Cstinlist,resE=resE,nbcomp=nbcomp,reslmR2=resrsq))   #,sousdos=sousdos
  }


.THEME.Predict<-function(CoeffMod,Xnew,Xcal=NULL,optnoneg=TRUE){
  ## CREER UNE AIDE
  resE<-CoeffMod$resE
	nbcomp<-CoeffMod$nbcomp

  nbeq<-length(resE$rEq)
	nbgroup<-length(nbcomp)

  Ypred<-vector("list",nbeq)
  CV_EAM<-vector("list")
  CV_EQM<-vector("list")
  Ynew<-lapply(1:nbeq,function(i)Xnew[[resE$rEq[[i]][1]]])
	for(e in 1:nbeq){
		Xselect<-resE$rEq[[e]][-1]


		Amod<-0
		Xmod<-Xnew[Xselect]
		lXmod<-length(Xmod)

		for(i in 1:lXmod){
      Amod<-Amod+as.matrix(Xmod[[i]])%*%CoeffMod$Coeffinlist[[e]][[Xselect[i]]]
			}

		Ypred[[e]]<-Amod+matrix(CoeffMod$Cstinlist[[e]],ncol=ncol(Amod),nrow=nrow(Amod),byrow=TRUE)
		if(!is.null(Xcal)) colnames(Ypred[[e]])<-colnames(Xcal[[resE$rEq[[e]][1]]])

		if(optnoneg==TRUE){
      Ypred[[e]][Ypred[[e]]<0]<-0
      }

		if(e<nbeq){
			selectYexpl<-resE$rEq[[e]][1]
      if(resE$rEq[[e]][1]%in%resE$rEq[[e+1]]){Xnew[[selectYexpl]]<-Ypred[[e]]} ##Attention avant octobre 2015 n'?tait pas active, le bloc X dans l'?quantion 2  et remplac? par sa pr?diction de l'?quation 1
			}

   if(is.null(Ynew[[e]])==FALSE){
            EQMnum<-apply((Ynew[[e]]-Ypred[[e]])^2,2,mean)
            EQMden<-apply((Ynew[[e]]-matrix(apply(Ynew[[e]],2,mean),ncol=ncol(Ynew[[e]]),nrow=nrow(Ynew[[e]]),byrow=TRUE))^2,2,mean)
            EAMnum<-apply(abs(Ynew[[e]]-Ypred[[e]]),2,mean)
            EAMden<-apply(abs(Ynew[[e]]-matrix(apply(Ynew[[e]],2,median),ncol=ncol(Ynew[[e]]),nrow=nrow(Ynew[[e]]),byrow=TRUE)),2,mean)
            CV_EAM[[e]]<-EAMnum/EAMden
            CV_EQM[[e]]<-EQMnum/EQMden
            }
  	}

  return(list(Ypred=Ypred,CV_EAM=CV_EAM,CV_EQM=CV_EQM))
	}


