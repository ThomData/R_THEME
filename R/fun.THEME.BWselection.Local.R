.fun.backwardselectionTe<-function(Ftot,r=0,P,E,resE){

	nbeq<-nrow(E)
	nbgroup<-resE$Rtot
	S<-vector("list",nbeq)
	SR2<-vector("list",nbeq)
	Te<-vector("list",nbeq)

	for(e in 1:nbeq){
		  s<-resE$rEq[[e]][1] 
			Ftotexpl.e<-(Reduce(cbind,Ftot[resE$rEq[[e]][-1]]))		
		  S[[e]]<-diag(t(Ftot[[s]])%*%P%*%(Ftot[[s]]))
		  SR2[[e]]<-sapply(1:length(S[[e]]),function(j)S[[e]][j]*summary(lm(Ftot[[s]][,j]~.,as.data.frame(Ftotexpl.e)))$r.squared)
		  Te[[e]]<-1/sum(S[[e]])*sum(SR2[[e]])
  		}
	
	return(list(Te=Te))
	}
	

.THEME.backwardselectionl<-function(Xtot,Ftot,Vtot,Mtot,P,E,OutputDir=NULL,pondSr=1,a=2,resE){
	
  ##  Global Criteria (Mode A - Default Mode A local)
  # r Block index
  # k Current component index from block r
  # S list containing the values S[[r]][k]
 
  nbcomptot<-resE$nbcomp 
	nbeq<-nrow(E)
	nbgroup<-resE$Rtot
	Te<-THEME:::.fun.backwardselectionTe(Ftot,r=0,P,E,resE)$Te
	Srstar<-vector("list",nbgroup)
	Snorm<-vector("list",nbgroup)
	ncompGroup<-resE$nbcomp 
	SelectGroup<-c(1:nbgroup)[ncompGroup>0] 	

	for(r in SelectGroup){
		Snorm[[r]]<-THEME:::.fun.CP1(Xtot[[r]],Mtot[[r]],P)$valp
		Sr<-sum(sapply(1:ncol(Xtot[[r]]),function(jj){sum(diag(t(Vtot[[r]])%*%(t(Xtot[[r]])%*%P%*%Xtot[[r]][,jj]%*%t(Xtot[[r]][,jj])%*%Xtot[[r]])%*%Vtot[[r]])^a)}))
		Sr2<-sum(sapply(1:ncol(Xtot[[r]]),function(jj){sum(diag(t(Ftot[[r]])%*%P%*%Xtot[[r]][,jj]%*%t(Xtot[[r]][,jj])%*%Ftot[[r]])^a)}))
		Srstar[[r]]<-Sr/ncol(Xtot[[r]])  
		}
		
	gamma<-rep(NA,nbgroup)
	delta<-rep(NA,nbgroup)
	logSr<-rep(NA,nbgroup)

	for(r in SelectGroup){
	  nbcomp<-resE$nbcomp
	  nbcomp[r]<-nbcomp[r]-1
	  neq<-(1:length(resE$rEq))[sapply(1:length(resE$rEq),function(i)r%in%resE$rEq[[i]])]
	  mytest<-NULL
	  for(i in neq){mytest<-c(mytest,sum(nbcomp[resE$rEq[[i]]]!=0,na.rm=TRUE))}

	  if(all(mytest>=2)){
  	  ePvr<-sum(E[,r])
  		eDvr<-sum(E[,nbgroup+r])
  		Pvr<-(1:nbeq)[as.logical(E[,r])]
  		Dvr<-(1:nbeq)[as.logical(E[,nbgroup+r])]
  		Ftot.mr<-Ftot
  		Enew<-E
  		resEnew<-resE
  		resEnew$nbcomp[r]<-resEnew$nbcomp[r]-1
  		if(ncompGroup[r]>1){	
  		  Ftot.mr[[r]]<-matrix(Ftot[[r]][,-ncol(Ftot[[r]])],ncol=ncol(Ftot[[r]])-1,byrow=FALSE)
  	  	}else{
  		      Ftot.mr[[r]]<-list()
  		      resEnew$R<-resEnew$R-1
  		      resEnew$rns<-c(resEnew$rns,r)
  		      resEnew$rF<-resEnew$rF[!resEnew$rF%in%resEnew$rns]
  		      resEnew$rEq<-lapply(1:length(resEnew$rEq),function(i)resEnew$rEq[[i]][!resEnew$rEq[[i]]%in%(r)])
  		      Enew[,nbgroup+r]<-0
  		      }
     
  		Temr<-THEME:::.fun.backwardselectionTe(Ftot.mr,r,P,Enew,resEnew)$Te
  		sumln.Pvr<-0
  		if(sum(Pvr)>0){
  		  sumln.Pvr<-sum(sapply(Pvr,function(ee)log(Te[[ee]])-log(Temr[[ee]])))
  			}
  		sumln.Dvr<-0
  		sumln.Dvrmr<-0
  		if(sum(Dvr)>0){
  		  sumln.Dvr<-sum(sapply(Dvr,function(ee)log(Te[[ee]])-log(Temr[[ee]])))
  			}
  		deltavr<-1/(eDvr+ePvr)*(sumln.Pvr+sumln.Dvr)
  		if(eDvr==0){gamma[r]<-NA}
  		if(eDvr>0){
  			gamma[r]<-pondSr*log(Srstar[[r]])+deltavr
  			delta[r]<-deltavr
  			logSr[r]<-log(Srstar[[r]])
  			}
		  }	
		}

		if(is.null(OutputDir)==FALSE){
	  nbcomp<-resE$nbcomp
	  if(!is.null(resE$rcov)){nbcomp[resE$rcov]<-"cov"}
	  vers<-paste(nbcomp,collapse="_")
		OutputDircomp<-paste(OutputDir,"/Model_",vers,"/BW",sep="")
		dir.create(paste(OutputDir,"/Model_",vers,"/BW",sep=""))
		write.table(gamma,file=paste(OutputDir,"/Model_",vers,"/BW/gamma.txt",sep=""))
		}

	return(list(gamma=gamma,logSr=logSr,delta=delta))
	}
	
	