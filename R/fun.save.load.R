##########################
## save data function
## functionsaveData
.sav.Data<-function(Xtot,Mtot,P,OutputDir=NULL,sousdos=NULL){
        if(is.null(sousdos)){sousdos<-c("Design","Themes","Metrics","Components","Correlations","Coefficients","R2","Tables","Prediction","CV")}
        if(!is.null(OutputDir)){
           sapply(sousdos[c(2,3)],function(isd)dir.create(paste(OutputDir,"/",isd,sep=""),showWarnings = FALSE))

           for(i in 1:length(Xtot)){
              write.table(Xtot[[i]],file=paste(OutputDir,"/",sousdos[2],"/Xtot_",i,".txt",sep=""))
              write.table(Mtot[[i]],file=paste(OutputDir,"/",sousdos[3],"/Minvtot_",i,".txt",sep=""))
              }
           write.table(P,file=paste(OutputDir,"/",sousdos[3],"/P.txt",sep=""))
           }
        }

##########################
## save Xorig function
## functionsaveXorig
.sav.Xorig<-function(Xtot,OutputDir=NULL,sousdos=NULL){
      if(is.null(sousdos)){sousdos<-c("Design","Themes","Metrics","Components","Correlations","Coefficients","R2","Tables","Prediction","CV")}
      if(!is.null(OutputDir)){
        sapply(sousdos[c(2,3)],function(isd)dir.create(paste(OutputDir,"/",isd,sep=""),showWarnings = FALSE))
        
        for(i in 1:length(Xtot)){
          write.table(Xtot[[i]],file=paste(OutputDir,"/",sousdos[2],"/Xtotorig_",i,".txt",sep=""))
        }
      }
    }

##########################
## save THEME results function
## functionsaveTHEME
.sav.THEME<-function(resfunTHEMEint,resfunTHEMEcoeff=NULL,resfunTHEMEpred=NULL,OutputDir=NULL,sousdos=NULL){

   if(!is.null(OutputDir)){
        resE<-resfunTHEMEint$resE
        nbcomp<-resE$nbcomp
        if(!is.null(resE$rcov)){nbcomp[resE$rcov]<-"cov"}
        vers<-paste(nbcomp,collapse="_")
        nbcomp<-resE$nbcomp
        OutputDircomp<-paste(OutputDir,"/Model_",vers,sep="")
        if(is.null(sousdos)){sousdos<-c("Design","Themes","Metrics","Components","Correlations","Coefficients","R2","Tables","Prediction","CV")}
        dir.create(paste(OutputDir,"/Model_",vers,sep=""),showWarnings = FALSE)
        sapply(sousdos[-c(2,3)],function(isd)dir.create(paste(OutputDir,"/Model_",vers,"/",isd,sep=""),showWarnings = FALSE))

        Flist<-resfunTHEMEint$Ftot
        E<-resfunTHEMEint$E
        nbg<-length(nbcomp)
        nbcomptot<-nbcomp

		    for(i in resE$rF){
			    write.table(Flist[[i]],file=paste(OutputDir,"/Model_",vers,"/",sousdos[4],"/Ftot_",i,".txt",sep=""))
			    }

       write.table(E,file=paste(OutputDir,"/Model_",vers,"/",sousdos[1],"/E.txt",sep=""))
		   write.table(nbg,file=paste(OutputDir,"/Model_",vers,"/",sousdos[1],"/nbgoupes.txt",sep=""))
		   write.table(nbcomptot,file=paste(OutputDir,"/Model_",vers,"/",sousdos[1],"/nbcomp.txt",sep=""))

      if(!is.null(resfunTHEMEcoeff)){
         Coeffinlist<-resfunTHEMEcoeff$Coeffinlist
         Cstinlist<-resfunTHEMEcoeff$Cstinlist
         for(q in 1:length(resE$rEq)){
           sapply(resE$rEq[[q]],function(r)write.table(Coeffinlist[[q]][[r]],file=paste(OutputDir,"/Model_",vers,"/",sousdos[6],"/Beta_",q,"_",r,".txt",sep="")))
           write.table(Cstinlist[[q]],file=paste(OutputDir,"/Model_",vers,"/",sousdos[6],"/Cste_",q,".txt",sep=""))
           }
         }
  
      if(!is.null(resfunTHEMEpred)){
        Ypred<-resfunTHEMEpred$Ypred
        for(q in 1:length(resE$rEq)){
              write.table(Ypred[[q]],file=paste(OutputDir,"/Model_",vers,"/",sousdos[9],"\\Ypred_SEERS_",q,".txt",sep=""))
              }
        }
      }
    }

##########################
## save THEME Cross Val function
## functionsaveTHEMECV
.sav.THEMECrossVal<-function(resTHEMECV,OutputDir=NULL,sousdos=NULL){
      if(!is.null(OutputDir)){
        resE<-resTHEMECV$resE
        nbcomp<-resE$nbcomp
        if(!is.null(resE$rcov)){nbcomp[resE$rcov]<-"cov"}
        vers<-paste(nbcomp,collapse="_")
        nbcomp<-resE$nbcomp
        OutputDircomp<-paste(OutputDir,"/Model_",vers,sep="")
        if(is.null(sousdos)){sousdos<-c("Design","Themes","Metrics","Components","Correlations","Coefficients","R2","Tables","Prediction","CV")}
        dir.create(paste(OutputDir,"/Model_",vers,sep=""),showWarnings = FALSE)
        sapply(sousdos[-c(2,3)],function(isd)dir.create(paste(OutputDir,"/Model_",vers,"/",isd,sep=""),showWarnings = FALSE))

        resCV<-resTHEMECV$resCV
        R2CV<-resTHEMECV$R2CV
        Ypredtot<-resTHEMECV$Ypredtot

		    for(i in 1:length(resE$rEq)){
          write.table(Ypredtot[[i]],file=paste(OutputDir,"/Model_",vers,"/",sousdos[10],"/CV_Ypred_","Eq_",i,".txt",sep=""))
          write.table(resCV[[i]],file=paste(OutputDir,"/Model_",vers,"/",sousdos[10],"/CV_Eq_",i,".txt",sep=""))
			    if(is.vector(R2CV[[i]])){   names(R2CV[[i]])<-   names(resCV[[i]])}
				  if(is.matrix(R2CV[[i]])){dimnames(R2CV[[i]])<-dimnames(resCV[[i]])}
				  write.table(R2CV[[i]],file=paste(OutputDir,"/Model_",vers,"/",sousdos[10],"/CV_R2_Eq_",i,".txt",sep=""))
          }
        }
      }

##########################
## load THEME data resuts function
## fun.readTHEME
.load.THEME<-function(OutputDir=NULL,vers,sousdos=NULL){
  if(is.null(sousdos)){sousdos<-c("Design","Themes","Metrics","Components","Correlations","Coefficients","R2","Tables","Prediction","CV")}
  
  if((!is.null(OutputDir))&(file.exists(OutputDir))){  
    E<-read.table(file=paste(OutputDir,"/",vers,"/",sousdos[1],"/E.txt",sep=""))
    
    nbgroups<-read.table(file=paste(OutputDir,"/",vers,"/",sousdos[1],"/nbgoupes.txt",sep=""))$x
    nbcomp<-read.table(file=paste(OutputDir,"/",vers,"/",sousdos[1],"/nbcomp.txt",sep=""))$x
    
    myFsel<-(1:nbgroups)[nbcomp!=0]
    Flist<-vector("list",nbgroups)
    Xlist<-vector("list",nbgroups)
    Xlistorig<-vector("list",nbgroups)
    Mlist<-vector("list",nbgroups)
    for(i in myFsel){
      Flist[[i]]<-read.table(file=paste(OutputDir,"/",vers,"/",sousdos[4],"/Ftot_",i,".txt",sep=""))
      Mlist[[i]]<-read.table(file=paste(OutputDir,"/",sousdos[3],"/Minvtot_",i,".txt",sep=""))
      }
    for(i in 1:nbgroups){
      Xlist[[i]]<-read.table(file=paste(OutputDir,"/",sousdos[2],"/Xtot_",i,".txt",sep=""))
      Xlistorig[[i]]<-read.table(file=paste(OutputDir,"/",sousdos[2],"/Xtotorig_",i,".txt",sep=""))
      }
    P<-read.table(file=paste(OutputDir,"/",sousdos[3],"/P.txt",sep=""))
    Ypred<-vector("list",nrow(E))
    
    for(q in 1:nrow(E)){
      Ypred[[q]]<-read.table(file=paste(OutputDir,"/",vers,"/",sousdos[9],"\\Ypred_SEERS_",q,".txt",sep=""))
      }
    }
  return(list(Xlist=Xlist,Xlistorig=Xlistorig,Flist=Flist,nbg=nbgroups,nbcomp=nbcomp,Mlist=Mlist,P=P,E=E,Ypred=Ypred))
  }
  
##########################
## load THEME CrossVal function
## fun.readTHEMECV  
.load.THEMECrossVal<-function(pathcv=NULL,neq=1){
    RMSECV<-NULL
    R2CV<-NULL
    
    if((!is.null(pathcv))&(file.exists(pathcv))){  
      RMSECV<-vector("list")
      R2CV<-vector("list")
      for(q in 1:neq){
        RMSECV[[q]]<-read.table(file=paste(pathcv,"/CV_Eq_",q,".txt",sep=""))
        R2CV[[q]]<-read.table(file=paste(pathcv,"/CV_R2_Eq_",q,".txt",sep=""))
        }
      
      }
    return(list(RMSECV=RMSECV,R2CV=R2CV))
    }
  
##########################
## Compile CV data function
## fun.compilCV  
.fun.compilCV<-function(pathcvall,neq=1){
  n<-length(pathcvall)
  splitpathcv<-strsplit(pathcvall[1],"/")[[1]]
  mysel<-length(splitpathcv)-1
  modelsnames<-sapply(1:n,function(i)strsplit(pathcvall[i],"/")[[1]][length(splitpathcv)-1])

  RMSECV<-NULL#vector("list",2)
  R2CV<-NULL#vector("list",2)
  
  for(i in 1:n){
    res<-THEME:::.load.THEMECrossVal(pathcvall[[i]],neq=neq)
    neq<-length(res$RMSECV)
  
    for(q in 1:neq){
      R2CV<-rbind(R2CV,data.frame(model=modelsnames[i],Eq=q,name=rownames(res$R2CV[[q]]),R2=res$R2CV[[q]][,1]))
      RMSECV<-rbind(RMSECV,data.frame(model=modelsnames[i],Eq=q,name=rownames(res$R2CV[[q]]),CV=res$RMSECV[[q]][,1]))
      }
    }

  return(list(RMSECV=RMSECV,R2CV=R2CV))
  }
