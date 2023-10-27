##########################
## save data function
## functionsaveData
.sav.Data<-function(Xtot,Mtot,P,OutputDir=NULL){
  if(!is.null(OutputDir)){
    listfolders<-THEME:::.fun_Buildfolders(OutputDir,nameModel=NULL,opt.build=TRUE)$list_mainfolders

    for(i in 1:length(Xtot)){
      Xtoti<-as.data.frame(Xtot[[i]])
      Xtoti<-data.frame("obs"=rownames(Xtoti),Xtoti)
      write.csv2(Xtoti,file=file.path(OutputDir,listfolders[1],paste0("Bcs_",i,".csv")),row.names=FALSE)
      write.csv2(Mtot[[i]],file=file.path(OutputDir,listfolders[2],paste0("Minvtot_",i,".csv")))
    }
    write.csv2(P,file=file.path(OutputDir,listfolders[2],"P.csv"))
    }
  }

##########################
## save Xorig function
## functionsaveXorig
.sav.Xorig<-function(Xtot,OutputDir=NULL){
  if(!is.null(OutputDir)){
    listfolders<-THEME:::.fun_Buildfolders(OutputDir,nameModel=NULL,opt.build=TRUE)$list_mainfolders

    for(i in 1:length(Xtot)){
      Xtoti<-as.data.frame(Xtot[[i]])
      Xtoti<-data.frame("obs"=rownames(Xtoti),Xtoti)
      write.csv2(Xtoti,file=file.path(OutputDir,listfolders[1],paste0("B_",i,".csv")),row.names=FALSE)
      }
    }
  }

##########################
## save THEME results function
## functionsaveTHEME
.sav.THEME<-function(resfunTHEMEint,resfunTHEMEcoeff=NULL,resfunTHEMEpred=NULL,OutputDir=NULL){

  if(!is.null(OutputDir)){

    resE<-resfunTHEMEint$resE
    nbcomp<-resE$nbcomp
    if(!is.null(resE$rcov)){nbcomp[resE$rcov]<-"cov"}
    vers<-paste(nbcomp,collapse="_")
    nbcomp<-resE$nbcomp

    param_yaml<-THEME:::.fun_Buildfolders(opt.build=FALSE)
    #OutputDircomp<-paste(OutputDir,"/",param_yaml$nam_subfolder,vers,sep="")
    nameModel<-paste0(param_yaml$nam_subfolder,vers)
    dir.create(file.path(OutputDir,nameModel),showWarnings = FALSE)

    res<-THEME:::.fun_Buildfolders(OutputDir,nameModel=nameModel,opt.build=TRUE)

    Flist<-resfunTHEMEint$Ftot
    E<-resfunTHEMEint$E
    nbg<-length(nbcomp)
    nbcomptot<-nbcomp

    for(i in resE$rF){
      Flisti<-as.data.frame(Flist[[i]])
      Flisti<-data.frame("obs"=rownames(Flisti),Flisti)
      colnames(Flisti)<-c("obs",paste0("F",1:(ncol(Flisti)-1)))
      write.csv2(Flisti,file=file.path(OutputDir,nameModel,param_yaml$list_subfolders[["2"]],paste0("F_B",i,".csv")),row.names=FALSE)
      }

    THEME:::.fun.createyaml(E,nbcomptot,OutputDir,nameModel)

    if(!is.null(resfunTHEMEcoeff)){
      Coeffinlist<-resfunTHEMEcoeff$Coeffinlist
      Cstinlist<-resfunTHEMEcoeff$Cstinlist
      R2vareq<-resfunTHEMEcoeff$reslmR2

      for(q in 1:length(resE$rEq)){
        if(q==length(resE$rEq)){x<-resE$rEq[[q]][-1]}else{x<-resE$rEq[[q]]}
        sapply(x,function(r){
          coeffcur<-as.data.frame(Coeffinlist[[q]][[r]])
          coeffcur<-data.frame("code"=rownames(coeffcur),coeffcur)
          write.csv2(coeffcur,file=file.path(OutputDir,nameModel,param_yaml$list_subfolders[["4"]],paste0("Beta_Eq",q,"_B",r,".csv")),row.names=FALSE)
          }
          )
        Cstinlistq<-as.data.frame(t(Cstinlist[[q]]))
        Cstinlistq<-data.frame("code"="cst.",Cstinlistq)
        write.csv2(Cstinlistq,file=file.path(OutputDir,nameModel,param_yaml$list_subfolders[["4"]],paste0("Cste_Eq",q,".csv")),row.names=FALSE)
        R2vareqq<-as.data.frame(t(R2vareq[[q]]))
        R2vareqq<-data.frame("code"="R2",R2vareq)
        write.csv2(R2vareq[[q]],  file=file.path(OutputDir,nameModel,param_yaml$list_subfolders[["5"]],paste0("R2_Eq",q,".csv")),row.names=FALSE)
      }
    }

    if(!is.null(resfunTHEMEpred)){
      Ypred<-resfunTHEMEpred$Ypred

      for(q in 1:length(resE$rEq)){
        Ypredq<-data.frame(Ypred[[q]])
        Ypredq<-data.frame("code"=rownames(Ypredq),Ypredq)
        write.csv2(Ypredq,file=file.path(OutputDir,nameModel,param_yaml$list_subfolders[["7"]],paste0("Ypred_Eq",q,".csv")),row.names=FALSE)
        }
      }
    }
  }


##########################
## save THEME Cross Val function
## functionsaveTHEMECV
.sav.THEMECrossVal<-function(resE,resCV,Ypredtot,R2CV,OutputDir=NULL){
  if(!is.null(OutputDir)){
    #resE<-resTHEMECV$resE
    nbcomp<-resE$nbcomp
    if(!is.null(resE$rcov)){nbcomp[resE$rcov]<-"cov"}
    vers<-paste(nbcomp,collapse="_")
    nbcomp<-resE$nbcomp

    param_yaml<-THEME:::.fun_Buildfolders(OutputDir,nameModel=NULL,opt.build=FALSE)
    #OutputDircomp<-paste(OutputDir,"/",param_yaml$nam_subfolder,vers,sep="")

    nameModel<-paste0(param_yaml$nam_subfolder,vers)
    dir.create(file.path(OutputDir,nameModel),showWarnings = FALSE)
    param_yaml<-THEME:::.fun_Buildfolders(OutputDir,nameModel=nameModel,opt.build=TRUE)

    #resCV<-resTHEMECV$resCV
    #R2CV<-resTHEMECV$R2CV
    #Ypredtot<-resTHEMECV$Ypredtot

    #namesY<-THEME:::.fun.writeorreadyaml()
    #browser()
    for(i in 1:length(resE$rEq)){
      namesY<-colnames(Ypredtot[[i]])
      Ypredtoti<-data.frame("obs"=rownames(Ypredtot[[i]]),as.data.frame(Ypredtot[[i]]))
      write.csv2(Ypredtoti,file=paste(OutputDir,"/",nameModel,"/",param_yaml$list_subfolder[["8"]],"/CV_Ypred_","Eq",i,".csv",sep=""),row.names=FALSE)
      R2CVi<-data.frame("code"=namesY,"R2cv"=as.vector(R2CV[[i]])) #namesY$saveparam_yaml$names.col
      resCVi<-data.frame("code"=namesY,"RMSECV"=as.vector(resCV[[i]])) #namesY$saveparam_yaml$names.col
      write.csv2(resCVi,file=paste0(OutputDir,"/",nameModel,"/",param_yaml$list_subfolder[["8"]],"/CV_Eq",i,".csv"),row.names=FALSE)
      write.csv2(R2CVi,file=paste0(OutputDir,"/",nameModel,"/",param_yaml$list_subfolder[["8"]],"/CV_R2_Eq",i,".csv"),row.names=FALSE)
    }
  }
}
##########################
## load THEME data resuts function
## fun.readTHEME
.load.THEME<-function(OutputDir=NULL,modelvers){

  if((!is.null(OutputDir))&(file.exists(OutputDir))){
    param_yaml<-THEME:::.fun_Buildfolders(opt.build=FALSE)

    par.design<-THEME:::.fun.readparamyaml(OutputDir,modelvers)
    E<-par.design$E
    nbgroups<-par.design$nbg
    nbcomp<-par.design$nbcomp

    myFsel<-(1:nbgroups)[nbcomp!=0]
    Flist<-vector("list",nbgroups)
    Xlist<-vector("list",nbgroups)
    Xlistorig<-vector("list",nbgroups)
    Mlist<-vector("list",nbgroups)
    for(i in myFsel){
      Flist[[i]]<-read.csv2(file=file.path(OutputDir,modelvers,param_yaml$list_subfolder[["2"]],paste0("F_B",i,".csv")),row.names=1)
      Mlist[[i]]<-read.csv2(file=file.path(OutputDir,param_yaml$list_mainfolder[["2"]],paste0("Minvtot_",i,".csv")))
      }
    for(i in 1:nbgroups){
      Xlist[[i]]<-read.csv2(file=file.path(OutputDir,param_yaml$list_mainfolder[["1"]],paste0("Bcs_",i,".csv")),row.names=1)
      Xlistorig[[i]]<-read.csv2(file=file.path(OutputDir,param_yaml$list_mainfolder[["1"]],paste0("B_",i,".csv")),row.names=1)
      }
    P<-read.csv2(file=file.path(OutputDir,param_yaml$list_mainfolder[["2"]],"P.csv"))
    Ypred<-vector("list",nrow(E))

    for(q in 1:nrow(E)){
      Ypred[[q]]<-read.csv2(file=file.path(OutputDir,modelvers,param_yaml$list_subfolder[["7"]],paste0("Ypred_Eq",q,".csv")),row.names=1)
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
        RMSECV[[q]]<-read.csv2(file=paste0(pathcv,"/CV_Eq",q,".csv"),row.names=1)
        R2CV[[q]]<-read.csv2(file=paste0(pathcv,"/CV_R2_Eq",q,".csv"),row.names=1)
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
