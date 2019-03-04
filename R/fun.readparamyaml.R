.fun.writeorreadyaml<-function(dbY=NULL){
  #recupere les noms des lignes et colonnes
  #créer un vecteur ligne si besoin


  if(is.null(dbY)){
    saveparam_yaml<-yaml:::read_yaml(file=file.path(OutputDir,"param_name.yaml"))
    }else{
      names.col<-colnames(dbY)
      names.row<-rownames(dbY)
      saveparam_yaml<-yaml:::as.yaml(list(names.col=names.col,names.row=names.row))
      saveparam_yaml<-yaml:::yaml.load(saveparam_yaml)
      yaml:::write_yaml(saveparam_yaml,file=file.path(OutputDir,"param_name.yaml"))
      }
 
  return(list(saveparam_yaml=saveparam_yaml))
}


.fun.createyaml<-function(E,nbcomp,OutputDir,nameModel){
  appDir <- system.file("myapp", package = "THEME")
  param<-yaml:::read_yaml(file=file.path(appDir,"Config.yaml"))

  nbEq<-nrow(E)
  nbg<-ncol(E)/2
  Elist<-lapply(1:nbEq,function(i)E[i,])
  names(Elist)<-paste0("Eq",1:nbEq)

  saveparam_yaml<-yaml:::as.yaml(list(E=Elist,nbcomp=nbcomp,nbEq=nbEq,nbg=nbg))
  saveparam_yaml<-yaml:::yaml.load(saveparam_yaml)

  yaml:::write_yaml(saveparam_yaml,file=file.path(OutputDir,nameModel,param$list_subfoldername[[1]],"param.yaml"))

  }


.fun.readparamyaml<-function(OutputDir,nameModel){
  appDir <- system.file("myapp", package = "THEME")
  param<-yaml:::read_yaml(file=file.path(appDir,"Config.yaml"))


  if(is.null(nameModel)){
    saveparam_yaml<-yaml:::read_yaml(file=file.path(OutputDir,param$list_subfoldername[[1]],"param.yaml"))
    }else{ saveparam_yaml<-yaml:::read_yaml(file=file.path(OutputDir,nameModel,param$list_subfoldername[[1]],"param.yaml"))}
  nbg<-saveparam_yaml$nbg
  nbEq<-length(saveparam_yaml$E)
  nbcomp<-unlist(saveparam_yaml$nbcomp)
  E<-matrix(unlist(saveparam_yaml$E), ncol =2*nbg, byrow = TRUE)

  return(list(nbg=nbg,nbEq=nbEq,nbcomp=nbcomp,E=E))
  }


