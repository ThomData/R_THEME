
.fun_Buildfolders<-function(OutputDir=NULL,nameModel=NULL,opt.build=FALSE){
  appDir <- system.file("myapp", package = "THEME")
  param<-yaml:::read_yaml(file=file.path(appDir,"Config.yaml"))
  nam_mainfolder<-param$Main_folder
  nam_subfolder<-param$Sub_Folder
  mainnamesfolder<-names(param$list_mainfoldername)
  subnamesfolder<-names(param$list_subfoldername)
  list_mainfolders<-sapply(mainnamesfolder,function(i){param$list_mainfoldername[[i]]})
  list_subfolders<-sapply(subnamesfolder,function(i){param$list_subfoldername[[i]]})
  
  if(opt.build==TRUE & is.null(OutputDir)==FALSE){
    subnamesfolder<-subnamesfolder[!sapply(subnamesfolder,function(i)strsplit(param$list_subfoldername[[i]],"_")[[1]][1])%in%"NOD"]
    if(is.null(nameModel)){
        res<-lapply(mainnamesfolder,function(i){dir.create(file.path(OutputDir,param$list_mainfoldername[[i]]),showWarnings = FALSE)})
        }else{
          res<-lapply(subnamesfolder,function(i){dir.create(file.path(OutputDir,nameModel,param$list_subfoldername[[i]]),showWarnings = FALSE)})}
        }
  
  return(list(list_mainfolders=list_mainfolders,list_subfolders=list_subfolders,nam_mainfolder=nam_mainfolder,nam_subfolder=nam_subfolder,subnamesfolder=subnamesfolder))
  }
