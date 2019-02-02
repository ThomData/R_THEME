.fun.listXE<-function(data,listnameblock,Design,nEq,nbcomp){
  nEq<-as.numeric(nEq)
  E<-NULL
  Xlist<-NULL
  nEqcheck<-sapply(1:nEq,function(i)is.null(Design[[i]]))
  cond1<-(sum(nEqcheck)==0)
  cond2<-(prod(sapply(1:nEq,function(i)sum(Design[[i]]=="Y",na.rm=TRUE))==1)==1)
  cond3<-(prod(sapply(1:nEq,function(i)sum(Design[[i]]=="X",na.rm=TRUE))>=1)==1)

    if(cond1&cond2&cond3){
    LogComp<-"Ok"
    allnames<-colnames(data)
    nblocks<-length(listnameblock)
    sizeblocks<-sapply(1:nblocks,function(i)sum(allnames%in%listnameblock[[i]]))
  
    if(sum(sizeblocks==0)==0){
      Xlist<-lapply(1:nblocks,function(i)data[,allnames%in%listnameblock[[i]],drop=FALSE])
      E<-matrix(unlist(Design),nrow=nEq,ncol=nblocks,byrow=TRUE)
      colnames(E)<-paste0("B",1:nblocks)
      cond4<-sapply(1:length(Xlist),function(i)ncol(Xlist[[i]])>=nbcomp[i])
      if(prod(cond4)==0){
        LogComp<-"Pb"  
        print("Pb")}
      }else{
        LogComp<-"Pb"  
        print("Pb")}}else{
          LogComp<-"Pb"  
          print("Pb")}

  return(list(Xlist=Xlist,E=E,LogComp=LogComp))
}
