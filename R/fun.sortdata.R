.fun.Sortdata<-function(dt){

  namesind<-rownames(dt)
  #VBA = Variable-to-Block Allocation 
  mysel<-substr(namesind,1,4)%in%"VBA_"

  dtcodeblocks<-NULL
  VBAname<-NULL
  listeBlock<-vector("list",3)

  if(sum(mysel)>0){
      dtcodeblocks<-dt[mysel,]
      VBAname<-substring(rownames(dtcodeblocks),5)
      dt<-dt[!mysel,]
      }

  namesvar<-colnames(dt)

  tempsfac<-(sapply(1:ncol(dt),function(i)is.factor(dt[,i])) & sapply(1:ncol(dt),function(i)nlevels(dt[,i])<nrow(dt)))
  tempsnum<-sapply(1:ncol(dt),function(i)is.numeric(dt[,i]))
  tempschar<-sapply(1:ncol(dt),function(i)is.character(dt[,i]))|(sapply(1:ncol(dt),function(i)is.factor(dt[,i]))&sapply(1:ncol(dt),function(i)nlevels(dt[,i])==nrow(dt)))
  tempsavail<-((tempsfac+tempsnum+tempschar)==0)

  if(sum(tempsfac)>0){listeBlock[[2]]<-namesvar[tempsfac]}
  if(sum(tempsnum)>0){listeBlock[[1]]<-namesvar[tempsnum]}
  if(sum(tempschar)>0){listeBlock[[3]]<-namesvar[tempschar]}
  if(sum(tempsavail)>0){listeBlock[[3]]<-namesvar[tempsavail]}


  dtcodecol<-dt[,listeBlock[[2]],drop=FALSE]
  dtcodeblocks<-dtcodeblocks[,listeBlock[[1]]]
  dt<-dt[,listeBlock[[1]]]

  return(list(dt=dt,dtcodecol=dtcodecol,dtcodeblocks=dtcodeblocks,VBAname=VBAname))
  }


