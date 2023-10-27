fun.blockcrossval<-function(nbtest,nind,optordersample=NULL){
  ## ADD an help
  ## Initialisation
  ## nbtest = nbre d'observations par groupes (nombre d'observations à mettre dans l'échantillon test à chaque itération)
  ## nind= nbre d'observations total

  if(is.null(optordersample)){ordersample<-sample(1:nind)}else{ordersample<-optordersample}
  listsamples<-split(ordersample, sort(ordersample%%round(nind/nbtest,0)))
  nbsteps<-length(listsamples)
  names(listsamples)<-paste0("CV",1:nbsteps)
  return(list(ordersample=ordersample,listsamples=listsamples,nbsteps=nbsteps))
  }
