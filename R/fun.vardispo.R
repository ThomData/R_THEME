.fun.blocksnames<-function(data,listname,myblock){
  vardispo<-colnames(data)[!colnames(data)%in%unlist(listname[!names(listname)%in%myblock])]
  return(list(vardispo=vardispo))
}
