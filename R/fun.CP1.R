
.fun.normind<-function(x,M=1){
  nc<-length(x)
  if(is.matrix(M)==FALSE){M<-diag(1,nc)}
  xc<-x 
  sdtemp<-1/as.numeric(sqrt(t(xc)%*%M%*%xc))
  x<-x*sdtemp
  return(list(x=x,sdtemp=sdtemp))
  }

####################

.fun.CP1<-function(X,M,P){
  
  nc<-ncol(X)
  nr<-nrow(X)
  
  u<-apply(X,2,sum)  
  u<-THEME:::.fun.normind(u,M)$x
  XM<-X%*%M
  aa<-0
  repeat{
    aa<-aa+1
    vectu<-XM%*%u
    un<-crossprod(X,P)%*%vectu
    un<-THEME:::.fun.normind(un,M)$x
    un<-as.numeric(sign(crossprod(u,un)))*un	
    if(sqrt(sum((un-u)^2))<10^(-7)){break}
    u<-un
  }
  u<-un
  F<-XM%*%u
  Fs<-F 
  lambda<-crossprod(Fs,P)%*%Fs
  return(list(u=u,F=F,valp=lambda))
  }
