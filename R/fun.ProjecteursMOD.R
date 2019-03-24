## Notations
# R le nombre total de blocs
# Q le nombre d'equations
# E matrice qui contient le design (ligne = nbre equation, colonne 2*nombre de themes). 1 si le theme est utilise dans l'equation q, 0 sinon. Les R premieres colonnes pour indiquer les  groupes explicatifs et les R suivantes pour indiquer les groupes dependants.
# W matrice des ponderations
# M metrique  de X
# Xr ou X le bloc (explicatif ou dependant) r de dimension n*Jr
# n nombre individu
# Jr ou J le nombre de colonne du bloc Xr
# C les composantes principales non-normees de XM^.5
# U les vecteurs propres de XM^.5
# V=M^.5U vecteur qui permet de remonter au X
# F=XV
# Zq les covariables de l'equations q


#####################################
## Norme function
.fun.Norm<-function(x,M=1){
	nc<-length(x)
	if(!is.matrix(M)){M<-diag(1,nc)}
  Normeucl<-as.numeric(sqrt(t(x)%*%M%*%x))
  Normeucl.inv<-1/Normeucl
	x<-x*Normeucl.inv
	return(list(x=x,Normeucl=Normeucl,Normeucl.inv=Normeucl.inv))
	}

#####################################
# Projection on F function
.fun.PprojXonF<-function(F,M=NULL,X=NULL){
  if(is.null(M)){M<-diag(1,nrow(F))}
  if(is.null(X)){X<-diag(1,nrow(F))}
  FMF<-crossprod(F,M)%*%F
  FMX<-crossprod(F,(M%*%X))
  Pp<-(F%*%solve(FMF))%*%FMX
  Pportho<-(X-Pp)
	return(list(Pp=Pp,Pportho=Pportho))
	}

#####################################
## Scaling function
.fun.scale<-function(Xcal,resE,Xval=NULL){
  Xcalorig<-
  rtocr<-sort(c(resE$rcov,resE$rF))
  Xcalmean<-vector("list",resE$R)
  Xcalsd<-vector("list",resE$R)
  Mlist<-vector("list",resE$R)
  W<-as.matrix(1/nrow(Xcal[[1]])*diag(1,nrow(Xcal[[1]])))
  for(r in rtocr){
    dimnamesXcalr<-dimnames(Xcal[[r]])
    res<-scale(Xcal[[r]])
    M<-diag(1,nrow(Xcal[[r]]))
    Xcalmean[[r]]<-attr(res,"scaled:center")
    Xcalsd[[r]]<-attr(res,"scaled:scale")*sqrt(nrow(Xcal[[r]])-1)
    Xcal[[r]]<-(1/sqrt(ncol(M)-1)*M)%*%res
    n<-ncol(M)
    if(!is.null(Xval)){
      ncmat<-ncol(Xval[[r]])
      nrmat<-nrow(Xval[[r]])
      Xval[[r]]<-((Xval[[r]]-matrix(Xcalmean[[r]],ncol=ncmat,nrow=nrmat,byrow=TRUE))/matrix(Xcalsd[[r]],ncol=ncmat,nrow=nrmat,byrow=TRUE))
      }
    dimnames(Xcal[[r]])<- dimnamesXcalr
    Mlist[[r]]<-diag(1/diag(crossprod(Xcal[[r]],W)%*%Xcal[[r]]))
    }

  return(list(Xcal=Xcal,Xval=Xval,Xcalmean=Xcalmean,Xcalsd=Xcalsd,Mlist=Mlist))
  }

#####################################
# eigen vector functioon
.fun.eigen_non_null<-function(A,J=1,optoptim=FALSE,myseuil=10^(-4)){
	nc<-ncol(A)
	nr<-nrow(A)
	AA<-crossprod(A)
  mySVD<-eigen(AA)
  if(optoptim==TRUE){J<-sum(na.omit((mySVD$values^.5))> myseuil)}
  U<-mySVD$vectors[,1:J]
  Lambda<-diag(mySVD$values[1:J]^.5,J)
  return(list(U=U,Lambda=Lambda))
	}

#####################################
# Quasi power function
.fun.quasi_power<-function(A,a){
  Aeigen<-THEME:::.fun.eigen_non_null(A,optoptim=TRUE,myseuil=10^(-4))
  L<-Aeigen$Lambda
  if(a<0){a<- -a;L<-solve(L)}
  Aa<-Aeigen$U%*%(L)^a%*%t(Aeigen$U)
  return(list(Aa=Aa))
  }

#####################################
# function: Compute pc C from X
.fun.XtoC<-function(X,M=NULL,W){
  if(is.null(M)){M<-diag(1,ncol(X))}
  Mundemi<-THEME:::.fun.quasi_power(solve(M),-.5)$Aa
  Xs<-X%*%Mundemi
  XsWXs<-crossprod(Xs,W)%*%Xs
  mysvd<-THEME:::.fun.eigen_non_null(XsWXs,optoptim=TRUE)
  C<-Xs%*%mysvd$U
  V<-(Mundemi)%*%mysvd$U
  return(list(C=C,V=V))
  }

#####################################
# function: Build Clist from Xlist
.fun.XlisttoClist<-function(Xlist,W,E,Einfo=NULL){
   if(is.null(Einfo)){Einfo<-THEME:::.fun.rvect(E)}
   R<-Einfo$R
   Rtot<-Einfo$Rtot
   rF<-Einfo$rF
   Clist<-vector("list",Rtot)
   Vlist<-vector("list",Rtot)
   Mlist<-Vlist
   for(r in rF){
     resXtoC<-THEME:::.fun.XtoC(X=Xlist[[r]],W=W)
     Clist[[r]]<-resXtoC$C
     Vlist[[r]]<-resXtoC$V
     Ccur<-as.matrix(resXtoC$C)
     Mlist[[r]]<-crossprod(Ccur,W)%*%Ccur
    }
   return(list(Clist=Clist,Vlist=Vlist,Mlist=Mlist))
   }

#####################################
# functions: Compute A and B matrices required in criteria
.fun.AB_R2dep<-function(C,W,H){
  if(is.null(H)==FALSE){
    PHC<-THEME:::.fun.PprojXonF(F=H,M=W,X=C)$Pp
    }else{PHC<-diag(1,nrow(W))}
  CW<-crossprod(C,W)
  A<-CW%*%PHC
  B<-CW%*%C
  return(list(A=A,B=B))
  }

.fun.AB_R2exp<-function(C,d,W,H){
  if(is.null(H)==FALSE){
    PH<-THEME:::.fun.PprojXonF(F=H,M=W)
    PHortho<-PH$Pportho
    PH<-PH$Pp
    }else{
       PH<-diag(0,nrow(C)) #Sparce Matrix
       PHortho<-diag(1,nrow(C)) #Sparce Matrix
    }
  PHorthoC<-PHortho%*%C
  dW<-crossprod(d,W)
  num<-as.numeric(dW%*%(PH%*%d))
  WddW<-crossprod(dW)

  part2<-(num*W+WddW)%*%PHorthoC

  A<-crossprod(PHorthoC,part2)
  #A<-t(PHorthoC)%*%(num*W+W%*%d%*%t(d)%*%W)%*%PHorthoC

  B<-crossprod(C,W)%*%PHorthoC
  rm(PHorthoC)
  gc()
  return(list(A=A,B=B))
  }


#####################################
### Compute rns, rF, rcov R from object E
.fun.rvect<-function(E,nbcomp=NULL){
 Esumcol<-apply(E,2,sum)
 Rtot<-ncol(E)/2
 R<-sum((Esumcol[1:Rtot]+Esumcol[(Rtot+1):ncol(E)])>0)
 rns<-c(1:Rtot)[(Esumcol[1:Rtot]+Esumcol[(Rtot+1):ncol(E)])==0]
 Esscov<-E*(E!=2)
 Esumcol<-apply(Esscov,2,sum)
 Rtot<-ncol(E)/2
 R<-sum((Esumcol[1:Rtot]+Esumcol[(Rtot+1):ncol(E)])>0)
 Esscov<-E*(E!=2)
 Esumcol<-apply(Esscov,2,sum)
 rF<-c(1:Rtot)[(Esumcol[1:Rtot]+Esumcol[(Rtot+1):ncol(E)])>0]
 rcov<-c(1:Rtot)[apply((E[,(Rtot+1):ncol(E),drop=FALSE]==2),2,sum)!=0]

 if(length(c(rcov,rF,rns))==length(1:(ncol(E)/2))){
   if(sum((c(rcov,rF,rns)%in%c(1:(ncol(E)/2)))==FALSE)==0){}
   }else{print("test= Invalide")}
 if(is.null(nbcomp)==FALSE){nbcomp[!c(1:(ncol(E)/2))%in%rF]<-0}
 rEq<-vector("list")
 for(Eq in 1:nrow(E)){rEq[[Eq]]<-c(1:Rtot,1:Rtot)[E[Eq,]!=0]}

 return(list(R=R,Rtot=Rtot,rns=rns,rF=rF,rcov=rcov,nbcomp=nbcomp,rEq=rEq))
 }

#####################################
### Compute Varphi, Tau and Xhi required in the maximisation of gamma
.fun.xichi<-function(E,nbcomp,s=.5,optEquiPondTau="Global",optEquiPondVarPhi="Theme",Einfo=NULL){
## Equipond Tau
 # Global = equiponderation globale des themes
 # Local = equiponderation des themes au sein de chaque equation

## Equipond VarPhi
 # Theme = Mode equitheme
 # Comp = Mode equicomp

 if(is.null(Einfo)){Einfo<-THEME:::.fun.rvect(E)}
 R<-Einfo$R
 Rtot<-Einfo$Rtot
 rF<-Einfo$rF
 xivect<-rep(NA,Rtot)
 chivect<-rep(NA,Rtot)

 for(r in rF){
       Q<-nrow(E)
       Rq<-sapply(1:Q,function(i)sum(E[i,]))[as.logical((E[,r+ncol(E)/2]==1)+(E[,r]==1))]
       tau<-(1-s)/Q
       taur<-s/R
       VarPhir<-1
       if(optEquiPondTau=="Local"){taur<-s/Q*(sum(1/Rq))}
       if(optEquiPondVarPhi=="Comp"){VarPhir<-1/nbcomp[r]}
       xir<-tau*VarPhir
       chir<-taur*VarPhir
       xivect[r]<-xir
       chivect[r]<-chir
       }
 return(list(xi=xivect,chi=chivect))
 }

#####################################
## Structural Relevance function
.fun.StructRel<-function(Cr,W,optSR="VPI",lambda=.5){
  G<-diag(1,ncol(Cr))
  if(optSR=="VPI"){
    CrWcr<-crossprod(Cr,W)%*%Cr
    G<-(1-lambda)*CrWcr+lambda*diag(1,ncol(Cr))
    }
  return(list(G=G))
  }

#####################################
## function: Build Ftilde
.fun.Ftilde<-function(Ftot,Ctot,E,r,Einfo=NULL){
  opt<-"OK"
  if(is.null(Einfo)){Einfo<-THEME:::.fun.rvect(E)}
  R<-Einfo$R
  Rtot<-Einfo$Rtot
  rF<-Einfo$rF
  rcov<-Einfo$rcov
  rEq<-Einfo$rEq
  Ftilde<-NULL

  if(!r%in%rF){
    cat("Thematic-Block without component Fr \n")
    opt<-"NO"
    }
  if(opt=="OK"){
    rsel<-NULL
    for(Eq in 1:nrow(E)){
     if(r%in%rEq[[Eq]]){rsel<-c(rsel,rEq[[Eq]][!rEq[[Eq]]%in%r])}
    }
    ## cf comment ca fonctionne et essayer de vire la boucle et le cbind
    Ftilde<-lapply(rsel,function(i){
      if(2%in%E[,i+ncol(E)/2]){
        cbind(Ftilde,Ctot[[i]])
        }else{cbind(Ftilde,Ftot[[i]])}
      })
    Ftilde<-do.call(cbind, Ftilde)

    }

  return(list(Ftilde=Ftilde))
  }

#####################################
##function: Build Hmr
.fun.Hmr<-function(Ftot,Ctot,E,r,q,compk,Einfo=NULL){
  opt<-"OK"
  if(is.null(Einfo)){Einfo<-THEME:::.fun.rvect(E)}
  R<-Einfo$R
  Rtot<-Einfo$Rtot
  rF<-Einfo$rF
  rcov<-Einfo$rcov
  Hmr<-NULL
  if(!r%in%rF){
    cat("Thematic-Block without component Fr \n")
    opt<-"NO"
    }
  if(opt=="OK"){
    if(E[q,r+ncol(E)/2]==1){
      BlocExpEqq<-(1:Rtot)[E[q,(1:Rtot)+ncol(E)/2]==1]
      BlocCovEqq<-(1:Rtot)[E[q,(1:Rtot)+ncol(E)/2]==2]
      if(length(BlocCovEqq)>0){

        Hmr<-do.call(cbind, Xlist[BlocCovEqq])
        #Hmr<-NULL
        #for(i in BlocCovEqq){
        #  Hmr<-cbind(Hmr,Ctot[[i]])
        #  }
        }else{Hmr<-NULL}
      BlocExpEqq<-BlocExpEqq[BlocExpEqq!=r]
      if(compk>1){Hmr<-cbind(Hmr,Ftot[[r]][,1:(compk-1)])}

      Hmr<-cbind(Hmr,do.call(cbind, Ftot[BlocExpEqq]))
      #for(i in BlocExpEqq){
      #  Hmr<-cbind(Hmr,Ftot[[i]])
      #  }
      }
    if(E[q,r]==1){
      BlocExpEqq<-(1:Rtot)[E[q,(1:Rtot)+ncol(E)/2]==1]
      BlocCovEqq<-(1:Rtot)[E[q,(1:Rtot)+ncol(E)/2]==2]
      if(length(BlocCovEqq)>0){
        Hmr<-cbind(Hmr,do.call(cbind, Ctot[BlocCovEqq]))
        #for(i in BlocCovEqq){
        #  Hmr<-cbind(Hmr,Ctot[[i]])
        #  }
      }else{Hmr<-NULL}
      Hmr<-cbind(Hmr,do.call(cbind, Ftot[BlocExpEqq]))
      #for(i in BlocExpEqq){
      #  Hmr<-cbind(Hmr,Ftot[[i]])
      #  }
      }
    }

  return(list(Hmr=Hmr))
  }

#####################################
## Initialisation of Ftot and Ttot
.fun.initialisation<-function(Clist,W,E,nbcomp,Einfo=NULL){
  if(is.null(Einfo)){Einfo<-THEME:::.fun.rvect(E)}
  R<-Einfo$R
  Rtot<-Einfo$Rtot
  rF<-Einfo$rF
  Ftot<-vector("list",Rtot)
  Ttot<-vector("list",Rtot)

  for(r in rF){
    Ftot[[r]]<-Clist[[r]][,1:nbcomp[r],drop=FALSE]
    }

  for(r in rF){
    Cr<-Clist[[r]]
    CrW<-crossprod(Cr,W)
    Fcur<-Ftot[[r]]
    Tcur<-matrix(NA,ncol=nbcomp[r],nrow=ncol(Cr))

    for(k in 1:nbcomp[r]){
      Ftilde<-THEME:::.fun.Ftilde(Ftot,Clist,E,r,Einfo=NULL)$Ftilde
      A<-CrW%*%Ftilde
      if(k>1){
        L<-CrW%*%Fcur[,1:(k-1)]
        Atilde<-THEME:::.fun.PprojXonF(L,M=diag(1,nrow(L)),A)$Pportho
        }else{
          Atilde<-A#t(Cr)%*%W
          }
      AAtilde<-Atilde%*%t(Atilde)
      tnew<-THEME:::.fun.eigen_non_null(AAtilde,J=1,optoptim=FALSE)$U
      MM<-CrW%*%Cr
      tnew<-THEME:::.fun.Norm(tnew,M=MM)$x
      fnew<-Cr%*%tnew
      Fcur[,k]<-fnew
      Tcur[,k]<-tnew
      Ftot[[r]]<-Fcur
      }
    Ttot[[r]]<-Tcur
    }
  return(list(Ttot=Ttot,Ftot=Ftot))
  }

#####################################
## Function: Gamma criteria maximisation
.fun.maxcrit<-function(Ftot,Ttot,r,E,Ctot,Xr,Wr,compk,nbcomp,s=.5,l=1,optEquiPondTau="Global",optEquiPondVarPhi="Theme"){
  tcur<-Ttot[[r]][,compk]
  Cr<-Ctot[[r]]
  myxichi<-THEME:::.fun.xichi(E,nbcomp,s=s,optEquiPondTau,optEquiPondVarPhi)

  mycritcur<- -100000
  kkk<-0

  repeat{
   kkk<-kkk+1
   #t0<-Sys.time()
   rescrit<-THEME:::.fun.crit(Xr,Ftot,Ctot,Ttot,E,r,compk,Einfo=NULL,Wr,myxichi,s=s,l=l)
   #t1<-Sys.time()
   #print(t1-t0)
   mycrit<-rescrit$mycrit
   tnew<-rescrit$tnew
   CritNablagamma<-rescrit$CritNablagamma

      optionopt<-FALSE
      if(kkk>500){
          optionopt<-TRUE
          cat("nb iter maxcrit=",kkk)}
      if(optionopt){
        Ttottemp<-Ttot
        Ftottemp<-Ftot
        Ttottemp[[r]][,compk]<-tnew
        Ftottemp[[r]][,compk]<-Cr%*%tnew
        #t0<-Sys.time()
        mycritnew<-THEME:::.fun.crit(Xr,Ftottemp,Ctot,Ttottemp,E,r,compk,Einfo=NULL,Wr,myxichi,s=s,l=l)$mycrit
        #t1<-Sys.time()
        #print(t1-t0)
        k<-0
        tcand<-tnew
        #t0<-Sys.time()
        repeat{
          if(mycritnew>=mycrit){break}
          k<-k+1
          tcand<-tcur+tnew/2^k
          M<-crossprod(Cr,Wr)%*%Cr
          tcand<-THEME:::.fun.Norm(tcand,M=M)$x
          Ttottemp[[r]][,compk]<-tcand
          Ftottemp[[r]][,compk]<-Cr%*%tcand
          mycritnew<-THEME:::.fun.crit(Xr,Ftottemp,Ctot,Ttottemp,E,r,compk,Einfo=NULL,Wr,myxichi,s=s,l=l)$mycrit
          }
        #t1<-Sys.time()
        #print(t1-t0)
        tnew<-as.numeric(sign(crossprod(tnew,tcand)))*tcand
        }
      tnew<-as.numeric(sign(crossprod(tcur,tnew)))*tnew
      print(kkk)
      print(cor(tnew,tcur))
      print(sum((tnew-tcur)^2))
      if(sum((tnew-tcur)^2)<10^(-6)){break}else{tcur<-tnew}
      Ttot[[r]][,compk]<-tnew
      Ftot[[r]][,compk]<-Cr%*%tnew
      }

  return(list(tnew=tnew,Ttot=Ttot,Ftot=Ftot))
  }

