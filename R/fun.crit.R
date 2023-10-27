.fun.crit<-function(Xr,Ftot,Ctot,Ttot,E,r,compk,Einfo=NULL,Wr,myxichi,s=.5,l=1){

 Cr<-Ctot[[r]]
 tcur<-Ttot[[r]][,compk]
 Q<-nrow(E)

 sumAtilde_p1<-0
 sumBtilde_p1<-0
 sumAtilde_p2<-0
 sumBtilde_p2<-0
 Psi.de.tdp<-0
 Psi.de.tdq<-0
 deltaPsi_p2<-0
 deltaPsi_p1<-0


 for(q in 1:Q){
   Hmr<-THEME:::.fun.Hmr(Ftot,Ctot,E,r,q,compk,Einfo=NULL)$Hmr
   dq<-order(E[q,1:(ncol(E)/2)]==1,decreasing=TRUE)[1]
   Fdq<-Ftot[[dq]]

   if(E[q,r+ncol(E)/2]==1){ #f is an explanatory component

    xir<-myxichi$xi[dq]
    ABexp<-lapply( 1:ncol(Fdq),function(j)THEME:::.fun.AB_R2exp(Cr,Fdq[,j],Wr,Hmr))
    Psi.de.texp<-sapply(1:ncol(Fdq),function(j){
      A<-ABexp[[j]]$A
      B<-ABexp[[j]]$B
      tAt<-as.numeric(crossprod(tcur,A)%*%tcur)
      tBt<-as.numeric(crossprod(tcur,B)%*%tcur)
      #tAt<-as.numeric(t(tcur)%*%A%*%tcur)
      #tBt<-as.numeric(t(tcur)%*%B%*%tcur)
      log(tAt/tBt)})
    Psi.de.tdq<-Psi.de.tdq+xir*sum(unlist(Psi.de.texp)) #Avant 11/03/2020 Psi.de.tdp+xir*sum(unlist(Psi.de.texp))

    Atilde_p2tp<-lapply(1:ncol(Fdq),function(j){
      A<-ABexp[[j]]$A
      #tAt<-as.numeric(t(tcur)%*%A%*%tcur)
      tAt<-as.numeric(crossprod(tcur,A)%*%tcur)
      Atilde<-(1/tAt)*A})

    sumAtilde_p2<-sumAtilde_p2+xir*Reduce("+",Atilde_p2tp)

    Btilde_p2tp<-lapply(1:ncol(Fdq),function(j){
      B<-ABexp[[j]]$B
      #tBt<-as.numeric(t(tcur)%*%B%*%tcur)
      tBt<-as.numeric(crossprod(tcur,B)%*%tcur)
      Btilde<-(1/tBt)*B})

    sumBtilde_p2<-sumBtilde_p2+xir*Reduce("+",Btilde_p2tp)

    deltaPsi_pq<-lapply(1:ncol(Fdq),function(j){2*(Atilde_p2tp[[j]]-Btilde_p2tp[[j]])%*%tcur})
    deltaPsi_p2<-deltaPsi_p2+xir*Reduce("+",deltaPsi_pq)
    }

   if(E[q,r]==1){  #f is dependent component
    ABdep<-THEME:::.fun.AB_R2dep(Cr,Wr,Hmr)
    A<-ABdep$A
    B<-ABdep$B
    tAt<-as.numeric(crossprod(tcur,A)%*%tcur)
    tBt<-as.numeric(crossprod(tcur,B)%*%tcur)
    #tAt<-as.numeric(t(tcur)%*%A%*%tcur)
    #tBt<-as.numeric(t(tcur)%*%B%*%tcur)
    Atilde<-(1/tAt)*A
    Btilde<-(1/tBt)*B
    xir<-myxichi$xi[r]
    sumAtilde_p1<-sumAtilde_p1+xir*(Atilde)
    sumBtilde_p1<-sumBtilde_p1+xir*(Btilde)

    Psi.de.tdp<-Psi.de.tdp+xir*log(tAt/tBt)
    deltaPsi_p1<-deltaPsi_p1+xir*2*(Atilde-Btilde)%*%tcur

    }
   }

   sumAtilde<-sumAtilde_p1+sumAtilde_p2
   sumBtilde<-sumBtilde_p1+sumBtilde_p2
   #rm(sumAtilde_p1,sumAtilde_p2,sumBtilde_p1,sumBtilde_p2)
   #gc()

    J<-ncol(Xr)
    omegaj<-1/J
    Ntilde_numerator<-0
    Ntilde_denominator<-0
    phi.de.t<-0

### HERE to continue diag
    #XrWr<-crossprod(Xr,Wr)
    #XrWrXr<-as.numeric(1/colSums(t(XrWr) * Xr))
    XrWr<-crossprod(Wr,Xr)
    XrWrXr<-as.numeric(1/colSums(XrWr * Xr)) ## CF XAVIER 2020
    CrWr<-crossprod(Cr,Wr)

    num<-sapply(1:J,function(j){
      CrWrXrj<-CrWr%*%Xr[,j]
      Nj<-XrWrXr[j]*tcrossprod(CrWrXrj)
      #t(tcur)%*%Nj%*%tcur
      crossprod(tcur,Nj)%*%tcur
      })

    Ntilde_numerator<-lapply(1:J,function(j){
      CrWrXrj<-CrWr%*%Xr[,j]
      Nj<-XrWrXr[j]*tcrossprod(CrWrXrj)
      as.numeric(omegaj*(num[j])^(l-1))*Nj
      })
    Ntilde_numerator<-Reduce("+",Ntilde_numerator)

    Ntilde_denominator<-omegaj*sum((num)^(l))
    phi.de.t<-omegaj*sum((num)^(l))

    Ntilde<-Ntilde_numerator*as.numeric(1/Ntilde_denominator)
    chir<-myxichi$chi[r]
    Ntilde_p3<-chir*(Ntilde)
    deltaPhi_p3<-2*chir*(Ntilde)%*%tcur
    phi.de.t<-chir*log((phi.de.t)^(1/l))

    ## Criteria and gradient
    mycrit<-Psi.de.tdq+Psi.de.tdp+phi.de.t
    CritNablagamma<-deltaPsi_p1+deltaPsi_p2+deltaPhi_p3

    ## ICI LE Gr à modifier avec lambda : Q Xavier lambda de la fun StructRel correspond à s ?
    Gr<-THEME:::.fun.StructRel(Cr,Wr,lambda=s)$G #avant lambda fix à .5
    CrWcr<-crossprod(Cr,Wr)%*%Cr
    Grinv<-solve(Gr)

    ## PatRed approach
    if(compk>1){
      Lr<-CrWcr%*%Ttot[[r]][,1:(compk-1)]
      varpiortho<-THEME:::.fun.PprojXonF(Lr,M=Grinv,X=NULL)$Pportho
      }else{varpiortho<-diag(1,nrow(Gr))}

    thetatcur<-varpiortho%*%((sumAtilde+Ntilde_p3)%*%tcur)
    delta<-(varpiortho%*%sumBtilde)+(as.numeric(chir)*Gr)
    deltainv<-try(solve(delta),TRUE)
    if(inherits(deltainv, "try-error")){deltainv<-chol2inv(chol(delta))}else{deltainv<-solve(delta)}

    tnew<-deltainv%*%thetatcur
    tnew<-THEME:::.fun.Norm(tnew,M=Gr)$x

    return(list(tnew=tnew,mycrit=mycrit,CritNablagamma=CritNablagamma))
    }

