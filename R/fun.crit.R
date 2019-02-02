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

   if(E[q,r+ncol(E)/2]==1){ #f is an explanatory componenet
    deltaPsi_pq<-0
    sumAtilde_p2tp<-0
    sumBtilde_p2tp<-0
    Psi.de.texp<-0

    for(j in 1:ncol(Fdq)){ 
      ABexp<-THEME:::.fun.AB_R2exp(Cr,Fdq[,j],Wr,Hmr)
      A<-ABexp$A
      B<-ABexp$B
      Atilde<-1/as.numeric(t(tcur)%*%A%*%tcur)*A
      Btilde<-1/as.numeric(t(tcur)%*%B%*%tcur)*B
      deltaPsi_pq<-deltaPsi_pq+2*(Atilde-Btilde)%*%tcur
      sumAtilde_p2tp<-sumAtilde_p2tp+Atilde 
      sumBtilde_p2tp<-sumBtilde_p2tp+Btilde 
      Psi.de.texp<-Psi.de.texp+log(as.numeric(t(tcur)%*%A%*%tcur)/as.numeric(t(tcur)%*%B%*%tcur))

      }

    xir<-myxichi$xi[dq]
    Psi.de.tdq<-Psi.de.tdq+xir*Psi.de.texp
    sumAtilde_p2<-sumAtilde_p2+xir*sumAtilde_p2tp
    sumBtilde_p2<-sumBtilde_p2+xir*sumBtilde_p2tp
    deltaPsi_p2<-deltaPsi_p2+xir*deltaPsi_pq
    }

   if(E[q,r]==1){  #f is dependent component
    ABdep<-THEME:::.fun.AB_R2dep(Cr,Wr,Hmr)
    A<-ABdep$A
    B<-ABdep$B
    Atilde<-1/as.numeric(t(tcur)%*%A%*%tcur)*A
    Btilde<-1/as.numeric(t(tcur)%*%B%*%tcur)*B
    xir<-myxichi$xi[r]
    sumAtilde_p1<-sumAtilde_p1+xir*(Atilde)
    sumBtilde_p1<-sumBtilde_p1+xir*(Btilde)

    Psi.de.tdp<-Psi.de.tdp+xir*log(as.numeric(t(tcur)%*%A%*%tcur)/as.numeric(t(tcur)%*%B%*%tcur))
    deltaPsi_p1<-deltaPsi_p1+xir*2*(Atilde-Btilde)%*%tcur

    }
   }

    J<-ncol(Xr)
    omegaj<-1/J
    Ntilde_numerator<-0
    Ntilde_denominator<-0
    phi.de.t<-0
    for(j in 1:J){
      Nj<-(t(Cr)%*%Wr%*%Xr[,j]%*%solve(t(Xr[,j])%*%Wr%*%Xr[,j])%*%t(Wr%*%Xr[,j])%*%Cr)
      Ntilde_numerator<-Ntilde_numerator+as.numeric(omegaj*(t(tcur)%*%Nj%*%tcur)^(l-1))*Nj
      Ntilde_denominator<-Ntilde_denominator+omegaj*(t(tcur)%*%Nj%*%tcur)^(l)
      phi.de.t<-phi.de.t+omegaj*(t(tcur)%*%Nj%*%tcur)^(l)
      }
    Ntilde<-Ntilde_numerator*as.numeric(1/Ntilde_denominator)
    chir<-myxichi$chi[r]
    Ntilde_p3<-chir*(Ntilde)
    deltaPhi_p3<-2*chir*(Ntilde)%*%tcur
    phi.de.t<-chir*log((phi.de.t)^(1/l))

    ## Criterai and gradient
    mycrit<-Psi.de.tdq+Psi.de.tdp+phi.de.t
    CritNablagamma<-deltaPsi_p1+deltaPsi_p2+deltaPhi_p3
    
    ## ICI LE Gr à modifié avec lambda
    Gr<-THEME:::.fun.StructRel(Cr,Wr,lambda=0.5)$G
    CrWcr<-t(Cr)%*%Wr%*%Cr
    Grinv<-solve(Gr)

    ## PatRed approach 
    if(compk>1){
      Lr<-CrWcr%*%Ttot[[r]][,1:(compk-1)]
      varpiortho<-THEME:::.fun.PprojXonF(Lr,M=Grinv,X=NULL)$Pportho 
      }else{varpiortho<-diag(1,nrow(Gr))}##HELP

    theta<-varpiortho%*%(sumAtilde_p1+sumAtilde_p2+Ntilde_p3)
    delta<-varpiortho%*%(sumBtilde_p1+sumBtilde_p2)+as.numeric(chir)*Gr
    deltainv<-try(solve(delta),TRUE)
    if(inherits(deltainv, "try-error")){deltainv<-chol2inv(chol(delta))}else{deltainv<-solve(delta)}
   
    tnew<-deltainv%*%theta%*%tcur
    tnew<-THEME:::.fun.Norm(tnew,M=Gr)$x
    
    ## Ping Approach (not implemented)
    #Grinv<-solve(Gr)
    #tnew<-Grinv%*%.fun.Norm(varpiortho%*%CritNablagamma,M=Grinv)$x
    #tnew<-THEME:::.fun.Norm(tnew,M=Gr)$x 
    
    return(list(tnew=tnew,mycrit=mycrit,CritNablagamma=CritNablagamma))
    }

