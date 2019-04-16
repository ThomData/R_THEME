#' THEME

#'

#' THEmatic Model Exploration is a both exploratory and predictive multiblock-multiequation-technique.
#' The dataset is partitioned by the user into thematic blocks of variables. The thematic blocks are linked by a thematic model consisting of one or several equations.
#' Each equation links a dependent theme to explanatory themes. THEME searches each block for a given number of components capturing the theme's information that best serves the overall model.
#' Then, THEME can perform cross-validation-based backward component-selection to identify the number of really useful components in themes.
#' This backward selection produces a decreasing sequence of models, each of which is associated with a vector of component-numbers in themes.
#' THEME outputs the prediction-error rates of dependent variables for each model in the sequence, so that the user can compare the performance of the models in detail.
#' THEME also outputs the loadings of variables on components in all themes, the coefficients of the component-regression models, among other relevant numeric information.

#' @param Xlist List of R matrix thematic blocks of the thematic model used for calibration.
#' @param Xnew  List of R matrix thematic blocks of the thematic model used for validation (the default-value NULL causes the prediction-error rates to be calculated on the calibration data).
#' @param E Design matrix assigning a role to each theme in each equation: rows correspond to the equations, columns to the themes, and each entry of E may be Y,X or A, according to whether the theme is dependent, explanatory, or additional-explanatory in the equation.
#' When a theme is declared additional-explanatory, no component will ba calculated in it, and its variables will enter the model directly as additional covariates.
#' @param nbcomp Vector containing the numbers of components to extract in themes.
#' @param s Parameter that tunes the balance of the structural relevance (SR) of components with respect to the goodness of fit (GOF) in the THEME criterion.
#' Value 1 means that only the SR will be taken into account, and not the GOF. Value 0 means that only the GOF will be taken into account, and not the SR. Default-value .5 gives equal importance to SR and GOF.
#' @param l Parameter that tunes the locality of variable-bundles the components should focus on. Default 1 is the minimum value and indicates that no local focussing is to be performed.
#' @param OutputDir Path Directory where to save the outputs (default "C:\\").
#' @param cvvChoice [lko] Number of units to be left out in cross-validation (default NA indicates no cross-validation is to be performed).
#' @param bwopondChoice [bwsw] Weight of SR in the backward-selection criterion (default NA indicates that no backward selection is to be performed).
#' @param updateProgress argument restricted to THEME.UI (keep the NULL default).

#' @keywords THEME

THEME<-function(Xlist,Xnew=NULL,E,nbcomp,s=.5,l=1,OutputDir=NULL,cvvChoice=NA,bwopondChoice=NA,updateProgress = NULL,myEps=10^(-6)){


  param_yaml<-.fun_Buildfolders(opt.build=FALSE)

  optEquiPondTau="Global"
  optEquiPondVarPhi="Theme"

  nbeq<-nrow(E)
  nbg<-ncol(E)
  EX<-matrix(0,ncol=nbeq,nrow=nbg)
  EX[t(E)==" "]<-0
  EX[t(E)=="X"]<-1
  EX[t(E)=="Y"]<-0
  EX[t(E)=="T"]<-2
  EY<-matrix(0,ncol=nbeq,nrow=nbg)
  EY[t(E)==" "]<-0
  EY[t(E)=="X"]<-0
  EY[t(E)=="Y"]<-1
  EY[t(E)=="T"]<-0
  E<-cbind(t(EY),t(EX))
  P<-as.matrix(1/nrow(Xlist[[1]])*diag(1,nrow(Xlist[[1]])))
  W<-P
  Xtotorig<-Xlist
  THEME:::.sav.Xorig(Xtotorig,OutputDir=OutputDir)
  if(is.null(Xnew)){Xnew<-Xlist}
  resE<-THEME:::.fun.rvect(E,nbcomp=nbcomp)
  resscale<-THEME:::.fun.scale(Xtotorig,resE)
  Xtot<-resscale$Xcal
  Xtotsd<-resscale$Xcalsd
  Xtotmean<-resscale$Xcalmean
  Mlist<-resscale$Mlist
  res<-THEME:::.fun.XlisttoClist(Xtot,W,E,Einfo=NULL)
  Clist<-res$Clist
  Vlist<-res$Vlist
  Mlist<-res$Mlist

  THEME:::.sav.Data(Xtot,Mlist,P,OutputDir=OutputDir)
  THEME:::.fun.writeorreadyaml(dbY=Xtot[[resE$rEq[[length(resE$rEq)]][1]]])

  if(is.null(updateProgress)){cat("THEME running... ")}
  resTHEME<-THEME:::.fun.THEMEint(Xtot,Ctot=Clist,E,resE,W,s=s,l=l,optEquiPondTau=optEquiPondTau,optEquiPondVarPhi=optEquiPondVarPhi,myEps=myEps)
  if(is.null(updateProgress)){cat(" completed")}
  Ftot<-resTHEME$Ftot
  Ttot<-resTHEME$Ttot
  Ftotorig<-Ftot
  Ttotorig<-Ttot
  Wlist<-lapply(1:length(Ttot),function(i)if(is.null(Vlist[[i]])){NULL}else{Vlist[[i]]%*%Ttot[[i]]})

  THEMEcoeff<-THEME:::.THEME.coeff(Vlist,Ftot,Ttot,Xtot,Xtotorig,Xtotmean,Xtotsd,resE)

  R2<-THEMEcoeff$reslmR2
  THEMEpred<-THEME:::.THEME.Predict(THEMEcoeff,Xnew=Xnew,Xcal=Xtot,optnoneg=FALSE)
  THEME:::.sav.THEME(resTHEME,THEMEcoeff,THEMEpred,OutputDir=OutputDir)
  Coeffinlist<-THEMEcoeff$Coeffinlist
  Cstinlist<-THEMEcoeff$Cstinlist

#############################SECTION CROSS-VALIDATION
  if(!is.na(cvvChoice)){
    cond.optimTHEME<-1
    text <- paste0("Model ", paste(nbcomp,collapse=" "))
    if (is.function(updateProgress)){updateProgress(detail = text)}

    updateProgress2 <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress2$getValue()
        value <- value + (progress2$getMax() - value) / 3
        }
      progress2$set(value = value, detail = detail)
      }

    progress2 <- shiny::Progress$new(style = "notification")
    progress2$set(message = "CV running", value = 0)
    on.exit(progress2$close())
    if(is.null(updateProgress)){cat("THEME CrossValidation ", text,"... ")}

    rescv<-THEME:::.THEME.CrossVal(Xtotorig,E,resE,nbtest=cvvChoice,optordersample=NULL,optEquiPondTau=optEquiPondTau,optEquiPondVarPhi=optEquiPondVarPhi,exps=s,expl=l,updateProgress=updateProgress2)
    THEME:::.sav.THEMECrossVal(rescv,OutputDir=OutputDir)
    ordersamplecv<-rescv$ordersample

    progress2$close()
    if(is.null(updateProgress)){cat(" completed\n")}

    }else{cond.optimTHEME<-0}

  if(!is.na(bwopondChoice)){
    cond.optimTHEME<-cond.optimTHEME
    func.backwardselection<-THEME:::.THEME.backwardselectiong
    if(optEquiPondTau=="Local"){func.backwardselection<-THEME:::.THEME.backwardselectionl}
    resbw<-func.backwardselection(Xtot=Clist,Ftot=Ftot,Vtot=Ttot,Mtot=Mlist,P=P,E=E,OutputDir=OutputDir,pondSr=as.numeric(bwopondChoice),resE=resE)
    resbworder<-order(resbw$gamma,decreasing=FALSE)[1]
    }else{cond.optimTHEME<-0}

  resEopti<-resE
  Eopti<-E
  nbcompopti<-nbcomp
  namemod<-NULL

  if(cond.optimTHEME==1){
    MatCV<-vector("list",nbeq)
    repeat{
      nbcompopti[resbworder]<-nbcompopti[resbworder]-1
      if(nbcompopti[resbworder]<=0){
        nbcompopti[resbworder]<-0
        Eopti[,resbworder]<-0
        Eopti[,resbworder+length(nbcompopti)]<-0
        }

      if (is.function(updateProgress)) {
        text <- paste0("Model ", paste(nbcompopti,collapse=" "))
        updateProgress(detail = text)
        }

      resEopti<-THEME:::.fun.rvect(Eopti,nbcomp=nbcompopti)
      updateProgress2 <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
          value <- progress2$getValue()
          value <- value + (progress2$getMax() - value) / 5
        }
        progress2$set(value = value, detail = detail)
      }

      progress2 <- shiny::Progress$new(style = "notification")
      progress2$set(message = "CV running", value = 0)
      on.exit(progress2$close())

      rescv<-THEME:::.THEME.CrossVal(Xtotorig,Eopti,resEopti,nbtest=cvvChoice,optordersample=ordersamplecv,optEquiPondTau=optEquiPondTau,optEquiPondVarPhi=optEquiPondVarPhi,exps=s,expl=l,updateProgress=updateProgress2)
      progress2$close()

      THEME:::.sav.THEMECrossVal(rescv,OutputDir=OutputDir)
      namemod<-c(namemod,paste("M_",paste(nbcomp,collapse="_"),sep=""))
      for(icv in 1:nbeq){
        MatCV[[icv]]<-cbind(MatCV[[icv]],namemod=rescv$resCV[[icv]])
        }
      res<-THEME:::.fun.XlisttoClist(Xtot,W,Eopti,Einfo=resEopti)
      Clist<-res$Clist
      Vlist<-res$Vlist
      Mlist<-res$Mlist

      resTHEME<-THEME:::.fun.THEMEint(Xtot,Ctot=Clist,Eopti,resEopti,W,s=s,l=l,optEquiPondTau="Global",optEquiPondVarPhi="Theme",myEps=myEps)
      Ftot<-resTHEME$Ftot
      Ttot<-resTHEME$Ttot
      mycoeff<-THEME:::.THEME.coeff(Vlist,Ftot,Ttot,Xtot,Xtotorig,Xtotmean,Xtotsd,resEopti)
      mypred<-THEME:::.THEME.Predict(mycoeff,Xnew=Xtotorig,Xcal=Xtotorig,optnoneg=FALSE)
      THEME:::.sav.THEME(resTHEME,mycoeff,mypred,OutputDir=OutputDir)

      if(sum(nbcompopti[!is.na(resbw$gamma)]>1)==0){break}
      resbw<-func.backwardselection(Xtot=Clist,Ftot=Ftot,Vtot=Ttot,Mtot=Mlist,P=P,E=Eopti,OutputDir=OutputDir,pondSr=as.numeric(bwopondChoice),resE=resEopti)
      if(all(is.na(resbw$gamma))){break}

      resbworder<-order(resbw$gamma,decreasing=FALSE)[1]
      }
  }

return(list(Flist=Ftotorig,Tlist=Ttotorig,P=P,Xtotorig=Xtotorig,Xtot=Xtot,THEMEpred=THEMEpred,R2=R2,THEMEcoeff=Coeffinlist,THEMEcst=Cstinlist))
}

