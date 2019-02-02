
#######################
## PLOT individuals
Plot.THEME.IndVar<-function(resTHEME,group=1,comp=c(1,2),titre="",mycex=0.8,myoffset=0.5,macol=1,labeloption="Visible",mycexlab=.8,mycexaxis=.8,mycextitle=2){
  Xl<-resTHEME$Xtot
  F<-resTHEME$Flist
  p.ind<-THEME:::.Plot.THEME.Individuals(F,Xl,group=group,comp=comp,titre=titre,mycex=mycex,myoffset=myoffset,macol=macol,labeloption=labeloption,mycexlab=mycexlab,mycexaxis=mycexaxis,mycextitle=mycextitle)$p
  p.var<-THEME:::.Plot.THEME.Variables(F,Xl,group=group,comp=comp,titre=titre,mycex=mycex,myoffset=myoffset,macol=macol,labeloption=labeloption,mycexlab=mycexlab,mycexaxis=mycexaxis,mycextitle=mycextitle)$p

  return(list(p.ind=p.ind,p.var=p.var))
  }
  
#######################
## PLOT individuals
.Plot.THEME.Individuals<-function(F,Xl,group=1,comp=c(1,2),titre="",mycex=0.8,myoffset=0.5,macol=1,labeloption="Visible",mycexlab=.8,mycexaxis=.8,mycextitle=2){
  Xlr<-as.matrix(Xl[[group]])
  Flr<-as.matrix(F[[group]])
  nr<-nrow(Xlr)
	nc<-ncol(Xlr)

	if(is.null(dimnames(Xlr)[[1]])){dimnames(Xlr)[[1]]<-paste0("I",1:nr)}
	if(is.null(dimnames(Xlr)[[2]])){dimnames(Xlr)[[2]]<-paste0("B",group,"_V",1:nc)}

  xmin<-trunc(min(Flr[,comp[1]]))-1
  xmax<-trunc(max((Flr[,comp[1]])))+1
  ymin<-trunc(min(Flr[,comp[2]]))-1
  ymax<-trunc(max((Flr[,comp[2]])))+1
  scaley<-pretty(c(ymin,ymax),n=10)
  scalex<-pretty(c(xmin,xmax),n=10)

	dtggplot<-data.frame(
	  "x"=Flr[,comp[1]],
	  "y"=Flr[,comp[2]],
	  "col"=rep("gray50",nr),
	  "code"=macol,
	  "label"=dimnames(Xlr)[[1]]
	  )
	
	set.seed(42)
	p=ggplot(data=dtggplot,aes(x=x,y=y, text=label)) #group=as.factor(collab))
	if(length(macol)==1){
	  p=p+geom_point()
	  }else{p=p+geom_point(aes(color=code))}
	if(labeloption=="Visible"){p=p+geom_text_repel(aes(label = label,color=macol),size = rel(mycex),show.legend =FALSE)} 
	p=p+labs(x =paste("Axis",comp[1]),y=paste("Axis",comp[2]),title=titre)
	p <- p + theme(axis.title.y = element_text(size = rel(mycexaxis), angle = 90),axis.text=element_text(size=rel(mycextitle)))
	p <- p + theme(axis.title.x = element_text(size = rel(mycexaxis), angle = 00),axis.text=element_text(size=rel(mycextitle)))
	p=p #+ coord_equal()
	
	return(list(p=p))
  }

#######################
## PLOT variables
.Plot.THEME.Variables<-function(F,Xl,P,group=1,comp=c(1,2),titre="",myoffset=0.1, mycex=.6,macol=4,labeloption="Visible",mycexlab=.8,mycexaxis=.8,mycextitle=2){
  Xlr<-as.matrix(Xl[[group]])
  Flr<-as.matrix(F[[group]])
  nr<-nrow(Xlr)
	nc<-ncol(Xlr)
	if(is.null(dimnames(Xlr)[[1]])){dimnames(Xlr)[[1]]<-1:nr}
	if(is.null(dimnames(Xlr)[[2]])){dimnames(Xlr)[[2]]<-paste0("B",group,"_V",1:ncol(Xlr))}
	
	monsd<-sqrt(diag(t(Xlr)%*%P%*%Xlr))
	X<-sapply(1:ncol(Xlr),function(r){Xlr[,r]/monsd[r]})

	monsd<-sqrt(diag(t(Flr)%*%P%*%Flr))
	Fcr<-sapply(1:ncol(Flr),function(r){Flr[,r]/monsd[r]})

	U1<-as.vector(t(Fcr[,comp[1]])%*%P%*%X)
	U2<-as.vector(t(Fcr[,comp[2]])%*%P%*%X)

	dtggplot<-data.frame(
	  "x"=U1,
	  "y"=U2,
	  "col"=rep("black",nc),
	  "collab"=macol,
	  "label"=dimnames(Xlr)[[2]]
	  )
	
	theta <- seq(0, 2*pi, len=100)
	dtcircle<-data.frame(
	  "x"=cos(theta),
	  "y"=sin(theta),
	  "col"=rep("gray",100)
	   )
	
	set.seed(42)
	p=ggplot(data=dtggplot,aes(x=x,y=y))
	p=p+scale_x_continuous(limits = c(-1.1,1.1))+scale_y_continuous(limits = c(-1.1,1.1))
	p=p+geom_segment(aes(x = 0, y = 0, xend = x, yend = y), colour =dtggplot$col)
	p=p+geom_path(data=dtcircle,aes(x,y),col=dtcircle$col)
	if(labeloption=="Visible"){p=p+geom_text_repel(aes(label = label),size = rel(mycex))}
	p=p+labs(x =paste("Axis",comp[1]),y=paste("Axis",comp[2]),title=titre)
	p=p + coord_equal()
  p=p+theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  p <- p + theme(axis.title.y = element_text(size = rel(mycexaxis), angle = 90))
  p <- p + theme(axis.title.x = element_text(size = rel(mycexaxis), angle = 00))
  
  return(list(p=p))
  }

#######################
## PLOT observed vs predicted values
Plot.THEME.Prediction<-function(resTHEME,varsel="all",neq=1,titre="",mycex=0.8,myoffset=0.5,macol=1,labeloption="Visible",mycexlab=.8,mycexaxis=.8,mycextitle=2){
  E<-resTHEME$E
  nbYblock<-order(E[neq,1:(ncol(E)/2)]==1,decreasing=TRUE)[1]
  Yblock<-resTHEME$Xlistorig[[nbYblock]]
  Ypred<-resTHEME$Ypred[[neq]]
  Yblock<-gather(data.frame(Yblock))
  Ypred<-gather(data.frame(Ypred))

  if(varsel!="all"){
    Ypred<-Ypred%>%filter(key==varsel)
    Yblock<-Yblock%>%filter(key==varsel) 
  }
  
  Y<-data.frame(Yblock,Ypred[,2],macol,rownames(resTHEME$Xlist[[nbYblock]]))
  colnames(Y)<-c("key","x","y","code","label")

  xmin<-trunc(min(Y[,"x"]))-1
  xmax<-trunc(max(Y[,"x"]))+1
  ymin<-trunc(min(Y[,"y"]))-1
  ymax<-trunc(max(Y[,"y"]))+1
  scaley<-pretty(c(ymin,ymax),n=10)
  scalex<-pretty(c(xmin,xmax),n=10)

  p=ggplot(data=Y,aes(x=x,y=y)) 
  p<-p+geom_smooth(method="lm",se=FALSE)
  if(varsel=="all"){p=p+facet_wrap( ~key,ncol=4,scales="free")}
  if(length(macol)==1){
    p=p+geom_point()
  }else{p=p+geom_point(aes(color=code))}
  if(labeloption=="Visible"){p=p+geom_text_repel(aes(label = label,color=code),size = rel(mycex),show.legend =FALSE)} 
  p=p+labs(x ="Measured",y="Predicted",title=titre)
  p <- p + theme(axis.title.y = element_text(size = rel(mycexaxis), angle = 90),axis.text=element_text(size=rel(mycextitle)))
  p <- p + theme(axis.title.x = element_text(size = rel(mycexaxis), angle = 00),axis.text=element_text(size=rel(mycextitle)))
 
  return(list(p=p))
  }


#######################
## PLOT RMSE
#GraphRMSE
.Plot.THEME.RMSE<-function(resCV,myq=1){
  
  RMSECV<-resCV$RMSECV
  R2CV<-resCV$R2CV
  R2CV$model<-as.factor(substring(R2CV$model,7))
  RMSECV$model<-as.factor(substring(RMSECV$model,7))
  
  RMSECV$model<-factor(RMSECV$model,levels=rev(levels(RMSECV$model)))
  R2CV$model<-factor(R2CV$model,levels=rev(levels(R2CV$model)))
  RMSECVmeanEQ<-data.frame(RMSECV%>%group_by(Eq,model)%>% summarise(CVmean = mean(CV)))
  RMSECVmean<-RMSECV%>%group_by(model)%>% summarise(CVmean = mean(CV))
  RMSECVmean<-RMSECVmean%>%mutate(Eq="Mean")
  RMSECVmean<-data.frame(RMSECVmean[,colnames(RMSECVmeanEQ)])
  RMSECVmeanTOT<-rbind(RMSECVmeanEQ,RMSECVmean)
  
  R2CVmeanEQ<-data.frame(R2CV%>%group_by(Eq,model)%>% summarise(R2mean = mean(R2)))
  R2CVmean<-R2CV%>%group_by(model)%>% summarise(R2mean = mean(R2))
  R2CVmean<-R2CVmean%>%mutate(Eq="Mean")
  R2CVmean<-data.frame(R2CVmean[,colnames(R2CVmeanEQ)])
  R2CVmeanTOT<-rbind(R2CVmeanEQ,R2CVmean)

  pCVmean<-ggplot(data=RMSECVmeanTOT,aes(group=Eq,x=model,y=CVmean,col=Eq))+geom_line()+theme(axis.text.x = element_text(angle = 45, hjust = 1))
  pR2mean<-ggplot(data=R2CVmeanTOT,aes(group=Eq,x=model,y=R2mean,col=Eq))+geom_line()+scale_y_continuous(limits=c(0,1))+theme(axis.text.x = element_text(angle = 45, hjust = 1))

  pCVall<-ggplot(data=RMSECV%>%filter(Eq==myq),aes(x=model,y=CV,group=name,col=name))+geom_line()+theme(axis.text.x = element_text(angle = 45, hjust = 1))
  pR2all<-ggplot(data=R2CV%>%filter(Eq==myq),aes(group=name,x=model,y=R2,col=name))+geom_line()+scale_y_continuous(limits=c(0,1))+theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(list(pCVmean=pCVmean,pR2mean=pR2mean,pCVall=pCVall,pR2all=pR2all))
  }
