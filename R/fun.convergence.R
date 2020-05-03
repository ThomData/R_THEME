## Function convergence
.fun.convergence<-function(resE,Ftot,Ftotold,aaarepeat,mycrit){
  optionbreak=FALSE

  mincor<-min(unlist(sapply(resE$rF,function(i)diag(abs(cor(Ftot[[i]],Ftotold[[i]]))))))
  oldw <- getOption("warn")
  options(warn = -1)
  meancorspace<-unlist(sapply(resE$rF,function(i)
    sapply(1:ncol(Ftot[[i]]),function(j)
      if(ncol(Ftot[[i]])==1){summary(lm(Ftot[[i]]~.,data = data.frame(Ftotold[[i]])))$r.squared
      }else{summary(lm(Ftot[[i]]~.,data = data.frame(Ftotold[[i]])))[[j]]$r.squared}
    )
  ))
  options(warn = oldw)

  meancorspace<-mean(meancorspace,na.rm=TRUE)

  if(aaarepeat>=1){cat("nber of iteration equals to ",aaarepeat,"; meancorspace",meancorspace,"; Criteria",mycrit,"\n")}
  if(aaarepeat>=5){if(meancorspace>.999){optionbreak=TRUE}}
  if(aaarepeat>=50){
    cat("nb of iteration equals to ",aaarepeat,"; STOP iteration before convergence \n")
    warningconv<-"PB"
    optionbreak=TRUE}

  return(list(optionbreak=optionbreak))
  }
