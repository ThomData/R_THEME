res2<<-NULL

function(input, output, session) {
  session$onSessionEnded(stopApp)
  optplot<-NULL
  optplot2<-NULL
  windsizecorr<-1
  optdos2<-FALSE
  optdos<-FALSE
  myblock<-NULL
  Oldvalue<-"NA"
  listnamesblocks<-vector("list",1)
  names(listnamesblocks)<-"B1"

  ## Calibration and Validation dataset
  Xdatacal<- reactive({
    inFilecal <- input$filecal
    if (is.null(inFilecal))
      return(NULL)
    Xdatacal<-read.csv(inFilecal$datapath,header=input$header, sep=input$sep,  dec = input$dec,row.names = 1)
    res<-THEME:::.fun.Sortdata(Xdatacal)

    })

  Xdataval<- reactive({
    inFileval <- input$filecal
    if (is.null(inFileval))
      return(NULL)
    Xdataval<-read.csv(inFileval$datapath,header=input$header, sep=input$sep,  dec = input$dec)
    res<-THEME:::.fun.Sortdata(Xdataval)
    Xdataval<-res$dt
    })

  output$datausers <- renderDataTable({Xdatacal()$dt})
  output$codeusers <- renderDataTable({Xdatacal()$dtcodeblocks})

## MODEL DESIGN Windows
   observeEvent({input$Blocks
     input$nEquations},{
      if(!is.null(input$Blocks)){
     output$SelectModDes1=renderUI({
        numSelectModDes<-input$Blocks
        lapply(1:numSelectModDes, function(i) {
          selectInput(paste0("EQ1ModDes",i),h5(paste0("Block",i)),c(" ","X","T","Y"),width="120px")  #)
          })
        })
     output$SelectModDesComp=renderUI({
       numSelectModDes<-input$Blocks
       lapply(1:numSelectModDes, function(i) {
         selectInput(paste0("EQModDesComp",i),h5(paste0("Block",i)),seq(1,10,by=1),width="120px")  #)
       })
     })
    output$textEq2 <- renderText({NULL})
    output$textEq3 <- renderText({NULL})
    output$textBlocksRolesEq2 <- renderText({NULL})
    output$textBlocksRolesEq3 <- renderText({NULL})
    output$SelectModDes2=renderUI({return(NULL)})
    output$SelectModDes3=renderUI({return(NULL)})
    output$SelectModDes4=renderUI({return(NULL)})

    if(input$nEquations>=2){
      output$textEq2 <- renderText({"Eq2."})
      output$textBlocksRolesEq2 <- renderText({"Blocks' roles"})
      output$SelectModDes2=renderUI({
        numSelectModDes<-input$Blocks
        lapply(1:numSelectModDes, function(i) {
          selectInput(paste0("EQ2ModDes",i),h5(paste0("Block",i)),c(" ","X","T","Y"),width="120px")
          })
      })
    }

    if(input$nEquations>=3){
      output$textEq3 <- renderText({"Eq3."})
      output$textBlocksRolesEq3 <- renderText({"Blocks' roles"})
      output$SelectModDes3=renderUI({
      numSelectModDes<-input$Blocks
      lapply(1:numSelectModDes, function(i) {
        selectInput(paste0("EQ3ModDes",i),h5(paste0("Block",i)),c(" ","X","T","Y"),width="120px")
        })
      })
    }
    if(input$nEquations>=4){
      output$SelectModDes4=renderUI({
      numSelectModDes<-input$Blocks
      lapply(1:numSelectModDes, function(i) {
        selectInput(paste0("EQ4ModDes",i),h5(paste0("Block",i)),c(" ","X","T","Y"),width="120px")
        })
      })
    }

    Templistnamesblocks<-vector("list",as.numeric(input$Blocks))
    for(i in (1:input$Blocks)[(1:input$Blocks)%in%(1:length(listnamesblocks))]){
      Templistnamesblocks[i]<-listnamesblocks[i]
    }
    names(Templistnamesblocks)<-paste0("B",1:input$Blocks)
    listnamesblocks<<-Templistnamesblocks
    }
    })

   State = reactive({
     stat=NULL
     stat=list(THEME:::.fun.getStateEQ1(input),THEME:::.fun.getStateEQ2(input),THEME:::.fun.getStateEQ3(input),THEME:::.fun.getStateEQ4(input))
     stat
   })
   NBcomp = reactive({
     nbcomp=NULL
     nbcomp=THEME:::.fun.getStateComp(input)
     nbcomp
   })


## BLOCK DESIGN Window

   output$BlocksNB=renderUI({
     selectInput("Blocks","Number of Thematic Blocks: ",c(2:15))
     })
   output$EquationsNB=renderUI({
     selectInput("nEquations",h5(strong("Number of Equations: ")),choices=c(1:3))
     })
   
   output$SelectBlocksconfig=renderUI({
     myselgcc<-NULL
     if(!is.null(Xdatacal()$dtcodeblocks)){myselgcc<-Xdatacal()$VBAname}
     selectInput("SelectBlocksconfig1","Auto Variable-to-Block Allocation (VBA)",c("None",myselgcc)) #)
     })

   output$SelectBlock=renderUI({
     selectInput("SelectBlock1","Thematic Block contents:",paste0("B",1:input$Blocks)) #)
     })

   observeEvent(input$Blocks,{
     updateSelectInput(session,"SelectBlocksconfig1", selected = "None")
     })

   observeEvent(input$SelectBlocksconfig1,{
    if((input$SelectBlocksconfig1!="None")){
      Oldvalue<-input$SelectBlocksconfig1
      listnamesblocks<-lapply(1:input$Blocks,function(i)colnames(Xdatacal()$dt)[Xdatacal()$dtcodeblocks[paste0("VBA_",input$SelectBlocksconfig1),]%in%i])
      names(listnamesblocks)<-paste0("B",1:input$Blocks)
      listnamesblocks<<-listnamesblocks
      for(k in paste0("B",1:input$Blocks)){
        output$VarSel=renderUI({
          if(is.null(Xdatacal()$dt)){
             mylisnamedispo<-NULL}else{
              mylisnamedispo<-THEME:::.fun.blocksnames(data=Xdatacal()$dt,listname=listnamesblocks,myblock=myblock)$vardispo
              }
          selectInput('Variableselection', label=input$SelectBlock, choices=mylisnamedispo,selected=listnamesblocks[[myblock]],multiple=TRUE, selectize=TRUE)
          })
        }
    }else{
      listnamesblocks<-lapply(1:input$Blocks,function(i)NULL)
      names(listnamesblocks)<-paste0("B",1:input$Blocks)
      listnamesblocks<<-listnamesblocks
      for(k in paste0("B",1:input$Blocks)){
        output$VarSel=renderUI({
          mylisnamedispo<-THEME:::.fun.blocksnames(data=Xdatacal()$dt,listname=NULL,myblock=myblock)$vardispo
          selectInput('Variableselection', label=input$SelectBlock, choices=mylisnamedispo,selected=listnamesblocks[[myblock]],multiple=TRUE, selectize=TRUE)
        })
      }  
      
      }
    })


   observeEvent(input$SelectBlock1,{
    listnamesblocksRe()
    myblock<<-input$SelectBlock1
    output$VarSel=renderUI({
      if(is.null(Xdatacal()$dt)){
        mylisnamedispo<-NULL}else{
        mylisnamedispo<-THEME:::.fun.blocksnames(data=Xdatacal()$dt,listname=listnamesblocks,myblock=myblock)$vardispo
        }
      selectInput('Variableselection', label=input$SelectBlock, choices=mylisnamedispo,selected=listnamesblocks[[myblock]],multiple=TRUE, selectize=TRUE)
      })
    })

   listnamesblocksRe<-reactive({
     input$Variableselection  ##il faut le laisser
     if(!is.null(myblock)){
      listnamesblocks[myblock]<-list(input$Variableselection)
      listnamesblocks<<-listnamesblocks
      }
   })


## BUTTON For lauching THEME
   observeEvent(input$goButton, {

    listnamesblocksRe()
    res<-THEME:::.fun.listXE(Xdatacal()$dt,listnamesblocks,State(),input$nEquations,as.numeric(NBcomp()))
  
    if(res$LogComp=="Ok"){
      CheminUser=getwd()#"C:\\Resultats"
      OutputDir<<-paste0(CheminUser,"/THEME","_",format(Sys.time(), "%d%m%y_%H%M%S"))
      dir.create(OutputDir)
      E<-res$E
      Xlist<-res$Xlist
      nbcomp<-as.numeric(NBcomp())

      s<-as.numeric(input$opts)
      if(s==0){s=0.001} #TEMP SEE XB
      l<-as.numeric(input$optl)
      cvvChoice<-as.numeric(input$optCV)
      bwopondChoice<-as.numeric(input$optBW)
  
      updateProgress <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value) / 5
        }
        progress$set(value = value, detail = detail)
      }

      progress <- shiny::Progress$new(style = "notification")
      progress$set(message = "THEME running", value = 0)
      on.exit(progress$close())

      resTHEME<-THEME(Xlist,Xnew=NULL,E,nbcomp,s=s,l=l,OutputDir=OutputDir,cvvChoice=cvvChoice,bwopondChoice=bwopondChoice,updateProgress=updateProgress)

      Xtot<<-resTHEME$Xtot
      Ftot<<-resTHEME$Flist
      P<<-resTHEME$P
      optplot<<-TRUE
      }else{
        showNotification("Please check the design",closeButton = TRUE,type="error") #cat("Please check the design\n")
        }
      })


## PLOT indivuald and variables window
   output$Modelplot=renderUI({
     if(input$goButton<1){return(NULL)}
     vectmodel<-list.files(path =OutputDir,pattern="Model")

     if(length(vectmodel)==0){
       nbcomp<-NBcomp()
       if(!is.null(resE$rcov)){nbcomp[resE$rcov]<-"cov"}
       vectmodel<-paste0("Model_",paste(nbcomp,collapse="_"),sep="")
        }else{optdos<<-TRUE}

     selectInput("Modelplot1","Model:",vectmodel,selected=vectmodel[1],width="200px") #)
     })

   output$blocktoplot=renderUI({
     input$goButton
     versmodel<-input$Modelplot1
     if(optdos){
       myblocks<-gsub(".txt", "", substring(list.files(path =paste0(OutputDir,"/",versmodel,"/Components",sep=""),pattern="ot"),6))
       myblocks<-paste0("B",myblocks)
       selectInput("blocktoplot1","Theme:",myblocks,selected=myblocks[1],width="120px") #)
       }else{return(NULL)}
     })

   output$Xaxe=renderUI({
     input$goButton
     versmodel<-input$Modelplot1
     if(optdos){
      nbcomp<-read.table(file=paste(OutputDir,"/",versmodel,"/Design/nbcomp.txt",sep=""))$x #as.numeric(NBcomp()) #read
      group<-as.numeric(substring(input$blocktoplot1,2))
      if(is.null(nbcomp)){return(NULL)}
      selectInput("Xaxe1","x-axis:",1:nbcomp[group],selected=1,width="120px") #)
      }else{return(NULL)}
      })
   output$Yaxe=renderUI({
     input$goButton
     versmodel<-input$Modelplot1
     if(optdos){
      nbcomp<-read.table(file=paste(OutputDir,"/",versmodel,"/Design/nbcomp.txt",sep=""))$x #as.numeric(NBcomp()) #read
      group<-as.numeric(substring(input$blocktoplot1,2))
      if(is.null(nbcomp)){return(NULL)}
      selectInput("Yaxe1","y-axis:",1:nbcomp[group],selected=min(2,nbcomp[group]),width="120px") #)
      }else{return(NULL)}
      })
   output$mycolor=renderUI({
     mycol<-colnames(Xdatacal()$dtcodecol)
     if(length(mycol)==0){mycol<-NULL}
     selectInput("mycolor1","Color code:",c("NA",mycol),selected="",width="120px") #)
     })

   fun.plotindvar<-function(opt="none"){
     corr<-1
     if(opt=="copy"){corr<-.5}
     
     vers<-input$Modelplot1
     resTHEME<-THEME:::.load.THEME(OutputDir=OutputDir,vers=vers)
     
     group=as.numeric(substring(input$blocktoplot1,2))
     comp=c(as.numeric(input$Xaxe1),as.numeric(input$Yaxe1))
     
     if(diff(comp)==0){return(NULL)}
     mycex=as.numeric(input$mycex)*corr
     macol=1
     if(!is.null(input$mycolor1)){
       if(input$mycolor1!="NA"){macol<-Xdatacal()$dtcodecol[,input$mycolor1]}
       }
     labeloption="None"
     if(input$Labelprintplot=="Yes")labeloption="Visible"
     mycexlab=as.numeric(input$mycexlab)*corr
     mycexaxis=as.numeric(input$mycexaxis)*corr
     mycextitle=as.numeric(input$mycextitle)*corr
     
     resGraphind<-THEME:::.Plot.THEME.Individuals(Ftot,Xtot,group,comp,titre="",mycex,myoffset=0.5,macol,labeloption,mycexlab,mycexaxis,mycextitle)
     resGraphvar<-THEME:::.Plot.THEME.Variables(Ftot,Xtot,P,group,comp,titre="",mycex,myoffset=0.5,1,labeloption,mycexlab,mycexaxis,mycextitle)
     plist<-list(resGraphind$p,resGraphvar$p)
     grid.arrange(grobs=plist,ncol=length(plist))
     }
   

   observeEvent(input$PlotButton, {
     output$plotind <- renderPlot({#ly({
       if (file.exists(OutputDir)==FALSE){return(NULL)}
       if (is.null(optplot)){return(NULL)}else{
         fun.plotindvar()
         }
        },height = function() {
         session$clientData$output_plotind_width/2.5
          })
       })

## PLOT PREDICTION Window
   output$Yblockeqplot=renderUI({
     if(input$goButton<1){return(NULL)}
     vers<-input$Modelplot1 
     nbeqmodel<-1
     if(file.exists(paste(OutputDir,"/",vers,"/Design/E.txt",sep=""))){
       chem<-paste(OutputDir,"/",vers,"/Design/E.txt",sep="")
       nbeqmodel<-nrow(read.table(file=chem))
       optdos2<<-TRUE
       selectInput("Yblockeqplot1","Equation:",1:nbeqmodel,selected=1,width="200px") #)
     }else{return(NULL)}
   })

   output$Yvarplot=renderUI({
     input$goButton
     versmodel<-input$Modelplot1
     nbeq<-as.numeric(input$Yblockeqplot1)
     if(length(nbeq)==0){return(NULL)}
     if(optdos2){
       Ypred<-read.table(file=paste(OutputDir,"/",versmodel,"/Prediction/Ypred_SEERS_",nbeq,".txt",sep=""))
       myYvar<-c("all",colnames(Ypred))
       optplot2<<-TRUE
       selectInput("Yvarplot1","Variable(s):",myYvar,selected="all",width="120px") #)
       }else{return(NULL)}
      })

   fun.plotY<-function(opt="copy"){
         corr<-1
         if(opt=="copy"){corr=.5}
         vers<-input$Modelplot1 #input$ModelplotPred1
         resTHEME<-THEME:::.load.THEME(OutputDir=OutputDir,vers=vers)
         
         mycex=as.numeric(input$mycex)*corr
         macol=1
         
         if(!is.null(input$mycolor1)){
           if(input$mycolor1!="NA"){macol<-Xdatacal()$dtcodecol[,input$mycolor1]}
           }
         
         labeloption="None"
         windsizecorr<<-.5
         if(input$Labelprintplot=="Yes")labeloption="Visible"
         if(input$Yvarplot1=="all")windsizecorr<<-1
         mycexlab=as.numeric(input$mycexlab)*corr
         mycexaxis=as.numeric(input$mycexaxis)*corr
         mycextitle=as.numeric(input$mycextitle)*corr
         
         resGraphPred<-Plot.THEME.Prediction(resTHEME,neq=as.numeric(input$Yblockeqplot1),varsel=input$Yvarplot1,titre="",mycex,myoffset=0.5,macol,labeloption,mycexlab,mycexaxis,mycextitle)
         p<-resGraphPred$p
         p
         }   
     
   
 observeEvent(input$PlotButton, {
  output$plotY <- renderPlot({#ly({
     if (file.exists(OutputDir)==FALSE){return(NULL)}
     if (is.null(optplot2)) {
       return(NULL) }else{
         fun.plotY()
       }
   },height = function() {
     session$clientData$output_plotY_width*windsizecorr ##A modifier si 1 ou plusieurs: tester sans. ## PB couleur si plusieurs
   }
   )
   })
   

   ##SAVE PLOTS 
   output$SavePlotButton2<-downloadHandler(filename=function(){paste("Plot.Predictions_Eq",input$Yblockeqplot1,".png",sep="")},content=function(file){
     ggsave(file,plot=fun.plotY(opt="copy"),device="png")
     })
   output$SavePlotButton<-downloadHandler(filename=function(){paste("Plot.IndVar_",input$blocktoplot1,"_",input$Xaxe1,".",input$Yaxe1,".png",sep="")},content=function(file){
     ggsave(file,plot=fun.plotindvar(opt="copy"),device="png",width = 16, height = 8,dpi=1200, units = "cm")
     })
   
   ## PLOT MODEL CV Window
   output$YEqRMSEC=renderUI({
     if(input$goButton<1){return(NULL)}
     nbeqmodel<-1
     vers<-list.files(OutputDir,pattern="Model_",full.names =FALSE)[1]

     allmodels<-list.files(OutputDir,pattern="Model_",full.names =TRUE)
     if(length(allmodels)>0){
       pathCV<-paste0(allmodels,"/","CV")
       chem<-paste(allmodels[1],"/Design/E.txt",sep="")
       nbeqmodel<-nrow(read.table(file=chem))
       selectInput("YEqRMSEC1","Equation:",c("All",1:nbeqmodel),selected=1,width="200px") #)
     }else{return(NULL)}
   })

   fun.plotModsel<-function(opt="none"){
     allmodels<-list.files(OutputDir,pattern="Model_",full.names =TRUE)
     
     if (length(allmodels)==0){return(NULL)}
     if (input$optCV=="NA"){return(NULL)}
     if (input$optBW=="NA"){return(NULL)}
     
     chem<-paste(allmodels[1],"/Design/E.txt",sep="")
     nbeqmodel<-nrow(read.table(file=chem))
     pathCV<-paste0(allmodels,"/","CV")
     
     resRMSE<-THEME:::.fun.compilCV(pathCV,neq=nbeqmodel)
     
     if(input$YEqRMSEC1=="All"){
       resPlotRMSE<-THEME:::.Plot.THEME.RMSE(resRMSE)
       p1<-resPlotRMSE$pCVmean
       p2<-resPlotRMSE$pR2mean
     }else{
       resPlotRMSE<-THEME:::.Plot.THEME.RMSE(resRMSE,myq=as.numeric(input$YEqRMSEC1))
       p1<-resPlotRMSE$pCVall
       p2<-resPlotRMSE$pR2all
     }
     plist<-list(p1,p2)
     grid.arrange(grobs=plist,ncol=length(plist))
     }
   
   observeEvent(input$PlotButton3, {
     output$plotRMSE <- renderPlot({#ly({
       allmodels<-list.files(OutputDir,pattern="Model_",full.names =TRUE)
       if (length(allmodels)==0){return(NULL)}
       if (input$optCV=="NA"){return(NULL)}
       if (input$optBW=="NA"){return(NULL)}
       fun.plotModsel()
        
     },height = function() {
       session$clientData$output_plotRMSE_width/2.5
     }
     )
   })

   output$SavePlotButton3<-downloadHandler(filename=function(){paste("Plot.Modsel_",input$YEqRMSEC1,".png",sep="")},content=function(file){
     ggsave(file,plot=fun.plotModsel(opt="copy"),device="png",width = 16, height = 8,dpi=1200, units = "cm")
     })
   
   observeEvent(input$goButton, {
     hideTab(inputId = "tabselected", target = "Cross-validation")
     })


   }
