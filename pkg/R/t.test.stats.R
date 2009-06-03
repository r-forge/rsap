t_test<-function(xbar,...){
  UseMethod("t_test")
}
 t_test.formula<-function(formula,data,subset,...){

  
      m <- match.call(expand.dots = FALSE)
      if (is.matrix(eval(m$data, parent.frame()))) 
        m$data <- as.data.frame(data)
    m[[1]] <- as.name("model.frame")
    m$... <- NULL
    mf <- eval(m, parent.frame())

    DNAME <- paste(names(mf), collapse = " by ")
    names(mf) <- NULL
    response <- attr(attr(mf, "terms"), "response")
    g <- factor(mf[[-response]])
    if (nlevels(g) != 2) 
        stop("grouping factor must have exactly 2 levels")
    DATA <- split(mf[[response]], g)
    names(DATA) <- c("x", "y")
#    browser()
 #   xbr<-tapply(mf[,1],mf[,2],mean)
 #   s<-tapply(mf[,1],mf[,2],sd)
 #   nn<-tapply(mf[,1],mf[,2],length)
    xbr<- as.numeric(lapply(DATA,mean))
    s<- as.numeric(lapply(DATA,sd))
    nn<- as.numeric(lapply(DATA,length))
    names(xbr)<-levels(g)
      
    y<-do.call("t_test",c(list(xbr),list(s),list(nn),list(...))) 
    invisible(y)
      
}

### Default t_test function
t_test.default<-function(xbar,sd,n,var.equal=F,alpha=.05,alternative=c("two.sided","less","greater"),method=c("p.value","conf.int","crit.val"),mu=0){
  alternative <- match.arg(alternative)
  method<-match.arg(method)
  twosamp <- length(xbar)==2

  if(twosamp){
    cat(paste("Two-sample t-Test   ( ",switch(method,p.value="p-value",conf.int="confidence interval",crit.val="critical value")," approach ) \n\n",sep=""))
  } else {
    cat(paste("One-sample t-Test   ( ",switch(method,p.value="p-value",conf.int="confidence interval",crit.val="critical value")," approach ) \n\n",sep=""))
  }

  if(alternative=="two.sided") {

    quant<-c(alpha/2,1-alpha/2)
  } else if (alternative=="less"){
    quant<-alpha
  } else {
    quant<-1-alpha
  }
  if(twosamp){
    if(var.equal){
      Sp<-sqrt(((n[1]-1)*sd[1]^2+(n[2]-1)*sd[2]^2)/(sum(n)-2))
      EST<-(xbar[1]-xbar[2])
      SE<-(Sp*sqrt(sum(1/n)))
      tobs<-EST/SE
      df<-(sum(n)-2)
      tcv<-round(qt(quant,df),4)
    } else{
      df<-(sd[1]^2/n[1]+sd[2]^2/n[2])^2/( (sd[1]^2/n[1])^2/(n[1]-1)+(sd[2]^2/n[2])^2/(n[2]-1))
      tcv<-round(qt(quant,df),4)
      EST<-(xbar[1]-xbar[2])
      SE<-(sqrt(sum(sd^2/n )))
      tobs<-EST/SE
    }} else{
      df<- n-1
      EST<-xbar
      SE<- sd/sqrt(n)
      tobs<-(EST-mu)/SE
      tcv<-round(qt(quant,df),4)
    }


  
  cvDR<-switch(alternative,
               two.sided=paste("Reject Ho: if t.obs <",tcv[1],"or if t.obs >",tcv[2],"\n"),
               less=paste("Reject Ho: if t.obs <",tcv[1],"\n"),
               greater=paste("Reject Ho: if t.obs >",tcv[1],"\n"))

  cvDR<-paste("df = ",round(df,4),"\n ",cvDR,sep="")
  
  alt.txt<-switch(alternative,two.sided="\u2260",less="<",greater=">")
 if(twosamp){
   cat("Ho:  \u03bc1 = \u03bc","2\n",sep="")
   cat("Ha:  \u03bc1",alt.txt,"\u03bc2 \n\n")
 } else {
   cat("Ho:  \u03bc = ",mu,"\n",sep="")
   cat("Ha:  \u03bc",alt.txt,mu," \n\n")
 }

  DR<-switch(method,p.value=paste("Reject Ho: if p-value < \u03b1 = ",alpha,"\n",sep=""),
             conf.int=paste("Reject Ho: if the ",100*(1-alpha),"% confidence interval does not contain ",mu,"\n",sep=""),
             crit.val=cvDR)
  cat("Decision Rule:\n",DR,"\n")

  cat("Calculate test statistic:\n\n")
  datfr<-data.frame(list(mean=xbar,s=sd,n=n))

#  browser()
  if(is.null(names(xbar))){
  row.names(datfr)<-if(twosamp) {c("  Sample 1 ","  Sample 2 ")} else {"  Sample 1 "}
  } else {
   row.names(datfr)<-names(xbar)
  }
  print(datfr)
  cat("\n")
  stat<-switch(method,
               crit.val=paste(if(var.equal){paste("    Sp = ",round(Sp,4),"\n",sep="")},
                        " t.obs = ",round(tobs,4),"\n",sep=""),
               p.value=paste(if(var.equal){paste("      Sp = ",round(Sp,4),"\n",sep="")},
                        "   t.obs = ",round(tobs,4),"\n      df = ",round(df,4),"\n p-value = ",sprintf("%.4f",switch(alternative,two.sided=2*pt(-abs(tobs),df),less=pt(tobs,df),greater=1-pt(tobs,df)  ),4),"\n"  ,sep=""),
               conf.int=paste(if(var.equal){paste("      Sp = ",round(Sp,4),"\n",sep="")},
                     if(!alternative=="two.sided"){"  "},   "   t.\u03b1",if(alternative=="two.sided"){"/2"}," = ",unique(abs(round(tcv,4))),"\n      df = ",round(df,4),"\n ", (1-alpha)*100,"% Conf Int = (",switch(alternative,
                                                                                                                                              two.sided=paste( sprintf("%.4f",EST-unique(abs(tcv))*SE)," , "  ,sprintf("%.4f",EST+unique(abs(tcv))*SE),sep=""),
                                                                                                                                              less=paste( " -INF , "  ,sprintf("%.4f",EST+unique(abs(tcv))*SE),sep=""),
                                                                                                                                              greater=paste( sprintf("%.4f",EST-unique(abs(tcv))*SE)," , INF ",sep="" )),")\n",sep="") )

  
  cat(stat)
  pv <- switch(alternative,two.sided=2*pt(-abs(tobs),df),less=pt(tobs,df),greater=1-pt(tobs,df) )

  cat("\nConclusion:\n ",if(pv<alpha){"Reject Ho:"} else {"Do not reject Ho:"},"\n")
  #tobs

}


mu<-function()  "\u03bc"
