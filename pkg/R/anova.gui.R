anova.gui<-function(){
	require(tcltk)
	tt<-tktoplevel()
	tkwm.title(tt,"ANOVA GUI")
	
	num <-tclVar()
	entry.num <-tkentry(tt,width="10",textvariable=num)
	

	Onrun <- function()
	{
		
	num <- as.numeric(tclvalue(num))
	
	anova1.gui(num)

		
	
	}
	Run.but <-tkbutton(tt,text="   Run   ", command=Onrun)
	
	
	fontHeading <- tkfont.create(family="times", size= 18, weight="bold")
	fontCopyright <- tkfont.create(family="times", size= 8, slant="italic")

	Label1a <- tklabel(tt,text="     Analysis of Variance     ", font=fontHeading)
	tkgrid(Label1a, columnspan=3)
	SpaceLabel <- tklabel(tt, text="        ")
	tkgrid(tklabel(tt,text="     "))
	SpaceLabel <- tklabel(tt, text="        ")
	ALabel2 <- tklabel(tt,text="How many samples do you have?")
	tkgrid( ALabel2, columnspan=3)
	SLabel <- tklabel(tt,text="        ")
	tkgrid(SLabel, entry.num, SpaceLabel)
	SLabela <- tklabel(tt,text="        ")
	tkgrid(SLabela,SLabela,SLabela)

	Labelb <- tklabel(tt,text="        ")
	tkgrid(Labelb, Run.but, Labelb)
	Labelc <- tklabel(tt,text="        ")
	tkgrid(Labelc, Run.but, Labelc)
	}


anova1.gui<-function(num){
	require(tcltk)
	tt<-tktoplevel()
	tkwm.title(tt,"Analysis of Variance GUI")



for(i in 1:num){
		eval(parse(text=paste("xbar",i," <- tclVar()",sep="")))
		eval(parse(text=paste("entry.xbar",i," <- tkentry(tt,width='10',textvariable=xbar",i,")",sep="")))
		eval(parse(text=paste("sd",i," <- tclVar()",sep="")))
		eval(parse(text=paste("entry.sd",i," <- tkentry(tt,width='10',textvariable=sd",i,")",sep="")))
		eval(parse(text=paste("n",i," <- tclVar()",sep="")))
		eval(parse(text=paste("entry.n",i," <- tkentry(tt,width='10',textvariable=n",i,")",sep="")))
		}





	
	alpha<-tclVar()
	entry.alpha <-tkentry(tt,width="10",textvariable=alpha)

	
	rb1 <- tkradiobutton(tt)
	rb2 <- tkradiobutton(tt)
	rb3 <- tkradiobutton(tt)
	rbValue <- tclVar("1")
	tkconfigure(rb1,variable=rbValue,value="1")  ##F-Statistic
	tkconfigure(rb2,variable=rbValue,value="2")  ##F-Crit
	tkconfigure(rb3,variable=rbValue,value="3")  ##P-Value


  

anova.test<- function(xbar,sd,n,alpha) {

	

	k<- length(xbar)
	n.len<- length(n)

	xgrand<- sum(xbar) / n.len 

	SSE<- sum((n-1)*(sd^2))
	SSTR<- sum(n*(xbar-xgrand)^2)
	

	MSTR<- SSTR/(k-1)

	MSE<- SSE/(sum(n)-k)

	F<- MSTR/MSE
	Fcrit<- qf(1-alpha,k-1,sum(n)-k)

	p.val<- 1-pf(F,k-1,sum(n)-k)

out<- c(F, Fcrit, p.val)
           

	names(out)<- c("F", "F-Crit", "P-Val")



cat("Analysis of Variance\n", sep="")
cat("\n",sep="")

cat("Decision Rule:\n", sep="")
cat("Reject Ho if p-value < 0.05\n", sep="")
cat("\n", sep="")

cat("Ho: All means are equal\n", sep="")
cat("Ha: not Ho\n", sep="")
cat("\n",sep="")


cat("F          F-Crit    P-value\n",sep="")
cat(out[1],out[2],out[3],"\n", sep="  ")
cat("\n",sep="")

if(F > Fcrit) dr<- cat("Decision: reject Ho\n", sep="")
if(F < Fcrit) dr<- cat("Decision: Do not reject Ho\n", sep="")



cat("\n",sep="")
cat(dr, sep="")

out

}
 




	Onrun <- function()
	{
		
		rbVal <- as.character(tclvalue(rbValue))
		



		for(i in 1:num){
		eval(parse(text=paste("xbar",i,"<-as.numeric(tclvalue(xbar",i,"))",sep="")))
		eval(parse(text=paste("sd",i,"<-as.numeric(tclvalue(sd",i,"))",sep="")))		
		eval(parse(text=paste("n",i,"<-as.numeric(tclvalue(n",i,"))",sep="")))
		}	
		
		XBAR <- xbar1
		SD <- sd1
		N <- n1

		for(i in 2:num){
			XBAR[i] <- eval(parse(text=paste("xbar",i,sep="")))
			SD[i] <- eval(parse(text=paste("sd",i,sep="")))
			N[i] <- eval(parse(text=paste("n",i,sep="")))
			}




		alpha<- as.numeric(tclvalue(alpha))		
		

		 
    		if (rbVal=="1") outa<- anova.test(XBAR,SD,N,alpha)
                if (rbVal=="2") outa<- anova.test(XBAR,SD,N,alpha)
                if (rbVal=="3") outa<- anova.test(XBAR,SD,N,alpha)

               
		



		if (rbVal=="1") cat("F= ",outa[1],sep="")
                if (rbVal=="2") cat("F-Crit= ",outa[2],sep="")
                if (rbVal=="3") cat("P-Value= ",outa[3],sep="")
		
    			
			
    	
		
	
	}
	Run.but <-tkbutton(tt,text="   Run   ", command=Onrun)
	
	VarEql <- tkcheckbutton(tt)
	VarEqlValue <- tclVar("0")
	
	fontHeading <- tkfont.create(family="times", size= 18, weight="bold")
	fontCopyright <- tkfont.create(family="times", size= 8, slant="italic")

	Label1 <- tklabel(tt,text="Analysis of Variance", font=fontHeading)
	tkgrid(Label1, columnspan=3)
	SpaceLabel <- tklabel(tt, text="        ")
	tkgrid(tklabel(tt,text="     "))
	
	SpaceLabel <- tklabel(tt, text="        ")
	Observed <- tklabel(tt, text="Mean")
	Stdev <- tklabel(tt, text="Standard Deviation")
	Size <- tklabel(tt,text= "  Sample Size  ")
	tkgrid(SpaceLabel,Observed,Stdev,Size)
	
for(i in 1:num){
	
	Mlabel<- tklabel(tt, text=paste("Sample ",i,sep="")) 
	
	eval(parse(text=paste("tkgrid(Mlabel, entry.xbar",i," ,entry.sd",i,",entry.n",i,")",sep="")))
	
	
	}

	tkgrid(tklabel(tt,text="     "))
	Alabel<-tklabel(tt, text="Alpha")
	tkgrid(Alabel, entry.alpha)
		

	SpaceLabel <- tklabel(tt, text="        ")
	tkgrid(SpaceLabel)
	SpaceLabel <- tklabel(tt, text="        ")

	tkgrid(SpaceLabel, SpaceLabel, SpaceLabel)

	
	
	tkgrid(SpaceLabel, SpaceLabel, SpaceLabel)

	tkgrid(tklabel(tt,text="F-Value "),tklabel(tt,text="F-Crit "),tklabel(tt,text="P-Value "))
	tkgrid(rb1, rb2, rb3)
	SpaceLabel <- tklabel(tt, text="        ")
	tkgrid(SpaceLabel, Run.but, SpaceLabel)
	tkgrid(tklabel(tt,text="     "))
}





