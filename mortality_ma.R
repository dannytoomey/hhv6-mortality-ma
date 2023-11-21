# great book on meta-analyses - https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/
# good reference for interpreting bayesian results - https://www.ncbi.nlm.nih.gov/pmc/articles/PMC10021079/

library(readxl)
library(esc)
library(meta)
library(dplyr)
library(metafor)
library(grid)
library(brms)
library(baggr)
library(ggplot2)

#options(error = function() traceback(3))
options(mc.cores = parallel::detectCores())

excel = read_excel('DT simplified datasheet 2023 update.xlsx',sheet="Included studies 2023 update")
excel <- data.frame(excel)
excel <- excel[c("Study","Excluded.from.analysis.","Outcome","HHV6.Monitoring","Cohort.type","HHV6.Positive.Analyzed","HHV6.Negative.Analyzed","HHV6.Positive.Died","HHV6.Negative.Died")]
excel <- excel[excel$`Excluded.from.analysis.`=="No",]

args <- commandArgs(trailingOnly = TRUE)
if(length(args)<1){
	stop("
Please indicate whether you would like to analyze
studies that systematically tested for HHV-6 or not.

Options are:
  -sys    : Analyze studies that systematically monitored 
            for HHV-6.
  -nonsys : Analyze studies that did NOT systematically monitor 
            for HHV-6.
  -all    : Analyze systematic and non-systematically monitored 
            studies together.

These flags are used as standard command line flags, so that a command
generating an analysis for systematically monitored studies on a UNIX OS
would be:
  Rscript mortality_ma.R -sys

")
}
if(args[1] == "-all"){
	save_path <- paste0("",Sys.Date()," all data")	
}
if(args[1] == "-sys"){
	save_path <- paste0("",Sys.Date()," systematic")
	excel <- excel[excel$`HHV6.Monitoring`=="Systematic",]
}
if(args[1] == "-nonsys"){
	save_path <- paste0("",Sys.Date()," non systematic")
	excel <- excel[excel$`HHV6.Monitoring`!="Systematic",]
}

if(!file.exists(save_path)){
	dir.create(save_path)
}
if(!file.exists(paste0(save_path,"/plots"))){
	dir.create(paste0(save_path,"/plots"))
}
if(!file.exists(paste0(save_path,"/details"))){
	dir.create(paste0(save_path,"/details"))
}

om_data = excel[excel$Outcome=="OM",]
rm_data = excel[excel$Outcome=="RM",]
nrm_data = excel[excel$Outcome=="NRM",]

peds_data = excel[excel$Outcome=="OM"&excel$`Cohort.type`=="Pediatric",]
adult_data = excel[excel$Outcome=="OM"&excel$`Cohort.type`=="Adult",]
both_data = excel[excel$Outcome=="OM"&excel$`Cohort.type`=="Both",]

list <- c("om_data","rm_data","nrm_data","peds_data","adult_data","both_data")

for(i in list){
	if(i == "om_data"){
		data <- om_data
		title <- "HHV-6 and OM"
		rma_pdf <- "/plots/1 - RMA HHV-6 OM.pdf"
		rma_title <- "Odds of Overall Mortality with HHV-6 positivity"
		bayes_title <- ggtitle("Bayesian Aggregations: Overall Mortality and HHV-6 positivity","Posterior distributions with 95% intervals")
		bayes_pdf <- "/plots/1 - Bayes HHV-6 OM.pdf"
		text_save <- "/details/1 - om model details.txt"
		rma_pdf_width <- 10
		rma_pdf_height <- log(dim(data)[1],9)*5.5
		bayes_pdf_width <- 8
		bayes_pdf_height <- log(dim(data)[1],9)*5.5
	}
	if(i == "rm_data"){
		data <- rm_data
		title <- "HHV-6 and RM"
		rma_pdf <- "/plots/2 - RMA HHV-6 RM.pdf"
		rma_title <- "Odds of Relapse Mortality with HHV-6 positivity"
		bayes_title <- ggtitle("Bayesian Aggregations: Relapse Mortality and HHV-6 positivity","Posterior distributions with 95% intervals")
		bayes_pdf <- "/plots/2 - Bayes HHV-6 RM.pdf"
		text_save <- "/details/2 - rm model details.txt"
		rma_pdf_width <- 10
		rma_pdf_height <- log(dim(data)[1],9)*5.25
		bayes_pdf_width <- 8
		bayes_pdf_height <- log(dim(data)[1],9)*5.25
	}
	if(i == "nrm_data"){
		data <- nrm_data
		title <- "HHV-6 and NRM"
		rma_pdf <- "/plots/3 - RMA HHV-6 NRM.pdf"
		rma_title <- "Odds of Non-Relapse Mortality with HHV-6 positivity"
		bayes_title <- ggtitle("Bayesian Aggregations: Non-Relapse Mortality and HHV-6 positivity","Posterior distributions with 95% intervals")
		bayes_pdf <- "/plots/3 - Bayes HHV-6 NRM.pdf"
		text_save <- "/details/3 - nrm model details.txt"
		rma_pdf_width <- 10
		rma_pdf_height <- 4.5
		bayes_pdf_width <- 8
		bayes_pdf_height <- log(dim(data)[1],9)*5.25
	}
	if(i == "both_data"){
		data <- both_data
		title <- "HHV-6 and Mixed age cohort Overall Mortality"
		rma_pdf <- "/plots/4 - RMA HHV-6 Mixed Age OM.pdf"
		rma_title <- "Odds of Mixed age cohort Overall Mortality with HHV-6 positivity"
		bayes_title <- ggtitle("Bayesian Aggregations: Mixed age cohort Overall and HHV-6 positivity","Posterior distributions with 95% intervals")
		bayes_pdf <- "/plots/4 - Bayes HHV-6 Mixed Age OM.pdf"
		text_save <- "/details/4 - both model details.txt"
		rma_pdf_width <- 10
		rma_pdf_height <- log(dim(data)[1],9)*5.25
		bayes_pdf_width <- 8
		bayes_pdf_height <- log(dim(data)[1],9)*5.25
	}
	if(i == "adult_data"){
		data <- adult_data
		title <- "HHV-6 and Adult Overall Mortality"
		rma_pdf <- "/plots/5 - RMA HHV-6 Adult OM.pdf"
		rma_title <- "Odds of Adult Overall Mortality with HHV-6 positivity"
		bayes_title <- ggtitle("Bayesian Aggregations: Adult Overall Mortality and HHV-6 positivity","Posterior distributions with 95% intervals")
		bayes_pdf <- "/plots/5 - Bayes HHV-6 Adult OM.pdf"
		text_save <- "/details/5 - adult model details.txt"
		rma_pdf_width <- 10
		rma_pdf_height <- log(dim(data)[1],9)*5.25
		bayes_pdf_width <- 8
		bayes_pdf_height <- log(dim(data)[1],9)*5.25
	}
	if(i == "peds_data"){
		data <- peds_data
		title <- "HHV-6 and Pediatric Overall Mortality"
		rma_pdf <- "/plots/6 - RMA HHV-6 Peds OM.pdf"
		rma_title <- "Odds of Pediatric Overall Mortality with HHV-6 positivity"
		bayes_title <- ggtitle("Bayesian Aggregations: Pediatric Overall Mortality and HHV-6 positivity","Posterior distributions with 95% intervals")
		bayes_pdf <- "/plots/6 - Bayes HHV-6 Peds OM.pdf"
		text_save <- "/details/6 - peds model details.txt"
		rma_pdf_width <- 10
		rma_pdf_height <- log(dim(data)[1],9)*5.25
		bayes_pdf_width <- 8
		bayes_pdf_height <- log(dim(data)[1],9)*5.25
	}

	meta_bin <- metabin(event.e = as.numeric(`HHV6.Positive.Died`), 
		                n.e = as.numeric(`HHV6.Positive.Analyzed`),
		                event.c = as.numeric(`HHV6.Negative.Died`),
		                n.c = as.numeric(`HHV6.Negative.Analyzed`),
		                studlab = Study,
		                data = data,
		                sm = "OR",
		                method = "MH",
		                MH.exact = TRUE,
		                fixed = FALSE,
		                random = TRUE,
		                method.tau = "PM",
		                hakn = TRUE,
		                title = title)

	pdf(file = paste0(save_path,rma_pdf), width = rma_pdf_width, height = rma_pdf_height)
	meta::forest.meta(meta_bin,
				 	  sortvar = TE,
	             	  print.tau2 = TRUE,
	             	  leftlabs = c("Study", "Deaths","Total","Deaths","Total"),
	             	  lab.e = "HHV-6 +",
	             	  lab.c = "HHV-6 -",
	)
	grid.text(rma_title, x=0.5,y=0.95, gp=gpar(fontsize=16))

	if(dim(data)[1] <= 10){
		prep_ma_df <- data.frame('study'=data$Study,
						 'a'=as.numeric(data$`HHV6.Positive.Died`),
						 'n1'=as.numeric(data$`HHV6.Positive.Analyzed`),
						 'c'=as.numeric(data$`HHV6.Negative.Died`),
						 'n2'=as.numeric(data$`HHV6.Negative.Analyzed`))
		baggr_bin <- baggr(prep_ma_df,group="study",effect="logOR",pooling="partial",iter=20000,chains=10)
		p <- plot(baggr_bin,hyper=TRUE,vline=FALSE,style="areas") + bayes_title
		pdf(file = paste0(save_path,bayes_pdf), width = bayes_pdf_width, height = bayes_pdf_height, onefile=FALSE)
		print(p)
		dev.off()
	}

	sink(paste0(save_path,text_save))
	print(paste0(i," ",Sys.time()))
	print("random effects model --")
	print(summary(meta_bin))
	if(dim(data)[1] <= 10){ 
		print("")
		print("bayesian aggregation model --")
		print(baggr_bin) 
	}
	sink()

	print(summary(meta_bin))

}
