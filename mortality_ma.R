# great book on meta-analyses - https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/
# good reference for interpreting bayesian results - https://www.ncbi.nlm.nih.gov/pmc/articles/PMC10021079/

lapply(c("openxlsx","esc","meta","dplyr","metafor","grid","brms","baggr","rstan","ggplot2"),require,character.only=TRUE)

#options(error = function() traceback(3))
options(mc.cores = parallel::detectCores())

excel = read.xlsx('Full Dataset - Mortality after HCT and HHV-6.xlsx',sheet="Included studies 2023 update")
excel <- data.frame(excel)
excel <- excel[c("Study","Excluded.from.analysis.","Outcome","HHV6.Monitoring","Cohort.type","HHV6.Positive.Analyzed","HHV6.Negative.Analyzed","HHV6.Positive.Died","HHV6.Negative.Died","Stem.cell.source","Follow.up.period")]
excel <- excel[excel$`Excluded.from.analysis.`=="No",]
colnames(excel)[colnames(excel) == 'Stem.cell.source'] <- 'Stem cell source'
colnames(excel)[colnames(excel) == 'Cohort.type'] <- 'Age of cohort'
colnames(excel)[colnames(excel) == 'Follow.up.period'] <- 'Follow up period'

save_path <- paste0("local_results/",gsub(":",";",substr(Sys.time(),1,19)),"")

if(!file.exists("local_results/")){
	dir.create("local_results/")
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

list <- c("om_data","rm_data","nrm_data")

for(i in list){
	if(i == "om_data"){
		data <- om_data
		title <- "HHV-6 and OM"
		rma_pdf <- "/plots/1 - RMA HHV-6 OM.pdf"
		stem_cell_rma_pdf <- "/plots/1 - RMA HHV-6 OM by Stem Cell Source.pdf"
		age_rma_pdf <- "/plots/1 - RMA HHV-6 OM by Age.pdf"
		fup_rma_pdf <- "/plots/1 - RMA HHV-6 OM by Follow-up Period.pdf"
		funnel_rma_pdf <- "/plots/1 - Funnel Plot HHV-6 OM.pdf"
		rma_title <- "Odds of Overall Mortality with HHV-6 positivity"
		stem_cell_rma_title <- "Odds of Overall Mortality with HHV-6 positivity by Stem Cell Source"
		age_rma_title <- "Odds of Overall Mortality with HHV-6 positivity by Age"
		fup_rma_title <- "Odds of Overall Mortality with HHV-6 positivity by Follow-up Period"
		funnel_rma_title <- "Publication Bias for Studies Reporting Odds of Overall Mortality with HHV-6 positivity"
		bayes_title <- ggtitle("Bayesian Aggregations: Overall Mortality and HHV-6 positivity","Posterior distributions with 95% intervals")
		bayes_pdf <- "/plots/1 - Bayes HHV-6 OM.pdf"
		text_save <- "/details/1 - om model details.txt"
		rma_pdf_width <- 10
		rma_pdf_height <- log(dim(data)[1],9)*5.75
		bayes_pdf_width <- 8
		bayes_pdf_height <- log(dim(data)[1],9)*5.75
	}
	if(i == "rm_data"){
		data <- rm_data
		title <- "HHV-6 and RM"
		rma_pdf <- "/plots/2 - RMA HHV-6 RM.pdf"
		stem_cell_rma_pdf <- "/plots/2 - RMA HHV-6 RM by Stem Cell Source.pdf"
		age_rma_pdf <- "/plots/2 - RMA HHV-6 RM by Age.pdf"
		fup_rma_pdf <- "/plots/2 - RMA HHV-6 RM by Follow-up Period.pdf"
		funnel_rma_pdf <- "/plots/2 - Funnel Plot HHV-6 RM.pdf"
		rma_title <- "Odds of Relapse Mortality with HHV-6 positivity"
		stem_cell_rma_title <- "Odds of Relapse Mortality with HHV-6 positivity by Stem Cell Source"
		age_rma_title <- "Odds of Relapse Mortality with HHV-6 positivity by Age"
		fup_rma_title <- "Odds of Relapse Mortality with HHV-6 positivity by Follow-up Period"
		funnel_rma_title <- "Publication Bias for Studies Reporting Odds of Relapse Mortality with HHV-6 positivity"
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
		stem_cell_rma_pdf <- "/plots/3 - RMA HHV-6 NRM by Stem Cell Source.pdf"
		age_rma_pdf <- "/plots/3 - RMA HHV-6 NRM by Age.pdf"
		fup_rma_pdf <- "/plots/3 - RMA HHV-6 NRM by Follow-up Period.pdf"
		funnel_rma_pdf <- "/plots/3 - Funnel Plot HHV-6 NRM.pdf"
		rma_title <- "Odds of Non-Relapse Mortality with HHV-6 positivity"
		stem_cell_rma_title <- "Odds of Non-Relapse Mortality with HHV-6 positivity by Stem Cell Source"
		age_rma_title <- "Odds of Non-Relapse Mortality with HHV-6 positivity by Age"
		fup_rma_title <- "Odds of Non-Relapse Mortality with HHV-6 positivity by Follow-up Period"
		funnel_rma_title <- "Publication Bias for Studies Reporting Odds of Non-Relapse Mortality with HHV-6 positivity"
		bayes_title <- ggtitle("Bayesian Aggregations: Non-Relapse Mortality and HHV-6 positivity","Posterior distributions with 95% intervals")
		bayes_pdf <- "/plots/3 - Bayes HHV-6 NRM.pdf"
		text_save <- "/details/3 - nrm model details.txt"
		rma_pdf_width <- 10
		rma_pdf_height <- 4.5
		bayes_pdf_width <- 8
		bayes_pdf_height <- log(dim(data)[1],9)*5.25
	}

	cat(paste0(i," ",Sys.time(),"\n"),file=paste0(save_path,text_save),append = TRUE)

	meta_bin <- metabin(
        event.e = as.numeric(`HHV6.Positive.Died`),
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
        title = title
	)
	cat("\n -- random effects model --\n\n",file=paste0(save_path,text_save),append = TRUE)
    cat(paste0(capture.output(summary(meta_bin)),"\n"),file=paste0(save_path,text_save),append = TRUE)
	pdf(file = paste0(save_path,rma_pdf), width = rma_pdf_width, height = rma_pdf_height)
	metafor::forest(
        meta_bin,
        sortvar = TE,
        print.tau2 = TRUE,
        leftlabs = c("Study", "Deaths","Total","Deaths","Total"),
        lab.e = "HHV-6 +",
        lab.c = "HHV-6 -",
	)
	grid.text(rma_title, x=0.5,y=0.95, gp=gpar(fontsize=16))

    stem_cell_meta_bin <- metabin(
        event.e = as.numeric(`HHV6.Positive.Died`),
        n.e = as.numeric(`HHV6.Positive.Analyzed`),
        event.c = as.numeric(`HHV6.Negative.Died`),
        n.c = as.numeric(`HHV6.Negative.Analyzed`),
        studlab = Study,
        subgroup= `Stem cell source`,
        data = data,
        sm = "OR",
        method = "MH",
        MH.exact = TRUE,
        fixed = FALSE,
        random = TRUE,
        method.tau = "PM",
        hakn = TRUE,
        title = title
    )
    cat("\n -- stem cell source subgroup analysis --\n\n",file=paste0(save_path,text_save),append = TRUE)
    cat(paste0(capture.output(summary(stem_cell_meta_bin)),"\n"),file=paste0(save_path,text_save),append = TRUE)
   	pdf(file = paste0(save_path,stem_cell_rma_pdf), width = rma_pdf_width, height = rma_pdf_height*1.4)
   	metafor::forest(
        stem_cell_meta_bin,
        sortvar = TE,
        print.tau2 = TRUE,
        leftlabs = c("Study", "Deaths","Total","Deaths","Total"),
        lab.e = "HHV-6 +",
        lab.c = "HHV-6 -",
   	)
   	grid.text(stem_cell_rma_title, x=0.5,y=0.95, gp=gpar(fontsize=16))
    dev.off()

    age_meta_bin <- metabin(
        event.e = as.numeric(`HHV6.Positive.Died`),
        n.e = as.numeric(`HHV6.Positive.Analyzed`),
        event.c = as.numeric(`HHV6.Negative.Died`),
        n.c = as.numeric(`HHV6.Negative.Analyzed`),
        studlab = Study,
        subgroup= `Age of cohort`,
        data = data,
        sm = "OR",
        method = "MH",
        MH.exact = TRUE,
        fixed = FALSE,
        random = TRUE,
        method.tau = "PM",
        hakn = TRUE,
        title = title
    )
    cat("\n -- age source subgroup analysis --\n\n",file=paste0(save_path,text_save),append = TRUE)
    cat(paste0(capture.output(summary(age_meta_bin)),"\n"),file=paste0(save_path,text_save),append = TRUE)
   	pdf(file = paste0(save_path,age_rma_pdf), width = rma_pdf_width, height = rma_pdf_height*1.4)
   	metafor::forest(
        age_meta_bin,
        sortvar = TE,
        print.tau2 = TRUE,
        leftlabs = c("Study", "Deaths","Total","Deaths","Total"),
        lab.e = "HHV-6 +",
        lab.c = "HHV-6 -",
   	)
   	grid.text(age_rma_title, x=0.5,y=0.95, gp=gpar(fontsize=16))
    dev.off()

    fup_meta_bin <- metabin(
        event.e = as.numeric(`HHV6.Positive.Died`),
        n.e = as.numeric(`HHV6.Positive.Analyzed`),
        event.c = as.numeric(`HHV6.Negative.Died`),
        n.c = as.numeric(`HHV6.Negative.Analyzed`),
        studlab = Study,
        subgroup= `Follow up period`,
        data = data,
        sm = "OR",
        method = "MH",
        MH.exact = TRUE,
        fixed = FALSE,
        random = TRUE,
        method.tau = "PM",
        hakn = TRUE,
        title = title
    )
    cat("\n -- follow-up period source subgroup analysis --\n\n",file=paste0(save_path,text_save),append = TRUE)
    cat(paste0(capture.output(summary(fup_meta_bin)),"\n"),file=paste0(save_path,text_save),append = TRUE)
   	pdf(file = paste0(save_path,fup_rma_pdf), width = rma_pdf_width, height = rma_pdf_height*1.2)
   	metafor::forest(
        fup_meta_bin,
        sortvar = TE,
        print.tau2 = TRUE,
        leftlabs = c("Study", "Deaths","Total","Deaths","Total"),
        lab.e = "HHV-6 +",
        lab.c = "HHV-6 -",
   	)
   	grid.text(fup_rma_title, x=0.5,y=0.95, gp=gpar(fontsize=16))
    dev.off()

    if(dim(data)[1] >= 10){
        # peters bias used for binary data
        cat("\n -- publication bias --\n\n",file=paste0(save_path,text_save),append = TRUE)
        cat(paste0(capture.output(print(metabias(meta_bin, method.bias = "peters"))),"\n"),file=paste0(save_path,text_save),append = TRUE)
        pdf(file = paste0(save_path,funnel_rma_pdf), width = 10, height = 10)
        meta::funnel(meta_bin,studlab=TRUE)
        grid.text(funnel_rma_title, x=0.5,y=0.95, gp=gpar(fontsize=16))
        dev.off()
    }

    if(dim(data)[1] <= 10){
    	prep_ma_df <- data.frame(
                'study'=data$Study,
                'a'=as.numeric(data$`HHV6.Positive.Died`),
                'n1'=as.numeric(data$`HHV6.Positive.Analyzed`),
                'c'=as.numeric(data$`HHV6.Negative.Died`),
                'n2'=as.numeric(data$`HHV6.Negative.Analyzed`)
    	)
    	baggr_bin <- baggr(prep_ma_df,group="study",effect="logOR",pooling="partial",iter=20000,chains=10)
        cat("\n -- bayesian aggregation model --\n\n",file=paste0(save_path,text_save),append = TRUE)
        if (max(rstan:::summary_sim(baggr_bin$fit@sim)$rhat) > 1.05) {
            cat("Operation terminated due to lack of chain convergence. \n",file=paste0(save_path,text_save),append = TRUE)
        } else {
            cat(paste0(capture.output(print(baggr_bin$fit)),"\n"),file=paste0(save_path,text_save),append = TRUE)
            cat("\n- Model results - \n",file=paste0(save_path,text_save),append = TRUE)
            cat(paste0(capture.output(print(baggr_bin)),"\n"),file=paste0(save_path,text_save),append = TRUE)
            p <- plot(baggr_bin,hyper=TRUE,vline=FALSE,style="areas") + bayes_title
           	pdf(file = paste0(save_path,bayes_pdf), width = bayes_pdf_width, height = bayes_pdf_height, onefile=FALSE)
           	print(p)
           	dev.off()
        }
	}
}
