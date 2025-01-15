############Note############
###v1: 12/17/2024: a. GAM by modeling region, region defined by provided shapefiles
###                b. hurdle model for positive data <= 80% and tweedie model for positive data > 80%
###                c. most areas used lat/lon+depth but NYB used lat/lon+lon/depth to stablize the estimates
###                   at the tip of the canyon
###                d. REML method to estimate model parameters
###                e. 10-fold cross validation for all models
###
###v2: 01/04/2025: a. hurdle model for positive data <= 75% and tweedie model for positive data > 75%
###                b. most areas used lat/lon+depth but NYB used lat/lon+lon/depth to stablize the estimates
###                   at the tip of the canyon
###                c. QNCV method to estimate hurdle and REML method to estimate tweedie model parameters
###                d. leave-one-out cross validation (carried by QNCV) for hurdle models and 
###                   10-fold cross validation for tweedie model
###
###v3: 01/12/2025: a. hurdle model for positive data <= 75%, tweedie model for positive data > 75%, 
###                   no model will be estimated for positive data <= 5%
###                b. QNCV method to select k values for tweedie model - estimate p using 
###                   REML method first, and then use the estimated p and QNCV method to 
###                   estimate final tweedie model parameters to save some time
###                c. REML method still used to estimate final tweedie model parameters - 
###                   tried both REML and QNCV and found that the estimates tend to be more extreme 
###                   when QNCV was used and feel uncomfortable with it 
###                d. only biomass and abundance used hurdle models, fishing mortality and 
###                   fishing effort used tweedie models for now
###
###v4: 01/13/2025: a. QNCV method to estimate hurdle and REML method to estimate tweedie model parameters,
###                   QNCV for tweedie is unstable, so switch back to REML with manual 5-fold cross validation
###                b. leave-one-out cross validation (carried by QNCV) for hurdle models
###                   and 5-fold cross validation for tweedie model
###                c. change 10-fold to 5-fold to avoid too little positive data for each fold
###                   and reduce run time
###
############################

#source("C:/Users/jui-han.chang/Desktop/GAM/R_GAM_GeoSAMS_ByRegion_NCV_v4.r")

options(warn=-1)
suppressMessages(library(mgcv))
suppressMessages(library(stringi))
options(warn=0)

# trailingOnly=TRUE means that only your arguments are returned, check:
# print(commandArgs(trailingOnly=FALSE))
args <- commandArgs(trailingOnly = TRUE)

###for testing
#for(gm in c("GB","MA"))
#{
#for(pa in c("FMOR","BIOM","FEFF"))
#{
#for(yr in 2022:2026) #for testing
#{
#args <- list(paste0("X_Y_",pa,"_AL",yr,"_",gm,"_BUFFER"),paste0(gm,"xyzLatLonRgn_REGION.csv"))
#setwd("C:/Users/jui-han.chang/Desktop/GAM/")
###end testing

# typical args
# INPUTS:
# [1] X_Y_BIOM_AL2022_GB_BUFFER 
# [2] GBRegionGrid.csv 
# OUTPUTS:
# residFname: X_Y_BIOM_AL2022_GB_RESID.csv
# predFname:  X_Y_BIOM_AL2022_GB_PRED.csv
dataFileName<- file.path("Data", args[1])
dataFname <- paste(dataFileName, '.csv', sep='')
gridFname <- file.path("Grids", args[2])
####i <- unlist(stri_locate_all(pattern=".", dataFname, fixed=TRUE))[1] - 1
residFname <- paste(dataFileName, '_RESID.csv',sep='')
predictFname <- paste(dataFileName, '_PRED.csv',sep='')
cat(dataFname, gridFname, residFname, predictFname, "\n")

###load data###

surveydata=read.csv(file=dataFname) #read in survey data
predictgrid=read.csv(file=gridFname) #read in grid data
#head(surveydata)
#head(predictgrid)

###data manipulation###

surveydata[,c("ID","PRESABS","FOLD","GAM1PREDICT","GAM2PREDICT","GAMPREDICTM2","GAMRESIDUALM2","BUFFER")]=0
predictgrid[,c("GAM1PREDICT","GAM2PREDICT","GAM1VAR","GAM2VAR","GAMPREDICTM2")]=0

startTime <- Sys.time()

###model construction###
	
if(all(surveydata$PARAM==0)) #if all the data is zero, no gam model created, NAs assigned to the gam columns (could switch to zero if needed), and output the data
{
	write.csv(surveydata,file=residFname,row.names=FALSE) #output for survey data
	write.csv(predictgrid,file=predictFname,row.names=FALSE) #output for grids

}else #do gam models and output the data
{
	surveydata$ID=1:dim(surveydata)[1]
	surveydata$PRESABS=ifelse(surveydata$PARAM>0,1,0) #add presense absense column to the survey data for gam model to use
	surveydata$FOLD=rep(1:5,length=dim(surveydata)[1]) #assign fold to the data for cross validation

	###doing GAM###

	##start up with data outside of modeling region and the buffer
	
    surveydata_buffer_cols=colnames(surveydata)[grep("BUFFER_",colnames(surveydata))]
	surveydata_result=surveydata[apply(surveydata[,surveydata_buffer_cols],1,function(x) sum(x)==0),] 	
	predictgrid_result=predictgrid[is.na(predictgrid$REGION)==TRUE,]
	
	for(r in unique(surveydata$REGION[is.na(surveydata$REGION)==FALSE])) #iterate by different regions
	{
		surveydata_temp=surveydata[surveydata[,paste0("BUFFER_",r)]==1,] #subset the survey data for each buffer
		surveydata_temp=surveydata_temp[order(surveydata_temp$FOLD),]
		predictgrid_temp=subset(predictgrid,REGION==r) #subset the grid data for each region
		percentpos=round(sum(surveydata_temp$PRESABS)/dim(surveydata_temp)[1],2) #percentage of positive data points
		print(paste(r,percentpos))

		allstat=c() #use to store cross-validation statistics
		
		if(percentpos<=.05) #if there is less than 5% of positive data, no gam will be estimated
		{	
			#print(subset(surveydata_temp,PARAM>0))
			surveydata_temp$BUFFER=r #record buffer
			surveydata_result=rbind(surveydata_result,surveydata_temp) #noted that surveydata_result have repeated values of survey records because some data at the bundary might be used by multiple regions (queried by buffer) 
			predictgrid_result=rbind(predictgrid_result,predictgrid_temp)
			next;
		}
		
		if(percentpos<.75 & grepl(c("FEFF"),args[1])==FALSE & grepl(c("FMOR"),args[1])==FALSE) #if there is less than 80% of positive data, do hurdle gam
		{			
			
			##iterate through different ks to find the best GAM based on cross-validation
			 
			if(r!="NYB_3D") #if needed, NYB do 3-D gam to stabilize the gam at the east part of the canyon, no iterates on k for the 3-D gam, use the default and let QNCV reduce the estimated k, so not included in here
			{			
				
				options(warn=2) #turned warnings into errors to stop the k loop when the models did not converge
				
				for(k1 in seq(5,30,5)) #iterate accross different k for lat*lon interaction term
				{	

					for(k2 in seq(5,30,5)) #iterate accross different k for depth term
					{
						
						#print(paste(r,k1,k2,"hurdle gam"))						
						if(r=="NYB") gam1f=try(gam(PRESABS~s(UTM_X,UTM_Y,k=k1)+s(UTM_X,DEPTH,k=k2),data=surveydata_temp,family=quasibinomial,method="QNCV")) #for NYB, depth needs to be conditioned on latitude so that we don't overestimate the biomass in the east part of the canyon. default to this, and if this doesn't work, do 3D gam. added try function and it would report error when model have issues, when model have issues, we will not use this k combination and move to test the next combination
						else gam1f=try(gam(PRESABS~s(UTM_X,UTM_Y,k=k1)+s(DEPTH,k=k2),data=surveydata_temp,family=quasibinomial,method="QNCV")) #hurdle gam1 constructed using presense absense data
						if(class(gam1f)[1]=="try-error") next; #if model have issues, break the loop and jump to next combination of k
						
						if(r=="NYB") gam2f=try(gam(PARAM~s(UTM_X,UTM_Y,k=k1)+s(UTM_X,DEPTH,k=k2),data=subset(surveydata_temp,PRESABS==1),family=quasipoisson,method="QNCV")) #for NYB, depth conditioned on latitude
						else gam2f=try(gam(PARAM~s(UTM_X,UTM_Y,k=k1)+s(DEPTH,k=k2),data=subset(surveydata_temp,PRESABS==1),family=quasipoisson,method="QNCV")) #hurdle gam2 constrctued using presense data only
						if(class(gam2f)[1]=="try-error") next;
						#print(paste(r,k1,k2,i,"hurdle gam done"))
						#print(summary(gam1f))
						#print(summary(gam2f))
						
						gam1_predvarf=predict(gam1f,type="response",newdata=surveydata_temp, se.fit = TRUE) #make probability prediction based on constructed gam1
						gam2_predvarf=predict(gam2f,type="response",newdata=surveydata_temp, se.fit = TRUE) #make quantity prediction based on constructed gam2
						surveydata_temp$GamPredict=gam1_predvarf[[1]]*gam2_predvarf[[1]] #gam1*gam2 is the final result from gam
						surveydata_temp$GamResidual=surveydata_temp$PARAM-surveydata_temp$GamPredict #residuals from gam that will be used to calculate cross validation statistics
						
						allstat=rbind(allstat,data.frame(region=r,k1=k1,k2=k2,ncv1=gam1f$NCV[1],ncv2=gam2f$NCV[1],totalncv=gam1f$NCV[1]+gam2f$NCV[1],CV=sqrt(sum(surveydata_temp$GamResidual^2)/length(surveydata_temp$GamResidual))))
						
						surveydata_temp$GamPredict=NULL
						surveydata_temp$GamResidual=NULL
							
					}
				}
				
				options(warn=0) #change warnings treatment back to storing warnings
			}
				
			##construct final GAM based on the selected k values and made predictions
		
			if(r=="NYB_3D") #for 3-D gam in NYB
			{
				gam1=gam(PRESABS~s(UTM_X,UTM_Y,DEPTH),data=surveydata_temp,family=quasibinomial,method="QNCV") #construct final model for gam1
				gam2=gam(PARAM~s(UTM_X,UTM_Y,DEPTH),data=subset(surveydata_temp,PRESABS==1),family=quasipoisson,method="QNCV") #construct final model for gam2

			}else
			{ #for the rest of the regions
			
				if(is.null(allstat)==TRUE)
				{
					surveydata_result=rbind(surveydata_result,surveydata_temp) #noted that surveydata_result have repeated values of survey records because some data at the bundary might be used by multiple regions (queried by buffer) 
					predictgrid_result=rbind(predictgrid_result,predictgrid_temp)
					next;
					
				}else
				{			
					k1final=allstat[which.min(allstat$CV),]$k1 #get final k value for lat*lon interaction term
					k2final=allstat[which.min(allstat$CV),]$k2 #get final k value for depth term or lon*depth for NYB
					print(paste(r,k1final,k2final))
					if(r=="NYB") gam1=gam(PRESABS~s(UTM_X,UTM_Y,k=k1final)+s(UTM_X,DEPTH,k=k2final),data=surveydata_temp,family=quasibinomial,method="QNCV") #construct final model for gam1 for NYB
					else gam1=gam(PRESABS~s(UTM_X,UTM_Y,k=k1final)+s(DEPTH,k=k2final),data=surveydata_temp,family=quasibinomial,method="QNCV") #construct final model for gam1 for the rest of the regions
					if(r=="NYB") gam2=gam(PARAM~s(UTM_X,UTM_Y,k=k1final)+s(UTM_X,DEPTH,k=k2final),data=subset(surveydata_temp,PRESABS==1),family=quasipoisson,method="QNCV") #construct final model for gam2 for NYB	
					else gam2=gam(PARAM~s(UTM_X,UTM_Y,k=k1final)+s(DEPTH,k=k2final),data=subset(surveydata_temp,PRESABS==1),family=quasipoisson,method="QNCV") #construct final model for gam2 for the rest of the regions
				}
			}
			
			gam1_predvars=predict(gam1,type="response", se.fit = TRUE) #made predictions for survey data for gam1		
			gam2_predvars=predict(gam2,type="response",newdata=surveydata_temp, se.fit = TRUE) #made predictions for survey data for gam2 
			surveydata_temp$GAM1PREDICT=gam1_predvars[[1]] #predicted probability for gam1 for survey data
			surveydata_temp$GAM2PREDICT=gam2_predvars[[1]] #predicted quantity for gam2 for survey data
			surveydata_temp$GAMPREDICTM2=gam1_predvars[[1]]*gam2_predvars[[1]] #gam1*gam2 is the final result from gam
			surveydata_temp$GAMRESIDUALM2=surveydata_temp$PARAM-surveydata_temp$GAMPREDICTM2 #residuals from gam
			surveydata_temp$BUFFER=r #record buffer
			
			gam1_predvar=predict(gam1,type="response",newdata=predictgrid_temp,se.fit = TRUE) #made predictions for grids for gam1		
			gam2_predvar=predict(gam2,type="response",newdata=predictgrid_temp, se.fit = TRUE) #made predictions for grids for gam2		
			predictgrid_temp$GAM1PREDICT=gam1_predvar[[1]] #predicted probability for gam1 for grids
			predictgrid_temp$GAM2PREDICT=gam2_predvar[[1]] #predicted quantity for gam2 for grids
			predictgrid_temp$GAM1VAR=gam1_predvar[[2]] #predicted variance for gam1 for grids
			predictgrid_temp$GAM2VAR=gam2_predvar[[2]] #predicted variance for gam2 for grids
			predictgrid_temp$GAMPREDICTM2=gam1_predvar[[1]]*gam2_predvar[[1]] #gam1*gam2 is the final result from gam

			surveydata_result=rbind(surveydata_result,surveydata_temp) #noted that surveydata_result have repeated values of survey records because some data at the bundary might be used by multiple regions (queried by buffer) 
			predictgrid_result=rbind(predictgrid_result,predictgrid_temp)
		
		}
		
		if(percentpos>=.75 | is.null(allstat)==TRUE | grepl(c("FEFF"),args[1])==TRUE | grepl(c("FMOR"),args[1])==TRUE) #if more than 80% of the data are positive or no k combinations works for the hurdle gam , do one tweedie gam for all data. found that 70% positive data with tweedie gam spikes k to 30 for both terms for all regions tested, doesn't seem to be stable or make sense, so use 80% as the cut off
		{
		
			##iterate through different ks to find the best GAM based on cross-validation
			
			if(r!="NYB_3D") #if needed, NYB do 3-D gam to stabilize the gam at the east part of the canyon, no iterates on k, use the default and let QNCV reduce the estimated k, so not included in here
			{
				
				options(warn=2) #turned warnings into errors to stop the k loop when the models did not converge
								
				for(k1 in seq(5,30,5)) #iterate accross different k for lat*lon interaction term
				{	
					for(k2 in seq(5,30,5)) #iterate accross different k for depth term
					{						
								
						surveydata_temp_allfold=c() #used to store cross validation data
						for(i in 1:5) #5 fold cross validation
						{
													
							surveydata_temp_leaveone=subset(surveydata_temp,FOLD!=i) #leave out one fold
							surveydata_temp_onefold=subset(surveydata_temp,FOLD==i) #one fold
							
							#print(paste(r,k1,k2,i,"tweedie gam"))
							if(r=="NYB") gam1f=try(gam(PARAM~s(UTM_X,UTM_Y,k=k1)+s(UTM_X,DEPTH,k=k2),data=surveydata_temp_leaveone,family=tw(),method="REML")) #gam1 constructed using all data and a tweedie distribution for NYB with lat*depth
							else gam1f=try(gam(PARAM~s(UTM_X,UTM_Y,k=k1)+s(DEPTH,k=k2),data=surveydata_temp_leaveone,family=tw(),method="REML")) #gam1 constructed using all data and a tweedie distribution for the rest of the regions
							if(class(gam1f)[1]=="try-error") break;
							#print(paste(r,k1,k2,i,"tweedie gam done"))
							
							gam1_predvarf=predict(gam1f,type="response",newdata=surveydata_temp_onefold, se.fit = TRUE) #make pprediction based on constructed gam1
							surveydata_temp_onefold$GamPredict=gam1_predvarf[[1]] #gam1 is the final result from gam
							surveydata_temp_onefold$GamResidual=surveydata_temp_onefold$PARAM-surveydata_temp_onefold$GamPredict #residuals from gam that will be used to calculate cross validation statistics
							surveydata_temp_allfold=rbind(surveydata_temp_allfold,surveydata_temp_onefold) #combine data from different folds
							
							if(i==5) allstat=rbind(allstat,data.frame(region=r,k1=k1,k2=k2,CV=sqrt(sum(surveydata_temp_allfold$GamResidual^2)/length(surveydata_temp_allfold$GamResidual)))) #combine cross validation statistics from different k values
						}	
					}
				}
				
				options(warn=0) #change warnings treatment back to storing warnings
			}
				
			if(r=="NYB_3D")
			{
				gam1=gam(PARAM~s(UTM_X,UTM_X,DEPTH),data=surveydata_temp,family=tw(),method="REML") #construct final model for gam1	for 3-D gam for NYB					
			
			}else
			{
				if(is.null(allstat)==TRUE)
				{
					surveydata_result=rbind(surveydata_result,surveydata_temp) #noted that surveydata_result have repeated values of survey records because some data at the bundary might be used by multiple regions (queried by buffer) 
					predictgrid_result=rbind(predictgrid_result,predictgrid_temp)
					next;
					
				}else
				{
					k1final=allstat[which.min(allstat$CV),]$k1 #get final k value for lat*lon interaction term
					k2final=allstat[which.min(allstat$CV),]$k2 #get final k value for depth term or lat*depth for NYB
					print(paste(r,k1final,k2final))
					if(r=="NYB") gam1=gam(PARAM~s(UTM_X,UTM_Y,k=k1final)+s(UTM_X,DEPTH,k=k2final),data=surveydata_temp,family=tw(),method="REML") #construct final model for gam1 for NYB with lat*depth
					else gam1=gam(PARAM~s(UTM_X,UTM_Y,k=k1final)+s(DEPTH,k=k2final),data=surveydata_temp,family=tw(),method="REML") #construct final model for gam1		
				}
			}		

			gam1_predvars=predict(gam1,type="response", se.fit = TRUE) #made predictions for survey data for gam1		
			surveydata_temp$GAM1PREDICT=gam1_predvars[[1]] #predicted quantity from gam1 for survey data
			surveydata_temp$GAM2PREDICT=NA #no gam2 here
			surveydata_temp$GAMPREDICTM2=gam1_predvars[[1]] #gam1 is the final result from gam
			surveydata_temp$GAMRESIDUALM2=surveydata_temp$PARAM-surveydata_temp$GAMPREDICTM2 #residuals from gam
			surveydata_temp$BUFFER=r
			
			gam1_predvar=predict(gam1,type="response",newdata=predictgrid_temp,se.fit = TRUE) #made predictions for grids for gam1		
			predictgrid_temp$GAM1PREDICT=gam1_predvar[[1]] #predicted quantity for gam1 for grids
			predictgrid_temp$GAM2PREDICT=NA #no gam2 here
			predictgrid_temp$GAM1VAR=gam1_predvar[[2]] #predicted variance for gam1 for grids
			predictgrid_temp$GAM2VAR=NA #no gam2 here
			predictgrid_temp$GAMPREDICTM2=gam1_predvar[[1]] #gam1 is the final result from gam

			surveydata_result=rbind(surveydata_result,surveydata_temp)
			predictgrid_result=rbind(predictgrid_result,predictgrid_temp)
			
			#print(plot(UTM_Y~UTM_X,predictgrid_temp,cex=(predictgrid_temp$GAMPREDICTM2+0.000001),pch=16))
	
		}
		print(allstat)
	}
		
	###output data###
	
	#print(aggregate(predictgrid_result$GAMPREDICTM2*(1.852^2),by=list(predictgrid_result$NEWSAMS),sum))
	#print(sum(aggregate(predictgrid_result$GAMPREDICTM2*(1.852^2),by=list(predictgrid_result$NEWSAMS),sum)$x))
	#print(plot(UTM_Y~UTM_X,predictgrid_result,cex=(predictgrid_result$GAMPREDICTM2+0.000001)*10,pch=16))
	
	write.csv(surveydata_result,file=residFname,row.names=FALSE) #output for survey data
	write.csv(predictgrid_result,file=predictFname,row.names=FALSE) #output for grids

	#source("C:/Users/jui-han.chang/Desktop/GAM/R_SpatialPlot.r") #for plotting while testing	

}

endTime <- Sys.time()

duration <- endTime - startTime
cat("Elapsed time: ", duration)

#}}} #for testing
