options(warn=-1)
suppressMessages(library(mgcv))
suppressMessages(library(stringi))
options(warn=0)

# trailingOnly=TRUE means that only your arguments are returned, check:
# print(commandArgs(trailingOnly=FALSE))
args <- commandArgs(trailingOnly = TRUE)

# typical args
# INPUTS:
# [1] X_Y_BIOM_AL2022_GB 
# [2] GBxyzLatLonRgn.csv 
# OUTPUTS:
# residFname: X_Y_BIOM_AL2022_GB_IDW.csv
# predFname:  X_Y_BIOM_AL2022_GB_PRED.csv
dataFileName<- file.path("Data", args[1])
dataFname <- paste(dataFileName, '.csv', sep='')
gridFname <- file.path("Grids", args[2])
####i <- unlist(stri_locate_all(pattern=".", dataFname, fixed=TRUE))[1] - 1
residFname <- paste(dataFileName, '_IDW.csv',sep='')
predictFname <- paste(dataFileName, '_PRED.csv',sep='')
cat(dataFname, gridFname, residFname, predictFname, "\n")

###load data###

surveydata=read.csv(file=dataFname) #read in survey data
predictgrid=read.csv(file=gridFname,header=FALSE) #read in grid data
#head(predictgrid)

###data manipulation###

if(all(surveydata$PARAM==0)) #if all the data is zero, no gam model created, 0.0 assigned to the gam columns (could switch to zero if needed), and output the data
{

	surveydata[,c("PRESABS","FOLD","GAM1PREDICT","GAM2PREDICT","GAMPREDICT","GAMRESIDUAL")]=0.0
	predictgrid[,c("GAM1PREDICT","GAM2PREDICT","GAM1VAR","GAM2VAR","GAMPREDICT")]=0.0
	
} else #do gam models and output the data
{

	surveydata$PRESABS=ifelse(surveydata$PARAM>0,1,0) #add presense absense column to the survey data for gam model to use
	surveydata$FOLD=rep(1:10,length=dim(surveydata)[1]) #assign fold to the data for cross validation
	#head(surveydata)
	colnames(predictgrid)=c("UTM_X","UTM_Y","DEPTH","LAT","LON","SAMS") #assign column name to grid data

	###doing GAM###
	##iterate accross different k values and select the k value based on cross validation result

	allstat=c()
	for(k1 in seq(5,40,5)) #iterate accross different k for lat*lon interaction term
	{	
		for(k2 in seq(5,30,5)) #iterate accross different k for depth term
		{
			surveydataallfold=c() #used to store cross validation data
			for(i in 1:10)
			{
				surveydataleaveone=subset(surveydata,FOLD!=i) #leave out one fold
				surveydataonefold=subset(surveydata,FOLD==i) #one fold
				gam1f=gam(PRESABS~s(UTM_X,UTM_Y,k=k1)+s(DEPTH,k=k2),data=surveydataleaveone,family=quasibinomial,method="REML") #gam1 constructed using presense absense data
				gam2f=gam(PARAM~s(UTM_X,UTM_Y,k=k1)+s(DEPTH,k=k2),data=subset(surveydataleaveone,PRESABS==1),family=quasipoisson,method="REML") #gam2 constrctued using presense data only
				
				gam1_predvarf=predict(gam1f,type="response",newdata=surveydataonefold, se.fit = TRUE) #make probability prediction based on constructed gam1
				gam2_predvarf=predict(gam2f,type="response",newdata=surveydataonefold, se.fit = TRUE) #make quantity prediction based on constructed gam2
				surveydataonefold$GamPredict=gam1_predvarf[[1]]*gam2_predvarf[[1]] #gam1*gam2 is the final result from gam
				surveydataonefold$GamResidual=surveydataonefold$PARAM-surveydataonefold$GamPredict #residuals from gam that will be used to calculate cross validation statistics
				surveydataallfold=rbind(surveydataallfold,surveydataonefold) #combine data from different folds
			}
				
			allstat=rbind(allstat,data.frame(k1=k1,k2=k2,CV=sqrt(sum(surveydataallfold$GamResidual^2)/length(surveydataallfold$GamResidual)))) #combine statistics from different k values
		}
	}
		
	##construct final GAM based on the selected k values and made predictions
		
	k1final=allstat[which.min(allstat$CV),]$k1 #get final k value for lat*lon interaction term
	k2final=allstat[which.min(allstat$CV),]$k2 #get final k value for depth term
	gam1=gam(PRESABS~s(UTM_X,UTM_Y,k=k1final)+s(DEPTH,k=k2final),data=surveydata,family=quasibinomial,method="REML") #construct final model for gam1
	gam2=gam(PARAM~s(UTM_X,UTM_Y,k=k1final)+s(DEPTH,k=k2final),data=subset(surveydata,PRESABS==1),family=quasipoisson,method="REML") #construct final model for gam2

	gam1_predvar=predict(gam1,type="response", se.fit = TRUE) #made predictions for survey data for gam1		
	gam2_predvar=predict(gam2,type="response",newdata=surveydata, se.fit = TRUE) #made predictions for survey data for gam2 
	surveydata$GAM1PREDICT=gam1_predvar[[1]] #predicted probability for gam1 for survey data
	surveydata$GAM2PREDICT=gam2_predvar[[1]] #predicted quantity for gam2 for survey data
	surveydata$GAMPREDICT=gam1_predvar[[1]]*gam2_predvar[[1]] #gam1*gam2 is the final result from gam
	surveydata$GAMRESIDUAL=surveydata$PARAM-surveydata$GAMPREDICT #residuals from gam that will be used to calculate cross validation statistics
	#head(surveydata)

	gam1_predvar=predict(gam1,type="response",newdata=predictgrid,se.fit = TRUE) #made predictions for grids for gam1		
	gam2_predvar=predict(gam2,type="response",newdata=predictgrid, se.fit = TRUE) #made predictions for grids for gam2		
	predictgrid$GAM1PREDICT=gam1_predvar[[1]] #predicted probability for gam1 for grids
	predictgrid$GAM2PREDICT=gam2_predvar[[1]] #predicted quantity for gam1 for grids
	predictgrid$GAM1VAR=gam1_predvar[[2]] #predicted quantity for gam1 for grids
	predictgrid$GAM2VAR=gam2_predvar[[2]] #predicted quantity for gam2 for grids
	predictgrid$GAMPREDICT=gam1_predvar[[1]]*gam2_predvar[[1]] #gam1*gam2 is the final result from gam
	#head(predictgrid)
	#aggregate(predictgrid$GAMPREDICT*(1.852^2),by=list(predictgrid$SAMS),sum)
}

###output data###

write.csv(surveydata,file=residFname,row.names=FALSE) #output for survey data
write.csv(predictgrid,file=predictFname,row.names=FALSE) #output for grids
