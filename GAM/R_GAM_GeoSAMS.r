options(warn=-1)
suppressMessages(library(mgcv))
suppressMessages(library(stringi))
options(warn=0)

# trailingOnly=TRUE means that only your arguments are returned, check:
# print(commandArgs(trailingOnly=FALSE))
args <- commandArgs(trailingOnly = TRUE)

# typical args
# X_Y_BIOM_AL2022_GB.csv GBxyzLatLonRgn.csv 
# X_Y_BIOM_AL2022_GB_GAM.csv
dataFname <- file.path("Data", args[1])  # "Data/X_Y_BIOM_AL2022_MA.csv"
gridFname <- file.path("Grids", args[2]) # "Grids/MAxyzLatLonRgn.csv"
##outFname  <- file.path("Data", args[3])  # "Data/X_Y_BIOM_AL2022_MA_GAM.csv"
i <- unlist(stri_locate_all(pattern=".", dataFname, fixed=TRUE))[1] - 1
outFname <- paste(substr(dataFname,0,i),'_GAM.csv',sep='')
cat(dataFname, gridFname, outFname)

###load data###
surveydata=read.csv(file=dataFname) #read in survey data
predictgrid=read.csv(file=gridFname,header=FALSE) #read in grid data
#surveydata=read.csv(file="A:/GeoSAMS/GAM/X_Y_BIOM_AL2022_MA.csv") #read in survey data
#predictgrid=read.csv(file="A:/GeoSAMS/GAM/MAxyzLatLonRgn.csv",header=FALSE) #read in grid data
#head(predictgrid)

###data manipulation###

surveydata$PRESABS=ifelse(surveydata$BIOM>0,1,0) #add presense absense column to the survey data for gam model to use
surveydata$FOLD=rep(1:10,length=dim(surveydata)[1]) #assign fold to the data for cross validation
#head(surveydata)
colnames(predictgrid)=c("UTM_X","UTM_Y","DEPTH","LAT","LON","SAMS") #assign column name to grid data

###doing GAM###
##iterate accross different k values and select the k value based on cross validation result

allstat=c()
for(k1 in seq(5,30,5)) #iterate accross different k for lat*lon interaction term
{	
	for(k2 in seq(5,30,5)) #iterate accross different k for depth term
	{
		surveydataallfold=c() #used to store cross validation data
		for(i in 1:10)
		{
			surveydataleaveone=subset(surveydata,FOLD!=i) #leave out one fold
			surveydataonefold=subset(surveydata,FOLD==i) #one fold
			gam1f=gam(PRESABS~s(UTM_X,UTM_Y,k=k1)+s(DEPTH,k=k2),data=surveydataleaveone,family=quasibinomial,method="REML") #gam1 constructed using presense absense data
			gam2f=gam(BIOM~s(UTM_X,UTM_Y,k=k1)+s(DEPTH,k=k2),data=subset(surveydataleaveone,PRESABS==1),family=quasipoisson,method="REML") #gam2 constrctued using presense data only
			
			gam1_predvarf=predict(gam1f,type="response",newdata=surveydataonefold, se.fit = TRUE) #make probability prediction based on constructed gam1
			gam2_predvarf=predict(gam2f,type="response",newdata=surveydataonefold, se.fit = TRUE) #make quantity prediction based on constructed gam2
			surveydataonefold$GamPredict=gam1_predvarf[[1]]*gam2_predvarf[[1]] #gam1*gam2 is the final result from gam
			surveydataonefold$GamResidual=surveydataonefold$BIOM-surveydataonefold$GamPredict #residuals from gam that will be used to calculate cross validation statistics
			surveydataallfold=rbind(surveydataallfold,surveydataonefold) #combine data from different folds
		}
			
		allstat=rbind(allstat,data.frame(k1=k1,k2=k2,CV=sqrt(sum(surveydataallfold$GamResidual^2)/length(surveydataallfold$GamResidual)))) #combine statistics from different k values
	}
}
	
##construct final GAM based on the selected k values and made predictions
	
k1final=allstat[which.min(allstat$CV),]$k1 #get final k value for lat*lon interaction term
k2final=allstat[which.min(allstat$CV),]$k2 #get final k value for depth term
gam1=gam(PRESABS~s(UTM_X,UTM_Y,k=k1final)+s(DEPTH,k=k2final),data=surveydata,family=quasibinomial,method="REML") #construct final model for gam1
gam2=gam(BIOM~s(UTM_X,UTM_Y,k=k1final)+s(DEPTH,k=k2final),data=subset(surveydata,PRESABS==1),family=quasipoisson,method="REML") #construct final model for gam2

gam1_predvar=predict(gam1,type="response", se.fit = TRUE) #made predictions for survey data for gam1		
gam2_predvar=predict(gam2,type="response",newdata=surveydata, se.fit = TRUE) #made predictions for survey data for gam2 
surveydata$GAM1PREDICT=gam1_predvar[[1]] #predicted probability for gam1 for survey data
surveydata$GAM2PREDICT=gam2_predvar[[1]] #predicted quantity for gam2 for survey data
surveydata$GAMPREDICT=gam1_predvar[[1]]*gam2_predvar[[1]] #gam1*gam2 is the final result from gam
surveydata$GAMRESIDUAL=surveydata$BIOM-surveydata$GAMPREDICT #residuals from gam that will be used to calculate cross validation statistics
#head(surveydata)

# gam1_predvar=predict(gam1,type="response",newdata=predictgrid,se.fit = TRUE) #made predictions for grids for gam1		
# gam2_predvar=predict(gam2,type="response",newdata=predictgrid, se.fit = TRUE) #made predictions for grids for gam2		
# predictgrid$GAM1PREDICT=gam1_predvar[[1]] #predicted probability for gam1 for grids
# predictgrid$GAM2PREDICT=gam2_predvar[[1]] #predicted quantity for gam1 for grids
# predictgrid$GAM1VAR=gam1_predvar[[2]] #predicted quantity for gam1 for grids
# predictgrid$GAM2VAR=gam2_predvar[[2]] #predicted quantity for gam2 for grids
# predictgrid$GAMPREDICT=gam1_predvar[[1]]*gam2_predvar[[1]] #gam1*gam2 is the final result from gam
# #head(predictgrid)
# #aggregate(predictgrid$GAMPREDICT*(1.852^2),by=list(predictgrid$SAMS),sum)

###output data###

write.csv(surveydata,file=outFname,row.names=FALSE) #output for survey data
## Not using this file ##write.csv(predictgrid,file="MAxyzLatLonRgn_MyGAM.csv",row.names=FALSE) #output for grids



