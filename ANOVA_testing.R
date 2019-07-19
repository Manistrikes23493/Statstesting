#' @title Provides the t-test and chi square's p value and statistic for binary targets and provides it in a dataframe for a set of columns or for a whole dataframe.In case of ANOVA provides all possible tests's summary
#' @description  1.Provides the t-test and chi square's p value and statistic and provides it in a dataframe for a set of columns or for a whole dataframe.
#' 2.In case of ANOVA() provides p values in form of a dataframe
#' Assumption: No indidivual columns are provided for function Hyp_test().In such case a normal one on one t-test or chi would be better.
#' @param   For ANOVA()----->data
#' @return  NULL
#' @examples ANOVA_test(df)
#' @export ANOVA_testing
#' @import xlsx




ANOVA_testing<-function(data,filename=NULL){
  target1=c()                             #segregating anova targets
  for (i in 1:ncol(data))
  {
    if((length(unique(data[,i]))>2) & (length(unique(data[,i]))<15))
    {
      target1[i]<-i
    }
    if(length(unique(data[,i]))>15 & length(unique(data[,i]))<20 | length(unique(data[,i]))==2){
      print("---------------------------------------------------------------------")
      print(paste("ANOVA not possible for ",names(data)[i],"as a target since it is a categorical variable with insufficient levels for anova"))
    }
  }
  target1=target1[!is.na(target1)]
  target1=as.data.frame(data[,target1])
  numeric=c()                              #segregating Numerical variable for ANOVA
  for (i in 1:ncol(data)){
    if (length(unique(data[,i]))>20){
      numeric[i]<-i
    }
    if(length(unique(data[,i]))<20){
      print("---------------------------------------------------------------------")
      print(paste(names(data)[i]," is not a numerical variable for testing ANOVA"))
    }
  }
  numeric=numeric[!is.na(numeric)]
  Numerical=as.data.frame(data[,numeric])
  names_11<-c()
  names_22<-c()
  ANOVA_1<-c()
  for (i in 1:ncol(target1))
  {
    for (j in 1:ncol(Numerical)){
      names_11[j]<-names(target1)[i]
      names_22[j]<-names(Numerical)[j]
      aov_1=aov(Numerical[,j]~target1[,i])
      ANOVA_1[j]<-unlist(summary(aov_1))['Pr(>F)1']
    }
    #print(ANOVA_1)
    a=data.frame(Dep_Variable=c(names_11),Ind_Variable=c(names_22),P_value=c(ANOVA_1))
    if(is.null(filename)){
      write.xlsx(a,file="ANOVA.xlsx",sheetName = names(target1)[i],append=TRUE)
    }
    else{
      write.xlsx(a,file=paste(filename,".xlsx"),sheetName = names(target1)[i],append=TRUE)
    }
  }
}
