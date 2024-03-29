#' @title Provides the t-test and chi square's p value and statistic for binary targets and provides it in a dataframe for a set of columns or for a whole dataframe.In case of ANOVA provides all possible tests's summary
#' @description  1.Provides the t-test and chi square's p value and statistic and provides it in a dataframe for a set of columns or for a whole dataframe.
#'    2.In case of ANOVA() provides p values in form of a dataframe
#'    Assumption: No individual columns are provided for function Hyp_test().In such case a normal one on one t-test or chi would be better.
#' @param For Hyp_test()----->data,type_of_test,User_BinaryTarget(optional),User_Variable(optional)
#' @return  NULL
#' @examples Hyp_test(df,"chi",c(1,2),c(3,4)),Hyp_test(df,"ttest",filename="Apple")
#' @export Hyp_test
#' @import xlsx




Hyp_test<-function(data,type_of_test,User_BinaryTarget=NULL,User_Variable=NULL,filename=NULL){
  if(is.data.frame(data)==FALSE){
    stop("Please specify a dataframe object for the field data")
  }
  ttest=c("ttest","Ttest","T-test")
  chi=c("chi","Chi","Chi Square")
  if ((is.null(User_BinaryTarget)) & is.null(User_Variable)){
    target=c()                               #segregating binary targets
    flag=0
    for (i in 1:ncol(data))
    {
      #print(i)
      if((length(unique(data[,i]))==2))
      {
        flag=1
        target[i]<-i
      }
      else{
        flag=0
      }
    }
    if(flag==0){
      stop("No binary target")
    }
    target=target[!is.na(target)]
    target=as.data.frame(data[,target])
    data1=c()                                #segregating non binary
    for (i in 1:ncol(data))
    {
      if((length(unique(data[,i])))!=2 )
      {
        data1[i]<-i
      }
    }
    data1=data1[!is.na(data1)]
    Non_binary=as.data.frame(data[,data1])
    numeric=c()                              #segregating Numerical variable for t test
    for (i in 1:ncol(Non_binary)){
      if (length(unique(Non_binary[,i]))>20 | is.factor(Non_binary[,i])==F){
        numeric[i]<-i
      }
    }
    numeric=numeric[!is.na(numeric)]
    Numerical=as.data.frame(Non_binary[,numeric])
    cat=c()                                 #segregating categorical for Chi square test
    for (i in 1:ncol(Non_binary)){
      if ((length(unique(Non_binary[,i]))<20 & length(unique(Non_binary[,i]))>2) | is.factor(Non_binary[,i])==T){
        cat[i]<-i
      }
    }
    cat=cat[!is.na(cat)]
    Categorical=as.data.frame(Non_binary[,cat])
    if (tolower(type_of_test) %in% chi)    #performing chi square
    {
      names_1<-c()
      names_2<-c()
      chisq<-c()
      stat<-c()
      for (i in 1:ncol(target))
      {
        for (j in 1:ncol(Categorical)){
          names_1[j]<-names(target)[i]
          names_2[j]<-names(Categorical)[j]
          chisq[j]<-chisq.test(Categorical[,j],target[,i])$p.value
          stat[j]<-chisq.test(Categorical[,j],target[,i])$statistic
        }
        a=data.frame(Dep_variable=c(names_1),Ind_Variable=c(names_2),Pvalue=c(chisq),Statistic=c(stat))
        if(is.null(filename)){
          write.xlsx(a,file="chi.xlsx",sheetName = names(target)[i],append=TRUE)
        }
        else{
          write.xlsx(a,file=paste(filename,".xlsx"),sheetName = names(target)[i],append=TRUE)
        }
      }
    }
    if (tolower(type_of_test) %in% ttest)    #performing t test
    {
      names_11<-c()
      names_22<-c()
      T_test<-c()
      stat<-c()
      for (i in 1:ncol(target))
      {
        for (j in 1:ncol(Numerical)){
          names_11[j]<-names(target)[i]
          names_22[j]<-names(Numerical)[j]
          T_test[j]<-t.test(as.numeric(Numerical[,j])~target[,i],alternative=c("two.sided", "less", "greater"))$p.value
          stat[j]<-t.test(as.numeric(Numerical[,j])~target[,i],alternative=c("two.sided", "less", "greater"))$statistic
        }
        v=data.frame(Dep_Variable=c(names_11),Ind_Variable=c(names_22),P_value=c(T_test),Statistic=c(stat))
        if(is.null(filename)){
          write.xlsx(v,file="ttest.xlsx",sheetName = names(target)[i],append=TRUE)
        }
        else{
          write.xlsx(v,file=paste(filename,".xlsx"),sheetName = names(target)[i],append=TRUE)
        }
      }
    }
    if((!(type_of_test %in% ttest)) & (!(type_of_test %in% chi))) # No type of test found
    {
      print("Not an appropriate test name!!!!")

    }
  }
  else if ((is.null(User_BinaryTarget))==F & is.null(User_Variable)==F)
  {
    tar<-c()
    for (var in User_BinaryTarget)
    {
      if(length(unique(data[,var]))==2){
        tar[var]<-var
      }
      else if(length(unique(data[,var]))!=2)
      {
        print("The target is not binary")
      }
    }
    tar=tar[!is.na(tar)]
    tar=as.data.frame(data[,tar])
    Num=c()
    Cat=c()
    for (var in User_Variable)
    {
      if(length(unique(data[,var]))!=2 & (length(unique(data[,var]))>20 | is.factor(data[,var])==F) )
      {
        Num[var]<-var
      }
      if((length(unique(data[,var]))<20 & length(unique(data[,var]))>2) | (is.factor(data[,var])==T))
      {
        Cat[var]<-var
      }
      else{
        print("Inappropriate variable")
      }
    }
    Num=Num[!is.na(Num)]
    Num=as.data.frame(data[,Num])
    Cat=Cat[!is.na(Cat)]
    Cat=as.data.frame(data[,Cat])
    if(tolower(type_of_test) %in% chi){
      names_1<-c()
      names_2<-c()
      chisq<-c()
      for (i in 1:ncol(tar))
      {
        for (j in 1:ncol(Cat)){
          names_1[j]<-names(tar)[i]
          names_2[j]<-names(Cat)[j]
          chisq[j]<-chisq.test(Cat[,j],tar[,i])$p.value
        }
        print(data.frame(Dep_variable=c(names_1),Ind_Variable=c(names_2),Pvalue=c(chisq)))
      }

    }
    if(tolower(type_of_test) %in% ttest){
      names_11<-c()
      names_22<-c()
      T_test<-c()
      for (i in 1:ncol(tar))
      {
        {
          for (j in 1:ncol(Num)){
            names_11[j]<-colnames(tar)[i]
            names_22[j]<-names(Num)[j]
            T_test[j]<-t.test(as.numeric(Num[,j])~tar[,i],alternative=c("two.sided", "less", "greater"))$p.value
          }
          print(data.frame(Dep_Variable=c(names_11),Ind_Variable=c(names_22),P_value=c(T_test)))
        }
      }
    }
    if((!(type_of_test %in% ttest)) & (!(type_of_test %in% chi))) # No type of test found
    {
      print("Not an appropriate test name!!!!")

    }
  }

}



