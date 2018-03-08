---
title: "Data_Science_Assessment for Persado"
output:
  html_document: default
  html_notebook: default
  word_document: default
---
 ![alt text](small_logo.png) 

#### This notebook serves as a document to the data science challenge. Commenting and documentation has been done wherever necessary. 


# Instructions for the zip folder

1) It contains the generated csv,txt files
2) The names make it clear.Files which are cleaned will have withcleaning as appended to the file name. eg Basic_Industrieswithcleaning.txt
3) To reproduce what I have done project hierarchies and folders would either have to be recreated or changed in the code. For eg I am retrieving the data from my local disk so the code would have something like D:/UIC/.....
4) After performing cleaning operation the files were loaded in a folder called CleanedText and R would sometimes change the operating directory and call that folder.
5) Inside would be  a clipping of my web scraping process.
6) Some code chunks are set to not evaluate at run time. Those chunks were used to build the dataset and should only be used if the data set is to be build. 


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE,cache = TRUE,autodep = TRUE,message = FALSE,warning=FALSE)
```


# Importing the dataset

The dataset which Persado sent can be seen here. For convenience the top 5 rows have been shown.
As you can see three variables such as company's name, it's label on stock market, the sector that it belongs to has been provided.


```{r warning=FALSE,message=FALSE}


library(readr)
company_dataset_original<- read_csv("D:/UIC/MyResearch/Mwrd/Persado_Data_Scientist/company_dataset.csv")

head(company_dataset_original)



```



# Working dataset


This dataset has been loaded after altering the original dataset.
Altering the dataset here means

1) Adding an extra column for description
2) Placing the scraped description so that the description ties strongly to the actual company and sits inside the data frame 
3) Deleting those rows which don't have any description 

We can see that there are 662 observation for which we don't have the observations.

We will show below how scraping is done 

All the tickers will be extracted from the second column of the given dataset

```{r warning=FALSE,message=FALSE,eval=TRUE}
# list of different handles which you want to extract
library(readr)
company_dataset<- read_csv("D:/UIC/MyResearch/Mwrd/Persado_Data_Scientist/company_dataset_c.csv",col_types = cols(X1 = col_skip()))

company_dataset<-company_dataset[which(!is.na(company_dataset$description)),]

head(company_dataset$description)

```



**Exploratory data analysis**

As we can see from the dataset there are 3276 observations having 3 variables of which important variables are second and third.

Second variable is <span style="color:blue">ticker</span> and third variable is <span style="color:blue">category</span> which is to be predicted

```{r warning=FALSE,message=FALSE,eval=FALSE}

library("dplyr")
# Create a empty variable known as description which will get filled once we have web scraping up and running 
company_dataset<-mutate(.data = company_dataset,description="")



```


# Web Scraping

To do web scraping we will be using R Selenium package.R vest is another option 

R Selenium gives power to the user since API's restrict in what a user wants to download. Furthermore, automating the process via a browser always helps. Inline documentation of the code gives an idea on how Rselenium works

*A video is attached to the document capturing the automation and web scraping*


```{r warning=FALSE,message=FALSE,eval=FALSE}

library("RSelenium")
library("dplyr")

# important because it downloads all the things(geckodriver,chrome driver,phantom js)
remDr<-rsDriver(port = 4444L,browser = "chrome",version = "latest",chromever = "latest",geckover = "latest",check = TRUE)

Client<-remDr$client


for(i in 1:3276){
Client$open()   
Client$navigate(paste0("https://finance.yahoo.com/quote/",company_dataset[i,2],"/profile?p=",company_dataset[i,2],"/"))

Css<-Client$findElements(using = "css selector",value = ".Mt\\(30px\\) .Lh\\(1\\.6\\)")

if (length(Css)>0){
CSS_DATA_FRAME<- unlist(lapply(Css, function(x){x$getElementText()}))
company_dataset[i,4]<-CSS_DATA_FRAME
Client$close()
}
else{
company_dataset[i,4]<-NA  
Client$close()
}

}



```

##### Post- Scraping 

There were some tickers whose description were not available and others which had a *W* attached to their ticker were not available. All such cases have been put under unclassified


<span style="color:red"> Following code is optional and may be used if the browser loses its internet connectivity or due to some other mishap</span>

```{r eval=FALSE}

sum(is.na(company_dataset$description[771:1671]))
# Add the indices of the data if you are searching from within a certain range of the data

# NotFound<-company_dataset[which(is.na(company_dataset$description[771:1671]))+770,]

NotFound<-company_dataset[which(is.na(company_dataset$description[771:1671]))+770,]


# The Selenium code was modified by 

Transferto<-NotFound[which(!is.na(NotFound$description)),]
# InsertatRows<-which(company_dataset$CompanyTicker %in% company_dataset$CompanyTicker)
# company_dataset[InsertatRows,]$description<-Transferto$description

```


# Analysis of the variable sector

Let's see the number of unique sectors in the given dataset

```{r}
unique(company_dataset$Sector)
length(unique(company_dataset$Sector))

```

We can see that there are 14 sectors of which two are categorised as "n/a" or NA.
Below is the frequency distribution of the sector


```{r}


# Converting from character to factor 

company_dataset$Sector<-as.factor(company_dataset$Sector)
library("ggplot2")
g1<-ggplot(data = company_dataset,aes(company_dataset$Sector))+
geom_bar(aes(fill=company_dataset$Sector))+
theme(axis.text.x = element_blank())  
plot(g1)  
  


```


We can see that the top categories are Finance and Healthcare.
Now we will calculate the frequencies of their occurences

```{r}
library(knitr)
SectorFrequencies<-as.data.frame(sort(table(company_dataset$Sector),decreasing = T))
colnames(SectorFrequencies)<-c("Sector","Frequency")
kable(SectorFrequencies)

```

# Model

We can infer from the sentences that are in the description and then create a word frequency distribution to predict the sector

There are 13 sectors of which one is n/a. Therefore we have 12 categories to be predicted

**We will be using 70% of the model for training and the rest will be for testing**

We have created subsets of the master data. This is done in order to produce files. This is done because this code can be reused later if we want to look at a single sector.The files created are situated in the project folder 

```{r warning=TRUE,message=TRUE,eval=TRUE}



library("dplyr")

set.seed(45)
ind=sample(2,nrow(company_dataset),replace=T,prob=c(0.65,0.35))
company_dataset_1=company_dataset[ind==1,]
company_test=company_dataset[ind==2,]


Finance_dataset<-company_dataset_1[company_dataset_1$Sector=="Finance",]

HealthCare<-company_dataset_1[company_dataset_1$Sector=="Health Care",]

Technology_dataset<-company_dataset_1[company_dataset_1$Sector=="Technology",]

Consumer_Services<-company_dataset_1[company_dataset_1$Sector=="Consumer Services",]

Capital_Goods<-company_dataset_1[company_dataset_1$Sector=="Capital Goods",]

Consumer_Non_Durables<-company_dataset_1[company_dataset_1$Sector=="Consumer Non-Durables",]

Consumer_Durables<-company_dataset_1[company_dataset_1$Sector=="Consumer Durables",]

Miscelleaneous<-company_dataset_1[company_dataset_1$Sector=="Miscellaneous",]

Basic_Industries<-company_dataset_1[company_dataset_1$Sector=="Basic Industries",]

Energy<-company_dataset_1[company_dataset_1$Sector=="Energy",]

Public_Utilities<-
company_dataset_1[company_dataset_1$Sector=="Public Utilities",]

Transportation<-company_dataset_1[company_dataset_1$Sector=="Transportation",]

Unclassified<-company_dataset_1[company_dataset_1$Sector=="n/a",]


```


**We will construct two models one would be based on term frequencies and other would be based on t-f and i-d-f**

Text Mining approach would involve the text from similar sector grouped together, and then we would create a bag of words which appear frequently in similar sectors.

The approach is to to count the frequency of words that matter and remove those words out which are of common English and to remove those words which have zero information content ( from our perspective and client's perspective )

1. <span style="color:red">Step 1 :</span> Collapse all the descriptions into a paragraph
2. <span style="color:red">Step 2 :</span> Replace punctuations such as ,_:/ etc.
3. <span style="color:red">Step 3 :</span> Repace all the digits
4. <span style="color:red">Step 4 :</span> Replace single letter words
5. <span style="color:red">Step 5 :</span> Replace stopwords words such as a,and,of,the
6. <span style="color:red">Step 6 :</span> Delete additional words in a retrospective manner 
7. <span style="color:red">Step 7 :</span> Make a textbag of words and after that create a frequency tablle



```{r warning=FALSE,message=FALSE}
##### Text Mining

library("stringr")
Sector_Names<-data.frame(Sector=c("Finance_dataset","HealthCare","Basic_Industries","Capital_Goods","Consumer_Services","Consumer_Non_Durables","Consumer_Durables","Energy","Miscelleaneous","Public_Utilities","Transportation","Unclassified","Technology_dataset"))

Sector_Names$Sector=as.character(Sector_Names$Sector)


library(tm)


for(i in 1:(nrow(Sector_Names))){

text<-paste(get(Sector_Names[i,])$description,collapse = "")



write(text,file=paste0(x=Sector_Names[i,],".txt"))

#punctuation replacement
text2<-gsub(pattern = "\\W",replacement = " ",text)

#While Scraping a box type element having ascii code <U 0092> is present, therefore we have to delete that type of element from our text

# digits replacement

text2<-gsub(pattern="\\d",replacement = "",text2)

#We will delete characters which are of single length

text2<-gsub(pattern = "\\b[A-z]\\b{1}",replacement = "",text2)

# words removed with no information
text2<-removeWords(text2,stopwords())


                   

text2<-stripWhitespace(text2)

# If we unstem the document it becomes easier and system is able to process faster (matching of the words).Depending on the sitaution of the analysis we will 

text2<-stemDocument(text2)

text2<-removeWords(text2,c("servic","compani","The","product","provid","Inc","It","includ","well","also","found","In","As","Its","develop","known","sell","offer","use","various","serv","busi","This"))

setwd("D:/UIC/MyResearch/Mwrd/Persado_Data_Scientist/CleanedText")

write(text2,file=paste0(x=Sector_Names[i,],"withcleaning",".txt"))

setwd("D:/UIC/MyResearch/Mwrd/Persado_Data_Scientist")



textbagWithoutCleaning<-str_split(text2,pattern="\\s+")
textbag<-unlist(textbagWithoutCleaning)

write.csv(head(sort(table(textbag),decreasing = T),n=200),file =paste0(x=Sector_Names[i,],"withoutcleaning",".csv"))

}

```


Now we wil look at the dataset and identify what words are occuring in most of the dataset. We see that words like 
After deleting these words we will be creating a tf-idf matrix
We are using the tm package and then the Document Term Matrix function


Creating a corpus which can help us 

1) *tf-idf*
2) *frequent words*
3) *comparison table*

```{r message=FALSE,warning=FALSE}
library(tm)

filePath<-("D:/UIC/MyResearch/Mwrd/Persado_Data_Scientist/CleanedText/")

Corpus_Yahoo<-Corpus(DirSource(filePath), readerControl = list(language="english"))
dtm_yahoo<-DocumentTermMatrix(Corpus_Yahoo)

# Calculate and sort by word frequencies

dtm_yahoo<-tm::removeSparseTerms(x = dtm_yahoo,sparse = 0.5)
word.freq<-sort(colSums(as.matrix(dtm_yahoo)),decreasing = T)
table_word_freq<-data.frame(word=names(word.freq),
abs.freq=word.freq,
relative.frequency=word.freq/length(word.freq))
rownames(table_word_freq)<-NULL


kable(x = table_word_freq[1:200,1:2],caption = "Top 200 Frequent words")



# Find association in this way we can create anagrams 

# tm::findAssocs(dtm_yahoo,c("treatment"),0.8)




```

#Model1


We will create a bag of words and then do the sentiment analysis. The conventional 
sentiment analysis is to classify as positive or negative. We will use the same 
approach but rather than classifying it as binary we will classify it into 13 different sectors


When we will check for a description.We will clean it and stem it because all the documents are stemmed 



```{r warning=FALSE,message=FALSE}

match_sector<-data.frame()

for(i in 1:nrow(company_test)){
  
  
  match_sector[i,1]<-gsub(pattern="\\d",replacement = "",x=company_test[i,4])
  match_sector[i,1]<-gsub(pattern = "\\W",replacement = " ",x=match_sector[i,1])
  match_sector[i,1]<-gsub(pattern = "\\b[A-z]\\b{1}",replacement = "",x=match_sector[i,1])
  match_sector[i,1]<-removeWords(as.character(match_sector[i,1]),stopwords())
  match_sector[i,1]<-stemDocument(match_sector[i,1])
  # match_sector[i,1]<-sectortest(match_sector[i,1])
  
  desc_split<-unlist(str_split(match_sector[i,1],pattern="\\s+"))
 setwd("D:/UIC/MyResearch/Mwrd/Persado_Data_Scientist/Cleanedtext")
 
 
 match_files<-as.data.frame(list.files())
 match_files$`list.files()`<-as.character(match_files$`list.files()`)
  
  for(j in 1:13){
  assign(value=sum(!is.na(match(desc_split,scan(match_files[j,1],what = 'character',sep = "",nmax=3000)))),x = paste0(match_files[j,1],"match_test"))
  }




# We want to retrieve the names from the global environment

Basic_Industries_Final<-as.data.frame(Basic_Industrieswithcleaning.txtmatch_test)
Capital_Goods_Final<-as.data.frame(Capital_Goodswithcleaning.txtmatch_test)
Consumer_Durables_Final<-as.data.frame(Consumer_Durableswithcleaning.txtmatch_test)
Consumer_Non_Durables_Final<-as.data.frame(Consumer_Non_Durableswithcleaning.txtmatch_test)
Consumer_Services_Final<-as.data.frame(Consumer_Serviceswithcleaning.txtmatch_test)
Energy_Final<-as.data.frame(Energywithcleaning.txtmatch_test)
Finance_dataset_Final<-as.data.frame(Finance_datasetwithcleaning.txtmatch_test)
HealthCare_Final<-as.data.frame(HealthCarewithcleaning.txtmatch_test)
Miscelleaneous_Final<-as.data.frame(Miscelleaneouswithcleaning.txtmatch_test)
Public_Utilities_Final<-as.data.frame(Public_Utilitieswithcleaning.txtmatch_test) 
Technology_dataset_Final<-as.data.frame(Technology_datasetwithcleaning.txtmatch_test)
Transportation_Final<-as.data.frame(Transportationwithcleaning.txtmatch_test)
Unclassified_Final<-as.data.frame(Unclassifiedwithcleaning.txtmatch_test)

test_1<-data.frame()

test_1<-cbind.data.frame(Basic_Industries_Final,Capital_Goods_Final,Consumer_Durables_Final,Consumer_Non_Durables_Final,Consumer_Services_Final,Energy_Final,Finance_dataset_Final,HealthCare_Final,Miscelleaneous_Final,Public_Utilities_Final,Technology_dataset_Final,Transportation_Final,Unclassified_Final)

match_sector[i,1]<-names(test_1)[which.max(test_1[1,])]

}


```


Cleaning up the names of the predicted sector and comparing results with the original

After that will break the data into training and testing and then would compare the results



```{r results='asis'}
library(knitr)
for(i in 1:nrow(match_sector)){
  match_sector[i,1]<-gsub(pattern = "withcleaning.txtmatch_test",replacement="",x=match_sector[i,1])
  }

for(i in 1:nrow(match_sector)){
  match_sector[i,1]<-gsub(pattern = "_dataset",replacement="",x=match_sector[i,1])
  }




Results_Sector<-as.data.frame(sort(table(match_sector$V1),decreasing = T))

kable(Results_Sector,caption = "which_Sector")




```





# Model 2 

We will build a decision tree using the most frequent terms. In this case we will take
n=200 and weights can be given using the frequencies




```{r warning=FALSE,message=FALSE}

decision_trees_file_list<-list.files(path=getwd(),pattern ="*withoutcleaning.csv")

for( i in 1:13){

library(readr)
assign(x=decision_trees_file_list[i],value =read_csv(paste0("D:/UIC/MyResearch/Mwrd/Persado_Data_Scientist/",decision_trees_file_list[i]), col_types = cols(X1 = col_skip())))
  
}

```



Delete the frequency column from them 



```{r}
decision_trees_file_list<-as.data.frame(decision_trees_file_list)

decision_trees_file_list$decision_trees_file_list<-as.character(decision_trees_file_list$decision_trees_file_list)

for(i in 1:nrow(decision_trees_file_list))
{
assign(value=get(decision_trees_file_list[i,])[,1],x=decision_trees_file_list[i,])
}
  
```





```{r}
for(i in 1:nrow(decision_trees_file_list))
{
assign(value=as.data.frame(t(get(decision_trees_file_list[i,]))),x=paste0("t_",decision_trees_file_list[i,]))
  
}



```

Merging all the datasets together


```{r message=FALSE,warning=FALSE}

library(knitr)

words_decision_build<-rbind(t_Basic_Industrieswithoutcleaning.csv,t_Capital_Goodswithoutcleaning.csv,t_Consumer_Durableswithoutcleaning.csv,t_Consumer_Non_Durableswithoutcleaning.csv,t_Consumer_Serviceswithoutcleaning.csv,t_Energywithoutcleaning.csv,t_Finance_datasetwithoutcleaning.csv,t_HealthCarewithoutcleaning.csv,t_Miscelleaneouswithoutcleaning.csv,t_Public_Utilitieswithoutcleaning.csv,t_Technology_datasetwithoutcleaning.csv,t_Transportationwithoutcleaning.csv,t_Unclassifiedwithoutcleaning.csv)

kable(x=words_decision_build,caption = "Comparison of frequent words across sectors")


```

Creating a column and putting in the target sector

```{r}

row.names(words_decision_build)<-NULL

words_decision_build<-cbind(words_decision_build,decision_trees_file_list)

#Stripping of unneccessary names from the target column

words_decision_build[,201]<-gsub(pattern = 'withoutcleaning.csv',replacement = "",x = words_decision_build[,201])

# Renaming the target variable

colnames(words_decision_build)[colnames(words_decision_build)=="decision_trees_file_list"] <- "Target_Sector"



```


We will create a data frame having 13 target variable and then 200 words
as it's dependent variable

#Building a decision tree

Few things to keep in mind

**The dataset will contain more instances of Finance and Healthcare**

**The description would be in isolation**

**Therefore from 1 and 2 even though the samples of Finance and Healthcare would be more. But within a sample frequency of words would be similar among all sectors**



```{r warning=FALSE,message=FALSE,eval=FALSE}

library("tree")



tree_model=tree(Target_Sector~.,words_decision_build[,c(1:99,201)])
plot(tree_model)

tree.control(nobs=100,mincut = 10)

set.seed(3)
cv_tree=cv.tree(tree_model,FUN=prune.tree,K = 10)

plot(cv_tree$size,cv_tree$dev,type="b")
plot(tree_model)

pruned_model<-prune.misclass(tree_model,best = 7)

plot(pruned_model)
text(pruned_model,pretty=0)



```


# Findings,Issue and Future Improvements

Findings. We will compare the sector given and compare it with the original sector


```{r}

compare_sector_original_test<-cbind.data.frame(company_test,match_sector)




for(i in 1:nrow(compare_sector_original_test)){

compare_sector_original_test[i,5]<-gsub(pattern = "Basic_Industries",replacement = "Basic Industries",x=compare_sector_original_test[i,5])

compare_sector_original_test[i,5]<-gsub(pattern = "HealthCare",replacement = "Health Care",x=compare_sector_original_test[i,5])

compare_sector_original_test[i,5]<-gsub(pattern = "Capital_Goods",replacement = "Capital Goods",x=compare_sector_original_test[i,5])

compare_sector_original_test[i,5]<-gsub(pattern = "Consumer_Durables",replacement = "Consumer Durables",x=compare_sector_original_test[i,5])

compare_sector_original_test[i,5]<-gsub(pattern = "Consumer_Non_Durables",replacement = "Consumer Non-Durables",x=compare_sector_original_test[i,5])

compare_sector_original_test[i,5]<-gsub(pattern = "Consumer_Services",replacement = "Consumer Services",x=compare_sector_original_test[i,5])

compare_sector_original_test[i,5]<-gsub(pattern = "Public_Utilities",replacement = "Public Utilities",x=compare_sector_original_test[i,5])

compare_sector_original_test[i,5]<-gsub(pattern = "Public_Utilities",replacement = "Public Utilities",x=compare_sector_original_test[i,5])

compare_sector_original_test[i,5]<-gsub(pattern = "Unclassified",replacement = 'n/a',x=compare_sector_original_test[i,5])

}

compare_sector_original_test$Sector<-as.factor(compare_sector_original_test$Sector)


```




```{r}
  sum=0


  for(i in 1:nrow(compare_sector_original_test)){

    if(compare_sector_original_test[i,3]==compare_sector_original_test[i,5]){
    sum=sum+1
  }
  }

print(sum)
accuracy=sum/nrow(compare_sector_original_test)
print(accuracy)


```

#We correctltly predicted 466 observations and our accuracy was 59.28 %


#### Issues

1. The code can be improved.One chunk is written poorly as I was not able to get the files in the global environment and hence was not able to retrieve the variables. 

2. Time constraint. If given more time I can build a decision tree and predict what words lead to what sector. Right now the model is in nascent stage and I need two more weeks to build a decision tree model

3. Certain sectors didn't have a description on Yahoo Finance so they were removed from the dataset. For those datasets which had n/a written in the Sector but whose description was available were included in the data


#### Future-Improvements

1. Cross-Validation can be performed on the number of words scanned from a file so that optimum number of words from the file are retrieved and thus computing time can be shortened 

2. Word-Associations haven't been tested and they are a beautiful way of checking out correlations

3. Other techniques such as SVM, KNN can be employed and then the best techniques can be 
used












