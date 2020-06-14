##从Endnote导出BibTex格式的文本文档后，由于缺少BibTex的文献标签(key)。
##以下R程序就是给导出的文本文档给每条文献逐条加上BibTex所需要的标签。
##标签的格式是'James:2010'。如果是两位作者，则为'James-White:2010'。如果是
##三位及其以上作者，则为'James-etal:2010'。
##若刚好碰到文献的标签有重复，如某一作者在同一年不同杂志上发表了多篇
##论文，则用a,b,c...来区分标记，即'James:2010a','James:2010b','James:2010c'。
##
## 斯幸峰(http://www.sixf.org)  2014年3月28日
##


oldbib.ori = read.delim(file.choose(),h=F,stringsAsFactors=F) # load the file "oldbib.txt"
oldbib=oldbib.ori[,1]

# check '@' for each line
at.f=function(test){
  test=as.character(test)
  ntest=nchar(test)
  testsplit=strsplit(test,NULL)[[1]]
  en.res=paste(testsplit[testsplit%in%c("@")],collapse="")
  en.res
}

# retrive English letters
en.f=function(test){
  test=as.character(test)
  ntest=nchar(test)
  testsplit=strsplit(test,NULL)[[1]]
  en.res=paste(testsplit[testsplit%in%c(LETTERS,letters)],collapse="")
  en.res
}

# retrive Chinese letter
marknotes=c(" ","-","‐",".","(",")",",","’","=","{","}")
specialletters=c("ç","ó","á","é","í","ø","ä","ú","ñ","å","Ø","ï","ã","š","ü")
zh.f=function(test){
  test=as.character(test)
  ntest=nchar(test)
  testsplit=strsplit(test,NULL)[[1]]
  zh.res=paste(testsplit[!testsplit%in%c(LETTERS,letters,marknotes,specialletters)],collapse="")
  zh.res
}

# Number of authors
author.f=function(test){
  test=as.character(test)
  if (length(strsplit(test,"author =")[[1]])>1) {
    author.no=length(strsplit(test," and ")[[1]])
  } else {
    author.no=0
  }
  author.no
}

# retrive the year
number.f=function(test){
  test=as.character(test)
  ntest=nchar(test)
  if (length(strsplit(test,"year =")[[1]])>1) {
    testsplit=strsplit(test,NULL)[[1]]
    number.res=paste(testsplit[testsplit%in%c(0:9)],collapse="")
  } else {number.res="NA"}
  number.res
}

# create a blank matrix to store the line number of each reference with
# corresponding line numbers of author and year.
idx.mat=matrix(NA,ncol=3,nrow=length(oldbib),dimnames=list(1:length(oldbib),c("ref","author","year")))
for (i in 1:1:length(oldbib)) {
  if (at.f(oldbib[i])=="@") idx.mat[i,1]=i
  if (author.f(oldbib[i])!=0) idx.mat[i,2]=i
  if (number.f(oldbib[i])!="NA") idx.mat[i,3]=i
}

# exclude the 'NA' in the 'idx.mat'
# so the rownumber of 'idx.ref' is the total number of reference.
idx.ref=idx.mat[!is.na(idx.mat[,1]),]
rownames(idx.ref)=1:dim(idx.ref)[1]
idx.ref[,2]=idx.mat[!is.na(idx.mat[,2]),2]
idx.ref[,3]=idx.mat[!is.na(idx.mat[,3]),3]

# function of creating the key for each reference
key.f=function(idx.ref){
  test=oldbib[idx.ref[2]]
  left.no = which(strsplit(test,NULL)[[1]]=="{")
  right.no = which(strsplit(test,NULL)[[1]]=="}")
  comma.no = which(strsplit(test,NULL)[[1]]==",")
  new.name.line=substr(test,left.no+1,right.no-1)
  author.no = length(strsplit(new.name.line," and ")[[1]])
  if (author.no==1) {  # only one author in the reference
    if (zh.f(test)!="") { # check whether this is a Chinese reference
      author=zh.f(strsplit(new.name.line," and ")[[1]][1])
      year=number.f(oldbib[idx.ref[3]])
      key=paste(author,":",year,sep="")      
    } else { # or else considering as English. Similar as below.
      author=en.f(substr(test,left.no+1,comma.no[1]-1))
      year=number.f(oldbib[idx.ref[3]])
      key=paste(author,":",year,sep="")      
    }
  }
  if (author.no==2) { # two authors in the reference
    if (nchar(en.f(test))<12) { # or use: zh.f(test)!=""
      author1=zh.f(strsplit(new.name.line," and ")[[1]][1])
      author2=zh.f(strsplit(new.name.line," and ")[[1]][2])
      year=number.f(oldbib[idx.ref[3]])
      key=paste(author1,"-",author2,":",year,sep="")
    } else {
      author1=en.f(substr(test,left.no+1,comma.no[1]-1))
      fullname2=strsplit(new.name.line," and ")[[1]][2]
      comma.no2=which(strsplit(fullname2,NULL)[[1]]==",")
      author2=substr(fullname2,1,comma.no2-1)
      year=number.f(oldbib[idx.ref[3]])
      key=paste(author1,"-",author2,":",year,sep="")
    }
  }
  if (author.no>2) { # three or more authors in the reference
    if (nchar(en.f(test))<20) { # or use: zh.f(test)!=""
      author=zh.f(strsplit(new.name.line," and ")[[1]][1])
      year=number.f(oldbib[idx.ref[3]])
      key=paste(author,"-etal:",year,sep="")      
    } else {
      author=en.f(substr(test,left.no+1,comma.no[1]-1))
      year=number.f(oldbib[idx.ref[3]])
      key=paste(author,"-etal:",year,sep="")
    }
  }
  key
}

# create the key
key.df=as.data.frame(idx.ref)
key.df$key=apply(idx.ref,1,key.f)

# add 'a','b','c'... as a suffix for the same keys
for (i in 1:dim(key.df)[1]) {
  key.rep=which(key.df$key%in%key.df$key[i])
  key.no=length(key.rep)
  if (key.no>1) {
    for (j in 1:key.no) {
      key.df[key.rep[j],"key"]=paste(key.df[key.rep[j],"key"],letters[j],sep="")
    }
  }
}

# check whether each key is unique.
max(table(key.df$key)) # if the result is 1, then continue

# create the new bibliography as 'newbib'
newbib=oldbib.ori[,1]
for (i in 1:dim(key.df)[1]) {
  newbib[key.df[i,"ref"]]=paste(oldbib[key.df[i,"ref"]],key.df[i,"key"],",",sep="")
}

# write the new bibliography 'newbib' as 'newbib.bib' in the console
write.table(newbib,"newbib.bib",row.names=F,col.names=F,quote=FALSE)
