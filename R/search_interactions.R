## input is a data.frame with two columns
## directional or not 

# an example 
# input_interactions = data.frame(id1 = c('FBgn0011656','FBgn0003149',"7872"),id2 = c('FBgn0003149','FBgn0003900',"9232"))
# source('/Users/joha/Dropbox/Yunlong/DynamicGeneNetwork/Keller/SloS_work/functions_fly/search_interactions.R')
# output = search_interaction(input_interactions=pairs,type="all")
# output%>%do(data.frame(type=.$type,.$outdata%>%select(id1,id2)))%>%group_by(type)%>%summarise(n=n())
# output%>%do(data.frame(type=.$type,.$outdata))%>%select(id1,id2,PMID_URL)%>%tail


search_interaction = function(input_interactions,type= c('all',"ppi","rna","tf","generic"),data_folder = "/Users/joha/Dropbox/Yunlong/DynamicGeneNetwork/Keller/SloS_work/functions_fly/interaction_data/")
{

library(dplyr)
input_int = unique(input_interactions)

source = type

if (type=="all") { source = c("ppi","rna","tf","genetic") } 

search_each = function(s,data_folder,input_interactions=input_int)
{

names(input_interactions) = c('id1','id2')	
if (s!='tf')
{
input_interactions2 = input_interactions[,c("id2","id1")];
names(input_interactions2) = c('id1','id2')
input_interactions = rbind.data.frame(input_interactions,input_interactions2)
}



if (s=="ppi")
{
(file  = list.files(data_folder,pattern=s,full.names=TRUE))

temp = read.delim(file,header=TRUE)
head(temp)
temp = temp%>%rename(id1 = FLY_GENE1,id2=FLY_GENE2)

res = input_interactions%>%merge(.,temp,by=c("id1","id2"))

res

}

if (s=="rna")
{

(file  = list.files(data_folder,pattern=s,full.names=TRUE))

temp = read.delim(file,header=TRUE)

fun <- function(x, y){ 
rna1 = as.character(subset(temp,FLY_TARGET_GENE%in%x)$RNA_SYMBOL)
rna2 = as.character(subset(temp,FLY_TARGET_GENE%in%y)$RNA_SYMBOL)
data.frame(rna=intersect(rna1,rna2))
}

res = input_interactions%>%rowwise()%>%do(rna = fun(.$id1[1],.$id2[1]))%>%mutate(n=nrow(rna),count = paste0(rna[,1],collapse="\\"))%>%select(n,count)%>%cbind.data.frame(input_interactions,.)


}

if (s=="tf")
{

(file  = list.files(data_folder,pattern=s,full.names=TRUE))

temp = read.delim(file,header=TRUE)
head(temp)
temp = temp%>%rename(id1 = FLY_TF_GENE,id2=FLY_TARGET_GENE)

res = input_interactions%>%merge(.,temp,by=c("id1","id2"))


}

if (s=="genetic")
{

(file  = list.files(data_folder,pattern=s,full.names=TRUE))

temp = read.delim(file,header=TRUE)
head(temp)
temp = temp%>%rename(id1 = FLY_GENE1,id2=FLY_GENE2)

res = input_interactions%>%merge(.,temp,by=c("id1","id2"))
}

return(res)
}

output = source%>%data.frame(type=.)%>%group_by(type)%>%do(outdata=search_each(.$type,data_folder=data_folder,input_interactions=input_int))


return(output)

}




# not run 
# data_folder = "/Users/joha/Dropbox/Yunlong/DynamicGeneNetwork/Keller/SloS_work/functions_fly/interaction_data/"
# input_interactions = data.frame(id1 = c('FBgn0011656','FBgn0003149',"7872"),id2 = c('FBgn0003149','FBgn0003900',"9232"))
# data_folder = "C:/Users/ynie01/Dropbox/Yunlong/DynamicGeneNetwork/Keller/SloS_work/functions_fly/interaction_data/"
# ut%>%extract2
# tidy(outdata)
# methods(class="tbl_df")
# output%>%mutate(n=nrow(outdata))%>%filter(n>0)%>%data.frame(.$outdata)%>%filter(n>0)%>%select(-outdata,-n.1)
# output%>%group_by(type)
# filter(nrow(.$outdata)>0)
# output%>%select(type)%>%do(type2 = data.frame(.$type)[1,1])
