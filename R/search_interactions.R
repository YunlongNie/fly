#' This function identity search the interactions in the flybase data
#'
#' @param input_interactions data.frame with two cols
#' @param type interaction type one of 'all',"ppi","rna","tf","generic"
#' @param type interaction type one of 'all',"ppi","rna","tf","generic"
#' @export
#' @import dplyr 
#' @import tidyr
#' @examples
#' \dontrun{
#' input_interactions = data.frame(id1 = c('FBgn0011656','FBgn0003149',"7872"),id2 = c('FBgn0003149','FBgn0003900',"9232"))
#' output = search_interaction(input_interactions=pairs,type="all")
#' output%>%do(data.frame(type=.$type,.$outdata%>%select(id1,id2)))%>%group_by(type)%>%summarise(n=n())
#' output%>%do(data.frame(type=.$type,.$outdata))%>%select(id1,id2,PMID_URL)%>%tail
#' }

search_interaction = function(input_interactions,type= c('all',"ppi","rna","tf","generic"))
{


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
data(flybase_ppi)

res = input_interactions%>%merge(.,flybase_ppi,by=c("id1","id2"))

res

}

if (s=="rna")
{

data(rna_gene)

fun <- function(x, y){ 
rna1 = as.character(subset(rna_gene,FLY_TARGET_GENE%in%x)$RNA_SYMBOL)
rna2 = as.character(subset(rna_gene,FLY_TARGET_GENE%in%y)$RNA_SYMBOL)
data.frame(rna=intersect(rna1,rna2))
}

res = input_interactions%>%rowwise()%>%do(rna = fun(.$id1[1],.$id2[1]))%>%mutate(n=nrow(rna),rna = paste0(rna[,1],collapse="\\"))%>%select(n,rna)%>%cbind.data.frame(input_interactions,.)


}

if (s=="tf")
{
data(tf_gene)
res = input_interactions%>%merge(.,tf_gene,by=c("id1","id2"))
}

if (s=="genetic")
{

data(fly_genetic_interactions)
fly_genetic_interactions= fly_genetic_interactions%>%rename(id1 = FLY_GENE1,id2=FLY_GENE2)
res = input_interactions%>%merge(.,fly_genetic_interactions,by=c("id1","id2"))
}

return(res)
}

output = source%>%data.frame(type=.)%>%group_by(type)%>%do(outdata=search_each(.$type,data_folder=data_folder,input_interactions=input_int))


return(output)

}



