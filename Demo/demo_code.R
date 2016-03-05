time_obs = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 7, 8, 9, 
10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)
library(flyfuns)
data(fdlist)
gene_target = "Myo31DF"
yfd = fdlist[[which(names(fdlist)==matched_id(gene_target)$CG_ID)]]
xnames = names(xfdlist)
yname = xnames[which(names(fdlist)==matched_id(gene_target)$CG_ID)]
xfdlist  = fdlist # each element in the xfdlist is a fd object 


# step 1 specify the tuning parameter grid
parameter_grid = expand.grid(lambda=10^c(-2,-3,-4),gamma=10^c(-2,-3,-4))

# step 2 run the estimation function for each tuning parameter set
res_all = parameter_grid%>%head(.,2)%>%rowwise()%>%do(lambda=.$lambda[1],gamma=.$gamma[1],res = regfun_slos(xfdlist,yfd,time_obs,yname,xnames,lambda=.$lambda[1],gamma=.$gamma[1]))

# step 3 select the optimal tuning parameter based on AICc

optimal_index=  which.min(sapply(res_all$res, function(x) x$AICc))

# step 4 extract the regulation function estimation

res = res_all$res[optimal_index]

flyids = matched_id(xnames)%>%dplyr::select(CG_ID,genesymbol)%>%dplyr::rename(xname=CG_ID) # merge the CG_ID with genesymbols 

regfd = res$estimated_fd%>%left_join(.,flyids,by="xname")

regfuns = regfd$regfd # regfuns contains all the estimated regulation functions

# for example we plot the estimated regulation function from gene sls, which is the 5th element in the list 

i=5
plot(regfuns[[i]],xlab="gene expression",ylab="regulation function",main=regfd$genesymbol[i])

