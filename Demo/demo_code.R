time_obs = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 7, 8, 9, 
10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)
library(flyfuns)
data(fdlist)
gene_target = "Myo31DF"
yfd = fdlist[[which(names(fdlist)==matched_id(gene_target)$CG_ID)]]
xnames = names(xfdlist)
xfdlist  = fdlist
res = regfun_slos(xfdlist,yfd,time_obs,yname=xnames[14],xnames)