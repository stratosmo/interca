interca <- function(data = data, numaxes = 10){
  out <- list()

  results <- MCA(data,ncp = numaxes,graph = F)#running multiple correspondence analysis with FactoMineR package
  coords <- results$var$coord#points' coordinates for first factorial axis
  ctr<- results$var$contrib#points' contribution for first factorial axis
  cor <- results$var$cos2#points' contribution for first factorial axis
 # eig <- results$eig
  lbl <- rownames(results$var$coord)
  signs <- apply(coords,2,sign)
  ecoords <- matrix(0,nrow(coords),ncol(coords))
  rownames(ecoords)=lbl
  colnames(ecoords)=colnames(results$var$coord)
  for (i in 1:numaxes) {
    ecoords[,i] <- sign(coords[,i])*ctr[,i]*results$eig[i,1]
  }

  out$coords <- coords
  out$ecoords <- ecoords
  out$ctr <- ctr
  out$cor <- cor
 # out$eig <- eig
  out$lbl <- lbl
  fviz_eig(results, addlabels=TRUE, hjust = -0.3,
           linecolor ="red") + theme_minimal()->plot
  
  out$plot<-plot
  class(out) <- "automca"
  out
}

# #Loading libraries (please ensure that they are installed in your system )
# 
# # Get the directory from the path of the current file.
# cur_dir2 = dirname(this.path())
# 
# # Set the working directory.
# setwd(cur_dir2)
# 
# data(wg93)
# wg93
# the_dataf=wg93
# results=MCA(the_dataf,ncp = 27,graph = F)#running multiple correspondence analysis with FactoMineR package
# 
# #create a data frame under the name auto_mca_table which contains
# #all numbers are rounded in the second decimal digit
# #required data for illustrating  first interpretive axis and first interpretive plane
# f1=results$var$coord[,1]#points' coordinates for first factorial axis
# f2=results$var$coord[,2]#points' coordinates for second factorial axis
# ctr1=results$var$contrib[,1]#points' contribution for first factorial axis
# ctr2=results$var$contrib[,2]#points' contribution for second factorial axis
# cor1=results$var$cos2[,1]#points' contribution for first factorial axis
# cor2=results$var$cos2[,2]#points' contribution for second factorial axis
# 
# ###determine how many first factorial axes to include in application of 2nd condition for first interpretive axis and first interpretive plane
# numaxes=5#use your preferred value
# 
# ##calculate maximum COR for each point in the first selected axes
# maxcoraxis=apply(results$var$cos2[,1:numaxes],1,max)#find best  axis for projection
# 
# which.maxcoraxis=apply(results$var$cos2[,1:numaxes],1,which.max)#find name of best  axis
# 
# ##calculate maximum sum of COR for each point for the combinations of planes for first selected factorial axes
# thecors=data.frame()
# positions=c()
# for (s in 1:nrow(results$var$cos2)){
#   k=0
#   megistos=0
# for (i in 1:numaxes){
#    for(j in 1:numaxes){
#     if (i<j){
#       k=k+1
#       thecors[s,k]=results$var$cos2[s,i]+results$var$cos2[s,j]
#       if(thecors[s,k]>megistos){
#         megistos=thecors[s,k]
#         xaxis=i
#         yaxis=j
#       }
#       
#       }
#    }
#  
#  
# }
# positions[s]=paste(xaxis,",",yaxis)
# }
# maxcorplane=apply(thecors,1,max)#find best  plane for projection
# which.maxcorplane=positions#find names of axes for best  plane
# 
# ###creation of auto_mca_table####
# #auto_mca_table contains information for results' verification
# #Columns explanations
# #f1:original point's coordinate for first factorial axis
# #f2:original point's coordinate for second factorial axis
# #ctr1:point's CTR value for first factorial axis
# #ctr2:point's CTR value for second factorial axis
# #cor1:point's COR value for first factorial axis
# #cor2:point's CTR value for first factorial axis
# #e1:point's interpretive coordinate for first factorial axis
# #e2:point's interpretive coordinate for second factorial axis
# #the_names:point's name
# #abs_e1:absolute value for point's interpretive coordinate for first factorial axis
# #maxcoreaxis: point's max COR value for the selected first factorial axes (see numaxes variable)
# #which.maxcoraxis: point's best axis for interpretation
# #maxcorplane:point's max COR value for the combinations of planes for the selected factorial axes (see numaxes variable)
# #which.maxcorplane: point's best plane for interpretation
# #(e.g 2,4 or 24 means that best plane is created from the second and the fourth axis)
# #c=abs(e1)+abs(e2) is used for the first condition of the interpretive plane
# 
# y=rep(0,length(f1))
# the_names=names(results$call$marge.col)
# e1=sign(f1)*ctr1*results$eig[1,1]
# e2=sign(f2)*ctr2*results$eig[2,1]
# auto_mca_table=as.data.frame(cbind(f1,f2,ctr1,ctr2,cor1,cor2,e1,e2,the_names))
# auto_mca_table$f1=as.numeric(auto_mca_table$f1)
# auto_mca_table$f2=as.numeric(auto_mca_table$f2)
# auto_mca_table$ctr1=as.numeric(auto_mca_table$ctr1)
# auto_mca_table$ctr2=as.numeric(auto_mca_table$ctr2)
# auto_mca_table$cor1=as.numeric(auto_mca_table$cor1)
# auto_mca_table$cor2=as.numeric(auto_mca_table$cor2)
# auto_mca_table$e1=as.numeric(auto_mca_table$e1)
# auto_mca_table$abs_e1=as.numeric(abs(auto_mca_table$e1))
# auto_mca_table$e2=as.numeric(auto_mca_table$e2)
# auto_mca_table$maxcoraxis=maxcoraxis
# auto_mca_table$which.maxcoraxis=which.maxcoraxis
# auto_mca_table$maxcorplane=maxcorplane
# auto_mca_table$which.maxcorplane=which.maxcorplane
# auto_mca_table$c=abs(e1)+abs(e2)
# 
# 
# #first factorial axis with first condition
# 
# auto_mca_table %>%filter(ctr1>100/nrow(auto_mca_table))->auto_mca_tablef1
# yf1=rep(0,nrow(auto_mca_tablef1))
#   auto_mca_tablef1%>%
#   ggplot() +
#   aes(x = f1, y = yf1) +
#   geom_point(shape = "circle", size = 1, colour = "#B22222") +
#   labs(x = "Coordinate (F)", title = "First Factorial axis with 1st condition") ->factorial1
# 
# factorial1+theme(axis.text.y=element_blank(),
#                  axis.ticks.y=element_blank())+
#   geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
#   geom_label_repel(
#     aes(label = paste(the_names)),
#     size = 6.5,max.overlaps = Inf,
#     point.padding = 0.9, 
#     min.segment.length = 0.6, 
#     box.padding = 0.1,
#     
#   )->factorial1
# 
# #first interpretive axis
# auto_mca_table %>%filter(abs(e1)>mean(abs(e1)))->auto_mca_tablee1
# ye1=rep(0,nrow(auto_mca_tablee1))
# auto_mca_tablee1%>%
#   ggplot() +
#   aes(x = e1, y = ye1) +
#   geom_point(shape = "circle", size = 1, colour = "#B22222") +
#   labs(x = "Interpretive Coordinate (e)", title = "First Interpretive axis with first condition") ->interpretive1
# 
# interpretive1+theme(axis.text.y=element_blank(),
#                     axis.ticks.y=element_blank())+
#   geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
#   geom_label_repel(
#     aes(label = paste(the_names)), 
#     size = 6.5,max.overlaps = Inf,
#     point.padding = 0.9, 
#     min.segment.length = 0.6, 
#     box.padding = 0.1
#     
#   ) ->interpretive1
# averageinterpretive1=sum(abs(auto_mca_table$e1))/nrow(auto_mca_table)
# 
# interpretive1+geom_vline(color="red",xintercept = averageinterpretive1)+
#   geom_vline(xintercept = -averageinterpretive1,color='red')->interpretive1
# 
# 
# #first interpretive axis with both conditions
# auto_mca_table %>%filter(abs(e1)>mean(abs(e1)))%>%filter(abs(cor1-maxcoraxis)<=0.00001)->auto_mca_tablee1cor
# 
# ye1cor=rep(0,nrow(auto_mca_tablee1cor))
# auto_mca_tablee1cor%>%
#   ggplot() +
#   aes(x = e1, y = ye1cor) +
#   geom_point(shape = "circle", size = 1, colour = "#B22222") +
#   labs(x = "Interpretive Coordinate (e)", title = "First Interpretive axis with both conditions") ->interpretive1cor
# 
# interpretive1cor+theme(axis.text.y=element_blank(),
#                     axis.ticks.y=element_blank())+
#   geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
#   geom_label_repel(
#     aes(label = paste(the_names)), 
#     size = 6.5,max.overlaps = Inf,
#     point.padding = 0.9, 
#     min.segment.length = 0.6, 
#     box.padding = 0.1
#     
#   ) ->interpretive1cor
# averageinterpretive1=sum(abs(auto_mca_table$e1))/nrow(auto_mca_table)
# 
# interpretive1cor+geom_vline(color="red",xintercept = averageinterpretive1)+
#   geom_vline(xintercept = -averageinterpretive1,color='red')->interpretive1cor
# 
# 
# #first factorial plane
# auto_mca_tablefp1=auto_mca_table
# auto_mca_tablefp1$avepl1=((results$eig[1,1]+results$eig[2,1])/nrow(auto_mca_tablefp1))*100
# auto_mca_tablefp1 %>%filter(abs(e1)+abs(e2)>avepl1)->auto_mca_tablefp1
# 
# auto_mca_tablefp1%>%
#   ggplot() +
#   aes(x = f1, y = f2) +
#   geom_point(shape = "circle", size = 2.5, colour = "#B22222") +
#   labs(x = "Factorial axis1 (F1)",y="Factorial axis2 (F2)" ,title = "1st Factorial plane") ->factorial_plane1
# 
# factorial_plane1+
#   geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
#   geom_label_repel(
#     aes(label = paste(the_names)), 
#     size = 6.5,
#     max.overlaps = Inf,
#     point.padding = 0.7, 
#     min.segment.length = 0.9, 
#     
#     box.padding = 1.2
#     
#   ) ->factorial_plane1
# 
# #first interpretive plane with "magic" squares and 1st condition
# auto_mca_tableep1=auto_mca_table
# #auto_mca_tableep1$avepl1=((results$eig[1,1]+results$eig[2,1])/nrow(auto_mca_tableep1))*100
# avepl1=mean(abs(auto_mca_tableep1$e1)+abs(auto_mca_tableep1$e2))
# auto_mca_tableep1 %>%filter(abs(e1)+abs(e2)>avepl1)->auto_mca_tableep1
# 
# sumccc=abs(auto_mca_table$e1)+abs(auto_mca_table$e2)
# averageccc=mean(sumccc)
# avexx=c(averageccc,0,-averageccc,0,averageccc)
# aveyy=c(0,-averageccc,0,averageccc,0)
# averagesquaree=round(as.data.frame(cbind(avexx,aveyy)),2)
# 
# ggplot()->interpretive_plane1
# interpretive_plane1+geom_path(data=averagesquaree,aes(x=avexx,y=aveyy),color='red')->interpretive_plane1 
# auto_mca_tableep1$c=abs(auto_mca_tableep1$e1)+abs(auto_mca_tableep1$e2)
# for (i in 1:nrow(auto_mca_tableep1)){
#   
#   thexx=c(auto_mca_tableep1$c[i],0,-auto_mca_tableep1$c[i],0,auto_mca_tableep1$c[i])
#   theyy=c(0,-auto_mca_tableep1$c[i],0,auto_mca_tableep1$c[i],0)
#   ccc=cbind(thexx,theyy)
#   ccc=as.data.frame(ccc)
#   
#   interpretive_plane1 +geom_path(data=ccc,aes(x=thexx,y=theyy),show.legend = T,size=0.1)->interpretive_plane1 
#   
# }
# interpretive_plane1+geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+coord_fixed()+
#   geom_point(data=auto_mca_tableep1,aes(x = e1, y = e2,label=the_names),shape = "circle", size = 1.5, colour = "black")->interpretive_plane1
# 
# interpretive_plane1+geom_label_repel(data=auto_mca_tableep1,
#                                      mapping=aes(x=e1,y=e2,label = paste(the_names)), # data point size
#                                      size = 6.5,
#                                      max.overlaps = Inf,
#                                      point.padding = 0.7, 
#                                      min.segment.length = 0.9, 
#                                      box.padding = 1.2
#                                      
# )+
#   labs(x = "Interpretive axis1 (e1)",y="Interpretive axis2 (e2)" ,title = "1st Interpretive plane with 1st condition")->interpretive_plane1
# 
# 
# 
# ###first interpretive plane with magic "squares" and both conditions
# 
# auto_mca_tableep1cor=auto_mca_table
# 
# avepl1=mean(abs(auto_mca_tableep1cor$e1)+abs(auto_mca_tableep1cor$e2))
# auto_mca_tableep1cor %>%filter(abs(e1)+abs(e2)>avepl1)%>%filter(abs(cor1+cor2-maxcorplane)<=0.00001)->auto_mca_tableep1cor
# 
# sumccc=abs(auto_mca_table$e1)+abs(auto_mca_table$e2)
# averageccc=mean(sumccc)
# avexx=c(averageccc,0,-averageccc,0,averageccc)
# aveyy=c(0,-averageccc,0,averageccc,0)
# averagesquaree=round(as.data.frame(cbind(avexx,aveyy)),2)
# 
# ggplot()->interpretive_plane1cor
# interpretive_plane1cor+geom_path(data=averagesquaree,aes(x=avexx,y=aveyy),color='red')->interpretive_plane1cor 
# auto_mca_tableep1cor$c=abs(auto_mca_tableep1cor$e1)+abs(auto_mca_tableep1cor$e2)
# for (i in 1:nrow(auto_mca_tableep1cor)){
#   
#   thexx=c(auto_mca_tableep1cor$c[i],0,-auto_mca_tableep1cor$c[i],0,auto_mca_tableep1cor$c[i])
#   theyy=c(0,-auto_mca_tableep1cor$c[i],0,auto_mca_tableep1cor$c[i],0)
#   ccc=cbind(thexx,theyy)
#   ccc=as.data.frame(ccc)
#   
#   interpretive_plane1cor +geom_path(data=ccc,aes(x=thexx,y=theyy),show.legend = T,size=0.1)->interpretive_plane1cor 
#   
# }
# interpretive_plane1cor+geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+coord_fixed()+
#   geom_point(data=auto_mca_tableep1cor,aes(x = e1, y = e2,label=the_names),shape = "circle", size = 1.5, colour = "black")->interpretive_plane1cor
# 
# interpretive_plane1cor+geom_label_repel(data=auto_mca_tableep1cor,
#                                      mapping=aes(x=e1,y=e2,label = paste(the_names)), # data point size
#                                      size = 6.5,
#                                      max.overlaps = Inf,
#                                      point.padding = 0.7, 
#                                      min.segment.length = 0.9, 
#                                      box.padding = 1.2
#                                      
# )+
#   labs(x = "Interpretive axis1 (e1)",y="Interpretive axis2 (e2)" ,title = "1st Interpretive plane with both conditions")->interpretive_plane1cor
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #autoMCAv2.R outputs
# 
# 
# #plotly plots: can be found under the Viewer tab
# 
# 
# 
# ggplotly(factorial1+aes(label=the_names))
# 
# ggplotly(interpretive1+aes(label=the_names))
# 
# ggplotly(interpretive1cor+aes(label=the_names))
# 
# ggplotly(factorial_plane1+aes(label=the_names))
# 
# ggplotly(interpretive_plane1)
# 
# ggplotly(interpretive_plane1cor)
# 
# #ggplot2 plots: can be found under the Plots tab
# 
# plot(factorial1) #first classical factorial axis with 1st condition
# 
# plot(interpretive1) #first interpretive axis with 1st condition
# 
# plot(interpretive1cor) #first interpretive axis with both condition
# 
# egg::ggarrange(factorial1,interpretive1,interpretive1cor,nrow=3,ncol=1)#parallel comparison of 3 above axes
# 
# plot(factorial_plane1)# first classical factorial plane
# 
# plot(interpretive_plane1) #first interpretive plane with squares and 1st condition
# 
# plot(interpretive_plane1cor)#first interpretive plane with squares and both conditions
# 
# egg::ggarrange(factorial_plane1,interpretive_plane1,interpretive_plane1cor,nrow=1,ncol=3)
# #applying 2nd condition is very strict
# 
# #auto_mca_table for proofing and numerical evidence
# auto_mca_table$c=abs(e1)+abs(e2)# c=abs(e1)+abs(e2)
# 
# auto_mca_table[,c(1:8,10:13,15)]=round(auto_mca_table[,c(1:8,10:13,15)],2)
# View(auto_mca_table)
# summary(results)
#acknowledgements and references
# Copyright (c) 2021 tidyverse authors

# # Permission is hereby granted, free of charge, to any person obtaining a copy of 
# this software and associated documentation files (the "Software"), to deal in the Software without restriction, 
# including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
# copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

#S. Lê, J. Josse, and F. Husson, “FactoMineR: 
#An R Package for Multivariate Analysis,” Journal 
#of Statistical Software, vol. 25, pp. 1–18, Mar. 2008, doi: 10.18637/jss.v025.i01.
#   The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

# Slowikowski, K., Schep, A., Hughes, S., Lukauskas, S., Irisson, J. O., Kamvar, Z. N., ... & Slowikowski, M. K. (2018). 
# Package ggrepel. Automatically position non-overlapping text labels with ‘ggplot2.

# Wickham, H., Chang, W., & Wickham, M. H. (2016). Package ‘ggplot2’. Create 
# elegant data visualisations using the grammar of graphics. Version, 2(1), 1-189.
# 
# Wickham, H., Bryan, J., Kalicinski, M., Valery, K., Leitienne, C., Colbert, B., 
# ... & Bryan, M. J. (2019). Package ‘readxl’. Computer Software]
# https://readxl. tidyverse. org.
# Wickham H, Averick M, Bryan J, Chang W, McGowan LD, François R, Grolemund G, Hayes A, Henry L, Hester J, Kuhn M, Pedersen TL, Miller E, Bache SM, Müller K, Ooms J, Robinson D, Seidel DP, Spinu V, Takahashi K, Vaughan D, Wilke C, Woo K, Yutani H (2019). “Welcome to the tidyverse.” Journal of Open Source Software, 4(43), 1686. doi:10.21105/joss.01686.
# To cite the package 'ca' in publications please use:
#   
#   Nenadic O, Greenacre M (2007). “Correspondence Analysis in R, with two- and three-dimensional graphics: The ca package.” Journal of Statistical Software, 20(3), 1-13. http://www.jstatsoft.org.
# Sievert C (2020). Interactive Web-Based Data Visualization with R, plotly, and shiny. Chapman and Hall/CRC. ISBN 9781138331457, https://plotly-r.com.
# Auguie, B., 2019. Extensions for 'ggplot2': Custom geom, custom themes, plot alignment, labelled panels, symmetric scales, and fixed panel size [R package egg version 0.4.5]. The Comprehensive R Archive Network. Available at: https://cran.r-project.org/web/packages/egg/index.html [Accessed August 31, 2022]. 
# 
# Anon, Package soc.ca. CRAN. Available at: https://cran.r-project.org/web/packages/soc.ca/index.html [Accessed August 31, 2022]. 
# 
# 
# https://cran.r-project.org/web/packages/this.path/index.html YEAR: 2022 COPYRIGHT HOLDER: Andrew Simmons(this.path package)
# Kassambara, A., & Mundt, F. (2021). Factoextra: Extract and Visualize the Results of Multivariate Data Analyses; R Package Version 1.0. 7. 2020.
# Alberti, G. (2015). CAinterprTools: An R package to help interpreting Correspondence Analysis’ results. SoftwareX, 1, 26-31.
# Xie, Y., Cheng, J., & Tan, X. (2018). DT: a wrapper of the JavaScript library ‘DataTables’. R package version 0.4.
# Chang, W., Cheng, J., Allaire, J. J., Xie, Y., & McPherson, J. (2015). Package ‘shiny’. See http://citeseerx. ist. psu. edu/viewdoc/download.
