plot1dslider <- function(df,avg) {
  #first factorial axis with first condition
  e1 <- as.vector(df)
  n <- length(e1)
  #  if (is.null(threshold))
  avge <- avg
  #  else
  #    avge <- threshold
  #lbl <- res$lbl[abs(e1)> avge] 
  #e1 <- round(e1[abs(e1)> avge],2)
  
  #first interpretive axis
  ye1 <- rep(0,length(e1))
  lbl<-names(df)
  auto_mca_tablee1 <- data.frame(cbind(e1,ye1,lbl))
  
  auto_mca_tablee1$e1 <- as.numeric(auto_mca_tablee1$e1)
  auto_mca_tablee1$ye1 <- as.numeric(auto_mca_tablee1$ye1)
  auto_mca_tablee1$lbl <- as.factor(auto_mca_tablee1$lbl)

  
  
  interpretive1 <- auto_mca_tablee1%>%
    ggplot() +
    aes(x = e1, y = ye1) +
    geom_point(shape = "circle", size = 1, colour = "#B22222") +
    labs(x = paste("Interpretive axis"), y="")+theme(axis.text.y=element_blank(),
                                                         axis.ticks.y=element_blank())+geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
    geom_label_repel(
      aes(label = lbl), 
      size = 6.5,max.overlaps = Inf,
      point.padding = 0.9, 
      min.segment.length = 0.6, 
      box.padding = 0.1
    ) 
  
  interpretive1<- interpretive1+geom_vline(color="red",xintercept = avge)+
    geom_vline(xintercept = -avge,color='red')
  
  interpretive1 
}




#first interpretive axis with both conditions
#auto_mca_table %>%filter(abs(e1)>mean(abs(e1)))%>%filter(abs(cor1-maxcoraxis)<=0.00001)->auto_mca_tablee1cor

# ye1cor=rep(0,nrow(auto_mca_tablee1cor))
#  auto_mca_tablee1cor%>%
#    ggplot() +
#    aes(x = e1, y = ye1cor) +
#    geom_point(shape = "circle", size = 1, colour = "#B22222") +
#    labs(x = "Interpretive Coordinate (e)", title = "First Interpretive axis with both conditions") ->interpretive1cor

#  interpretive1cor+theme(axis.text.y=element_blank(),
#                         axis.ticks.y=element_blank())+
#    geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
#    geom_label_repel(
#      aes(label = paste(the_names)), 
#      size = 6.5,max.overlaps = Inf,
#      point.padding = 0.9, 
#      min.segment.length = 0.6, 
#      box.padding = 0.1

#    ) ->interpretive1cor
#  averageinterpretive1=sum(abs(auto_mca_table$e1))/nrow(auto_mca_table)

#  interpretive1cor+geom_vline(color="red",xintercept = averageinterpretive1)+
#    geom_vline(xintercept = -averageinterpretive1,color='red')->interpretive1cor


#first factorial plane
#  auto_mca_tablefp1=auto_mca_table
#  auto_mca_tablefp1$avepl1=((results$eig[1,1]+results$eig[2,1])/nrow(auto_mca_tablefp1))*100
#  auto_mca_tablefp1 %>%filter(abs(e1)+abs(e2)>avepl1)->auto_mca_tablefp1

#   auto_mca_tablefp1%>%
#     ggplot() +
#     aes(x = f1, y = f2) +
#     geom_point(shape = "circle", size = 2.5, colour = "#B22222") +
#     labs(x = "Factorial axis1 (F1)",y="Factorial axis2 (F2)" ,title = "1st Factorial plane") ->factorial_plane1
#   
#   factorial_plane1+
#     geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
#     geom_label_repel(
#       aes(label = paste(the_names)), 
#       size = 6.5,
#       max.overlaps = Inf,
#       point.padding = 0.7, 
#       min.segment.length = 0.9, 
#       
#       box.padding = 1.2
#       
#     ) ->factorial_plane1
#   
#   #first interpretive plane with "magic" squares and 1st condition
#   auto_mca_tableep1=auto_mca_table
#   #auto_mca_tableep1$avepl1=((results$eig[1,1]+results$eig[2,1])/nrow(auto_mca_tableep1))*100
#   avepl1=mean(abs(auto_mca_tableep1$e1)+abs(auto_mca_tableep1$e2))
#   auto_mca_tableep1 %>%filter(abs(e1)+abs(e2)>avepl1)->auto_mca_tableep1
#   
#   sumccc=abs(auto_mca_table$e1)+abs(auto_mca_table$e2)
#   averageccc=mean(sumccc)
#   avexx=c(averageccc,0,-averageccc,0,averageccc)
#   aveyy=c(0,-averageccc,0,averageccc,0)
#   averagesquaree=round(as.data.frame(cbind(avexx,aveyy)),2)
#   
#   ggplot()->interpretive_plane1
#   interpretive_plane1+geom_path(data=averagesquaree,aes(x=avexx,y=aveyy),color='red')->interpretive_plane1 
#   auto_mca_tableep1$c=abs(auto_mca_tableep1$e1)+abs(auto_mca_tableep1$e2)
#   for (i in 1:nrow(auto_mca_tableep1)){
#     
#     thexx=c(auto_mca_tableep1$c[i],0,-auto_mca_tableep1$c[i],0,auto_mca_tableep1$c[i])
#     theyy=c(0,-auto_mca_tableep1$c[i],0,auto_mca_tableep1$c[i],0)
#     ccc=cbind(thexx,theyy)
#     ccc=as.data.frame(ccc)
#     
#     interpretive_plane1 +geom_path(data=ccc,aes(x=thexx,y=theyy),show.legend = T,size=0.1)->interpretive_plane1 
#     
#   }
#   interpretive_plane1+geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+coord_fixed()+
#     geom_point(data=auto_mca_tableep1,aes(x = e1, y = e2,label=the_names),shape = "circle", size = 1.5, colour = "black")->interpretive_plane1
#   
#   interpretive_plane1+geom_label_repel(data=auto_mca_tableep1,
#                                        mapping=aes(x=e1,y=e2,label = paste(the_names)), # data point size
#                                        size = 6.5,
#                                        max.overlaps = Inf,
#                                        point.padding = 0.7, 
#                                        min.segment.length = 0.9, 
#                                        box.padding = 1.2
#                                        
#   )+
#     labs(x = "Interpretive axis1 (e1)",y="Interpretive axis2 (e2)" ,title = "1st Interpretive plane with 1st condition")->interpretive_plane1
#   
#   
#   
#   ###first interpretive plane with magic "squares" and both conditions
#   
#   auto_mca_tableep1cor=auto_mca_table
#   
#   avepl1=mean(abs(auto_mca_tableep1cor$e1)+abs(auto_mca_tableep1cor$e2))
#   auto_mca_tableep1cor %>%filter(abs(e1)+abs(e2)>avepl1)%>%filter(abs(cor1+cor2-maxcorplane)<=0.00001)->auto_mca_tableep1cor
#   
#   sumccc=abs(auto_mca_table$e1)+abs(auto_mca_table$e2)
#   averageccc=mean(sumccc)
#   avexx=c(averageccc,0,-averageccc,0,averageccc)
#   aveyy=c(0,-averageccc,0,averageccc,0)
#   averagesquaree=round(as.data.frame(cbind(avexx,aveyy)),2)
#   
#   ggplot()->interpretive_plane1cor
#   interpretive_plane1cor+geom_path(data=averagesquaree,aes(x=avexx,y=aveyy),color='red')->interpretive_plane1cor 
#   auto_mca_tableep1cor$c=abs(auto_mca_tableep1cor$e1)+abs(auto_mca_tableep1cor$e2)
#   for (i in 1:nrow(auto_mca_tableep1cor)){
#     
#     thexx=c(auto_mca_tableep1cor$c[i],0,-auto_mca_tableep1cor$c[i],0,auto_mca_tableep1cor$c[i])
#     theyy=c(0,-auto_mca_tableep1cor$c[i],0,auto_mca_tableep1cor$c[i],0)
#     ccc=cbind(thexx,theyy)
#     ccc=as.data.frame(ccc)
#     
#     interpretive_plane1cor +geom_path(data=ccc,aes(x=thexx,y=theyy),show.legend = T,size=0.1)->interpretive_plane1cor 
#     
#   }
#   interpretive_plane1cor+geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+coord_fixed()+
#     geom_point(data=auto_mca_tableep1cor,aes(x = e1, y = e2,label=the_names),shape = "circle", size = 1.5, colour = "black")->interpretive_plane1cor
#   
#   interpretive_plane1cor+geom_label_repel(data=auto_mca_tableep1cor,
#                                           mapping=aes(x=e1,y=e2,label = paste(the_names)), # data point size
#                                           size = 6.5,
#                                           max.overlaps = Inf,
#                                           point.padding = 0.7, 
#                                           min.segment.length = 0.9, 
#                                           box.padding = 1.2
#                                           
#   )+
#     labs(x = "Interpretive axis1 (e1)",y="Interpretive axis2 (e2)" ,title = "1st Interpretive plane with both conditions")->interpretive_plane1cor
#   
#   #plotly plots: can be found under the Viewer tab
#   
#   
#   
#   ggplotly(factorial1+aes(label=the_names))
#   
#   ggplotly(interpretive1+aes(label=the_names))
#   
#   ggplotly(interpretive1cor+aes(label=the_names))
#   
#   ggplotly(factorial_plane1+aes(label=the_names))
#   
#   ggplotly(interpretive_plane1)
#   
#   ggplotly(interpretive_plane1cor)
#   
#   #ggplot2 plots: can be found under the Plots tab
#   
#   plot(factorial1) #first classical factorial axis with 1st condition
#   
#   plot(interpretive1) #first interpretive axis with 1st condition
#   
#   plot(interpretive1cor) #first interpretive axis with both condition
#   
#   egg::ggarrange(factorial1,interpretive1,interpretive1cor,nrow=3,ncol=1)#parallel comparison of 3 above axes
#   
#   plot(factorial_plane1)# first classical factorial plane
#   
#   plot(interpretive_plane1) #first interpretive plane with squares and 1st condition
#   
#   plot(interpretive_plane1cor)#first interpretive plane with squares and both conditions
#   
#   egg::ggarrange(factorial_plane1,interpretive_plane1,interpretive_plane1cor,nrow=1,ncol=3)
#   #applying 2nd condition is very strict
#   
#   #auto_mca_table for proofing and numerical evidence
#   auto_mca_table$c=abs(e1)+abs(e2)# c=abs(e1)+abs(e2)
#   
#   auto_mca_table[,c(1:8,10:13,15)]=round(auto_mca_table[,c(1:8,10:13,15)],2)
#   View(auto_mca_table)
#   summary(results)
# }

#library(tidyverse)
# library(ggrepel)
# library(ggplot2)
# library(FactoMineR)
# library(ca)
# library(plotly)
# library(this.path)
# library(egg)
# library(soc.ca)
# library(factoextra)
# library(CAinterprTools)
# library(DT)
# library(shiny)

