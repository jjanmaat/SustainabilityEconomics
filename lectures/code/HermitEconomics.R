#
# Figures for hermit economics slides
#
# Hermit needs 200 - 250 kg of grain per year, and 150 - 180 kg of fish
#
# For further complexity, will add stock variables.  
# Stock of fish, determines how many fish will be there in the future
# Health of lake, a function of the amount of sediment that is added to 
#    the lake by farming the land.
# Health of land, a function of how much work is spent caring for the land.
#    could connect this to the fishing, as fish adds nitrogen to the land,
#    increasing its fertility.  Or could add work in mulching and integrating
#    straw and fish, which would be easier to just burn and dispose of.
#
# Create a file to track output that will be used to update document.
cat("***** Critical Output ****",
    "\n\nOutput of values to use in course text and presentations.",
    "\n",file="SustEconValues.txt")
# Grain production, input labour
grain <- function(l,a=1,b=0.5) {
  1000*(l/50)^b*a^(1-b)
}
#
# Fish production
fish <- function(e,b=0.5) {
  10*(e/0.5)^b
}
#
# Time available
tVec <- 0:365
#
# get yeilds
gYield <- grain(tVec)
fYield <- fish(tVec)
#
# Plot production functions.  
#
datPl <- data.frame(t=c(tVec,tVec),Ylab=c(rep("grain",length(tVec)),
                                          rep("fish",length(tVec))),
                    Y=c(gYield,fYield))
library(ggplot2)
pl <- ggplot(datPl,aes(x=t,y=Y)) +
  geom_line(linewidth=1.5) +
  labs(y="Yield (kg)", x="Labour/Effort (days)") +
  scale_x_continuous(expand=expansion(mult=c(0,0.02))) +
  scale_y_continuous(expand=expansion(mult=c(0,0.02))) +
  facet_grid(Ylab ~ ., scales="free_y")
print(pl)
ggsave(file="HermitProdFun.png",plot=pl,width=6,height=4)
#
# Add lines to use for total product. 
#
# Mark a production level to illustrate total production and average product.
#
tVal <- 180
# Create text to mark locations on plot, and their locations
datPl2 <- data.frame(t=rep(c(tVal,tVal,0)),Ylab=c(rep("grain",3),rep("fish",3)),
                     Y=c(0,rep(grain(tVal),2),0,rep(fish(tVal),2)),
                     label=c(paste("italic(L) == ",round(tVal,digits=1)),"",
                             paste("italic(Y) == ",round(grain(tVal),digits=1)),
                             paste("italic(L) == ",round(tVal,digits=1)),"",
                             paste("italic(Y) == ",round(fish(tVal),digits=1)))
                     )
# Create text that will be plotted using geom_label on the plot
datPl2a <- data.frame(t=rep(tVal+20,2),Ylab=c("grain","fish"),
                      Y=c(mean(range(gYield)),mean(range(fYield))),
                      l1=c(paste0("italic(TP) == ",round(grain(tVal),digits=1)),
                           paste0("italic(TP) == ",round(grain(tVal),digits=1))),
                      l2=c("italic(AP) == italic(TP) / italic(L)",
                           "italic(AP) == italic(TP) / italic(L)"),
                      l3=c(paste0("italic(AP) ==",round(fish(tVal)/tVal,digits=1)),
                           paste0("italic(AP) ==",round(fish(tVal)/tVal,digits=1))))
# trying to create a label with several lines, so that the values for the total 
# product and average product are printed on the plot.
# datPl2a$stLabs <- paste0("atop(textstyle(", datPl2a$l1, 
#                          "), atop(textstyle(", datPl2a$l2, 
#                          "), textstyle(", datPl2a$l3,")))")
plTP <- pl + 
  geom_path(aes(x=t,y=Y),data=datPl2,linewidth=0.75,linetype="dashed",color="grey30") +
  geom_text(aes(x=t,y=Y,label=label),data=datPl2,hjust=-0.1,vjust=-0.4,parse=TRUE) +
  # geom_label(aes(x=t,y=Y,label=stLabs),data=datPl2a,parse=TRUE,size=5) +
  facet_grid(Ylab ~ ., scales="free_y")
print(plTP)
ggsave(file="HermitProdFunTP.png",plot=plTP,width=6,height=4)
cat("\n*****",
    "\nTotal Product (grain) = ", round(grain(tVal),digits=2),
    "\nTotal Product (fish = ", round(fish(tVal),digits=2),
    "\nAverage Product (grain) at L = ",tVal," is ",round(grain(tVal)/tVal),
    "\nAverage Product (fish) at L = ",tVal," is ",round(fish(tVal)/tVal),
    "\n*****",file="SustEconValues.txt",append=TRUE)
#
# Add lines to use for marginal product
tVal <- 180
tDel <- 10
datPl3 <- data.frame(t=rep(c(tVal-tDel,tVal-tDel,0)),Ylab=c(rep("grain",3),rep("fish",3)),
                     Y=c(0,rep(grain(tVal-tDel),2),0,rep(fish(tVal-tDel),2)))
datPl4 <- data.frame(t=rep(c(tVal+tDel,tVal+tDel,0)),Ylab=c(rep("grain",3),rep("fish",3)),
                     Y=c(0,rep(grain(tVal+tDel),2),0,rep(fish(tVal+tDel),2)),
                     label=c(paste("Delta*italic(L) == ",
                                   round((tVal+tDel)-tVal,digits=1)),"",
                             paste("Delta*italic(Y) == ",
                                   round(grain(tVal+tDel)-grain(tVal),digits=1)),
                             paste("Delta*italic(L) == ",
                                   round((tVal+tDel)-tVal,digits=1)),"",
                             paste("Delta*italic(Y) == ",
                                   round(fish(tVal+tDel)-fish(tVal),digits=1))
                             )
)
plMP <- pl + 
  geom_path(aes(x=t,y=Y),data=datPl3,linewidth=0.75,linetype="dashed",color="grey30") +
  geom_path(aes(x=t,y=Y),data=datPl4,linewidth=0.75,linetype="dashed",color="grey30") +
  geom_text(aes(x=t,y=Y,label=label),data=datPl4,hjust=-0.1,vjust=-0.4,parse=TRUE) +
  facet_grid(Ylab ~ ., scales="free_y")
print(plMP)
ggsave(file="HermitProdFunMP.png",plot=plMP,width=6,height=4)
#
# Add tangency lines
library(numDeriv)
tDel <- 150
gMP <- grad(grain,tVal)
fMP <- grad(fish,tVal)
datPl5 <- data.frame(t=rep(c(tVal-tDel,tVal+tDel),2),
                     Ylab=c(rep("grain",2),rep("fish",2)),
                     Y=c(grain(tVal)+gMP*c(-tDel,tDel),
                         fish(tVal)+fMP*c(-tDel,tDel)),
                     label=c(paste("partialdiff*italic(Y)/partialdiff*italic(L) ==",
                                       round(gMP,digits=1)),"",
                             paste("partialdiff*italic(Y)/partialdiff*italic(L) ==",
                                       round(fMP,digits=1)),""
                             )
)
plTan <- pl + 
  geom_path(aes(x=t,y=Y),data=datPl2,linewidth=0.75,linetype="dashed") +
  geom_path(aes(x=t,y=Y),data=datPl5,linewidth=0.75,color="darkred") +
  geom_text(aes(x=t,y=Y,label=label),data=datPl5,hjust=0.3,vjust=-1,parse=TRUE) +
  facet_grid(Ylab ~ ., scales="free_y")
print(plTP)
ggsave(file="HermitProdFunTan.png",plot=plTan,width=6,height=4)
#
# Now put them together in a production possibilities frontier
datPl <- data.frame(grain=gYield,fish=rev(fYield))
pl <- ggplot(datPl,aes(x=grain,y=fish)) +
  geom_line(linewidth=1.5) +
  scale_x_continuous(expand=expansion(mult=c(0,0.02))) +
  scale_y_continuous(expand=expansion(mult=c(0,0.02))) +
  labs(x="Grain (kg)",y="Fish (kg)")
print(pl)
ggsave(file="HermitPPF.png",plot=pl,width=4,height=4)
#
# Add lines to the production possibilities frontier to show the 
# opportunity cost.
tVal <- 120
tDel <- 20
datPl1 <- data.frame(grain=c(rep(grain(tVal),2),0),
                     fish=c(0,rep(fish(365-tVal),2)))
datPl2 <- data.frame(grain=c(rep(grain(tVal+tDel),2),0),
                     fish=c(0,rep(fish(365-tVal-tDel),2)))
tVal <- 240
tDel <- 20
datPl3 <- data.frame(grain=c(rep(grain(tVal),2),0),
                     fish=c(0,rep(fish(365-tVal),2)))
datPl4 <- data.frame(grain=c(rep(grain(tVal+tDel),2),0),
                     fish=c(0,rep(fish(365-tVal-tDel),2)))
plPPFoc <- pl +
  geom_path(aes(x=grain,y=fish),data=datPl1) +
  geom_path(aes(x=grain,y=fish),data=datPl2) +  
  geom_path(aes(x=grain,y=fish),data=datPl3) +
  geom_path(aes(x=grain,y=fish),data=datPl4)
ggsave(file="HermitPPFoc.png",plot=plPPFoc,width=4,height=4)
