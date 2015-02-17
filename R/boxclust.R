boxclust <- function(data, outcome, treatment, cluster=NULL, covariate=NULL,
                     xlabel="Treatment", ylabel="Outcome", option="dotplot",
                     legpos="top", psize=2.5, hjitter=0, vlines="none",
                     pneg=NULL, ppos=NULL, stars=FALSE, pvalsize=3, printN=TRUE,
                     nsize=5, labelsize=12, titlesize=15, white=FALSE){
  
  if(is.null(cluster)){
    cluster <- as.factor(data[, treatment])
    if(option=="dotplot"){
      option <- "color"
    }
  }else{
    cluster <- as.factor(data[, cluster])
  }
  
  option <- match.arg(option, choices=c("dotplot", "color", "uni", "none"))
  legpos <- match.arg(legpos, choices=c("top", "bottom", "left", "right", "none"))
  vlines <- match.arg(vlines, choices=c("none", "fg", "bg"))
  
  if(is.null(covariate)){
    dat <- data.frame(outcome=data[, outcome], treatment=as.factor(data[, treatment]),
                      cluster=cluster)
  }else{
    dat <- data.frame(outcome=data[, outcome], treatment=as.factor(data[, treatment]),
                      cluster=cluster, covariate=as.factor(data[, covariate]))
  }
  
  dat <- ddply(dat, .(treatment), transform, howmany=as.numeric(cluster) - min(as.numeric(cluster)) + 1,
               clusters=length(unique(cluster)))
  
  dats <- ddply(dat, .(treatment), summarize, mean=mean(outcome), sd=sd(outcome),
                n=paste("n=", length(outcome), sep=""), N=paste("N=", length(unique(cluster)), sep=""))
  
  if(is.null(pneg)==TRUE & is.null(ppos)==TRUE){
    pnegtext <- geom_blank()
    ppostext <- geom_blank()    
  }else{
    if(is.null(pneg)==FALSE & is.null(ppos)==TRUE){
      if(stars==FALSE){
        pneg2 <- c("", ifelse(pneg < 0.001, "p<0.001", paste("p=", round(pneg, 3), sep="")))
        pnegtext <- annotate("text", label=pneg2, x=1:nlevels(as.factor(data[, treatment])) + 0.45, y=dats$mean + dats$sd, size=pvalsize, vjust=-1)
        ppostext <- geom_blank()
      }else{
        pnegstars <- c("", ifelse(pneg < 0.001, "***", ifelse(pneg < 0.01, "**", ifelse(pneg < 0.05, "*", "n.s."))))
        pnegtext <- annotate("text", label=pnegstars, x=1:nlevels(as.factor(data[, treatment])) + 0.45, y=dats$mean + dats$sd, size=pvalsize, vjust=-1)
        ppostext <- geom_blank()
      }
    }else{
      if(is.null(pneg)==FALSE & is.null(ppos)==FALSE){
        if(stars==FALSE){
          pneg2 <- c("", ifelse(pneg < 0.001, "p<0.001", paste("p=", round(pneg, 3), sep="")), "")
          pnegtext <- annotate("text", label=pneg2, x=1:nlevels(as.factor(data[, treatment])) + 0.45, y=dats$mean + dats$sd, size=pvalsize, vjust=-1)
          ppos2 <- c("", ifelse(ppos < 0.001, "p<0.001", paste("p=", round(ppos, 3), sep="")), "")
          ppostext <- annotate("text", label=ppos2, x=1:nlevels(as.factor(data[, treatment])) + 0.45, y=dats$mean - dats$sd, size=pvalsize, vjust=2)
        }else{
          pnegstars <- c("", ifelse(pneg < 0.001, "***", ifelse(pneg < 0.01, "**", ifelse(pneg < 0.05, "*", "n.s."))), "")
          pnegtext <- annotate("text", label=pnegstars, x=1:nlevels(as.factor(data[, treatment])) + 0.45, y=dats$mean + dats$sd, size=pvalsize, vjust=-1)
          pposstars <- c("", ifelse(ppos < 0.001, "***", ifelse(ppos < 0.01, "**", ifelse(ppos < 0.05, "*", "n.s."))), "")
          ppostext <- annotate("text", label=pposstars, x=1:nlevels(as.factor(data[, treatment])) + 0.45, y=dats$mean - dats$sd, size=pvalsize, vjust=2)
        }
      }
    }
  }
  
  if(white==FALSE){
    thefill <- "white"      
  }else{
    thefill <- "gray95"
  }
  
  thetheme <- theme(axis.text.x = element_text(size=labelsize),
                    axis.text.y = element_text(size=labelsize),
                    axis.title.x = element_text(size=titlesize),
                    axis.title.y = element_text(size=titlesize),
                    legend.position = legpos,
                    legend.key = element_blank())
  
  if(is.null(covariate)){
    
    if(option=="dotplot"){
      
      if(vlines=="none"){
        theplot <- ggplot(dat, aes(x=treatment, y=outcome)) +
          geom_boxplot(fill=thefill, outlier.size=0) +
          geom_point(aes(x=as.numeric(treatment) - 0.325 + 0.6 * howmany/clusters, group=treatment),
                     size=psize, colour="gray40", alpha=0.7, position=position_jitter(height=0, width=hjitter)) +
          geom_errorbar(data=dats, aes(x=as.numeric(treatment) + 0.45, y=NULL, ymin=mean - sd, ymax=mean + sd),
                        width=0.15, position=position_identity(width=0.5), colour="gray50") +
          geom_point(data=dats, aes(x=as.numeric(treatment) + 0.45, y=mean), shape=3, colour="gray50") +
          geom_text(data=dats, aes(y=Inf, label=n), size=nsize, vjust=2) +
          pnegtext +
          ppostext +
          labs(shape=covariate) +
          guides(shape=guide_legend(title=NULL)) +
          xlab(xlabel) +
          ylab(ylabel) +
          ylim(min(min(dat$outcome), dats$mean - dats$sd), max(max(dat$outcome), dats$mean + dats$sd))        
        if(white==F){
          if(printN==T){
            print(theplot + geom_text(data=dats, aes(y=-Inf, label=N), size=nsize, vjust=-2) + thetheme)
          }else{
            print(theplot + thetheme)
          }
        }else{
          if(printN==T){
            print(theplot + geom_text(data=dats, aes(y=-Inf, label=N), size=nsize, vjust=-2) + theme_bw() + thetheme)
          }else{
            print(theplot + theme_bw() + thetheme)
          }
        }
      }
      
      if(vlines=="bg"){
        theplot <- ggplot(dat, aes(x=treatment, y=outcome)) +
          geom_boxplot(fill=thefill, outlier.size=0) +
          geom_vline(aes(xintercept=as.numeric(treatment) - 0.33 + 0.6 * howmany/clusters, group=treatment),
                     linetype=2, colour="gray70", alpha=0.7) +
          geom_boxplot(outlier.size=0) +
          geom_point(aes(x=as.numeric(treatment) - 0.325 + 0.6 * howmany/clusters, group=treatment),
                     size=psize, colour="gray40", alpha=0.7, position=position_jitter(height=0, width=hjitter)) +
          geom_errorbar(data=dats, aes(x=as.numeric(treatment) + 0.45, y=NULL, ymin=mean - sd, ymax=mean + sd),
                        width=0.15, position=position_identity(width=0.5), colour="gray50") +
          geom_point(data=dats, aes(x=as.numeric(treatment) + 0.45, y=mean), shape=3, colour="gray50") +
          geom_text(data=dats, aes(y=Inf, label=n), size=nsize, vjust=2) +
          pnegtext +
          ppostext +
          labs(shape=covariate) +
          guides(shape=guide_legend(title=NULL)) +
          xlab(xlabel) +
          ylab(ylabel) +
          ylim(min(min(dat$outcome), dats$mean - dats$sd), max(max(dat$outcome), dats$mean + dats$sd))        
        if(white==F){
          if(printN==T){
            print(theplot + geom_text(data=dats, aes(y=-Inf, label=N), size=nsize, vjust=-2) + thetheme)
          }else{
            print(theplot + thetheme)
          }
        }else{
          if(printN==T){
            print(theplot + geom_text(data=dats, aes(y=-Inf, label=N), size=nsize, vjust=-2) + theme_bw() + thetheme)
          }else{
            print(theplot + theme_bw() + thetheme)
          }
        }
      }
      
      if(vlines=="fg"){
        theplot <- ggplot(dat, aes(x=treatment, y=outcome)) +
          geom_boxplot(fill=thefill, outlier.size=0) +
          geom_vline(aes(xintercept=as.numeric(treatment) - 0.33 + 0.6 * howmany/clusters, group=treatment),
                     linetype=2, colour="gray70", alpha=0.7) +
          geom_point(aes(x=as.numeric(treatment) - 0.325 + 0.6 * howmany/clusters, group=treatment),
                     size=psize, colour="gray40", alpha=0.7, position=position_jitter(height=0, width=hjitter)) +
          geom_errorbar(data=dats, aes(x=as.numeric(treatment) + 0.45, y=NULL, ymin=mean - sd, ymax=mean + sd),
                        width=0.15, position=position_identity(width=0.5), colour="gray50") +
          geom_point(data=dats, aes(x=as.numeric(treatment) + 0.45, y=mean), shape=3, colour="gray50") +
          geom_text(data=dats, aes(y=Inf, label=n), size=nsize, vjust=2) +
          pnegtext +
          ppostext +
          labs(shape=covariate) +
          guides(shape=guide_legend(title=NULL)) +
          xlab(xlabel) +
          ylab(ylabel) +
          ylim(min(min(dat$outcome), dats$mean - dats$sd), max(max(dat$outcome), dats$mean + dats$sd))        
        if(white==F){
          if(printN==T){
            print(theplot + geom_text(data=dats, aes(y=-Inf, label=N), size=nsize, vjust=-2) + thetheme)
          }else{
            print(theplot + thetheme)
          }
        }else{
          if(printN==T){
            print(theplot + geom_text(data=dats, aes(y=-Inf, label=N), size=nsize, vjust=-2) + theme_bw() + thetheme)
          }else{
            print(theplot + theme_bw() + thetheme)
          }
        }
      }
      
    }
    
    if(option=="color"){
      
      theplot <- ggplot(dat, aes(x=treatment, y=outcome)) +
        geom_boxplot(fill=thefill, outlier.size=0) +
        geom_point(aes(x=as.numeric(treatment), group=treatment, colour=cluster),
                   size=psize, alpha=0.7, position=position_jitter(height=0, width=hjitter)) +
        geom_errorbar(data=dats, aes(x=as.numeric(treatment) + 0.45, y=NULL, ymin=mean - sd, ymax=mean + sd),
                      width=0.15, position=position_identity(width=0.5), colour="gray50") +
        geom_point(data=dats, aes(x=as.numeric(treatment) + 0.45, y=mean), shape=3, colour="gray50") +
        geom_text(data=dats, aes(y=Inf, label=n), size=nsize, vjust=2) +
        pnegtext +
        ppostext +
        labs(colour=cluster, shape=covariate) +
        guides(colour=guide_legend(title=NULL), shape=guide_legend(title=NULL)) +
        xlab(xlabel) +
        ylab(ylabel) +
        ylim(min(min(dat$outcome), dats$mean - dats$sd), max(max(dat$outcome), dats$mean + dats$sd))        
      if(white==F){
        if(printN==T){
          print(theplot + geom_text(data=dats, aes(y=-Inf, label=N), size=nsize, vjust=-2) + thetheme)
        }else{
          print(theplot + thetheme)
        }
      }else{
        if(printN==T){
          print(theplot + geom_text(data=dats, aes(y=-Inf, label=N), size=nsize, vjust=-2) + theme_bw() + thetheme)
        }else{
          print(theplot + theme_bw() + thetheme)
        }
      }
      
    }
    
    if(option=="uni"){
      
      theplot <- ggplot(dat, aes(x=treatment, y=outcome)) +
        geom_boxplot(fill=thefill, outlier.size=0) +
        geom_point(aes(x=as.numeric(treatment), group=treatment), colour="black",
                   size=psize, alpha=0.7, position=position_jitter(height=0, width=hjitter)) +
        geom_errorbar(data=dats, aes(x=as.numeric(treatment) + 0.45, y=NULL, ymin=mean - sd, ymax=mean + sd),
                      width=0.15, position=position_identity(width=0.5), colour="gray50") +
        geom_point(data=dats, aes(x=as.numeric(treatment) + 0.45, y=mean), shape=3, colour="gray50") +
        geom_text(data=dats, aes(y=Inf, label=n), size=nsize, vjust=2) +
        pnegtext +
        ppostext +
        labs(shape=covariate) +
        guides(shape=guide_legend(title=NULL)) +
        xlab(xlabel) +
        ylab(ylabel) +
        ylim(min(min(dat$outcome), dats$mean - dats$sd), max(max(dat$outcome), dats$mean + dats$sd))        
      if(white==F){
        if(printN==T){
          print(theplot + geom_text(data=dats, aes(y=-Inf, label=N), size=nsize, vjust=-2) + thetheme)
        }else{
          print(theplot + thetheme)
        }
      }else{
        if(printN==T){
          print(theplot + geom_text(data=dats, aes(y=-Inf, label=N), size=nsize, vjust=-2) + theme_bw() + thetheme)
        }else{
          print(theplot + theme_bw() + thetheme)
        }
      }
      
    }
    
    if(option=="none"){
      
      theplot <- ggplot(dat, aes(x=treatment, y=outcome)) +
        geom_boxplot(fill=thefill, outlier.size=0) +
        geom_errorbar(data=dats, aes(x=as.numeric(treatment) + 0.45, y=NULL, ymin=mean - sd, ymax=mean + sd),
                      width=0.15, position=position_identity(width=0.5), colour="gray50") +
        geom_point(data=dats, aes(x=as.numeric(treatment) + 0.45, y=mean), shape=3, colour="gray50") +
        geom_text(data=dats, aes(y=Inf, label=n), size=nsize, vjust=2) +
        pnegtext +
        ppostext +
        xlab(xlabel) +
        ylab(ylabel) +
        ylim(min(min(dat$outcome), dats$mean - dats$sd), max(max(dat$outcome), dats$mean + dats$sd))        
      if(white==F){
        if(printN==T){
          print(theplot + geom_text(data=dats, aes(y=-Inf, label=N), size=nsize, vjust=-2) + thetheme)
        }else{
          print(theplot + thetheme)
        }
      }else{
        if(printN==T){
          print(theplot + geom_text(data=dats, aes(y=-Inf, label=N), size=nsize, vjust=-2) + theme_bw() + thetheme)
        }else{
          print(theplot + theme_bw() + thetheme)
        }
      }
      
    }
    
  }else{
    
    if(option=="dotplot"){
      
      if(vlines=="none"){
        theplot <- ggplot(dat, aes(x=treatment, y=outcome)) +
          geom_boxplot(fill=thefill, outlier.size=0) +
          geom_point(aes(x=as.numeric(treatment) - 0.325 + 0.6 * howmany/clusters, group=treatment, shape=covariate),
                     size=psize, colour="gray40", alpha=0.7, position=position_jitter(height=0, width=hjitter)) +
          geom_errorbar(data=dats, aes(x=as.numeric(treatment) + 0.45, y=NULL, ymin=mean - sd, ymax=mean + sd),
                        width=0.15, position=position_identity(width=0.5), colour="gray50") +
          geom_point(data=dats, aes(x=as.numeric(treatment) + 0.45, y=mean), shape=3, colour="gray50") +
          geom_text(data=dats, aes(y=Inf, label=n), size=nsize, vjust=2) +
          pnegtext +
          ppostext +
          labs(shape=covariate) +
          guides(shape=guide_legend(title=NULL)) +
          xlab(xlabel) +
          ylab(ylabel) +
          ylim(min(min(dat$outcome), dats$mean - dats$sd), max(max(dat$outcome), dats$mean + dats$sd))        
        if(white==F){
          if(printN==T){
            print(theplot + geom_text(data=dats, aes(y=-Inf, label=N), size=nsize, vjust=-2) + thetheme)
          }else{
            print(theplot + thetheme)
          }
        }else{
          if(printN==T){
            print(theplot + geom_text(data=dats, aes(y=-Inf, label=N), size=nsize, vjust=-2) + theme_bw() + thetheme)
          }else{
            print(theplot + theme_bw() + thetheme)
          }
        }
      }
      
      if(vlines=="bg"){
        theplot <- ggplot(dat, aes(x=treatment, y=outcome)) +
          geom_boxplot(fill=thefill, outlier.size=0) +
          geom_vline(aes(xintercept=as.numeric(treatment) - 0.33 + 0.6 * howmany/clusters, group=treatment),
                     linetype=2, colour="gray70", alpha=0.7) +
          geom_boxplot(outlier.size=0) +
          geom_point(aes(x=as.numeric(treatment) - 0.325 + 0.6 * howmany/clusters, group=treatment, shape=covariate),
                     size=psize, colour="gray40", alpha=0.7, position=position_jitter(height=0, width=hjitter)) +
          geom_errorbar(data=dats, aes(x=as.numeric(treatment) + 0.45, y=NULL, ymin=mean - sd, ymax=mean + sd),
                        width=0.15, position=position_identity(width=0.5), colour="gray50") +
          geom_point(data=dats, aes(x=as.numeric(treatment) + 0.45, y=mean), shape=3, colour="gray50") +
          geom_text(data=dats, aes(y=Inf, label=n), size=nsize, vjust=2) +
          pnegtext +
          ppostext +
          labs(shape=covariate) +
          guides(shape=guide_legend(title=NULL)) +
          xlab(xlabel) +
          ylab(ylabel) +
          ylim(min(min(dat$outcome), dats$mean - dats$sd), max(max(dat$outcome), dats$mean + dats$sd))        
        if(white==F){
          if(printN==T){
            print(theplot + geom_text(data=dats, aes(y=-Inf, label=N), size=nsize, vjust=-2) + thetheme)
          }else{
            print(theplot + thetheme)
          }
        }else{
          if(printN==T){
            print(theplot + geom_text(data=dats, aes(y=-Inf, label=N), size=nsize, vjust=-2) + theme_bw() + thetheme)
          }else{
            print(theplot + theme_bw() + thetheme)
          }
        }
      }
      
      if(vlines=="fg"){
        theplot <- ggplot(dat, aes(x=treatment, y=outcome)) +
          geom_boxplot(fill=thefill, outlier.size=0) +
          geom_vline(aes(xintercept=as.numeric(treatment) - 0.33 + 0.6 * howmany/clusters, group=treatment),
                     linetype=2, colour="gray70", alpha=0.7) +
          geom_point(aes(x=as.numeric(treatment) - 0.325 + 0.6 * howmany/clusters, group=treatment, shape=covariate),
                     size=psize, colour="gray40", alpha=0.7, position=position_jitter(height=0, width=hjitter)) +
          geom_errorbar(data=dats, aes(x=as.numeric(treatment) + 0.45, y=NULL, ymin=mean - sd, ymax=mean + sd),
                        width=0.15, position=position_identity(width=0.5), colour="gray50") +
          geom_point(data=dats, aes(x=as.numeric(treatment) + 0.45, y=mean), shape=3, colour="gray50") +
          geom_text(data=dats, aes(y=Inf, label=n), size=nsize, vjust=2) +
          pnegtext +
          ppostext +
          labs(shape=covariate) +
          guides(shape=guide_legend(title=NULL)) +
          xlab(xlabel) +
          ylab(ylabel) +
          ylim(min(min(dat$outcome), dats$mean - dats$sd), max(max(dat$outcome), dats$mean + dats$sd))        
        if(white==F){
          if(printN==T){
            print(theplot + geom_text(data=dats, aes(y=-Inf, label=N), size=nsize, vjust=-2) + thetheme)
          }else{
            print(theplot + thetheme)
          }
        }else{
          if(printN==T){
            print(theplot + geom_text(data=dats, aes(y=-Inf, label=N), size=nsize, vjust=-2) + theme_bw() + thetheme)
          }else{
            print(theplot + theme_bw() + thetheme)
          }
        }
      }
      
    }
    
    if(option=="color"){
      
      theplot <- ggplot(dat, aes(x=treatment, y=outcome)) +
        geom_boxplot(fill=thefill, outlier.size=0) +
        geom_point(aes(x=as.numeric(treatment), group=treatment, colour=cluster, shape=covariate),
                   size=psize, alpha=0.7, position=position_jitter(height=0, width=hjitter)) +
        geom_errorbar(data=dats, aes(x=as.numeric(treatment) + 0.45, y=NULL, ymin=mean - sd, ymax=mean + sd),
                      width=0.15, position=position_identity(width=0.5), colour="gray50") +
        geom_point(data=dats, aes(x=as.numeric(treatment) + 0.45, y=mean), shape=3, colour="gray50") +
        geom_text(data=dats, aes(y=Inf, label=n), size=nsize, vjust=2) +
        pnegtext +
        ppostext +
        labs(colour=cluster, shape=covariate) +
        guides(colour=guide_legend(title=NULL), shape=guide_legend(title=NULL)) +
        xlab(xlabel) +
        ylab(ylabel) +
        ylim(min(min(dat$outcome), dats$mean - dats$sd), max(max(dat$outcome), dats$mean + dats$sd))        
      if(white==F){
        if(printN==T){
          print(theplot + geom_text(data=dats, aes(y=-Inf, label=N), size=nsize, vjust=-2) + thetheme)
        }else{
          print(theplot + thetheme)
        }
      }else{
        if(printN==T){
          print(theplot + geom_text(data=dats, aes(y=-Inf, label=N), size=nsize, vjust=-2) + theme_bw() + thetheme)
        }else{
          print(theplot + theme_bw() + thetheme)
        }
      }
      
    }
    
    if(option=="uni"){
      
      theplot <- ggplot(dat, aes(x=treatment, y=outcome)) +
        geom_boxplot(fill=thefill, outlier.size=0) +
        geom_point(aes(x=as.numeric(treatment), group=treatment, shape=covariate), colour="black",
                   size=psize, alpha=0.7, position=position_jitter(height=0, width=hjitter)) +
        geom_errorbar(data=dats, aes(x=as.numeric(treatment) + 0.45, y=NULL, ymin=mean - sd, ymax=mean + sd),
                      width=0.15, position=position_identity(width=0.5), colour="gray50") +
        geom_point(data=dats, aes(x=as.numeric(treatment) + 0.45, y=mean), shape=3, colour="gray50") +
        geom_text(data=dats, aes(y=Inf, label=n), size=nsize, vjust=2) +
        pnegtext +
        ppostext +
        labs(shape=covariate) +
        guides(shape=guide_legend(title=NULL)) +
        xlab(xlabel) +
        ylab(ylabel) +
        ylim(min(min(dat$outcome), dats$mean - dats$sd), max(max(dat$outcome), dats$mean + dats$sd))        
      if(white==F){
        if(printN==T){
          print(theplot + geom_text(data=dats, aes(y=-Inf, label=N), size=nsize, vjust=-2) + thetheme)
        }else{
          print(theplot + thetheme)
        }
      }else{
        if(printN==T){
          print(theplot + geom_text(data=dats, aes(y=-Inf, label=N), size=nsize, vjust=-2) + theme_bw() + thetheme)
        }else{
          print(theplot + theme_bw() + thetheme)
        }
      }
      
    }
    
    if(option=="none"){
      
      theplot <- ggplot(dat, aes(x=treatment, y=outcome)) +
        geom_boxplot(fill=thefill, outlier.size=0) +
        geom_errorbar(data=dats, aes(x=as.numeric(treatment) + 0.45, y=NULL, ymin=mean - sd, ymax=mean + sd),
                      width=0.15, position=position_identity(width=0.5), colour="gray50") +
        geom_point(data=dats, aes(x=as.numeric(treatment) + 0.45, y=mean), shape=3, colour="gray50") +
        geom_text(data=dats, aes(y=Inf, label=n), size=nsize, vjust=2) +
        pnegtext +
        ppostext +
        xlab(xlabel) +
        ylab(ylabel) +
        ylim(min(min(dat$outcome), dats$mean - dats$sd), max(max(dat$outcome), dats$mean + dats$sd))        
      if(white==F){
        if(printN==T){
          print(theplot + geom_text(data=dats, aes(y=-Inf, label=N), size=nsize, vjust=-2) + thetheme)
        }else{
          print(theplot + thetheme)
        }
      }else{
        if(printN==T){
          print(theplot + geom_text(data=dats, aes(y=-Inf, label=N), size=nsize, vjust=-2) + theme_bw() + thetheme)
        }else{
          print(theplot + theme_bw() + thetheme)
        }
      }
      
    }
    
  }
  
}