\name{boxclust}
\alias{boxclust}
\title{
Enhanced Boxplots for Clustered Data
}
\description{
A function for drawing jittered boxplots with error bars using the ggplot2 framework. This is especially illustrative for toxicological data with substructures such as technical replicates, clustering, or subsampling.
}
\usage{
boxclust(data, outcome, treatment, cluster, covariate = NULL,
         xlabel = "Treatment", ylabel = "Outcome", option = "dotplot",
         legpos = "top", psize = 2.5, hjitter = 0, vlines = "none",
         pneg = NULL, ppos = NULL, pposneg = NULL, stars = FALSE,
         pvalsize=3, printN=TRUE, nsize=4, labelsize = 11,
         titlesize = 14, white = FALSE)
}
\arguments{
  \item{data}{A data frame.}
  \item{outcome}{A column in the data frame containing the outcome variable.}
  \item{treatment}{A column in the data frame containing the grouping factor.}
  \item{cluster}{A column in the data frame containing the cluster variable. If set to NULL, a jittered boxplot is drawn without discrimination of clusters.}
  \item{covariate}{A column in the data frame containing an (optional) categorical covariate (e.g., sex) to be distinguished by point shapes. Default is NULL. Ignored if option="none".}
  \item{xlabel}{Label of the horizontal axis.}
  \item{ylabel}{Label of the vertical axis.}
  \item{option}{How to distinguish clusters and color points. Choose among "dotplot" (default, draws Cleveland dot plots), "color" (points have cluster-specific colors), "uni" (all points are black), or "none" (no points are printed).}
  \item{legpos}{Position of the legend for the categorical covariate (if present) and the colors (if option="color"). Choose among "top" (default), "bottom", "left", "right", or "none".}
  \item{psize}{Size of the jittered points. Default is 2.5. Ignored if option="none".}
  \item{hjitter}{Amount of jittering in horizontal direction. Default is 0. Ignored if option="none".}
  \item{vlines}{Should vertical auxiliary lines be drawn for the dot plots? If so, should they be in the foreground or background (relative to the boxes)? Choose among "none" (default), "fg", or "bg". Ignored unless option="dotplot".}
  \item{pneg}{An optional vector of p-values from comparisons to a \bold{negative control} to be printed alongside the non-control treatments. Default is NULL. Please see details.}
  \item{ppos}{An optional vector of p-values from comparisons to a \bold{positive control} to be printed alongside the non-control treatments. Default is NULL. Please see details.}
  \item{pposneg}{An optional numeric p-value from a comparison between \bold{positive} and \bold{positive control} to be printed alongside the positive control treatments. Default is NULL. Please see details.}
  \item{stars}{Should significance stars ("***" for p < 0.001, "**" for p < 0.01, "*" for p < 0.05, and "n.s." otherwise) be plotted instead of p-values? Default is FALSE. Ignored if both pneg=NULL and ppos=NULL.}
  \item{pvalsize}{Size of the p-values or significance stars. Default is 3. Ignored if both pneg=NULL and ppos=NULL.}
  \item{printN}{Should the cluster sizes be printed below the boxes? Default is TRUE.}
  \item{nsize}{Size of the sample size annotations (n and N). Default is 4.}
  \item{labelsize}{Size of the axis labels. Default is 11.}
  \item{titlesize}{Size of the axis titles. Default is 14.}
  \item{white}{Should the background area be white rather than gray? Default is FALSE.}
}
\details{
Standard box-and-whisker plots are supplemented with error bars presenting sample means and standard deviations alongside the boxes. Sample sizes and numbers of clusters are printed for each sample. Single observations are sprinkled across the plot with some random noise added in horizontal direction ("jittering"). Cluster affiliations of the single observations can be designated via colors (which is difficult to tell apart with > 10 clusters) or by overlaying Cleveland dot plots.

If p-values are to be printed, they should reasonably result from some Dunnett-type test procedure (many-to-one comparisons of group means versus a common control) e.g., based on a linear mixed-effects model. The p-values must be in the same order the treatments appear in the plot. There are two basic requirements: a) if there is a negative control, it must be the leftmost group, and b) if there is a positive control, it must be the rightmost group. Whenever there is a positive control, there must also be a negative control. The comparison between positive and negative control is usually performed with a two-sample test. The p-values are automatically rounded to 3 digits at most, and p-values smaller than 0.001 are replaced by "p<0.001".
}
\value{
A graphical display.
}
%\references{}
\author{
Philip Pallmann \email{pallmann@biostat.uni-hannover.de}
}
\examples{
##### Using the ratpup data from the WWGbook package #####

library(WWGbook)
data(ratpup)
ratpup$treatment <- factor(ratpup$treatment, levels=c("Control", "Low", "High"))

##### Option 1a: No jittering at all #####

boxclust(data=ratpup, outcome="weight", treatment="treatment", cluster="litter",
         xlabel="", ylabel="Pup weight [g]", option="none")

##### Option 1b: Cluster-unspecific jittering #####

boxclust(data=ratpup, outcome="weight", treatment="treatment", cluster=NULL,
         xlabel="", ylabel="Pup weight [g]", option="color", hjitter=0.3, legpos="none")

##### Option 2: Distinguish clusters by colors #####

### Reduce the number of clusters (for illustration)
ratpup2 <- subset(ratpup, litter==1 | litter==4 | litter==13 | litter==17 | litter==22 | litter==24)

# Add some horizontal noise
boxclust(data=ratpup2, outcome="weight", treatment="treatment", cluster="litter",
         xlabel="", ylabel="Pup weight [g]", option="color", hjitter=0.2)

# Distinguish sexes by point shapes, move the legend to the right, enlargen axis labels and titles
boxclust(data=ratpup2, outcome="weight", treatment="treatment", cluster="litter", covariate="sex",
         xlabel="", ylabel="Pup weight [g]", option="color", legpos="right", hjitter=0.2,
         labelsize=15, titlesize=20)

##### Option 3: Distinguish clusters with Cleveland dot plots #####

# With default settings
boxclust(data=ratpup, outcome="weight", treatment="treatment", cluster="litter",
         xlabel="", ylabel="Pup weight [g]")

# With gentle horizontal jittering, faint auxiliary lines in the background,
# sexes distinguished by point shapes, slightly larger points, and the legend on the right
boxclust(data=ratpup, outcome="weight", treatment="treatment", cluster="litter", covariate="sex",
         xlabel="", ylabel="Pup weight [g]", legpos="right", psize=3, hjitter=0.01, vlines="bg")

# With gentle horizontal jittering, faint auxiliary lines in the foreground,
# sexes distinguished by point shapes, slightly smaller points, and white background
boxclust(data=ratpup, outcome="weight", treatment="treatment", cluster="litter", covariate="sex",
         xlabel="", ylabel="Pup weight [g]", psize=2, hjitter=0.01, vlines="fg", white=TRUE)

# Additionally with (fake) p-values from comparisons to a negative control
boxclust(data=ratpup, outcome="weight", treatment="treatment", cluster="litter", covariate="sex",
         xlabel="", ylabel="Pup weight [g]", psize=2, hjitter=0.01, vlines="fg",
         pneg=c(0.03, 0.0004), white=TRUE)
}
\keyword{~hplot}
