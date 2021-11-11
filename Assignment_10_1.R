# Topic Sensitive Page Rank

library(miniCRAN)
pkgs <- "ggplot2"
pkgDep(pkgs, suggests=FALSE, enhances=FALSE, includeBasePkgs = TRUE)
pkgDep(pkgs, suggests=TRUE, enhances=FALSE, includeBasePkgs = TRUE)
plot(makeDepGraph(pkgs, includeBasePkgs=FALSE, suggests=FALSE, enhances=TRUE), 
     legendPosEdge = c(-1, 1), legendPosVertex = c(1, 1), vertex.size=20)


# page rank implementation\
library(miniCRAN)
library(igraph)
library(magrittr)
MRAN <- "http://mran.revolutionanalytics.com/snapshot/2015-04-01/"
pdb <- MRAN %>%
  contrib.url(type = "source") %>%
  available.packages(type="source", filters = NULL)
g <- pdb[, "Package"] %>%
  makeDepGraph(availPkgs = pdb, suggests=FALSE, enhances=TRUE, includeBasePkgs = FALSE)


pr <- g %>%
  page.rank(directed = FALSE) %>%
  use_series("vector") %>%
  sort(decreasing = TRUE) %>%
  as.matrix %>%
  set_colnames("page.rank")

head(pr, 20)
