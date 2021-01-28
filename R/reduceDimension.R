#' Title
#'
#' @param D.sub preprocessed matrix with top variable genes
#' @param dim.method dimension reduction method among tSNE and UMAP
#' @param nrProjections number of projections to be made by dimension reduction method
#'
#' @return
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach
#' @importFrom SingleCellExperiment SingleCellExperiment counts reducedDim
#' @importFrom scater runTSNE
#' @importFrom SummarizedExperiment assay
#' @export
#'
#' @examples
reduceDimension <- function(D.sub, method, nrProjections){

  myfunc <- function(x){

    if(method=="tSNE"){

      temp_sce <- SingleCellExperiment::SingleCellExperiment(assays = list(counts = D.sub))
      SummarizedExperiment::assay(temp_sce, "log2") <- log2(counts(temp_sce) + 1)
      temp_sce <- scater::runTSNE(temp_sce, ncomponents = 3, ntop=dim(temp_sce)[1], exprs_values = "log2", rand_seed = NULL) #perplexity is not used
      temp_proj=as.data.frame(SingleCellExperiment::reducedDim(temp_sce, "TSNE"))
    }

    else if(dim.method=="UMAP"){

      D.sub <- log2(D.sub + 1)
      temp_proj = umap::umap(t(D.sub), method="umap-learn", n_components=2)
      temp_proj = temp_proj$layout
    }

    return(temp_proj)
  }

  #Do pararell computing
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores-1)
  doParallel::registerDoParallel(cl)
  D.proj <- foreach(i=1:nrProjections) %dopar% myfunc(D.sub)
  stopCluster(cl)

  return(D.proj)


}
