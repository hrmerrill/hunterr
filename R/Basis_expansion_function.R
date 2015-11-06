#' Construct basis expansion matrices.
#'
#' This function was created to avoid repeating it over and over in my research. The penalty for each marginal term is \eqn{\int \|f''(x)\|^2 dx}.
#'
#' @param df data frame for which to expand. Must already contain an intercept in column 1, and all covariates should be scaled between 0 and 1.
#' @param num.knots a vector containing the number of knots to use for each predictor.
#' @param bases a vector containing the bases to use for each expansion. Cubic (\code{cr}) and cyclic (\code{cc}) radial splines are supported.
#' @param interactions logical: should two-way interactions (tensor products) be computed?
#' @param knots.list optional list containing knots to use. Useful for predictions. If present, num.knots is ignored.
#' @return A list with components:
#' \itemize{
#'  \item{\code{C.full}}{ a matrix containing all basis expansions, with the intercept.}
#'  \item{\code{partition}}{ a numeric vector corresponding to group identification. \code{0} corresponds to the intercept.}
#'  \item{\code{nvar, pc, pz, px}}{ scalars giving the number of total predictors (including 2-way interactions as individual predictors, if \code{interactions == TRUE}),
#'  total dimension of \code{C.full}, and the dimensions of the linear and basis expansion terms, respectively.}
#'  \item{\code{knots.list}}{ a list of knots used for expansion. Useful for creating the same basis expansion on an independent data set (for, say, validation).}
#' }
#' @export

get_basis_expansion = function(df,
                               num.knots,
                               bases,
                               interactions,
                               knots.list = NULL){
  #library(cluster) #for clara
  library(mgcv) #for cyclical expansion

  #df must already contain the intercept in the first column
  n.predictors = ncol(df) - 1

  if(is.null(knots.list)){
    knots.list = list()
    for(i in 2:(n.predictors+1)){
      if(bases[i-1] == "cr"){
        knots.list[[i-1]] = quantile(df[[i]], probs=seq(0, 1, len=num.knots[i-1]))
      } else if(bases[i-1] == "cc"){
        knots.list[[i-1]] = quantile(c(0, df[[i]], 1), probs=seq(0, 1, len=num.knots[i-1]+2))
      } else stop("Bases must be cr or cc")
    }
  }

  # one dimensional penalty function to integrate
  s.ij = function(indi,indj,tknots,covar){
    ki = tknots[indi]
    kj = tknots[indj]

    36*abs((covar - ki)*(covar - kj))
  }
  integrand = seq(0,1,length=100) #covariates scaled to [0,1]

  # get penalty matrices
  pen.mats = list()
  for(j in 1:length(knots.list)){
    S.pen.cov = list() #list of penalty matrices to average
    for(i in 1:100){ #100 of them
      dims = num.knots[j]

      tmp.mat = matrix(nrow=dims,ncol=dims)
      for(first in 1:dims){
        for(second in 1:dims){
          tmp.mat[first,second] = s.ij(first,second,tknots=knots.list[[j]], covar=integrand[i])
        }
      }
      S.pen.cov[[i]] = tmp.mat
    }
    pen.mats[[j]] = Reduce('+', S.pen.cov)/100
  }

  #compute inverse sqrts
  OMEGA.mats = list()
  for(i in 1:length(knots.list)){
    OMEGA.mats[[i]] = backsolve(chol(pen.mats[[i]]), diag(num.knots[i]), transpose=T)
  }

  #get matrices
  Z.marg.list = list()
  for(i in 1:length(knots.list)){
    if(bases[i]!="cr" & bases[i]!="cc") {
      stop("Basis must be cr or cc")
    }
    if(bases[i] == "cr"){
      Z.marg.list[[i]] = abs(outer(df[[i+1]], knots.list[[i]], "-"))^3
      Z.marg.list[[i]] = Z.marg.list[[i]] %*% OMEGA.mats[[i]]
    } else if(bases[i] == "cc"){
      tmpx = tmpy = df[[i+1]]
      tmpdf = data.frame("Y" = tmpy, "X" = tmpx)
      fit.gam = mgcv::gam(Y~-1+s(X, bs="cc", k=num.knots[i]+2),
                          data=tmpdf,
                          knots=list("X" = knots.list[[i]]))
      Z.marg.list[[i]] = mgcv::predict(fit.gam, type="lpmatrix")
      PenMat = fit.gam$smooth[[1]]$S[[1]]
      #Z.marg.list[[i]] = Z.marg.list[[i]] %*% backsolve(chol(PenMat), diag(ncol(PenMat)), transpose=T)
    }
    colnames(Z.marg.list[[i]]) = rep(paste('smooth',i,sep=' '), dim(Z.marg.list[[i]])[2])
  }

  #intercept, linear terms, interaction terms
  which.x.belongs = c(2:(n.predictors+1))
  which.x.belongs = which.x.belongs[bases == "cr"]
  X.full = as.matrix(cbind(1, df[,which.x.belongs]))
  Z.full = cbind(Z.marg.list[[1]], Z.marg.list[[2]])
  for(i in 3:length(Z.marg.list)){
    Z.full = cbind(Z.full, Z.marg.list[[i]])
  }

  partition = c(0, (1:n.predictors)[bases=="cr"], rep(1:n.predictors, times=num.knots))
  nvar=n.predictors

  if(interactions){
    #interactions (tensor products)
    Z.prod.list = list() #basis expansion
    X.prod = list() #marginal terms
    P.list = list() #projection matrices into null space of marginals

    for(i in 1:(length(knots.list)-1)){
      Z.prod.list[[i]] = list()
      X.prod[[i]] = list()
      P.list[[i]] = list()
      dim1 = ncol(Z.marg.list[[i]])
      is.cr1 = NULL
      if(bases[i] == "cr") {dim1 = dim1 + 1; is.cr1 = 1}

      for(j in (i+1):length(knots.list)){
        dim2 = ncol(Z.marg.list[[j]])
        is.cr2 = NULL
        if(bases[j] == "cr") {dim2 = dim2 + 1; is.cr2 = 1}

        Z.prod.list[[i]][[j]] = matrix(ncol = (dim1)*(dim2), nrow = dim(Z.marg.list[[1]])[1])
        X.prod[[i]][[j]] = cbind(1, is.cr1*df[[i+1]], is.cr2*df[[j+1]], Z.marg.list[[i]], Z.marg.list[[j]])
        P.list[[i]][[j]] = X.prod[[i]][[j]] %*%
          chol2inv(chol(crossprod(X.prod[[i]][[j]]) + diag(rep(1e-09, ncol(X.prod[[i]][[j]]))))) %*% t(X.prod[[i]][[j]])
        #chol2inv(chol(crossprod(X.prod[[i]][[j]]))) %*% t(X.prod[[i]][[j]])

        for(k in 1:dim(Z.marg.list[[1]])[1]){
          Z.prod.list[[i]][[j]][k,] = cbind(is.cr1*df[[i+1]], Z.marg.list[[i]])[k,] %x%
            cbind(is.cr2*df[[j+1]], Z.marg.list[[j]])[k,]
        }
        #do projections here
        Z.prod.list[[i]][[j]] = (diag(dim(Z.marg.list[[1]])[1]) - P.list[[i]][[j]]) %*%
          Z.prod.list[[i]][[j]]
        colnames(Z.prod.list[[i]][[j]]) = rep(paste('int',i,j,sep=' '), dim(Z.prod.list[[i]][[j]])[2])

      }
    }

    #unlist a level:
    Z.prods = unlist(Z.prod.list, recursive=F)

    for(i in 1:length(Z.prods)){
      Z.full = cbind(Z.full, Z.prods[[i]])
    }

    num.knots.extra = ifelse(bases == "cr", num.knots+1, num.knots)
    part.extra = outer(num.knots.extra, num.knots.extra) #number of knots for interactions
    part.extra = part.extra*upper.tri(part.extra) #remove repeats
    part.extra = ifelse(part.extra == 0, NA, part.extra) #remove zeros
    part.extra = c(t(part.extra)) #make it a vector, by row
    part.extra = part.extra[!is.na(part.extra)] #remove NAs.

    partition = c(partition, rep(1:choose(n.predictors,2) + n.predictors,
                                 times=part.extra))
    nvar = nvar + choose(n.predictors,2)
  }
  C.full = as.matrix(cbind(X.full,Z.full))

  #center, for effects about the mean
  for(i in 2:dim(C.full)[2]){
    C.full[,i] = C.full[,i] - mean(C.full[,i])
  }

  out = list("C.full"=C.full,
             "partition" = partition,
             "nvar" = nvar,
             "pc" = dim(C.full)[2],
             "pz" = dim(Z.full)[2],
             "px" = dim(X.full)[2],
             "knots" = knots.list) #for use with predictions

  return(out)
}









