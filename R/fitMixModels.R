
#' Helper function to assing initial cluster membership to saccades.
#'
#' @param Saccades Vector of saccade duration values
#' @param k Number of clusters
#'
#' @return returns vector of factors of initial cluster membership.
clusterMembership <- function(Saccades, k) {
  #Find median like values that splits the saccades in k groups.
  splitValues <- sort(Saccades)[length(Saccades)/k * c(1:(k-1))]
  output <- vector(mode = 'integer', length = length(Saccades))

  #Assign saccades to a cluster based on duration
  for(i in 1:length(splitValues)){
    output[Saccades > splitValues[i]] <- i
  }
  return(as.factor(output))
}

#' Helper function to assing cluster membership probabilities to saccades.
#'
#' @param Saccades Vector of saccade duration values
#' @param k Number of clusters
#'
#' @return Matrix with k columns of probabilites

clusterProbabilities <- function(Saccades, k){
  clusterVector <- clusterSplit(Saccades, k)
  if(k == 1){
    prop <- 1
    clusterMatrix <- matrix(.1,ncol = k, nrow = length(Saccades))
  } else if(k == 2){
    prop <- .6
    clusterMatrix <- matrix(.4,ncol = k, nrow = length(Saccades))
  } else if(k == 3){
    prop <- .4
    clusterMatrix <- matrix(.3,ncol = k, nrow = length(Saccades))
  } else if(k == 4){
    prop <- .34
    clusterMatrix <- matrix(.22,ncol = k, nrow = length(Saccades))
  } else if(k == 5){
    prop <- .20
    clusterMatrix <- matrix(.2,ncol = k, nrow = length(Saccades))
  }

  for(i in 1:k){
    clusterMatrix[clusterVector == (i -1),i] <- prop
  }
  return(clusterMatrix)
}


#' Function that uses the flexmix package to fit the mixture models.
#'
#' @param Saccades A dataframe contaning the saccades duration and participant labels. Columnames must be Duration and PP respectively.
#' @param type Fit mixture models on the group as a whole or on the individual participants. Must be 'Group' or 'Individual'.
#' @param cluster Initial cluster assigments. Can be random, assignment into membership or probabilites.
#'
#' @return Returns a list containg all the fitted mixture models.
#' @import flexmix
#' @export
#'
fitMixModels <- function(Saccades,
                         type = c('Group', 'Individual'),
                         cluster = c('Random', 'Membership', 'Propabilities')) {
  type <- match.arg(type)
  cluster <- match.arg(cluster)

  if(type == 'Group'){
    fit <- list(gamma = vector(mode = 'list', length = 5),
                   lnorm = vector(mode = 'list', length = 5),
                   weibull = vector(mode = 'list', length = 5))
    distribution <- c('gamma', 'lnorm', 'weibull')
    for(k in 1:5){

      if(cluster == 'Random'){
        clus <- NULL
      } else if(cluster == 'Membership'){
        clus <- clusterMembership(Saccades$Duration, k)
      } else if(cluster == 'Propabilities'){
        clus <- clusterProbabilities(Saccades$Duration, k)
      }

      for(dis in c('gamma', 'lnorm', 'weibull')){
        fit[[dis]][[k]] <- flexmix(Duration ~ 1, data = Saccades,
                                      cluster = clus,
                                      control = list(iter.max = 1000, nrep = 10),
                                      model = FLXMCdist1(dist = dis))
      }
    }
  } else if(type == 'Group') {
    PP <- unique(Saccades$PP)
    fit <- vector(mode = 'list', length = length(PP))
    names(fit) <- PP

    for(pp in PP){
      print(pp)
      fit[[pp]] <- list(gamma = vector(mode = 'list', length = 5),
                        lnorm = vector(mode = 'list', length = 5),
                        weibull = vector(mode = 'list', length = 5))
      for(k in 1:5){

        if(cluster == 'Random'){
          clus <- NULL
        } else if(cluster == 'Membership'){
          clus <- clusterMembership(Saccades$Duration[Saccades$PP == pp], k)
        } else if(cluster == 'Propabilities'){
          clus <- clusterProbabilities(Saccades$Duration[Saccades$PP == pp], k)
        }

        for(dis in distribution){
          fit[[pp]][[dis]][[k]] <- flexmix(Duration ~ 1, data = Saccades[Saccades$PP == pp,],
                                           cluster = clus,
                                           control = list(iter.max = 1000000, nrep = 5,
                                                          minprior = 0.05),
                                           model = FLXMCdist1(dist = dis))
        }
      }
    }

  }
  return(fit)
}


