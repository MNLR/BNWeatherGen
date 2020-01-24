fixIntermediate <- function(names.distribution, structure.learning.args.list, whitelist){
  all.nodes <- as.vector(unlist(names.distribution))
  return(addtoBlacklist2( all.nodes, structure.learning.args.list)) # whitelist has precedence over the blacklist
}
