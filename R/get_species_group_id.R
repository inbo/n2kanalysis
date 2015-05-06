#' Get the species group id of a n2kModel object
#' @param x the n2kModel object
#' @return the species group id of the object
#' @name get_species_group_id
#' @rdname get_species_group_id
#' @exportMethod get_species_group_id
#' @docType methods
#' @importFrom methods setGeneric
#' @include n2kModel_class.R
setGeneric(
  name = "get_species_group_id", 
  def = function(x){
    standard.generic("get_species_group_id")
  }
)

#' @rdname get_species_group_id
#' @aliases get_species_group_id,n2kModel-methods
#' @importFrom methods setMethod
setMethod(
  f = "get_species_group_id",
  signature = signature(x = "n2kModel"),
  definition = function(x){
    return(x@SpeciesGroupID)
  }
)
