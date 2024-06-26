% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/decision_tree_exposure.R
\name{decision_tree_exposure}
\alias{decision_tree_exposure}
\title{Poisson Decision Trees with Exposures}
\usage{
decision_tree_exposure(
  mode = "regression",
  engine = "rpart_exposure",
  cost_complexity = NULL,
  tree_depth = NULL,
  min_n = NULL
)
}
\arguments{
\item{mode}{A single character string for the type of model. The only
possible value for this model is "regression"}

\item{engine}{A single character string specifying what computational engine
to use for fitting.}

\item{cost_complexity}{A positive number for the the cost/complexity
parameter (a.k.a. \code{Cp}) used by CART models (specific engines only).}

\item{tree_depth}{An integer for maximum depth of the tree.}

\item{min_n}{An integer for the minimum number of data points
in a node that are required for the node to be split further.}
}
\value{
A model specification object with the classes
\code{decision_tree_exposure} and \code{model_spec}.
}
\description{
\code{decision_tree_exposure()} defines a Poisson decision tree model with
weighted exposures (observation times).
}
\details{
This function is similar to \code{\link[parsnip:decision_tree]{parsnip::decision_tree()}} except that
specification of an exposure column is required.
}
\examples{
parsnip::show_model_info("decision_tree_exposure")

decision_tree_exposure()

}
\seealso{
\code{\link[parsnip:decision_tree]{parsnip::decision_tree()}}
}
