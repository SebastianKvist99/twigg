#' Step 5: build the GLLRM graph from screening results
#'
#' Constructs the graph implied by the item-screening procedure after genuine
#' local dependence, genuine DIF, and score-covariate associations have been
#' identified. The function always returns node and edge tables. If the
#' \pkg{igraph} package is installed, it also returns an \code{igraph} object.
#'
#' The graph contains item nodes, exogenous covariate nodes, and optionally a
#' score node. Edges are added for:
#' \itemize{
#'   \item genuine local dependence between item pairs,
#'   \item genuine DIF between items and exogenous covariates,
#'   \item Step 4 score-covariate associations retained after backwards
#'     elimination.
#' }
#'
#' @param items Character vector naming the item response variables.
#' @param covariates Character vector naming the exogenous covariates.
#' @param ld Output from \code{\link{genuine_LD}} or a data frame with genuine
#'   LD rows. If a list is supplied, \code{ld$genuine_ld} is used.
#' @param dif Output from \code{\link{combine_step3bc}} or a data frame with DIF
#'   rows. If a list is supplied, \code{dif$table} is used.
#' @param step4 Output from \code{\link{step4_structure_screen}} or a data frame
#'   containing retained score-covariate associations.
#' @param score Optional alias for \code{step4}, retained for convenient use
#'   with graph terminology.
#' @param score_node Character string naming the score node.
#' @param include_score_node Logical. If \code{TRUE}, include the score node and
#'   Step 4 score-covariate edges when \code{step4} or \code{score} is supplied.
#' @param directed Logical passed to \code{igraph::graph_from_data_frame} when
#'   \pkg{igraph} is installed. The default is \code{FALSE}, matching the usual
#'   GLLRM graph interpretation.
#'
#' @returns An object of class \code{"gllrm_graph"}, a list containing:
#' \describe{
#'   \item{nodes}{Node table with node names and types.}
#'   \item{edges}{Unified edge table.}
#'   \item{ld_edges}{LD-only edge table.}
#'   \item{dif_edges}{DIF-only edge table.}
#'   \item{score_edges}{Step 4 score-covariate edge table.}
#'   \item{graph}{An \code{igraph} object if \pkg{igraph} is installed,
#'     otherwise \code{NULL}.}
#'   \item{directed}{Whether the graph was requested as directed.}
#'   \item{call}{The matched function call.}
#' }
#'
#' @export
#'
#' @examples
#' ld <- list(genuine_ld = data.frame(
#'   item1 = "item1",
#'   item2 = "item2",
#'   gamma_cond_Ri = 0.35,
#'   gamma_cond_Rj = 0.40,
#'   arithmetic_mean_gamma = 0.375,
#'   weighted_partial_gamma = 0.38
#' ))
#'
#' dif <- list(table = data.frame(
#'   item = "item2",
#'   DIF_source = "sex",
#'   gamma_1 = 0.25,
#'   p_value_1 = 0.01,
#'   conditioned_on_1 = "Score",
#'   gamma_2 = 0.30,
#'   p_value_2 = 0.02,
#'   conditioned_on_2 = "Score",
#'   conclusion = "DIF"
#' ))
#'
#' build_gllrm_graph(
#'   items = c("item1", "item2"),
#'   covariates = "sex",
#'   ld = ld,
#'   dif = dif
#' )
build_gllrm_graph <- function(items, covariates,
                              ld = NULL,
                              dif = NULL,
                              step4 = NULL,
                              score = NULL,
                              score_node = "Score",
                              include_score_node = TRUE,
                              directed = FALSE) {
  call <- match.call()

  step5_check_names(items, "items")
  step5_check_names(covariates, "covariates")
  step5_check_names(score_node, "score_node")
  if (length(score_node) != 1) {
    stop("'score_node' must be a single character string", call. = FALSE)
  }

  if (!is.logical(include_score_node) || length(include_score_node) != 1 ||
      is.na(include_score_node)) {
    stop("'include_score_node' must be TRUE or FALSE", call. = FALSE)
  }
  if (!is.logical(directed) || length(directed) != 1 || is.na(directed)) {
    stop("'directed' must be TRUE or FALSE", call. = FALSE)
  }
  if (!is.null(step4) && !is.null(score)) {
    stop("Use either 'step4' or 'score', not both", call. = FALSE)
  }
  if (is.null(step4)) step4 <- score

  ld_edges <- step5_ld_edges(ld, items)
  dif_edges <- step5_dif_edges(dif, items, covariates)
  score_edges <- step5_score_edges(
    step4 = step4,
    covariates = covariates,
    score_node = score_node,
    include_score_node = include_score_node
  )

  edges <- step5_bind_edges(list(ld_edges, dif_edges, score_edges))
  if (nrow(edges) > 0) {
    edges$edge_id <- seq_len(nrow(edges))
    edges <- edges[, c("from", "to", "edge_id",
                       setdiff(names(edges), c("from", "to", "edge_id"))),
                   drop = FALSE]
  } else {
    edges$edge_id <- integer(0)
    edges <- edges[, c("from", "to", "edge_id",
                       setdiff(names(edges), c("from", "to", "edge_id"))),
                   drop = FALSE]
  }

  nodes <- step5_node_table(
    items = items,
    covariates = covariates,
    score_node = score_node,
    include_score_node = include_score_node
  )

  graph <- NULL
  if (requireNamespace("igraph", quietly = TRUE)) {
    graph <- igraph::graph_from_data_frame(
      d = edges,
      directed = directed,
      vertices = nodes
    )
  }

  out <- list(
    nodes = nodes,
    edges = edges,
    ld_edges = ld_edges,
    dif_edges = dif_edges,
    score_edges = score_edges,
    graph = graph,
    directed = directed,
    call = call
  )

  structure(out, class = "gllrm_graph")
}

#' @param ... Arguments passed to \code{build_gllrm_graph()}.
#' @rdname build_gllrm_graph
#' @export
step5_build_graph <- function(...) {
  build_gllrm_graph(...)
}

step5_check_names <- function(x, arg) {
  if (!is.character(x) || length(x) == 0 || anyNA(x) || any(x == "")) {
    stop("'", arg, "' must be a non-empty character vector", call. = FALSE)
  }
  invisible(TRUE)
}

step5_empty_edges <- function() {
  data.frame(
    from = character(0),
    to = character(0),
    edge_type = character(0),
    source_step = character(0),
    gamma = numeric(0),
    p_value = numeric(0),
    adjusted_p_value = numeric(0),
    conditioned_on = character(0),
    gamma_cond_Ri = numeric(0),
    gamma_cond_Rj = numeric(0),
    arithmetic_mean_gamma = numeric(0),
    weighted_partial_gamma = numeric(0),
    gamma_1 = numeric(0),
    conditioned_on_1 = character(0),
    p_value_1 = numeric(0),
    gamma_2 = numeric(0),
    conditioned_on_2 = character(0),
    p_value_2 = numeric(0),
    stringsAsFactors = FALSE
  )
}

step5_add_missing_edge_cols <- function(x) {
  empty <- step5_empty_edges()
  for (nm in names(empty)) {
    if (!nm %in% names(x)) {
      x[[nm]] <- empty[[nm]][NA_integer_]
    }
  }
  x[, names(empty), drop = FALSE]
}

step5_bind_edges <- function(edge_list) {
  edge_list <- lapply(edge_list, step5_add_missing_edge_cols)
  edge_list <- edge_list[vapply(edge_list, nrow, integer(1)) > 0]

  if (length(edge_list) == 0) {
    return(step5_empty_edges())
  }

  out <- do.call(rbind, edge_list)
  row.names(out) <- NULL
  out
}

step5_as_data_frame <- function(x, arg) {
  out <- tryCatch(
    as_screening_data_frame(x),
    error = function(e) {
      stop(
        "'", arg, "' must be a data frame or an output object with a data ",
        "frame component",
        call. = FALSE
      )
    }
  )
  out
}

step5_require_columns <- function(x, cols, arg) {
  missing <- setdiff(cols, names(x))
  if (length(missing) > 0) {
    stop(
      "'", arg, "' is missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

step5_numeric_col <- function(x, nm) {
  if (nm %in% names(x)) {
    return(as.numeric(x[[nm]]))
  }
  rep(NA_real_, nrow(x))
}

step5_character_col <- function(x, nm) {
  if (nm %in% names(x)) {
    return(as.character(x[[nm]]))
  }
  rep(NA_character_, nrow(x))
}

step5_row_mean <- function(a, b) {
  out <- rowMeans(cbind(a, b), na.rm = TRUE)
  out[is.nan(out)] <- NA_real_
  out
}

step5_row_max <- function(a, b) {
  out <- apply(cbind(a, b), 1, function(z) {
    z <- z[!is.na(z)]
    if (length(z) == 0) return(NA_real_)
    max(z)
  })
  as.numeric(out)
}

step5_ld_edges <- function(ld, items) {
  if (is.null(ld)) return(step5_empty_edges())
  if (is.list(ld) && !is.data.frame(ld) && "genuine_ld" %in% names(ld)) {
    ld <- ld$genuine_ld
  }
  if (is.character(ld)) return(step5_empty_edges())

  ld <- step5_as_data_frame(ld, "ld")
  if (nrow(ld) == 0) return(step5_empty_edges())

  step5_require_columns(ld, c("item1", "item2"), "ld")

  missing_items <- setdiff(unique(c(ld$item1, ld$item2)), items)
  if (length(missing_items) > 0) {
    stop(
      "'ld' contains item(s) not listed in 'items': ",
      paste(missing_items, collapse = ", "),
      call. = FALSE
    )
  }

  weighted <- step5_numeric_col(ld, "weighted_partial_gamma")
  arithmetic <- step5_numeric_col(ld, "arithmetic_mean_gamma")
  gamma <- ifelse(is.na(weighted), arithmetic, weighted)

  data.frame(
    from = as.character(ld$item1),
    to = as.character(ld$item2),
    edge_type = "local_dependence",
    source_step = "step3a",
    gamma = gamma,
    p_value = NA_real_,
    adjusted_p_value = NA_real_,
    conditioned_on = NA_character_,
    gamma_cond_Ri = step5_numeric_col(ld, "gamma_cond_Ri"),
    gamma_cond_Rj = step5_numeric_col(ld, "gamma_cond_Rj"),
    arithmetic_mean_gamma = arithmetic,
    weighted_partial_gamma = weighted,
    stringsAsFactors = FALSE
  )
}

step5_dif_edges <- function(dif, items, covariates) {
  if (is.null(dif)) return(step5_empty_edges())
  if (is.list(dif) && !is.data.frame(dif) && "table" %in% names(dif)) {
    dif <- dif$table
  }
  if (is.character(dif)) return(step5_empty_edges())

  dif <- step5_as_data_frame(dif, "dif")
  if (nrow(dif) == 0) return(step5_empty_edges())

  step5_require_columns(dif, c("item", "DIF_source"), "dif")

  if ("conclusion" %in% names(dif)) {
    dif <- dif[dif$conclusion == "DIF", , drop = FALSE]
  }
  if (nrow(dif) == 0) return(step5_empty_edges())

  missing_items <- setdiff(unique(dif$item), items)
  if (length(missing_items) > 0) {
    stop(
      "'dif' contains item(s) not listed in 'items': ",
      paste(missing_items, collapse = ", "),
      call. = FALSE
    )
  }

  missing_covariates <- setdiff(unique(dif$DIF_source), covariates)
  if (length(missing_covariates) > 0) {
    stop(
      "'dif' contains covariate(s) not listed in 'covariates': ",
      paste(missing_covariates, collapse = ", "),
      call. = FALSE
    )
  }

  gamma_1 <- step5_numeric_col(dif, "gamma_1")
  gamma_2 <- step5_numeric_col(dif, "gamma_2")
  p_value_1 <- step5_numeric_col(dif, "p_value_1")
  p_value_2 <- step5_numeric_col(dif, "p_value_2")

  data.frame(
    from = as.character(dif$item),
    to = as.character(dif$DIF_source),
    edge_type = "DIF",
    source_step = "step3bc",
    gamma = step5_row_mean(gamma_1, gamma_2),
    p_value = step5_row_max(p_value_1, p_value_2),
    adjusted_p_value = NA_real_,
    conditioned_on = NA_character_,
    gamma_1 = gamma_1,
    conditioned_on_1 = step5_character_col(dif, "conditioned_on_1"),
    p_value_1 = p_value_1,
    gamma_2 = gamma_2,
    conditioned_on_2 = step5_character_col(dif, "conditioned_on_2"),
    p_value_2 = p_value_2,
    stringsAsFactors = FALSE
  )
}

step5_score_edges <- function(step4, covariates, score_node,
                              include_score_node) {
  if (is.null(step4) || !include_score_node) return(step5_empty_edges())

  retained <- NULL
  if (is.list(step4) && !is.data.frame(step4) &&
      "retained_covariates" %in% names(step4)) {
    retained <- step4$retained_covariates
  }

  if (is.list(step4) && !is.data.frame(step4) &&
      "criterion_validity" %in% names(step4)) {
    step4 <- step4$criterion_validity
  } else if (!is.null(retained)) {
    step4 <- data.frame(covariate = retained, stringsAsFactors = FALSE)
  }

  if (is.character(step4)) return(step5_empty_edges())

  step4 <- step5_as_data_frame(step4, "step4")
  if (nrow(step4) == 0) return(step5_empty_edges())

  step5_require_columns(step4, "covariate", "step4")

  if ("supports_criterion_validity" %in% names(step4)) {
    keep <- !is.na(step4$supports_criterion_validity) &
      step4$supports_criterion_validity
    step4 <- step4[keep, , drop = FALSE]
  } else if (!is.null(retained)) {
    step4 <- step4[step4$covariate %in% retained, , drop = FALSE]
  }
  if (nrow(step4) == 0) return(step5_empty_edges())

  missing_covariates <- setdiff(unique(step4$covariate), covariates)
  if (length(missing_covariates) > 0) {
    stop(
      "'step4' contains covariate(s) not listed in 'covariates': ",
      paste(missing_covariates, collapse = ", "),
      call. = FALSE
    )
  }

  data.frame(
    from = score_node,
    to = as.character(step4$covariate),
    edge_type = "score_association",
    source_step = "step4",
    gamma = step5_numeric_col(step4, "gamma"),
    p_value = step5_numeric_col(step4, "p_value"),
    adjusted_p_value = step5_numeric_col(step4, "adjusted_p_value"),
    conditioned_on = step5_character_col(step4, "conditioned_on"),
    stringsAsFactors = FALSE
  )
}

step5_node_table <- function(items, covariates, score_node,
                             include_score_node) {
  node_names <- unique(c(
    if (include_score_node) score_node else character(0),
    items,
    covariates
  ))

  node_type <- ifelse(
    node_names %in% items,
    "item",
    ifelse(node_names %in% covariates,
           "covariate",
           ifelse(node_names == score_node, "score", "other"))
  )

  data.frame(
    name = node_names,
    label = node_names,
    type = node_type,
    stringsAsFactors = FALSE
  )
}

#' @export
print.gllrm_graph <- function(x, ...) {
  cat("GLLRM Step 5 graph\n")
  cat("------------------\n")
  cat("Nodes: ", nrow(x$nodes), "\n", sep = "")
  cat("Edges: ", nrow(x$edges), "\n", sep = "")

  if (nrow(x$edges) > 0) {
    edge_counts <- table(x$edges$edge_type)
    for (nm in names(edge_counts)) {
      cat("  ", nm, ": ", unname(edge_counts[[nm]]), "\n", sep = "")
    }
  }

  cat("igraph object: ",
      ifelse(is.null(x$graph), "not available", "available"),
      "\n", sep = "")

  invisible(x)
}

#' @export
summary.gllrm_graph <- function(object, ...) {
  edge_counts <- if (nrow(object$edges) == 0) {
    data.frame(edge_type = character(0), n = integer(0))
  } else {
    counts <- as.data.frame(table(object$edges$edge_type),
                            stringsAsFactors = FALSE)
    names(counts) <- c("edge_type", "n")
    counts
  }

  out <- list(
    n_nodes = nrow(object$nodes),
    n_edges = nrow(object$edges),
    nodes = object$nodes,
    edges_by_type = edge_counts,
    edges = object$edges,
    has_igraph = !is.null(object$graph),
    directed = object$directed
  )

  class(out) <- "summary.gllrm_graph"
  out
}

#' @export
print.summary.gllrm_graph <- function(x, ...) {
  cat("Summary of GLLRM Step 5 graph\n")
  cat("-----------------------------\n")
  cat("Nodes: ", x$n_nodes, "\n", sep = "")
  cat("Edges: ", x$n_edges, "\n", sep = "")
  cat("Directed: ", x$directed, "\n", sep = "")

  if (nrow(x$edges_by_type) > 0) {
    cat("\nEdges by type:\n")
    print(x$edges_by_type, row.names = FALSE)
  }

  invisible(x)
}

#' Convert a Step 5 graph object to igraph
#'
#' @param x A \code{"gllrm_graph"} object.
#' @param ... Ignored.
#'
#' @returns An \code{igraph} object.
#' @export
as_igraph <- function(x, ...) {
  UseMethod("as_igraph")
}

#' @export
as_igraph.gllrm_graph <- function(x, ...) {
  if (!inherits(x, "gllrm_graph")) {
    stop("'x' must be a gllrm_graph object", call. = FALSE)
  }
  if (!is.null(x$graph)) return(x$graph)
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("The 'igraph' package must be installed to create an igraph object",
         call. = FALSE)
  }

  igraph::graph_from_data_frame(
    d = x$edges,
    directed = x$directed,
    vertices = x$nodes
  )
}
