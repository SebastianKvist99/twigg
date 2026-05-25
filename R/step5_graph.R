#' Step 5: build the GLLRM graph from screening results
#'
#' Constructs the graph implied by the item-screening procedure after genuine
#' local dependence, genuine DIF, and score-covariate associations have been
#' identified. The function returns node and edge tables. Detailed statistical
#' evidence for each edge type is kept in the component-specific edge tables.
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
#'
#' @returns An object of class \code{"gllrm_graph"}, a list containing:
#' \describe{
#'   \item{nodes}{Node table with node names and types.}
#'   \item{edges}{Minimal graph topology table with \code{from}, \code{to},
#'     and \code{edge_type}.}
#'   \item{ld_edges}{LD-only edge table with Step 3a evidence.}
#'   \item{dif_edges}{DIF-only edge table with Step 3b/3c evidence.}
#'   \item{score_edges}{Step 4 score-covariate edge table with criterion
#'     validity evidence.}
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
                              include_score_node = TRUE) {
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
  edges <- step5_topology_edges(edges)

  nodes <- step5_node_table(
    items = items,
    covariates = covariates,
    score_node = score_node,
    include_score_node = include_score_node
  )

  out <- list(
    nodes = nodes,
    edges = edges,
    ld_edges = ld_edges,
    dif_edges = dif_edges,
    score_edges = score_edges,
    call = call
  )

  structure(out, class = "gllrm_graph")
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

step5_topology_edges <- function(edges) {
  out <- edges[, c("from", "to", "edge_type"), drop = FALSE]
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

step5_empty_mixed_edges <- function() {
  data.frame(
    from = character(0),
    to = character(0),
    edge_type = character(0),
    directed = logical(0),
    stringsAsFactors = FALSE
  )
}

step5_mixed_edges <- function(from, to, edge_type, directed) {
  if (length(from) == 0 || length(to) == 0) {
    return(step5_empty_mixed_edges())
  }

  data.frame(
    from = as.character(from),
    to = as.character(to),
    edge_type = rep(edge_type, length.out = length(from)),
    directed = rep(directed, length.out = length(from)),
    stringsAsFactors = FALSE
  )
}

step5_bind_mixed_edges <- function(edge_list) {
  edge_list <- edge_list[vapply(edge_list, nrow, integer(1)) > 0]
  if (length(edge_list) == 0) return(step5_empty_mixed_edges())

  out <- do.call(rbind, edge_list)
  row.names(out) <- NULL
  out
}

step5_unique_mixed_edges <- function(edges) {
  if (nrow(edges) == 0) return(edges)

  key_from <- ifelse(edges$directed, edges$from, pmin(edges$from, edges$to))
  key_to <- ifelse(edges$directed, edges$to, pmax(edges$from, edges$to))
  key <- paste(edges$directed, key_from, key_to, sep = "\r")
  key_levels <- unique(key)

  out <- do.call(rbind, lapply(key_levels, function(k) {
    idx <- which(key == k)
    first <- idx[1]
    data.frame(
      from = key_from[first],
      to = key_to[first],
      edge_type = paste(unique(edges$edge_type[idx]), collapse = "+"),
      directed = edges$directed[first],
      stringsAsFactors = FALSE
    )
  }))
  row.names(out) <- NULL
  out
}

step5_pair_edges <- function(nodes, edge_type) {
  if (length(nodes) < 2) return(step5_empty_mixed_edges())
  pairs <- utils::combn(nodes, 2)
  step5_mixed_edges(pairs[1, ], pairs[2, ], edge_type, FALSE)
}

step5_validate_graph_object <- function(x) {
  if (!inherits(x, "gllrm_graph")) {
    stop("'x' must be a gllrm_graph object", call. = FALSE)
  }
  invisible(TRUE)
}

step5_graph_nodes_of_type <- function(x, type) {
  as.character(x$nodes$name[x$nodes$type == type])
}

step5_covariate_association_edges <- function(covariate_edges, covariates) {
  if (is.null(covariate_edges)) return(step5_empty_mixed_edges())

  covariate_edges <- step5_as_data_frame(covariate_edges, "covariate_edges")
  if (nrow(covariate_edges) == 0) return(step5_empty_mixed_edges())

  step5_require_columns(covariate_edges, c("from", "to"), "covariate_edges")

  missing_covariates <- setdiff(
    unique(c(covariate_edges$from, covariate_edges$to)),
    covariates
  )
  if (length(missing_covariates) > 0) {
    stop(
      "'covariate_edges' contains covariate(s) not listed in the graph: ",
      paste(missing_covariates, collapse = ", "),
      call. = FALSE
    )
  }

  edge_type <- if ("edge_type" %in% names(covariate_edges)) {
    as.character(covariate_edges$edge_type)
  } else {
    rep("covariate_association", nrow(covariate_edges))
  }

  step5_mixed_edges(
    from = covariate_edges$from,
    to = covariate_edges$to,
    edge_type = edge_type,
    directed = FALSE
  )
}

step5_irt_node_table <- function(items, covariates, theta) {
  node_names <- unique(c(theta, items, covariates))
  data.frame(
    name = node_names,
    label = node_names,
    type = ifelse(node_names == theta,
                  "latent",
                  ifelse(node_names %in% items, "item", "covariate")),
    stringsAsFactors = FALSE
  )
}

step5_moral_node_table <- function(items, covariates, score_node) {
  node_names <- unique(c(score_node, items, covariates))
  data.frame(
    name = node_names,
    label = node_names,
    type = ifelse(node_names == score_node,
                  "score",
                  ifelse(node_names %in% items, "item", "covariate")),
    stringsAsFactors = FALSE
  )
}

#' Build the IRT graph implied by a Step 5 graph
#'
#' Converts the Step 5 evidence graph into an IRT-style chain graph. The latent
#' variable points to all items, retained score-covariate associations point to
#' the latent variable, DIF edges point from covariates to items, and local
#' dependence edges remain undirected between items.
#'
#' @param x A \code{"gllrm_graph"} object returned by
#'   \code{\link{build_gllrm_graph}}.
#' @param theta Character string naming the latent variable node.
#' @param covariate_edges Optional data frame with columns \code{from} and
#'   \code{to} describing associations among exogenous covariates.
#'
#' @returns An object of class \code{"gllrm_irt_graph"}, a list containing
#'   plotting-ready \code{nodes} and \code{edges} tables.
#' @export
build_irt_graph <- function(x, theta = "theta", covariate_edges = NULL) {
  call <- match.call()
  step5_validate_graph_object(x)
  step5_check_names(theta, "theta")
  if (length(theta) != 1) {
    stop("'theta' must be a single character string", call. = FALSE)
  }

  items <- step5_graph_nodes_of_type(x, "item")
  covariates <- step5_graph_nodes_of_type(x, "covariate")
  if (theta %in% c(items, covariates)) {
    stop("'theta' must not duplicate an item or covariate name",
         call. = FALSE)
  }

  measurement_edges <- step5_mixed_edges(
    from = theta,
    to = items,
    edge_type = "measurement",
    directed = TRUE
  )
  score_edges <- step5_mixed_edges(
    from = x$score_edges$to,
    to = theta,
    edge_type = "score_association",
    directed = TRUE
  )
  dif_edges <- step5_mixed_edges(
    from = x$dif_edges$to,
    to = x$dif_edges$from,
    edge_type = "DIF",
    directed = TRUE
  )
  ld_edges <- step5_mixed_edges(
    from = x$ld_edges$from,
    to = x$ld_edges$to,
    edge_type = "local_dependence",
    directed = FALSE
  )
  covariate_edges <- step5_covariate_association_edges(
    covariate_edges,
    covariates
  )

  edges <- step5_bind_mixed_edges(list(
    measurement_edges,
    score_edges,
    dif_edges,
    ld_edges,
    covariate_edges
  ))
  edges <- step5_unique_mixed_edges(edges)

  out <- list(
    nodes = step5_irt_node_table(items, covariates, theta),
    edges = edges,
    measurement_edges = measurement_edges,
    score_edges = score_edges,
    dif_edges = dif_edges,
    ld_edges = ld_edges,
    covariate_edges = covariate_edges,
    theta = theta,
    call = call
  )

  structure(out, class = c("gllrm_irt_graph", "gllrm_graph"))
}

step5_moral_parent_edges <- function(score_node, score_covariates,
                                     dif_edges) {
  score_parent_edges <- step5_pair_edges(
    score_covariates,
    "moralized_parent"
  )

  if (nrow(dif_edges) == 0) {
    return(score_parent_edges)
  }

  dif_score_edges <- step5_mixed_edges(
    from = score_node,
    to = dif_edges$to,
    edge_type = "moralized_parent",
    directed = FALSE
  )

  item_sources <- split(dif_edges$to, dif_edges$from)
  dif_source_edges <- step5_bind_mixed_edges(lapply(item_sources, function(z) {
    step5_pair_edges(unique(z), "moralized_parent")
  }))

  step5_bind_mixed_edges(list(
    score_parent_edges,
    dif_score_edges,
    dif_source_edges
  ))
}

#' Build the moralized marginal graph implied by a Step 5 graph
#'
#' Constructs the Figure 6b-style undirected graph used to read off minimal
#' global Markov property hypotheses. The latent variable is replaced by a
#' total score node, item-score moralization adds item-item edges, and directed
#' parent structures are moralized by adding undirected edges between common
#' parents.
#'
#' @param x A \code{"gllrm_graph"} object returned by
#'   \code{\link{build_gllrm_graph}}.
#' @param score_node Character string naming the total score node.
#' @param covariate_edges Optional data frame with columns \code{from} and
#'   \code{to} describing associations among exogenous covariates.
#'
#' @returns An object of class \code{"gllrm_moral_graph"}, a list containing
#'   plotting-ready \code{nodes} and \code{edges} tables. All edges are
#'   undirected.
#' @export
build_moralized_graph <- function(x, score_node = "#",
                                  covariate_edges = NULL) {
  call <- match.call()
  step5_validate_graph_object(x)
  step5_check_names(score_node, "score_node")
  if (length(score_node) != 1) {
    stop("'score_node' must be a single character string", call. = FALSE)
  }

  items <- step5_graph_nodes_of_type(x, "item")
  covariates <- step5_graph_nodes_of_type(x, "covariate")
  if (score_node %in% c(items, covariates)) {
    stop("'score_node' must not duplicate an item or covariate name",
         call. = FALSE)
  }

  score_item_edges <- step5_mixed_edges(
    from = score_node,
    to = items,
    edge_type = "score_item",
    directed = FALSE
  )
  item_moral_edges <- step5_pair_edges(
    items,
    "item_score_moralization"
  )
  score_edges <- step5_mixed_edges(
    from = score_node,
    to = x$score_edges$to,
    edge_type = "score_association",
    directed = FALSE
  )
  dif_edges <- step5_mixed_edges(
    from = x$dif_edges$from,
    to = x$dif_edges$to,
    edge_type = "DIF",
    directed = FALSE
  )
  ld_edges <- step5_mixed_edges(
    from = x$ld_edges$from,
    to = x$ld_edges$to,
    edge_type = "local_dependence",
    directed = FALSE
  )
  covariate_edges <- step5_covariate_association_edges(
    covariate_edges,
    covariates
  )
  moral_parent_edges <- step5_moral_parent_edges(
    score_node = score_node,
    score_covariates = x$score_edges$to,
    dif_edges = x$dif_edges
  )

  edges <- step5_bind_mixed_edges(list(
    score_item_edges,
    item_moral_edges,
    score_edges,
    dif_edges,
    ld_edges,
    covariate_edges,
    moral_parent_edges
  ))
  edges <- step5_unique_mixed_edges(edges)

  out <- list(
    nodes = step5_moral_node_table(items, covariates, score_node),
    edges = edges,
    score_item_edges = score_item_edges,
    item_moral_edges = item_moral_edges,
    score_edges = score_edges,
    dif_edges = dif_edges,
    ld_edges = ld_edges,
    covariate_edges = covariate_edges,
    moral_parent_edges = moral_parent_edges,
    score_node = score_node,
    call = call
  )

  structure(out, class = c("gllrm_moral_graph", "gllrm_graph"))
}

#' @export
print.gllrm_graph <- function(x, ...) {
  title <- if (inherits(x, "gllrm_irt_graph")) {
    "GLLRM IRT graph"
  } else if (inherits(x, "gllrm_moral_graph")) {
    "GLLRM moralized marginal graph"
  } else {
    "GLLRM Step 5 graph"
  }
  cat(title, "\n", sep = "")
  cat(paste(rep("-", nchar(title)), collapse = ""), "\n", sep = "")
  cat("Nodes: ", nrow(x$nodes), "\n", sep = "")
  cat("Edges: ", nrow(x$edges), "\n", sep = "")

  if (nrow(x$edges) > 0) {
    edge_counts <- table(x$edges$edge_type)
    for (nm in names(edge_counts)) {
      cat("  ", nm, ": ", unname(edge_counts[[nm]]), "\n", sep = "")
    }
  }

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
    edges = object$edges
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

  if (nrow(x$edges_by_type) > 0) {
    cat("\nEdges by type:\n")
    print(x$edges_by_type, row.names = FALSE)
  }

  invisible(x)
}
