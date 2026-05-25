# Step 5 graph construction

Step 5 translates the screening results into the structural graph for a
graphical loglinear Rasch model. It does not perform new statistical tests.
Instead, it assembles the edges supported by earlier steps:

- Step 3a contributes item-item edges for genuine local dependence.
- Step 3b/3c contributes item-covariate edges for genuine DIF.
- Step 4 contributes score-covariate edges for retained score associations.

The implementation in `build_gllrm_graph()` returns explicit node and edge
tables so that the result is inspectable without plotting packages. The main
`edges` table contains only graph topology: `from`, `to`, and `edge_type`.
Detailed evidence for why edges were included is kept in the component-specific
`ld_edges`, `dif_edges`, and `score_edges` tables.

Two derived graph constructors translate this evidence graph into graph forms
used in the article:

- `build_irt_graph()` creates the IRT chain graph. A latent node, by default
  `theta`, points to all items. Retained Step 4 score-covariate associations are
  represented as covariate-to-theta arrows, DIF as covariate-to-item arrows, and
  LD as undirected item-item edges.
- `build_moralized_graph()` creates the moralized marginal graph. The latent
  node is replaced by a total-score node, by default `#`. All edges are
  undirected. The function adds score-item edges, item-item moralization edges
  induced by the total score, and moralized parent edges between common parents.

## Interpretation

Local dependence edges indicate conditional association between two items after
conditioning on rest scores during the Step 3a screening procedure. DIF edges
indicate that an item remains associated with an exogenous covariate after both
Step 3b and Step 3c spurious-source elimination. Score-covariate edges indicate
that the total score remains conditionally associated with the exogenous
covariate after Step 4 backwards elimination.

## Integration points

The function expects the current package output shapes:

- `genuine_LD()` output through the `genuine_ld` component.
- `combine_step3bc()` output through the `table` component.
- `step4_structure_screen()` output through the `criterion_validity` component.

Direct data frames with the same columns can also be supplied, which makes the
graph builder useful for examples, tests, and external screening results.

## Assumptions and ambiguities

The article describes graph construction conceptually, but does not specify a
single R object representation. This implementation represents the graph as
plain data frames: a node table, a minimal edge table, and separate evidence
tables for each source step. The edge table should be interpreted as an
undirected conditional-association topology rather than a causal model.

The derived IRT graph is mixed: measurement, score-association, and DIF edges
are directed, while LD and optional covariate-covariate edges are undirected.
The derived moralized graph is fully undirected and is intended for identifying
separating sets for minimal GMP hypotheses, not for representing causal
directions.
