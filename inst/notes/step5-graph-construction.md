# Step 5 graph construction

Step 5 translates the screening results into the structural graph for a
graphical loglinear Rasch model. It does not perform new statistical tests.
Instead, it assembles the edges supported by earlier steps:

- Step 3a contributes item-item edges for genuine local dependence.
- Step 3b/3c contributes item-covariate edges for genuine DIF.
- Step 4 contributes score-covariate edges for retained score associations.

The implementation in `build_gllrm_graph()` returns explicit node and edge
tables so that the result is usable without plotting packages. If `igraph` is
installed, the same information is also returned as an `igraph` object.

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
single R object representation. This implementation treats the graph as
undirected by default, because LD, DIF, and score-covariate associations are
interpreted as conditional association edges in the GLLRM graph. The user may
request a directed `igraph` object, but the edge table itself is still an audit
trail rather than a causal model.

