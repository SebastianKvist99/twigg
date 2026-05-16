# Step 4 Architecture Note

Step 4 screens the structural association between the total score and
exogenous covariates. It is not a DIF or LD step. For each currently retained
covariate `X_j`, the tested hypothesis is:

```text
Score independent of X_j given all other currently retained covariates
```

The implementation uses `partial_gamma_coin_test()` so Step 4 shares the same
partial gamma and conditional Monte Carlo testing infrastructure as the DIF
workflow. If only one covariate remains, the test conditions on a single
constant block and the displayed conditioning set is `None`.

## Elimination Logic

`step4_structure_screen()` performs backwards elimination:

1. Start with all supplied covariates.
2. In each pass, test every current covariate against `Score`, conditioning on
   the other current covariates.
3. Optionally adjust p-values within the current pass using `p.adjust()`.
4. Remove the first covariate whose decision p-value is larger than `alpha`.
5. Retest the reduced covariate set.
6. Stop when all remaining covariates are significant, or no covariates remain.

The output keeps every test pass in `tests` and every removal in
`elimination_path`, so the final retained set can be audited. It also exposes
both `initial_criterion_validity` and `criterion_validity`: the first reports
the all-covariates Step 4 tests described in the article text, while the second
reports the final retained covariates after backwards elimination. The
`criterion_validity_comparison` table joins these two views by covariate.

## Assumptions

Items are numeric and the total score is their row sum unless a score is
supplied. Covariates may be numeric, factor, or character. Factors and character
covariates are converted to numeric scores using the package's existing
`check_covariate()` helper. Directional interpretation of partial gamma for
unordered categorical covariates therefore depends on this coding.

NA handling follows the existing screening convention: complete cases are
formed over the score and Step 4 covariates before testing.

## Criterion Validity

The `initial_criterion_validity` element lists all covariates tested in the
initial all-covariates pass. The `criterion_validity` element lists retained
covariates, their final partial gamma, direction, p-value, adjustment result,
conditioning set, and whether the final decision p-value supports criterion
validity at `alpha`. Monte Carlo p-values returned numerically as zero are kept
as numeric zero for computation but are accompanied by labels such as
`< 0.0001`, reflecting the resolution implied by the number of resamples.

## Step 5 Integration

The retained covariates are the covariate nodes that should remain connected to
the score/latent-trait part of a later GLLRM graph. Downstream graph
construction can use `retained_covariates` directly, and can use `tests` or
`criterion_validity` to annotate edges with partial gamma, p-values, and
conditioning sets.

## Ambiguities

The article specifies the backwards elimination principle, but leaves some
implementation details open:

- whether to adjust p-values during Step 4;
- which nonsignificant covariate to remove if several fail in the same pass;
- how to define direction for nominal categorical covariates.

This package defaults to unadjusted p-values, removes the first nonsignificant
covariate in the user-supplied covariate order, and reports the gamma direction
under the package's current covariate coding.
