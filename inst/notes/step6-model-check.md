# Step 6 model check

Step 6 checks the GLLRM defined by Step 5. It does not discover new edges from
scratch. Instead, it retests the retained DIF and LD edges under conditioning
sets implied by the Step 5 graph and removes unsupported edges.

## Implemented first layer

`step6_check_gllrm()` implements the C5/C7/C9 checking layer:

- DIF edges are checked with C5-style hypotheses. For an item-covariate edge
  `Yi -- Xj`, the tested conditioning set is the total score, other items with
  DIF for `Xj`, and other DIF sources for `Yi`.
- LD edges are checked with two C7/C9-style hypotheses. For an item-item edge
  `Ya -- Yb`, the function tests conditional independence using rest scores
  defined by the LD component containing `Ya` and the LD component containing
  `Yb`, with relevant DIF sources added to the conditioning set.

The testing engine is the same `partial_gamma_coin_test()` helper used in the
DIF, LD, and Step 4 workflows.

## Decision rule

A DIF edge is retained when its C5 decision p-value is at most `alpha`.
An LD edge is retained when at least one of its two C7/C9-style decision
p-values is at most `alpha`. Optional p-value adjustment is applied separately
within the DIF and LD Step 6 test families.

## Output

The function returns all Step 6 test rows, the removed DIF and LD edges, an
updated `final_graph`, and a `moralized_graph` built from that final graph.

## Not yet implemented

The article also discusses testing minimal GMP hypotheses found by separation
in the moralized marginal graph. That requires searching for minimal separating
sets in the moralized graph and then testing each candidate separator. This
should be implemented as a second layer after the C5/C7/C9 model check.
