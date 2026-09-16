# Documentation helper for groupByRoute parameter

Documentation helper for groupByRoute parameter

## Arguments

- groupByRoute:

  (\`logical(1)\`) If \`TRUE\`, further splits each medication group by
  \`route_concept_id\`, calculating CMA separately for each route found
  within the group. Drug exposure records with a missing or zero
  \`route_concept_id\` remain part of the general ingredient-level
  group. Default: \`FALSE\`.
