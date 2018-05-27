graph/scatterplot=negemot with govact.

correlations variables = negemot govact/statistics descriptives.

regression/statistics default ci/dep=govact/method=enter negemot.

compute u=rv.uniform(0,1).
sort cases by u.
temporary.
select if ($casenum < 51).
regression/dep=govact/method=enter negemot.

