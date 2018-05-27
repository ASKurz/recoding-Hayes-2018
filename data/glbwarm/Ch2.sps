regression/statistics defaults ci zpp/dep=govact/method=enter negemot posemot ideology sex age.

regression/dep=negemot/method=enter  posemot ideology sex age/save resid.
rename variables res_1=negresid.
regression/dep=govact/method=enter negemot posemot ideology sex age.
rename variables res_1=govresid.
regression/dep=govresid/method=enter negresid.

regression/statistics defaults change/dep=govact/method=enter ideology sex age
   /method=enter negemot posemot.
