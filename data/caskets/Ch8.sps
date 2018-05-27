unianova interest BY policy kerry/emmeans=tables(policy)/emmeans=tables
   (kerry)/emmeans=tables(policy*kerry).

compute kerryc=kerry-.5.
compute policyc=policy-.5.
process vars=interest policyc kerryc/y=interest/x=policyc/m=kerryc/
   plot=1/model=1.

