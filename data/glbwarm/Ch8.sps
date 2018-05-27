compute negemotc=negemot-3.558.
compute agec=age-49.536.
compute negage=negemotc*agec.
regression/dep=govact/method=enter negemotc agec negage posemot ideology sex.

process vars=govact negemot age posemot ideology sex/y=govact/x=negemot
   /m=age/model=1/center=1/quantile=1/jn=1/plot=1.

data list free/negemot age govact.
begin data.
    -2.2280   -22.5362     3.9792
    -1.2280   -22.5362     4.2697
      .1120   -22.5362     4.6590
     1.4420   -22.5362     5.0454
     2.1120   -22.5362     5.2401
    -2.2280   -13.5362     3.8401
    -1.2280   -13.5362     4.1876
      .1120   -13.5362     4.6532
     1.4420   -13.5362     5.1154
     2.1120   -13.5362     5.3482
    -2.2280     1.4638     3.6082
    -1.2280     1.4638     4.0507
      .1120     1.4638     4.6436
     1.4420     1.4638     5.2320
     2.1120     1.4638     5.5285
    -2.2280    13.4638     3.4227
    -1.2280    13.4638     3.9411
      .1120    13.4638     4.6358
     1.4420    13.4638     5.3253
     2.1120    13.4638     5.6726
    -2.2280    20.4638     3.3145
    -1.2280    20.4638     3.8772
      .1120    20.4638     4.6313
     1.4420    20.4638     5.3797
     2.1120    20.4638     5.7568
end data.
compute age=age+49.536.
compute negemot=negemot+3.558.
graph/scatterplot=negemot with govact by age.

data list free/age.
begin data.
18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50 52
54 56 58 60 62 64 66 68 70 72 74 76 78 80 82 84 86 88
end data.
compute agemc=age-49.536.
compute b1=0.4332.
compute b3=0.0063.
compute seb1=0.0262.
compute seb3=0.0015.
compute convb1b3=-0.0000029.
compute theta=b1+b3*agemc.
compute tcrit=1.963.
compute se=sqrt((seb1*seb1)+(2*agemc*convb1b3)+(agemc*agemc*seb3*seb3)).
compute llci=theta-tcrit*se.
compute ulci=theta+tcrit*se.
graph/scatter(overlay)=age age age WITH llci ulci theta (pair).
