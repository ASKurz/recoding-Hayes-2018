compute proxsex=protest*sexism.
regression/dep=liking/method=enter protest sexism proxsex.

process vars=protest liking sexism/y=liking/x=protest/m=sexism/model=1/jn=1/quantile=1/plot=1.

compute sexismp=sexism-5.117.
compute proxsex=protest*sexism.
regression/dep=liking/method=enter protest sexismp proxsex.

compute sexismp=sexism-4.333.
compute proxsexp=protest*sexismp.
regression/dep=liking/method=enter protest sexismp proxsex.

compute sexismp=sexism-5.901.
compute proxsex=protest*sexism.
regression/dep=liking/method=enter protest sexismp proxsex.

data list free/sexism effect llci ulci.
begin data.
     sexism     Effect      LLCI       ULCI
     2.8700    -1.3804    -2.5133     -.2474
     3.0765    -1.2082    -2.2476     -.1689
     3.2830    -1.0361    -1.9831     -.0891
     3.4895     -.8640    -1.7203     -.0077
     3.5087     -.8480    -1.6959      .0000
     3.6960     -.6918    -1.4596      .0759
     3.9025     -.5197    -1.2020      .1625
     4.1090     -.3476     -.9487      .2535
     4.3155     -.1755     -.7017      .3508
     4.5220     -.0033     -.4642      .4576
     4.7285      .1688     -.2407      .5783
     4.9350      .3409     -.0370      .7188
     4.9753      .3745      .0000      .7490
     5.1415      .5131      .1419      .8842
     5.3480      .6852      .2947     1.0757
     5.5545      .8573      .4249     1.2898
     5.7610     1.0294      .5381     1.5208
     5.9675     1.2016      .6398     1.7633
     6.1740     1.3737      .7337     2.0137
     6.3805     1.5458      .8224     2.2693
     6.5870     1.7180      .9074     2.5285
     6.7935     1.8901      .9898     2.7904
     7.0000     2.0622     1.0704     3.0541
end data.
graph/scatter(overlay)=sexism sexism sexism with llci ulci effect (pair).

