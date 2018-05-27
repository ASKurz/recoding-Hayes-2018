compute toneexp=negexp*negtone.
regression/dep=negtone/method=enter dysfunc.
regression/dep=perform/method=enter dysfunc negtone negexp toneexp.

process vars=dysfunc negtone negexp perform/y=perform/x=negtone/m=negexp
   /model=1/quantile=1.

process vars=dysfunc negtone negexp perform
   /y=perform/x=dysfunc/m=negtone/v=negexp/model=14/quantile=1/boot=10000.
