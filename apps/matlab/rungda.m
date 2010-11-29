function exectime = rungda(gpu)
addpath gda
if (gpu==1)
    exectime = gdagpu('../../data/ml/gda/1024-1200x.dat','../../data/ml/gda/q1y.dat');
elseif (gpu==2)
    exectime = gdajacket('../../data/ml/gda/1024-1200x.dat','../../data/ml/gda/q1y.dat');
else
    exectime = gda('../../data/ml/gda/1024-1200x.dat','../../data/ml/gda/q1y.dat');
end