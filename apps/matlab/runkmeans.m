function exectime = runkmeans(gpu)
addpath kmeans
if (gpu == 1)
    exectime = kmeansgpu('../../data/ml/kmeans/mandrill-large.dat', '../../data/ml/kmeans/initmu.dat');
elseif (gpu == 2)
    exectime = kmeansjacket('../../data/ml/kmeans/mandrill-large.dat', '../../data/ml/kmeans/initmu.dat');
else
    exectime = kmeans('../../data/ml/kmeans/mandrill-large.dat', '../../data/ml/kmeans/initmu.dat');
end