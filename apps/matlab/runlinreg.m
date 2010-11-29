function exectime = runlinreg(gpu)
addpath linreg
xfile = '../../data/ml/linreg/x-1024.dat';
yfile = '../../data/ml/linreg/y-1024.dat';
if (gpu == 1)
    exectime = linreggpu(xfile, yfile);
elseif (gpu == 2)
    exectime = linregjacket(xfile, yfile);
else
    exectime = linreg(xfile, yfile);
end