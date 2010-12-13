function exectime = runsvm(gpu)
addpath svm
trainfile = '../../data/ml/svm/MATRIX.TRAIN.800'
tol = 0.001
rand('state', 0);
if (gpu == 1)
    exectime = svm_traingpu(trainfile, tol);
elseif (gpu == 2)
    exectime = svm_trainjacket(trainfile, tol);
else
    exectime = svm_train(trainfile, tol);
end