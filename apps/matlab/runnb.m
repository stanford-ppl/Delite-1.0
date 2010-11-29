function exectime = runnb(gpu)
addpath nb
trainfile = '../../data/ml/nb/MATRIX.TRAIN.25k';
if (gpu == 1)
    exectime = nb_traingpu(trainfile);
elseif(gpu == 2)
    exectime = nb_trainjacket(trainfile);
else
    exectime = nb_train(trainfile);
end
