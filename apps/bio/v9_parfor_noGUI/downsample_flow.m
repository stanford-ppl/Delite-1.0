function [is_keep,local_density,keep_prob] = downsample_flow(data,target_prctile,kernel_width_para)

if ~exist('kernel_width_para')
    kernel_width_para = 10;
end
if ~exist('target_prctile')
    target_prctile = 5;
end

min_dist = zeros(1,min(size(data,2),5000));
fprintf('finding empirical dist of the min distance between cells ... %4d%%',0);
for i=1:length(min_dist)
    i;
    ind = randsample(1:size(data,2),1);
    all_dist = sum(abs(repmat(data(:,ind),1,size(data,2)-1) - data(:,setdiff(1:end,ind))),1);
    min_dist(i) = min(all_dist);
    fprintf('\b\b\b\b\b%4d%%',floor(i/length(min_dist)*100));
end
med_min_dist = median(min_dist);
kernel_width = kernel_width_para * med_min_dist;
fprintf('\nFor this %d channel data, KERNEL_WIDTH is %3.3f\n', size(data,1),kernel_width);

% save tmp_file kernel_width med_min_dist
% load tmp_file
local_density = zeros(1,size(data,2));
fprintf('finding local density for each cell ... %4d%%',0);
for i=1:size(data,2)
    i;
    if local_density(i)==0
        all_dist = sum(abs(repmat(data(:,i),1,size(data,2)) - data),1);
        local_density(i) =  sum(kernel_width>=all_dist); %sum(max(kernel_width-all_dist,0)/kernel_width);
        local_density(all_dist< 3*med_min_dist) = local_density(i);
    end
    if mod(i,1000)==1 || i == size(data,2)
        fprintf('\b\b\b\b\b%4d%%',floor(i/length(local_density)*100));
    end
end
fprintf('\n', size(data,1),kernel_width);
% save tmp_file local_density -append

% load tmp_file
target_density = prctile(local_density,target_prctile);
keep_prob = (target_density./local_density);
fprintf('Down-sample cells ... \n');
is_keep = rand(1,size(data,2))<keep_prob;
return
    