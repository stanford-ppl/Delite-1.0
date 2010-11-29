function parfor_2_prepare_PooledDownsampledData(all_filenames, target_prctile, exclude_prctile,PooledDownsampledDataFilename)

if ~exist('PooledDownsampledDataFilename')
    PooledDownsampledDataFilename = 'PooledDownsampledData.mat';
end

all_data=[];
tube_channel=[];
all_local_density=[];
for i=1:length(all_filenames)
    display(['downsampling and pooling fcs file: ',num2str(i),'/',num2str(length(all_filenames))]);
    display(all_filenames{i});
    load([all_filenames{i}(1:end-4),'.mat'])
    if i==1
        RefDataSize = size(data,2);
        all_marker_names = marker_names;
        used_marker_names = marker_names(used_markers);
    else
        used_marker_names = unique([used_marker_names;marker_names(used_markers)]);
    end
    
    data(:,local_density<=prctile(local_density,exclude_prctile))=[];
    local_density(local_density<=prctile(local_density,exclude_prctile))=[];
    if target_prctile<100
        target_density = prctile(local_density,target_prctile);
    elseif target_prctile>=100  % then this variable contains the number of desired cells we want after downsampling
        num_desired_cells = target_prctile;
        target_density = downsample_to_certain_num_cells(data, local_density, num_desired_cells);
    end
    keep_prob = min(1,(target_density./local_density));
    is_keep = rand(1,length(local_density))<keep_prob;  
    is_keep(find(sum(isnan(data))~=0))=0;
    display([num2str(sum(is_keep)),' cells keeped in this fcs file'])
    data = data(:,is_keep);
    local_density = local_density(is_keep)/length(is_keep)*RefDataSize;
    
    if isequal(marker_names,all_marker_names)
        all_data = [all_data,data];
    else
        new_marker_names = setdiff(marker_names,all_marker_names);
        all_marker_names = [all_marker_names;new_marker_names];
        all_data = [all_data;repmat(NaN,length(new_marker_names),size(all_data,2))];
        data_tmp = zeros(size(all_data,1),size(data,2))+NaN;
        [C,IA,IB] = intersect(marker_names,all_marker_names);
        data_tmp(IB,:) = data(IA,:);
        all_data = [all_data, data_tmp];
    end
    all_local_density = [all_local_density,local_density];
    tube_channel = [tube_channel,repmat(i,1,size(data,2))];
end
all_data = [all_data;tube_channel];
all_marker_names{end+1} = 'FileInd';

data = all_data;
marker_names = all_marker_names;
local_density = all_local_density;
[C,used_markers,IB] = intersect(marker_names, used_marker_names); 
used_markers = sort(used_markers);
save(PooledDownsampledDataFilename, 'data', 'local_density', 'marker_names', 'used_markers');

display(' ')
display(['PooledDownsampledData has ', num2str(size(data,2)), ' cells from ', num2str(length(all_filenames)), ' files'])
display(' ')




function target_density = downsample_to_certain_num_cells(data, local_density, desired_num)
% keep_prob = x./local_density
% need to find the value of "x", such that if we downsample according to
% "keep_prob", we end up with about "desired_num" cells
% therefore, need to solve the following
%      sum(min(x/local_density(i),1)) = desired_num
% which is equivalent to
%      x = (desired_num-i) / sum(1/local_density(i+1:end)) && local_density(i)<=x<=local_density(i+1) 

if desired_num>=length(local_density)
    target_density = max(local_density)+1;
    return
end
ld = [sort(local_density,'ascend')];
if desired_num/sum(1./local_density) <= ld(1)
    target_density = desired_num/sum(1./local_density);
    return
end
for i=1:length(ld)-1
    x = (desired_num-i) / sum(1./ld(i+1:end));
    if ld(i)<=x && x<=ld(i+1) 
        break;
    end
end
target_density = x;
return
