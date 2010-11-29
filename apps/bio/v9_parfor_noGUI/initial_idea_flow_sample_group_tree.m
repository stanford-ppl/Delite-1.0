function initial_idea_flow_sample_group_tree


load simulated_data\simulated_flow.mat t data

target_num_nodes = 100;

idx_samples = 1:size(data,2);
counter = 1;
while max(idx_samples)>=1.5*target_num_nodes
    fprintf('merging %d groups into about %d groups, round %d\n', max(idx_samples), ceil(max(idx_samples)/2), counter);
    idx_samples = merge_sample_groups(data, idx_samples,t);
    counter = counter + 1;
end

sample_modules = get_module_mean(data',idx_samples)';
sample_group_progression = mst(sample_modules');
% display_mst(mst(sample_modules'))
[is_connected, shortest_hop] = check_graph_connectivity(sample_group_progression); shortest_hop = shortest_hop-eye(size(shortest_hop));
[ind_i,ind_j]  = find_matrix_top_element(shortest_hop);
ind_i = ind_i(1); ind_j = ind_j(1);
back_bones = find(shortest_hop(ind_i,:)+shortest_hop(ind_j,:)==shortest_hop(ind_i,ind_j));
[dummy,I] = sort(shortest_hop(ind_i,back_bones));

progression_stage_ind = zeros(max(idx_samples),1);
progression_stage_ind(back_bones(I)) = 1:length(back_bones);
while sum(progression_stage_ind==0)~=0
    tmp = sample_group_progression*progression_stage_ind;
    progression_stage_ind(progression_stage_ind==0) = tmp(progression_stage_ind==0);
end

t_interval = 100/size(sample_modules,2);
used_intervals = 0;
sample_t_est = zeros(1,size(data,2));
sample_stage_est = zeros(1,size(data,2));
for i=1:max(progression_stage_ind)
    groups_in_this_stage = find(progression_stage_ind==i);
    needed_intervals = length(groups_in_this_stage);
    samples_in_this_stage=[];
    for j=1:length(groups_in_this_stage)
        samples_in_this_stage = [samples_in_this_stage,find(idx_samples==groups_in_this_stage(j))];
    end
    sample_stage_est(samples_in_this_stage)=i;
    sample_t_est(samples_in_this_stage) = rand(1,length(samples_in_this_stage))*needed_intervals*t_interval+used_intervals*t_interval;
    used_intervals = used_intervals + needed_intervals;
end

figure(1); subplot(2,2,1);plot(t,data,'.')
figure(1); subplot(2,2,2);plot(t,sample_t_est,'.')
figure(1); subplot(2,2,3);plot(sample_t_est,data,'.')
figure(1); subplot(2,2,4);plot(100-sample_t_est,data,'.')

return


function idx_new = merge_sample_groups(data, idx,t)
% the samples grouped into give N groups by idx, 
% reduce the number of groups by a factor of 2
% look at the groups one by one, 
% for each group, use single linkage to find the closet other group
% merge them together, and this other group is out of the game


isactive_module = ones(1,max(idx));
isactive_sample = ones(1,size(data,2));
fprintf('merging in progress ... groups left: %7d',max(idx))
while sum(isactive_module)>1
    module_ind = find(isactive_module==1,1);
    sample_in_module = find(idx==module_ind);
    if length(sample_in_module)~=1
        sample_in_module = randsample(sample_in_module,1);
    end
    dist = sum(abs(repmat(data(:,sample_in_module),1,size(data,2)) - data));
    dist(find(idx==module_ind)) = max(dist);    % dist to everyone in my own group
    dist(isactive_sample==0) = max(dist);       % dist to everyone that already grouped
    [m,mind] = min(dist);
    module_to_be_deleted = idx(mind);
    isactive_module(module_to_be_deleted)=[];
    isactive_module(module_ind)=0;
    idx(idx==module_to_be_deleted) = module_ind;  % merge
    isactive_sample(idx==module_ind)=0;

    tmp = idx>module_to_be_deleted;
    idx(tmp) =  idx(tmp) -1;
    fprintf('\b\b\b\b\b\b\b%7d',max(idx))
end
fprintf('\n')
idx_new = idx;
return    