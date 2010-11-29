function [marker_similarity_matrix, p_value, idx_samples, sample_modules] = suggest_meaningful_markers_SPA(data, target_num_nodes)

idx_samples = 1:size(data,2);
counter = 1;
while max(idx_samples)>=1.5*target_num_nodes
    fold = max(2,round(max(idx_samples)/5000)); 
    fprintf('merging %d groups into about %d groups, round %d\n', max(idx_samples), ceil(max(idx_samples)/fold), counter);
    idx_samples = merge_sample_groups_old(data, fold,idx_samples);
    counter = counter + 1;
end 


sample_modules = get_module_mean(data',idx_samples)';
% sample_group_progression_tree = mst(sample_modules');

candidate_trees = zeros(size(sample_modules,2),size(sample_modules,2),size(sample_modules,1));
for i=1:size(sample_modules,1)
    [adj,adj2,cost] = mst(sample_modules(i,:)');
    candidate_trees(:,:,i) = adj;
end
fprintf('\n\n finished building candidate trees for %d markers \n\n', size(sample_modules,1));


iter=2000;
p_value = [];
fprintf('fitting candidate trees and modules, pairs %4d, %4d', 0,0);
for j=1:size(sample_modules,1)
    distance_matrix = get_row_distance(sample_modules(j,:)');
    for i=1:size(sample_modules,1)
        progression_tree = candidate_trees(:,:,i);
        fprintf('\b\b\b\b\b\b\b\b\b\b%4d, %4d', i,j); drawnow;
        score = fit_data_mst(progression_tree, sample_modules(j,:)); % fit_data_mst(progression_tree, data(idx==j,:), [],distance_matrix)

        null_scores = zeros(1,iter);
        non_zero_ind = find(triu(progression_tree)~=0); denominator = sum(progression_tree(non_zero_ind));
        for k=1:iter, 
            [dummy, null_perm] = sort(rand(1,size(sample_modules,2)));
            distance_matrix_tmp = distance_matrix(null_perm,null_perm);
            null_scores(k) = sum(progression_tree(non_zero_ind).*distance_matrix_tmp(non_zero_ind))/denominator;
        end
        p_value(i,j) =  sum(score>=null_scores)/iter;
    end
end

marker_similarity_matrix = zeros(size(sample_modules,1),size(sample_modules,1));
p_threshold = 0.001;
for i=1:size(sample_modules,1)
    for j=1:size(sample_modules,1)
%         marker_similarity_matrix(i,j) = -sum(p_value(:,i).*p_value(:,j));
        marker_similarity_matrix(i,j) = sum(p_value(:,i)<=p_threshold & p_value(:,j)<=p_threshold);
    end
end
 

return




function idx_new = merge_sample_groups_old(data, fold, idx)
% the samples grouped into give N groups by idx, 
% reduce the number of groups by a factor of fold (min of fold is 2)
% look at the groups one by one, 
% for each group, use single linkage to find the closet other group
% merge them together, and this other group is out of the game

isactive_module = ones(1,max(idx));
module_size = zeros(1,max(idx)); for i=1:length(module_size), module_size(i) = sum(idx==i); end
isactive_sample = ones(1,size(data,2));
fprintf('merging in progress ... groups left: %7d',max(idx));
iter = 1; total_num_modules = max(idx);
while sum(isactive_module)>1
%     module_ind = randsample(find(isactive_module==1),1);
    tmp = module_size; tmp(module_size==0)=Inf;  tmp(isactive_module==0) = Inf;
    [dummy, module_ind] = min(tmp);
    sample_in_module = find(idx==module_ind);
    module_center = median(data(:,sample_in_module),2);
    dist = sum(abs(repmat(module_center,1,size(data,2)) - data),1);
    dist(idx==module_ind) = Inf;    % dist to everyone in my own group
    [Y,I] = sort(dist,'ascend'); 
    module_to_be_deleted = idx(I(1:fold-1));
    first_inactive_module_ind = find(isactive_module(module_to_be_deleted)==0,1);
    if ~isempty(first_inactive_module_ind) && first_inactive_module_ind==1, module_to_be_deleted=[]; end
    if ~isempty(first_inactive_module_ind) && first_inactive_module_ind~=1, module_to_be_deleted=module_to_be_deleted(1:first_inactive_module_ind-1); end
    if isempty(module_to_be_deleted)
        isactive_module(module_ind)=0;
        continue;
    end
    isactive_module(module_to_be_deleted)=0;
    isactive_module(module_ind)=0;
    for i=1:length(module_to_be_deleted)
        idx(idx==module_to_be_deleted(i)) = module_ind;  % merge
    end
    module_size(module_to_be_deleted)=0; module_size(module_ind) = sum(idx==module_ind);
    isactive_sample(idx==module_ind)=0;
    total_num_modules = total_num_modules - length(module_to_be_deleted);
    fprintf('\b\b\b\b\b\b\b%7d',total_num_modules);
    iter = iter + 1; % iter is only serving for the drawnow in the following lines
    if round(iter/10)==iter/10
        drawnow;
    end
end
fprintf('\n')
idx_new = standardize_idx(idx);
return    

