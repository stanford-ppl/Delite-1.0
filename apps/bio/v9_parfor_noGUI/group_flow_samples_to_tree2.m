function [sample_group_progression_tree, idx_samples, sample_modules] = group_flow_samples_to_tree2(data, target_num_nodes)

disp('working on clustering now ... please wait')
idx_samples = topology_clustering(data, target_num_nodes);
sample_modules = get_module_mean(data',idx_samples)';
sample_group_progression_tree = mst(sample_modules');

return

function [idx_samples] = topology_clustering(data, target_num_nodes)
% initialize kmeans centers, k = target_num_nodes.
seeds_ind = zeros(1,target_num_nodes);
seeds_min_neighbor_dist = zeros(1,target_num_nodes);
nodes_weights = zeros(1,size(data,2));
for i=1:target_num_nodes
    if i==1
        seeds_ind(i) = randsample(1:size(data,2),1);
        dist_tmp = sum((data - repmat(data(:,seeds_ind(i)),1,size(data,2))).^2); dist_tmp(seeds_ind(i))=Inf; dist_tmp = dist_tmp./min(dist_tmp);
        kernel_square = 100^2;
        nodes_weights = 1/sqrt(2*pi*kernel_square)*exp(-dist_tmp/2/kernel_square);
        nodes_weights(seeds_ind(i)) = Inf;
        nodes_weights(nodes_weights==0) = min(nodes_weights(nodes_weights~=0));
    else
        [Y,I] = sort(nodes_weights);
        seeds_ind(i) = I(1);
        dist_tmp = sum((data - repmat(data(:,seeds_ind(i)),1,size(data,2))).^2); dist_tmp(seeds_ind(i))=Inf; dist_tmp = dist_tmp./min(dist_tmp);
        kernel_square = 100^2;
        nodes_weights = max([nodes_weights ; 1/sqrt(2*pi*kernel_square)*exp(-dist_tmp/2/kernel_square)]);
        nodes_weights(seeds_ind(i)) = Inf;
    end
end
% plot3(data(1,:),data(2,:),data(3,:),'g.'); hold on;
% plot3(data(1,seeds_ind),data(2,seeds_ind),data(3,seeds_ind),'b.')
[idx_samples] = kmeans_phase1(data', target_num_nodes, 'start',data(:,seeds_ind)');

% c = 'bgrcmyk';
% for i=1:max(idx)
% %     plot3(data(1,idx==i),data(2,idx==i),data(3,idx==i),[c(mod(i,7)+1),'.']);
%     plot(data(1,idx==i),data(2,idx==i),[c(mod(i,7)+1),'.']);hold on
% end
return

