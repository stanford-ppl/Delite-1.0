function fitting_score = fit_data_mst(tree_adj, data, working_mode,distance_matrix)
% tree_adj is a minimum spanning tree, either binary or weighted
% if weighted, smaller value between i,j indicates closer, strong bond

if size(tree_adj,1)==size(data,2)
    data = data';           % make sure that each row of data is one entity in the comparison, one node in the mst
end
if ~exist('working_mode')
    working_mode = 'euclidean';
end
if isempty(intersect({'euclidean','corr','abs_corr'}, working_mode))
    working_mode = 'euclidean';
end



[pairs] = find_matrix_big_element(triu(tree_adj,1),1e-10);

if ~exist('distance_matrix')
    data_distances = zeros(size(pairs,1),1);
    edge_length    = zeros(size(pairs,1),1);
    for i=1:size(pairs,1)
        edge_length(i) = tree_adj(pairs(i,1),pairs(i,2));
        if isequal(working_mode, 'euclidean')
            data_distances(i) = comp_dist_euclidean(data, pairs(i,1),pairs(i,2)); 
        elseif isequal(working_mode, 'corr')
            data_distances(i) = comp_dist_corr(data, pairs(i,1),pairs(i,2));
        elseif isequal(working_mode, 'abs_corr')
            data_distances(i) = comp_dist_abs_corr(data, pairs(i,1),pairs(i,2));
        end
    end
else
    data_distances = zeros(size(pairs,1),1);
    edge_length    = zeros(size(pairs,1),1);
    for i=1:size(pairs,1)
        edge_length(i) = tree_adj(pairs(i,1),pairs(i,2));
        data_distances(i) = distance_matrix(pairs(i,1),pairs(i,2)); 
    end
end


weights = 1./(edge_length - min(edge_length)+1);
fitting_score = sum(weights.*data_distances)/sum(weights);
return




function dist = comp_dist_euclidean(X,ind1,ind2)
dist = zeros(length(ind1),length(ind2));
for i=1:length(ind1)
    dist(i,:) = sqrt(sum((repmat(X(ind1(i),:),length(ind2),1) - X(ind2,:)).^2,2)); 
end
return


function dist = comp_dist_corr(X,ind1,ind2)
dist = zeros(length(ind1),length(ind2));
corr = X(ind1,:)*X(ind2,:)';
dist = 1-corr; 
return


function dist = comp_dist_abs_corr(X,ind1,ind2)
dist = zeros(length(ind1),length(ind2));
corr = X(ind1,:)*X(ind2,:)';
dist = 1-abs(corr); 
return

