function node_median = DrawTree_subfunc_ClearNaNs_in_node_median(node_median,adj)

tmp = sum(isnan(node_median),3);
if isempty(setdiff(unique(tmp(:)),0)) 
    return  % great, there is no NaN's
elseif length(setdiff(unique(tmp(:)),0))>1
    error('Error: CAUTION, this is not supposed to happen, ask Peng to debug it');
    return
end

% % if the code gets here, it means that there are NaNs in node_median, and
% % the NaNs in each slide (for each marker) are at the same positions
% % Now, let get ride of them, replacing the NaNs by their neighbors in adj
% % the reason is that, when upsampling one file, if one node doesn't have 
% % any cells, it's node_median is set as the mean of its direct neighbors.  

for file_ind = 1:size(node_median,1)  % for each file
    while sum(isnan(node_median(file_ind,:,1)))~=0
        nan_nodes = find(isnan(node_median(file_ind,:,1)));
        for i=1:length(nan_nodes)
            this_nan_node = nan_nodes(i);
            not_nan_neighbors = setdiff(find(adj(this_nan_node,:)==1),nan_nodes);
            if isempty(not_nan_neighbors)
                continue;
            else
                for slide = 1:size(node_median,3)
                    node_median(file_ind,this_nan_node,slide) = mean(node_median(file_ind,not_nan_neighbors,slide));
                end
            end
        end
    end
end
return