function [cluster_median] = DrawTree_subfunc_get_node_median(data, idx)
% each row of data is one marker/channel
% each column is a cell
% idx stores the clustering assignment of each cell
% if there is a node i, 1 <= i <= max(idx), and this node does NOT
% have any cell, meaning that sum(idx==i)==0, then the median of this
% node is NaN for all the channels

cluster_median = zeros(size(data,1),max(idx))+NaN;
for i=1:max(idx)
    ind = find(idx==i);
    if ~isempty(ind)
        cluster_median(:,i) = nanmedian(data(:,ind),2);
    end
end
return

