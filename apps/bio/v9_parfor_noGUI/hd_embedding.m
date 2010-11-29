function [positions] = hd_embedding(distmatrix,hd)
% [positions] = hd_embedding(distmatrix,hd)
% if the network defined by the distmatrix is not connected, it is possible
% that positins will contain Inf


distmatrix = triu(distmatrix,1)+triu(distmatrix,1)';
if ~exist('hd')
    hd = min(size(distmatrix,1),10);
end

cluster_centers = zeros(1,hd);
cluster_centers(1) = 1; %ceil(rand*size(distmatrix,1));
dist = zeros(hd,size(distmatrix,1));
for i=2:hd
    dist(i-1,:) = find_graph_dist(cluster_centers(i-1), distmatrix);
    [Y,I]= sort(sum(dist),'descend');
    ind = 1;
    while ~isempty(intersect(I(ind), cluster_centers(1:i-1)))
        ind = ind + 1;
    end
    cluster_centers(i) = I(ind);
end
dist(hd,:) = find_graph_dist(cluster_centers(hd), distmatrix);

positions = dist;


return



function dist = find_graph_dist(start_pt, distmatrix)

dist = Inf*ones(1,size(distmatrix,1));
first_token = struct('current_pt',start_pt, 'past_pts',start_pt, 'path_len',0);
tokens(1) = first_token;
dist(tokens(1).current_pt) = tokens(1).path_len;
to_be_propogated_ind =1;
while to_be_propogated_ind<=length(tokens)
    [to_be_propogated_ind,length(tokens)]
    cp = tokens(to_be_propogated_ind).current_pt;
    if dist(cp)<tokens(to_be_propogated_ind).path_len
        to_be_propogated_ind = to_be_propogated_ind+1; continue;
    end
    neighbors = find(distmatrix(cp,:)~=0);
    np = setdiff(neighbors,tokens(to_be_propogated_ind).past_pts);
    if isempty(np)
        to_be_propogated_ind = to_be_propogated_ind+1;
        continue;
    end
    for i=1:length(np)
        next_token = struct('current_pt',np(i), 'past_pts',[tokens(to_be_propogated_ind).past_pts,np(i)], 'path_len',tokens(to_be_propogated_ind).path_len+distmatrix(cp,np(i)));
        if next_token.path_len<dist(next_token.current_pt)
            dist(next_token.current_pt) = next_token.path_len;
            tokens(end+1) = next_token;
        end
    end
    to_be_propogated_ind = to_be_propogated_ind+1;
end
% 
% results = zeros(2,length(tokens));
% for i=1:length(tokens)
%     results(i,1) = tokens(i).current_pt;
%     results(i,2) = tokens(i).path_len;
% end
% for i=1:size(distmatrix,1)
%     ind = find(results(:,1)==i);
%     if isempty(ind)
%         dist(i) = Inf;
%     else
%         dist(i) = min(results(find(results(:,1)==i),2));
%     end
% end
%     

