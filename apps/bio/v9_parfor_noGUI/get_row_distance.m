function distance_matrix = get_row_distance(data, working_mode)

if size(data,1)>1000
    disp('warning, each row of data is one entity in the comparison')
    return
end

if ~exist('working_mode')
    working_mode = 'euclidean';
end
if isempty(intersect({'euclidean','corr','abs_corr'}, working_mode))
    working_mode = 'euclidean';
end

distance_matrix = zeros(size(data,1));
for i=1:size(data,1)
    for j=i:size(data,1)
        if isequal(working_mode, 'euclidean')
            distance_matrix(i,j) = comp_dist_euclidean(data, i,j); 
        elseif isequal(working_mode, 'corr')
            distance_matrix(i,j) = comp_dist_corr(data,i,j);
        elseif isequal(working_mode, 'abs_corr')
            distance_matrix(i,j) = comp_dist_abs_corr(data, i,j);
        end
        distance_matrix(j,i) = distance_matrix(i,j);
    end
end

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

