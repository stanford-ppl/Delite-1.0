function [FlowSPD_output_filename] = parfor_4_Process_DataForSPD_replace_GUI(FlowSPD_input_filename,num_target_clusters)

handles = replace_load_button(FlowSPD_input_filename,num_target_clusters);
FlowSPD_output_filename = handles.result_filename;


handles = replace_GO_button(handles);

replace_save_button(handles)


return

%%%%%%%%%%%%%%%
function replace_save_button(handles)
data = handles.data;
marker_names = handles.marker_names;
is_marker_selected = handles.is_marker_selected;
target_num_sample_groups = handles.target_num_sample_groups;
sample_group_assign = handles.sample_group_assign;
sample_group_progression_tree= handles.sample_group_progression_tree;
node_position = handles.node_position;
group_shortest_hop = handles.group_shortest_hop;
progression_structure = handles.progression_structure;
data_filename = handles.data_filename;
result_filename = handles.result_filename;
marker_similarity_matrix = handles.marker_similarity_matrix;
p_value = handles.marker_tree_pvalue;  %i,j is the pvalue between tree i and marker j
local_density = handles.local_density;
save(result_filename,'data','marker_names','is_marker_selected','target_num_sample_groups',...
    'sample_group_assign','sample_group_progression_tree','node_position','group_shortest_hop',...
    'progression_structure','data_filename','result_filename','marker_similarity_matrix','p_value','local_density');


%%%%%%%%%%%%%%%
function handles = replace_GO_button(handles)


% grouping and find mst
    [handles.sample_group_progression_tree, handles.sample_group_assign] = group_flow_samples_to_tree(handles.data(handles.is_marker_selected==1,:), handles.target_num_sample_groups);
    handles.data(:,handles.sample_group_assign==0)=[];
    handles.local_density(handles.sample_group_assign==0)=[];
    handles.sample_group_assign(handles.sample_group_assign==0)=[];

% get visualization node_position
    disp('Working on visualization of the tree structure ... ');
    disp('This may take a few minutes if the number of nodes is large ');

    mst_tree = handles.sample_group_progression_tree;
    tree_for_embedding = triu(mst_tree,1);
    for i=1:size(tree_for_embedding,1)-1
        for j=i+1:size(tree_for_embedding,2)
            if tree_for_embedding(i,j)==0, continue; end
            tree_for_embedding(i,j) = norm(mean(handles.data(handles.is_marker_selected==1,handles.sample_group_assign==i),2) - mean(handles.data(handles.is_marker_selected==1,handles.sample_group_assign==j),2));
            tree_for_embedding(j,i) = tree_for_embedding(i,j);
        end
    end    
    coeff = viewtree_arch_for_parfor_script(double(tree_for_embedding~=0));
    handles.node_position = coeff;

function node_positions = viewtree_arch_for_parfor_script(tree_adj)
warning off;
% tree_adj is undirected
adj = triu(tree_adj,1); adj = adj + adj';
[is_connected, shortest_hop] = check_graph_connectivity(adj); 
shortest_hop = shortest_hop - diag(diag(shortest_hop));

% find the backbone, which is the longest path
[ind_j,ind_i]  = find_matrix_top_element(shortest_hop);
ind_i = ind_i(1); ind_j = ind_j(1);
back_bones = find(shortest_hop(ind_i,:)+shortest_hop(ind_j,:)==shortest_hop(ind_i,ind_j));
[dummy,I] = sort(shortest_hop(ind_i,back_bones));
back_bones = back_bones(I);

% find all the side_chains
side_chains = cell(0);
side_chain_roots = back_bones;
counter=1;
while counter<=length(side_chain_roots)
    root_node = side_chain_roots(counter);
    first_neighbors = setdiff(find(adj(root_node,:)~=0),side_chain_roots);
    if isempty(first_neighbors)
        counter = counter + 1;
        continue;
    end
    for i=1:length(first_neighbors)
        % find the side chain starting from root_node to first_neighbor(i) to as far as it can go
        % 1 find members on this side chain and the subside chains
        subtree_nodes_through_this_neighbor = find(shortest_hop(root_node,:)>shortest_hop(first_neighbors(i),:));
        [dummy,I] = max(shortest_hop(root_node,subtree_nodes_through_this_neighbor));
        end_node = subtree_nodes_through_this_neighbor(I);
        sub_backbone = find(shortest_hop(root_node,:)+shortest_hop(end_node,:)==shortest_hop(root_node,end_node));
        [dummy,I] = sort(shortest_hop(root_node,sub_backbone));
        sub_backbone = sub_backbone(I);
        
        side_chain_roots = [side_chain_roots, sub_backbone(2:end)];
        side_chains = [side_chains;{sub_backbone}];
    end
end


node_positions = zeros(2,size(adj,1));
position_assigned_flag = zeros(1,size(adj,1));
% determin the nodes location of the backbones
backbone_node_angles = 1:length(back_bones);
backbone_node_angles = backbone_node_angles - mean(backbone_node_angles);
backbone_node_angles = backbone_node_angles./max(backbone_node_angles).*(pi/90*25);
node_positions(1,back_bones) = sin(backbone_node_angles);
node_positions(2,back_bones) = -cos(backbone_node_angles);
node_positions = node_positions./norm(node_positions(:,back_bones(1))-node_positions(:,back_bones(2)));
position_assigned_flag(back_bones)=1;

coeff = node_positions - repmat(median(node_positions,2),1,size(node_positions,2));
coeff(1,:) = coeff(1,:)/max(abs(coeff(1,:)))*50;
coeff(2,:) = coeff(2,:)/max(abs(coeff(2,:)))*50;
% DrawTree(adj(position_assigned_flag==1,position_assigned_flag==1), coeff(:,position_assigned_flag==1)); drawnow
fprintf('Drawing a total of %d nodes ... %6d', size(tree_adj,1),sum(position_assigned_flag==1));


% determin the order of subbackbones to draw
centernees = [];
for k=1:length(side_chains)
    if ismember(side_chains{k}(1),back_bones)==0, break; end
    centernees(k) = abs(shortest_hop(side_chains{k}(1),back_bones(1)) - shortest_hop(side_chains{k}(1),back_bones(end)));
end
[dummy,I] = sort(centernees);
% determin the nodes location of each node in the side chains
for i=[I,k:length(side_chains)];
    for j=1:length(side_chains{i})
        if position_assigned_flag(side_chains{i}(j))==1
            continue;
        end
        new_node = side_chains{i}(j);
        attaching_node = find(adj(new_node,:)~=0 & position_assigned_flag==1);
        r = 0.3:0.04:0.9;
        theta = 0:2*pi/180:2*pi;
        potential_position_force = zeros(length(r),length(theta),2);
        for m = 1:length(r)
            for n = 1:length(theta)
                potential_position = node_positions(:,attaching_node) + r(m)*[cos(theta(n));sin(theta(n))];
                repel_vector = repmat(potential_position,1,sum(position_assigned_flag==1)) - node_positions(:,position_assigned_flag==1);
                repel_force = sum(repel_vector./repmat(sqrt(sum(repel_vector.^2,1)).^5,2,1),2);
%                 repel_force = sum(repel_vector./repmat(sqrt(sum(repel_vector.^2,1)).^5,2,1).*repmat(sum(adj(:,position_assigned_flag==1)~=0),2,1),2);
                
                cos_alfa = (repel_force'*[cos(theta(n));sin(theta(n))])/norm(repel_force);
                sin_alfa = sqrt(1-cos_alfa^2);
                potential_position_force(m,n,1) = norm(repel_force)*sin_alfa; % force perpendicular to string
                potential_position_force(m,n,2) = norm(repel_force)*cos_alfa; % force on string
            end
        end
        best_for_each_layer = []; best_string_force_each_layer=[];
        for m = 1:length(r)
            n_s = find(potential_position_force(m,:,2)>=0);
            if isempty(n_s), continue; end
            [dummy,I] = min(potential_position_force(m,n_s,1));
            best_for_each_layer = [best_for_each_layer;[m,n_s(I)]];
            best_string_force_each_layer = [best_string_force_each_layer; potential_position_force(m,n_s(I),2)];
        end
        [dummy,I] = min(best_string_force_each_layer);
        best_r = r(best_for_each_layer(I,1));
        best_theta = theta(best_for_each_layer(I,2));
        
        node_positions(:,new_node) = node_positions(:,attaching_node) + best_r*[cos(best_theta);sin(best_theta)];
        position_assigned_flag(side_chains{i}(j))=1;
        
        coeff = node_positions - repmat(median(node_positions,2),1,size(node_positions,2));
        coeff(1,:) = coeff(1,:)/max(abs(coeff(1,:)))*50;
        coeff(2,:) = coeff(2,:)/max(abs(coeff(2,:)))*50;
%         DrawTree(adj(position_assigned_flag==1,position_assigned_flag==1), coeff(:,position_assigned_flag==1)); drawnow
        fprintf('\b\b\b\b\b\b%6d', sum(position_assigned_flag==1));
    end
end
node_positions = node_positions - repmat(median(node_positions,2),1,size(node_positions,2));
node_positions(1,:) = node_positions(1,:)/max(abs(node_positions(1,:)))*50;
node_positions(2,:) = node_positions(2,:)/max(abs(node_positions(2,:)))*50;
fprintf('\n');

return

function DrawTree(adj, node_positions, node_sizes)

coeff = node_positions;

hold off; plot(0); hold on;
pairs = find_matrix_big_element(triu(adj,1),1);
for k=1:size(pairs,1), line(coeff(1,pairs(k,:)),coeff(2,pairs(k,:)),'color','g'); end
% % draw labels
% for k=1:size(coeff,2), text(coeff(1,k)+2,coeff(2,k),num2str(k),'FontSize',7); end

if exist('node_sizes')
    log_node_size = log10(node_size); log_node_size(log_node_size<=0) = min(log_node_size>0);
    draw_node_size = round(log_node_size/median(log_node_size)*8);
    draw_node_size(draw_node_size==0) = 1;
else
    draw_node_size = ones(1,size(coeff,2))*8;
end

for k=1:size(coeff,2), 
    plot(coeff(1,k),coeff(2,k),'o','markersize',draw_node_size(k),'markerfacecolor',[0 0 0],'color',[0 0 0]); 
end
hold off;
axis(reshape([-max(abs(coeff)');+max(abs(coeff)')],1,4)*1.1);

%%%%%%%%%%%%%%%%
function handles = replace_load_button(FlowSPD_input_filename,num_target_clusters)

    handles.data_filename = FlowSPD_input_filename;
    load(handles.data_filename)
    handles.data = data; 
    if exist('marker_names') && length(marker_names)==size(data,1)
        handles.marker_names = marker_names;
    else
        for i=1:size(data,1)
            marker_names(i,1) = {num2str(i)};
        end
        handles.marker_names = marker_names;
    end
    if exist('local_density')
        handles.local_density = local_density;
    else
        handles.local_density = ones(1,size(handles.data,2));
    end
    if ~exist('used_markers')
        handles.is_marker_selected = ones(1,size(data,1)); 
    else
        handles.is_marker_selected = zeros(1,size(data,1)); 
        handles.is_marker_selected(used_markers)=1;
    end
    handles.target_num_sample_groups = num_target_clusters;
    handles.sample_group_assign = [];
    handles.sample_group_progression_tree = [];
    handles.node_position = [];
    handles.group_shortest_hop=[];
    handles.progression_structure=[];
    handles.marker_similarity_matrix=[];
    handles.marker_tree_pvalue=[];
    
    % set save_filename
    if sum(handles.is_marker_selected)==size(data,1)
        handles.result_filename = [handles.data_filename(1:end-4),'_result_TargetGroups',num2str(handles.target_num_sample_groups),'_Markers_all.mat'];
    else
        tmp = num2str(find(handles.is_marker_selected==1)); tmp(strfind(tmp,'  '))='_'; tmp(tmp==' ')=[];
        handles.result_filename = [handles.data_filename(1:end-4),'_result_TargetGroups',num2str(handles.target_num_sample_groups),'_Markers',tmp,'.mat'];
    end
    
return



