function DrawTrees_SPDoutputUpsample(FlowSPD_output_filename, FlowSPD_output_upsample_filename, file_annot, markers_to_draw,is_full_color_range,is_universal_scale,colormap_scheme)
% the tree node size is based on the number of cells in the
% original files (assuming that each tube has equal number of cells) 


if ~exist('is_full_color_range') || ~isequal(is_full_color_range,1)
    is_full_color_range=0;
end

if ~exist('is_universal_scale') || ~isequal(is_universal_scale,1)
    is_universal_scale=0;
end

if ~exist('colormap_scheme')
    colormap_scheme = 'JET';
end



close all;
warning off
mkdir('figs')

loaded_result_file = FlowSPD_output_filename;
load(FlowSPD_output_filename)
if ~exist('template'), template = []; end
downsampled_data = data;
downsampled_assign = sample_group_assign;
adj = sample_group_progression_tree;
coeff = node_position;
for i=1:size(adj,1),  node_size(i) = sum(local_density(downsampled_assign==i)); end
load(FlowSPD_output_upsample_filename)


if exist('markers_to_draw') && ~isempty(markers_to_draw)
    if isnumeric(markers_to_draw)
        surf_ind = markers_to_draw;
    elseif iscell(markers_to_draw)
        [C,IA,IB] = intersect(all_marker_names, markers_to_draw);
        surf_ind = sort(IA);
    end
else
    surf_ind = 1:size(downsampled_data,1);
end


% get rid of the highest and lowest 1 prctile and take care of full/half
% color range
for i=1:size(node_median,1)
    for j=1:size(node_median,3)
        tmp = prctile(node_median(i,~isnan(node_median(i,:,j)),j),[1,99]);
        node_median(i,node_median(i,:,j)<=tmp(1),j) = tmp(1);
        node_median(i,node_median(i,:,j)>=tmp(2),j) = tmp(2);
        if is_full_color_range==1   % utilize full color range. 
            node_median(i,:,j) = node_median(i,:,j) - (max(node_median(i,:,j))+min(node_median(i,:,j)))/2;
        end
    end
end



mkdir('figs\Figures_from_SPDoutputUpsample\')
for i=1:length(surf_ind)
    for file_ind = 1:size(node_median,1)
        h2 = figure(2); 
        num_NaNs = sum(isnan(node_median(file_ind,:,surf_ind(i))));
        if num_NaNs==size(node_median,2)
            continue;
        elseif num_NaNs~=0 
            node_median(file_ind,:,surf_ind(i)) = DrawTree_subfunc_ClearNaNs_in_node_median(node_median(file_ind,:,surf_ind(i)),adj);
        end
        
        if is_universal_scale==1
            per_file_max = max(max(nanmax(abs(node_median(file_ind,:,:)))));
            [coeff] = DrawTree_subfunc_drawtree(adj,coeff,node_size,all_marker_names{surf_ind(i)},node_median(file_ind,:,surf_ind(i)), per_file_max,template, colormap_scheme);
        else
            [coeff] = DrawTree_subfunc_drawtree(adj,coeff,node_size,all_marker_names{surf_ind(i)},node_median(file_ind,:,surf_ind(i)), [],template, colormap_scheme);
        end
        
        set(h2,'paperunits','inches')
        set(h2,'paperorientation','portrait')
        set(h2,'papersize',[8,6]) % Desired outer dimensions of figure
        set(h2,'paperposition',[-0.5,0,8,6]) % Place plot on figure [dist from left, dist from bottom, width, height]
        print(h2,['figs\Figures_from_SPDoutputUpsample\',remove_bad_filename_letters(all_marker_names{surf_ind(i)}),'_',file_annot{file_ind},'_from_SPDoutputUpsample.pdf'],'-dpdf')
    end
end


return

