function DrawTrees_SPDoutputFile(FlowSPD_output_filename,markers_to_draw,is_full_color_range,is_universal_scale,colormap_scheme,nocolorbars)
% the tree node size is based on the estimated number of cells in the
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

if ~exist('nocolorbars')
    nocolorbars = 0;
end


close all
warning off
mkdir('figs')

loaded_result_file = FlowSPD_output_filename;
load(loaded_result_file)
if ~exist('template'), template = []; end
downsampled_data = data;
downsampled_assign = sample_group_assign;
adj = sample_group_progression_tree;
coeff = node_position;
for i=1:size(adj,1),  node_size(i) = sum(local_density(downsampled_assign==i)); end

if exist('markers_to_draw') && ~isempty(markers_to_draw)
    if isnumeric(markers_to_draw)
        surf_ind = markers_to_draw;
    elseif iscell(markers_to_draw)
        [C,IA,IB] = intersect(marker_names, markers_to_draw);
        surf_ind = sort(IA);
    end
else
    surf_ind = 1:size(downsampled_data,1);
end

[cluster_median] = DrawTree_subfunc_get_node_median(downsampled_data, downsampled_assign); 
if is_full_color_range==1   % utilize full color range. 
    for i=1:size(cluster_median,1)
        cluster_median(i,:) = cluster_median(i,:) - (max(cluster_median(i,:))+min(cluster_median(i,:)))/2;
    end
end
max_node_intensity = max(max(abs(cluster_median(surf_ind,:))));
h1 = figure(1);
[nx,ny] = DrawTree_subfunc_get_subplot_grid(length(surf_ind));

if exist('is_universal_scale')  && is_universal_scale==1
    mkdir('figs\Figures_from_SPDoutputFile_UniScale\')
    for i=1:length(surf_ind)
        figure(1); subplot(nx,ny,i);
        [coeff] = DrawTree_subfunc_drawtree(adj,coeff,node_size,marker_names{surf_ind(i)},cluster_median(surf_ind(i),:), max_node_intensity,template, colormap_scheme, nocolorbars);
        h2 = figure(2); 
        [coeff] = DrawTree_subfunc_drawtree(adj,coeff,node_size,marker_names{surf_ind(i)},cluster_median(surf_ind(i),:), max_node_intensity,template, colormap_scheme, nocolorbars);
        print(h2,['figs\Figures_from_SPDoutputFile_UniScale\',remove_bad_filename_letters(marker_names{surf_ind(i)}),'_from_SPDoutputFile.jpg'],'-djpeg')
    end
    print(h1,['figs\Figures_from_SPDoutputFile_UniScale\markers_from_SPDoutputFile','.jpg'],'-djpeg')
else
    mkdir('figs\Figures_from_SPDoutputFile\')
    for i=1:length(surf_ind)
        figure(1); subplot(nx,ny,i);
        [coeff] = DrawTree_subfunc_drawtree(adj,coeff,node_size,marker_names{surf_ind(i)},cluster_median(surf_ind(i),:), [],template, colormap_scheme, nocolorbars);
        h2 = figure(2); 
        [coeff] = DrawTree_subfunc_drawtree(adj,coeff,node_size,marker_names{surf_ind(i)},cluster_median(surf_ind(i),:), [],template, colormap_scheme, nocolorbars);
        print(h2,['figs\Figures_from_SPDoutputFile\',remove_bad_filename_letters(marker_names{surf_ind(i)}),'_from_SPDoutputFile.jpg'],'-djpeg')
    end
    print(h1,['figs\Figures_from_SPDoutputFile\markers_from_SPDoutputFile','.jpg'],'-djpeg')
end


return


