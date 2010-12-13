function draw_trees_SurfMarkers_FuncMarkersStimUnstimRatio(FlowSPD_output_filename,FlowSPD_output_upsample_filename,stim,surf_ind,functional_ind,ref_files)

warning off
mkdir('figs')

loaded_result_file = FlowSPD_output_filename;

load(FlowSPD_output_upsample_filename,'node_median','all_assign');
load(loaded_result_file,'data','local_density','sample_group_assign','sample_group_progression_tree','node_position','marker_names')
downsampled_data = data;
downsampled_assign = sample_group_assign;
adj = sample_group_progression_tree;
coeff = node_position;
for i=1:size(adj,1),  node_size(i) = sum(local_density(downsampled_assign==i)); end



mkdir('figs\downsample_data_figures\')
[cluster_median] = get_cluster_median(downsampled_data, downsampled_assign); 
max_intensity = max(max(abs(cluster_median(surf_ind,:))));
h1 = figure(1); h2 = figure(2);
[nx,ny] = get_subplot_grid(length(surf_ind));
for i=1:length(surf_ind)
    figure(1); subplot(nx,ny,i);
    [coeff] = draw_on_Axes_mst_here2(adj,coeff,max_intensity, node_size,marker_names{surf_ind(i)},cluster_median(surf_ind(i),:));
    close(2); h2 = figure(2); 
    [coeff] = draw_on_Axes_mst_here2(adj,coeff,max_intensity, node_size,marker_names{surf_ind(i)},cluster_median(surf_ind(i),:));
%     h_tmp = h2; tmp = get(h_tmp,'OuterPosition'); tmp(3:4) = tmp(3:4)/2; set(h_tmp,'OuterPosition',tmp);
    print(h2,['figs\downsample_data_figures\downsample_data_surface_markers_',remove_bad_filename_letters(marker_names{surf_ind(i)}),'.jpg'],'-djpeg')
end
print(h1,['figs\downsample_data_figures\downsample_data_surface_markers','.jpg'],'-djpeg')

node_freq = zeros(length(all_assign),size(adj,2));
for file_ind = 1:size(node_freq,1)
    for node = 1:size(node_freq,2)
        node_freq(file_ind,node) = sum(all_assign{file_ind}==node);
    end
    node_freq(file_ind,:) = node_freq(file_ind,:)./sum(node_freq(file_ind,:));
end

case_files = setdiff(1:size(node_freq,1),ref_files);
[nx,ny] = get_subplot_grid(length(case_files));
h5 = figure(5); h6 = figure(6);
mkdir('figs\CellFreq\');
ref_freq = mean(node_freq(ref_files,:),1);
max_diff = 1;
for i=1:length(case_files)
    diff_signal = (node_freq(case_files(i),:) - ref_freq)./max([node_freq(case_files(i),:); ref_freq]);
    figure(5); subplot(nx,ny,i);
    [coeff] = draw_on_Axes_mst_here2(adj,coeff,max_diff,node_size,['ratio ',stim{case_files(i)},' vs ref'],diff_signal);
    close(6); h6 = figure(6); 
    [coeff] = draw_on_Axes_mst_here2(adj,coeff,max_diff,node_size,['ratio ',stim{case_files(i)},' vs ref'],diff_signal);
%     h_tmp = h6; tmp = get(h_tmp,'OuterPosition'); tmp(3:4) = tmp(3:4)/2; set(h_tmp,'OuterPosition',tmp);
    print(h6,['figs\CellFreq\FreqRatio_',remove_bad_filename_letters(stim{case_files(i)}),'_vs_Ref.jpg'],'-djpeg');
end
print(h5,['figs\CellFreq\FreqRatio_AllStim_vs_Ref','.jpg'],'-djpeg')

% node_median = clear_NaNs_in_node_median(node_median,adj);
% case_files = setdiff(1:size(node_median,1),ref_files);
% [nx,ny] = get_subplot_grid(length(case_files));
% for k=1:length(functional_ind)
%     close all;
%     h3 = figure(3);
%     marker_name_tmp = remove_bad_filename_letters(marker_names{functional_ind(k)});
%     mkdir(['figs\',marker_name_tmp,'\']);
%     node_median_this_marker = node_median(:,:,functional_ind(k));
%     ref_signal = mean(node_median_this_marker(ref_files,:),1);
%     diff_signal = node_median_this_marker(case_files,:) - repmat(ref_signal,length(case_files),1);
%     max_diff = max(max(abs(diff_signal)));
%     for i=1:length(case_files)
%         figure(3); subplot(nx,ny,i);
%         [coeff] = draw_on_Axes_mst_here2(adj,coeff,max_diff,node_size,stim{case_files(i)},diff_signal(i,:));
%         h4 = figure(4); 
%         [coeff] = draw_on_Axes_mst_here2(adj,coeff,max_diff,node_size,[marker_names{functional_ind(k)}, ' ratio ',stim{case_files(i)},' vs ref'],diff_signal(i,:));
%         print(h4,['figs\',marker_name_tmp,'\ratio_',marker_name_tmp,'_',remove_bad_filename_letters(stim{case_files(i)}),'.jpg'],'-djpeg');
%     end
%     print(h3,['figs\',marker_name_tmp,'\ratio_',marker_name_tmp,'_AllStim','.jpg'],'-djpeg')
% end
return



function [coeff] = draw_on_Axes_mst_here2(adj,coeff,max_intensity, node_size, marker_name, cluster_median) 
% draw edges

% get node size
draw_node_size =  round(node_size/max(node_size)*10); 
draw_node_size(draw_node_size<5)=5;
% get color code
color_code_vector = standardize_color_code_here(cluster_median);

hold off; plot(0); hold on;
pairs = find_matrix_big_element(triu(adj,1),1);
for k=1:size(pairs,1), line(coeff(1,pairs(k,:)),coeff(2,pairs(k,:)),'color','g'); end
% draw nodes
        %colormap(lbmapv2(1000,'blueyellow')); cmap_tmp = colormap;
        colormap('jet'); cmap_tmp = colormap;
        for k=1:size(coeff,2), 
            color_tmp = interp1(((1:size(cmap_tmp,1))'-1)/(size(cmap_tmp,1)-1),cmap_tmp,color_code_vector(k));  % blue-red        
            plot(coeff(1,k),coeff(2,k),'o','markersize',draw_node_size(k),'markerfacecolor',color_tmp,'color',color_tmp); 
        end
hold off;
axis(reshape([-max(abs(coeff)');+max(abs(coeff)')],1,4)*1.1); axis off;

x_shift = 50-max_intensity; y_shift = -50+max_intensity;
patch([-max_intensity,max_intensity,max_intensity,-max_intensity]+x_shift,[0,0,5,5]+y_shift,[1 1 1]); hold on; 
for i=-max_intensity:(max_intensity)/100:max_intensity
    % color code is standardized between 0 and 1, according to max(abs(cluster_median))
    if abs(i)>max(abs(cluster_median)), continue; end
    color_tmp = interp1(((1:size(cmap_tmp,1))'-1)/(size(cmap_tmp,1)-1),cmap_tmp,i/max(abs(cluster_median))/2+0.5);
    patch([i,i+(max_intensity)/50,i+(max_intensity)/50,i]+x_shift,[0,0,5,5]+y_shift,color_tmp,'edgecolor',color_tmp); 
end
text(-max_intensity+x_shift-3, 0+y_shift-3, num2str(round(-max_intensity*10)/10),'fontsize',7) 
text(+max_intensity+x_shift, 0+y_shift-3, num2str(round(max_intensity*10)/10),'fontsize',7) 
text(max(abs(cluster_median))+x_shift-2, 5+y_shift+2, num2str(round(max(abs(cluster_median))*10)/10),'fontsize',7) 
title(marker_name)
return



%%%%%%%%%%%%%%%%
function color_code_vector = standardize_color_code_here(color_code_vector)
        color_code_vector(isinf(color_code_vector)) = min(color_code_vector(~isinf(color_code_vector)));
        if sum(isnan(color_code_vector))~=0  % this will never happen
            color_code_vector(isnan(color_code_vector)) = nanmedian(color_code_vector);
        end
        color_code_vector = ((color_code_vector ./ max(abs(color_code_vector)) + 1)/2);
return





%%%%%%%%%%%%%%%%%
function [cluster_median] = get_cluster_median(data, idx)

cluster_median = zeros(size(data,1),max(idx))+NaN;
for i=1:max(idx)
    ind = find(idx==i);
    if ~isempty(ind)
        cluster_median(:,i) = median(data(:,ind),2);
    end
end
return




%%%
function [nx,ny] = get_subplot_grid(num_plots)
subplot_grid = [1, 1; ...
                1, 2; ...
                2, 2; ...
                2, 2; ...
                2, 3; ...
                2, 3; ...
                2, 4; ...
                3, 3; ...
                3, 3; ...
                3, 4; ...
                3, 4; ...
                3, 4; ...
                3, 5; ...
                3, 5; ...
                3, 5; ...
                4, 4; ...
                4, 5; ...
                4, 5; ...
                4, 5; ...
                4, 5; ...
                4, 6; ...
                4, 6; ...
                4, 6; ...
                4, 6; ...
                5, 5; ...
                5, 6; ...
                5, 6; ...
                5, 6; ...
                5, 6; ...
                5, 6; ...
                5, 7; ...
                5, 7; ...
                5, 7; ...
                5, 7; ...
                5, 7; ...
                5, 8; ...
                5, 8; ...
                5, 8; ...
                5, 8; ...
                5, 8; ...
                6, 8; ...
                6, 8; ...
                6, 8; ...
                6, 8; ...
                6, 8; ...
                6, 8; ...
                6, 8; ...
                6, 8; ];                
if num_plots>size(subplot_grid,1)
    error('Error: ask Peng how to fix this, or, look at the above variable in this function and figure it out yourself :)');
    return
end
nx = subplot_grid(num_plots,1);
ny = subplot_grid(num_plots,2);
return


%%%%%%%%%%%%%%%%%
function somename = remove_bad_filename_letters(somename)
somename = somename((somename>=48 & somename<=57) | (somename>=65 & somename<=90) | (somename>=97 & somename<=122) | (somename=='_'));
return



%%%%%%%%%%%%%%%%%
function node_median = clear_NaNs_in_node_median(node_median,adj)

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







% %%% this part is useless now
% function [coeff] = draw_on_Axes_mst_here(adj,coeff,max_intensity, data, marker_name, assign,local_density) 
% % get node size
% for i=1:size(adj,1), 
%     if exist('local_density') && length(local_density)==length(data)
%         node_size(i) = sum(local_density(assign==i)); 
%     else
%         node_size(i) = sum(assign==i); 
%     end
% end
% draw_node_size =  round(node_size/max(node_size)*10); 
% draw_node_size(draw_node_size<3)=3;
% % get color code
% [cluster_median] = get_cluster_median(data, assign);
% color_code_vector = standardize_color_code_here(cluster_median);
% 
% hold off; plot(0); hold on;
% pairs = find_matrix_big_element(triu(adj,1),1);
% for k=1:size(pairs,1), line(coeff(1,pairs(k,:)),coeff(2,pairs(k,:)),'color','g'); end
% % draw nodes
%         %colormap(lbmapv2(1000,'blueyellow')); cmap_tmp = colormap;
%         colormap('jet'); cmap_tmp = colormap;
%         for k=1:size(coeff,2), 
%             color_tmp = interp1(((1:size(cmap_tmp,1))'-1)/(size(cmap_tmp,1)-1),cmap_tmp,color_code_vector(k));  % blue-red        
%             plot(coeff(1,k),coeff(2,k),'o','markersize',draw_node_size(k),'markerfacecolor',color_tmp,'color',color_tmp); 
%         end
% hold off;
% axis(reshape([-max(abs(coeff)');+max(abs(coeff)')],1,4)*1.1); axis off;
% 
% x_shift = 50-max_intensity; y_shift = -50+max_intensity;
% patch([-max_intensity,max_intensity,max_intensity,-max_intensity]+x_shift,[0,0,5,5]+y_shift,[1 1 1]); hold on; 
% for i=-max_intensity:(max_intensity)/100:max_intensity
%     % color code is standardized between 0 and 1, according to max(abs(cluster_median))
%     if abs(i)>max(abs(cluster_median)), continue; end
%     color_tmp = interp1(((1:size(cmap_tmp,1))'-1)/(size(cmap_tmp,1)-1),cmap_tmp,i/max(abs(cluster_median))/2+0.5);
%     patch([i,i+(max_intensity)/50,i+(max_intensity)/50,i]+x_shift,[0,0,5,5]+y_shift,color_tmp,'edgecolor',color_tmp); 
% end
% text(-max_intensity+x_shift-3, 0+y_shift-3, num2str(round(-max_intensity*10)/10),'fontsize',7) 
% text(+max_intensity+x_shift, 0+y_shift-3, num2str(round(max_intensity*10)/10),'fontsize',7) 
% text(max(abs(cluster_median))+x_shift-2, 5+y_shift+2, num2str(round(max(abs(cluster_median))*10)/10),'fontsize',7) 
% title(marker_name)
% return



