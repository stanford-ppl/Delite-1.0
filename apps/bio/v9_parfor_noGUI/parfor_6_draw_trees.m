function parfor_6_draw_trees(FlowSPD_output_filename,FlowSPD_output_upsample_filename,stim,surf_ind,functional_ind)

mkdir figs

loaded_result_file = FlowSPD_output_filename;

load(FlowSPD_output_upsample_filename,'node_median','all_assign');
load(loaded_result_file,'data','local_density','sample_group_assign','sample_group_progression_tree','node_position','marker_names')
downsampled_data = data;
downsampled_assign = sample_group_assign;
adj = sample_group_progression_tree;
coeff = node_position;
for i=1:size(adj,1),  node_size(i) = sum(local_density(downsampled_assign==i)); end

[cluster_median] = get_cluster_median(downsampled_data, downsampled_assign); 
max_intensity = max(max(abs(cluster_median(surf_ind,:))));
h = figure(1);
for i=1:length(surf_ind)
    subplot(3,5,i);
    [coeff] = draw_on_Axes_mst_here(adj,coeff,max_intensity, downsampled_data(surf_ind(i),:),marker_names{surf_ind(i)},downsampled_assign,local_density); 
    drawnow
end
print(h,['figs/downsample_data_surface_markers','.jpg'],'-djpeg')



for i=[functional_ind]
    h = figure(3)
    max_intensity = max(max(max(abs(node_median(:,:,i) - repmat(node_median(1,:,i),size(node_median,1),1)))));
    for file_ind=1:6
        subplot(2,3,file_ind)
        if file_ind==1, title(marker_names{i}); axis off; continue; end
        [coeff] = draw_on_Axes_mst_here2(adj,coeff,max_intensity,node_size,marker_names{i},node_median(file_ind,:,i)-node_median(1,:,i)); 
        if file_ind~=1, title(stim{file_ind}); end
    end
    print(h,['figs/unstim_stim_ratio_',marker_names{i}(marker_names{i}~='/'),'.jpg'],'-djpeg')
end

return


function [coeff] = draw_on_Axes_mst_here2(adj,coeff,max_intensity, node_size, marker_name, cluster_median) 
% draw edges

% get node size
draw_node_size =  round(node_size/max(node_size)*10); 
draw_node_size(draw_node_size<3)=3;
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



%%%
function [coeff] = draw_on_Axes_mst_here(adj,coeff,max_intensity, data, marker_name, assign,local_density) 
% draw edges

% get node size
for i=1:size(adj,1), 
    if exist('local_density') && length(local_density)==length(data)
        node_size(i) = sum(local_density(assign==i)); 
    else
        node_size(i) = sum(assign==i); 
    end
end
draw_node_size =  round(node_size/max(node_size)*10); 
draw_node_size(draw_node_size<3)=3;
% get color code
[cluster_median] = get_cluster_median(data, assign);
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


function color_code_vector = standardize_color_code_here(color_code_vector)
        color_code_vector(isinf(color_code_vector)) = min(color_code_vector(~isinf(color_code_vector)));
        if sum(isnan(color_code_vector))~=0  % this will never happen
            color_code_vector(isnan(color_code_vector)) = nanmedian(color_code_vector);
        end
        color_code_vector = ((color_code_vector ./ max(abs(color_code_vector)) + 1)/2);
return



%%%
function map_downsampled_back_to_orignial(loaded_result_file, all_filenames)

load(loaded_result_file);
downsampled_data = data;
downsampled_assign = sample_group_assign;
adj = sample_group_progression_tree;

node_median = zeros(length(all_filenames),size(adj,1),size(data,1)-1);

for file_ind = 1:length(all_filenames)
%     load([loaded_result_file(1:end-4),'_node_medians'],'node_median','all_assign');
    load([all_filenames{file_ind}(1:end-4)],'data')
    assign = zeros(1,size(data,2));
    fprintf(['mapping/gating ',all_filenames{file_ind},' ... %d cells\n %10d'],length(assign),0);
    tmp = zeros(1,size(adj,1));
% 
% %     tmp1=[];tmp2=[];
% %     for i=1:size(downsampled_data,2) 
% %         dist = sum(abs(downsampled_data(is_marker_selected==1,:) - repmat(downsampled_data(is_marker_selected==1,i),1,size(downsampled_data,2))));
% %         [Y,I] = sort(dist); 
% %         first_other_group_neighbor = find(downsampled_assign(I)~=downsampled_assign(i),1);
% %         dist_threshold = Y(first_other_group_neighbor)/3;
% %         dist = sum(abs(data(is_marker_selected==1,:) - repmat(downsampled_data(is_marker_selected==1,i),1,size(data,2))));
% %         if  ~isempty(setdiff(unique(assign(dist<dist_threshold)),[0,downsampled_assign(i)]))
% %             1; %% something wrong
% %         end
% %         assign(dist<dist_threshold) = downsampled_assign(i); 
% %         fprintf('\b\b\b\b\b\b\b\b\b\b%10d',-sum(assign~=0));
% %         tmp(downsampled_assign(i)) = tmp(downsampled_assign(i)) + sum(dist<dist_threshold);
% %         figure(file_ind);semilogy(tmp+1,'o'); axis tight; drawnow
% %         figure(9); tmp1 = [tmp1,dist_threshold]; plot(tmp1);drawnow
% %         figure(10); tmp2 = [tmp2,sum(dist<dist_threshold)]; plot(tmp2);drawnow
% %     end
% 
% 


% %     tmp1=[];tmp2=[];
    for i=1:size(data,2)
        if assign(i)~=0, continue; end
        dist = sum(abs(downsampled_data(is_marker_selected==1,:) - repmat(data(is_marker_selected==1,i),1,size(downsampled_data,2))));
        [Y,I] = sort(dist);
        if downsampled_assign(I(1))==downsampled_assign(I(2))
            assign(i) = downsampled_assign(I(1)); tmp(downsampled_assign(I(1))) = tmp(downsampled_assign(I(1))) + 1;
        elseif Y(1)<Y(2)/2
            assign(i) = downsampled_assign(I(1)); tmp(downsampled_assign(I(1))) = tmp(downsampled_assign(I(1))) + 1;
        else
            assign(i) = -downsampled_assign(I(1));
        end
        fprintf('\b\b\b\b\b\b\b\b\b\b%10d',i);
%         figure(file_ind);semilogy(tmp+1,'o'); axis tight; drawnow
%         figure(9); tmp1 = [tmp1,downsampled_data(end,I(1))*sign(assign(i))]; plot(tmp1);drawnow
%         figure(10); tmp2 = [tmp2,find(downsampled_assign(I)~=downsampled_assign(I(1)),1)]; plot(tmp2);ylim([0,10]);drawnow
    end
    fprintf('\n');
    all_assign{file_ind} = assign;

    for i=1:size(adj,1)
        if sum(assign==i)==0
            node_median(file_ind,i,:)=NaN;
        else
            node_median(file_ind,i,:)=reshape(median(data(:,assign==i) ,2) , 1,1,size(node_median,3));
        end
    end
    save([loaded_result_file(1:end-4),'_node_medians'],'node_median','all_assign');
    
end



%%%
function [cluster_median] = get_cluster_median(data, idx)

cluster_median = zeros(size(data,1),max(idx))+NaN;
for i=1:max(idx)
    ind = find(idx==i);
    if ~isempty(ind)
        cluster_median(:,i) = median(data(:,ind),2);
    end
end
return

