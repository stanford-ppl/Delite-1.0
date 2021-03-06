function DrawTrees_UnstimStimRatio(FlowSPD_output_filename,FlowSPD_output_upsample_filename,file_annot,markers_to_draw, ref_files_ind, is_universal_scale, colormap_scheme)


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
        ind = markers_to_draw;
    elseif iscell(markers_to_draw)
        [C,IA,IB] = intersect(all_marker_names, markers_to_draw);
        ind = sort(IA);
    end
else
    ind = 1:size(downsampled_data,1);
end


case_files = setdiff(1:size(node_median,1),ref_files_ind);
[nx,ny] = DrawTree_subfunc_get_subplot_grid(length(case_files));
if exist('is_universal_scale')  && is_universal_scale==1
    mkdir('figs\Figures_RatioTrees_UniScale\')
    mkdir('figs\Figures_RatioTrees_UniScale\all_RatioTrees\')
    mkdir('figs\Figures_RatioTrees_UniScale\Compact_subplots\')
else
    mkdir('figs\Figures_RatioTrees\')
    mkdir('figs\Figures_RatioTrees\all_RatioTrees\')
    mkdir('figs\Figures_RatioTrees\Compact_subplots\')
end

for k=1:length(ind)  % for each markre to draw
    close all;
    
    h4 = figure(4);
    marker_name_tmp = remove_bad_filename_letters(all_marker_names{ind(k)});
    if exist('is_universal_scale')  && is_universal_scale==1
        mkdir(['figs\Figures_RatioTrees_UniScale\',marker_name_tmp,'\']);
    else
        mkdir(['figs\Figures_RatioTrees\',marker_name_tmp,'\']);
    end
    node_median_this_marker = node_median(:,:,ind(k));
    ref_signal = mean(node_median_this_marker(ref_files_ind,:),1);
    diff_signal = node_median_this_marker(case_files,:) - repmat(ref_signal,length(case_files),1);
    max_diff = prctile(abs(diff_signal(:)),98);
    for i=1:length(case_files)
        num_NaNs = sum(isnan(diff_signal(i,:)));
        if num_NaNs==size(diff_signal,2)
            disp([all_marker_names{ind(k)},' does not exist in ref or ',file_annot{case_files(i)}]);
            continue;
        elseif num_NaNs~=0 
            diff_signal(i,:) = DrawTree_subfunc_ClearNaNs_in_node_median(diff_signal(i,:),adj);
        end
        figure(4); subplot(nx,ny,i);
        if exist('is_universal_scale')  && is_universal_scale==1
            [coeff] = DrawTree_subfunc_drawtree(adj,coeff,node_size,[all_marker_names{ind(k)},' - ',file_annot{case_files(i)},' vs Ref'],diff_signal(i,:), max_diff,template, colormap_scheme);
            h5 = figure(5); 
            [coeff] = DrawTree_subfunc_drawtree(adj,coeff,node_size,[all_marker_names{ind(k)},' --- ',file_annot{case_files(i)},' vs Ref Ratio'],diff_signal(i,:), max_diff,template, colormap_scheme);
            %%%%%%%%%%%%%%%%%%%%%  ES-Start  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            set(h5,'paperunits','inches')
            set(h5,'paperorientation','portrait')
            set(h5,'papersize',[8,6]) % Desired outer dimensions of figure
            set(h5,'paperposition',[-0.5,0,8,6]) % Place plot on figure [dist from left, dist from bottom, width, height]

            print(h5,['figs\Figures_RatioTrees_UniScale\',marker_name_tmp,'\',marker_name_tmp,'_',remove_bad_filename_letters(file_annot{case_files(i)}),'vsRefRatio.pdf'],'-dpdf');
            print(h5,['figs\Figures_RatioTrees_UniScale\all_RatioTrees\',marker_name_tmp,'_',remove_bad_filename_letters(file_annot{case_files(i)}),'vsRefRatio.pdf'],'-dpdf');
            %%%%%%%%%%%%%%%%%%%%  ES-End  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        else
            [coeff] = DrawTree_subfunc_drawtree(adj,coeff,node_size,[all_marker_names{ind(k)},' - ',file_annot{case_files(i)},' vs Ref'],diff_signal(i,:), [],template, colormap_scheme);
            h5 = figure(5); 
            [coeff] = DrawTree_subfunc_drawtree(adj,coeff,node_size,[all_marker_names{ind(k)},' --- ',file_annot{case_files(i)},' vs Ref Ratio'],diff_signal(i,:), [],template, colormap_scheme);
            %%%%%%%%%%%%%%%%%%%%%  ES-Start  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            set(h5,'paperunits','inches')
            set(h5,'paperorientation','portrait')
            set(h5,'papersize',[8,6]) % Desired outer dimensions of figure
            set(h5,'paperposition',[-0.5,0,8,6]) % Place plot on figure [dist from left, dist from bottom, width, height]

            print(h5,['figs\Figures_RatioTrees\',marker_name_tmp,'\',marker_name_tmp,'_',remove_bad_filename_letters(file_annot{case_files(i)}),'vsRefRatio.pdf'],'-dpdf');
            print(h5,['figs\Figures_RatioTrees\all_RatioTrees\',marker_name_tmp,'_',remove_bad_filename_letters(file_annot{case_files(i)}),'vsRefRatio.pdf'],'-dpdf');
            %%%%%%%%%%%%%%%%%%%%  ES-End  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        end
    end
    if exist('is_universal_scale')  && is_universal_scale==1
        %%%%%%%%%%%%%%%%%%%%%  ES-Start  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        set(h4,'paperunits','inches')
        set(h4,'paperorientation','portrait')
        set(h4,'papersize',[8,6]) % Desired outer dimensions of figure
        set(h4,'paperposition',[-0.5,0,8,6]) % Place plot on figure [dist from left, dist from bottom, width, height]

        print(h4,['figs\Figures_RatioTrees_UniScale\all_RatioTrees\',marker_name_tmp,'_AllStimvsRefRatio','.pdf'],'-dpdf')
        print(h4,['figs\Figures_RatioTrees_UniScale\Compact_subplots\',marker_name_tmp,'_AllStimvsRefRatio','.pdf'],'-dpdf')
        print(h4,['figs\Figures_RatioTrees_UniScale\',marker_name_tmp,'\',marker_name_tmp,'_AllStimvsRefRatio','.pdf'],'-dpdf')
        %%%%%%%%%%%%%%%%%%%%  ES-End %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    else
        
        %%%%%%%%%%%%%%%%%%%%%  ES-Start  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        set(h4,'paperunits','inches')
        set(h4,'paperorientation','portrait')
        set(h4,'papersize',[8,6]) % Desired outer dimensions of figure
        set(h4,'paperposition',[-0.5,0,8,6]) % Place plot on figure [dist from left, dist from bottom, width, height]

        print(h4,['figs\Figures_RatioTrees\all_RatioTrees\',marker_name_tmp,'_AllStimvsRefRatio','.pdf'],'-dpdf')
        print(h4,['figs\Figures_RatioTrees\Compact_subplots\',marker_name_tmp,'_AllStimvsRefRatio','.pdf'],'-dpdf')
        print(h4,['figs\Figures_RatioTrees\',marker_name_tmp,'\',marker_name_tmp,'_AllStimvsRefRatio','.pdf'],'-dpdf')
        %%%%%%%%%%%%%%%%%%%%  ES-End %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    end
        
end


return

