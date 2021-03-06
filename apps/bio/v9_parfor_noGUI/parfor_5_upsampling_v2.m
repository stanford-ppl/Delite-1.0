function [FlowSPD_output_upsample_filename] = parfor_5_upsampling_v2(FlowSPD_output_filename,all_filenames)

data=[]; sample_group_assign=[];sample_group_progression_tree=[];is_marker_selected=[];

loaded_result_file = FlowSPD_output_filename;
FlowSPD_output_upsample_filename = [loaded_result_file(1:end-4),'_node_medians'];

load(loaded_result_file,'data','sample_group_assign','sample_group_progression_tree','is_marker_selected','marker_names');
downsampled_data = data;
downsampled_marker_names = marker_names;
downsampled_assign = sample_group_assign;
adj = sample_group_progression_tree;
used_marker_names = sort(marker_names(is_marker_selected==1));
[C, ind_downsampled_data, IB] = intersect(downsampled_marker_names,used_marker_names);

node_median = NaN + zeros(length(all_filenames),size(adj,1),length(downsampled_marker_names));

matlabpool
for file_ind = 1:length(all_filenames)
    tic
    load([all_filenames{file_ind}(1:end-4),'.mat'],'data','marker_names');
    [C, ind_original_data, IB] = intersect(marker_names,used_marker_names);
    assign = zeros(1,size(data,2));
    fprintf(['mapping/gating ',all_filenames{file_ind},' ... %d cells\n %10d'],length(assign),0);
    tmp = zeros(1,size(adj,1));
    
    block_size = 1000;   % after a few try and error, 1000 seems to be the best block size in terms of run time. 
    for k=1:block_size:size(data,2)
        data_tmp = data(:,k:min(k+block_size-1,size(data,2)));
        assign_tmp = zeros(1,size(data_tmp,2));
        dist = zeros(size(data_tmp,2),size(downsampled_data,2));
        parfor i=1:size(dist,2)
            dist(:,i) = sum(abs(data_tmp(ind_original_data,:) - repmat(downsampled_data(ind_downsampled_data,i),1,size(data_tmp,2))))';
        end
        parfor i=1:size(data_tmp,2)
            [Y,I] = sort(dist(i,:));
            if downsampled_assign(I(1))==downsampled_assign(I(2))
                assign_tmp(i) = downsampled_assign(I(1)); 
            elseif Y(1)<Y(2)/2
                assign_tmp(i) = downsampled_assign(I(1)); 
            else
                assign_tmp(i) = -downsampled_assign(I(1));
            end
        end
        assign(k:k+length(assign_tmp)-1) = assign_tmp;
        fprintf('\b\b\b\b\b\b\b\b\b\b%10d',min(k+block_size-1,size(data,2))); drawnow;
    end
    fprintf('\n');
    all_assign{file_ind} = assign;

    
    [C,IA,IB] = intersect(downsampled_marker_names,marker_names);
    for i=1:size(adj,1)
        if sum(abs(assign)==i)==0
            node_median(file_ind,i,:)=NaN;
        else
            node_median(file_ind,i,IA)=reshape(nanmedian(data(IB,abs(assign)==i) ,2) , 1,1,length(IB));
        end
    end
    save(FlowSPD_output_upsample_filename,'node_median','all_assign');
    toc
end

matlabpool close
all_marker_names = downsampled_marker_names;
save(FlowSPD_output_upsample_filename, 'all_marker_names', '-append')
return

% 
% %%%
% function [cluster_median] = get_cluster_median(data, idx)
% 
% cluster_median = zeros(size(data,1),max(idx))+NaN;
% for i=1:max(idx)
%     ind = find(idx==i);
%     if ~isempty(ind)
%         cluster_median(:,i) = median(data(:,ind),2);
%     end
% end
% return
% 
% 
