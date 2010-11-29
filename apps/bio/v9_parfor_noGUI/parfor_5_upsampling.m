function [FlowSPD_output_upsample_filename] = parfor_5_upsampling(FlowSPD_output_filename,all_filenames)

data=[]; sample_group_assign=[];sample_group_progression_tree=[];is_marker_selected=[];

loaded_result_file = FlowSPD_output_filename;
FlowSPD_output_upsample_filename = [loaded_result_file(1:end-4),'_node_medians'];

load(loaded_result_file,'data','sample_group_assign','sample_group_progression_tree','is_marker_selected');
downsampled_data = data;
downsampled_assign = sample_group_assign;
adj = sample_group_progression_tree;

% define the node_median matrix
load([all_filenames{1}(1:end-4),'.mat'],'data');
node_median = zeros(length(all_filenames),size(adj,1),size(data,1));

matlabpool
for file_ind = 1:length(all_filenames)
    tic
%     load([loaded_result_file(1:end-4),'_node_medians'],'node_median','all_assign');
    load([all_filenames{file_ind}(1:end-4),'.mat'],'data')
    assign = zeros(1,size(data,2));
%     fprintf(['mapping/gating ',all_filenames{file_ind},' ... %d cells\n %10d'],length(assign),0);
    fprintf(['mapping/gating ',all_filenames{file_ind},' ... %d cells\n'],length(assign));
    tmp = zeros(1,size(adj,1));
    
    ind = find(is_marker_selected==1);
    parfor i=1:size(data,2)
        if assign(i)~=0, continue; end
        dist = sum(abs(downsampled_data(ind,:) - repmat(data(ind,i),1,size(downsampled_data,2))));
        [Y,I] = sort(dist);
        if downsampled_assign(I(1))==downsampled_assign(I(2))
            assign(i) = downsampled_assign(I(1)); 
        elseif Y(1)<Y(2)/2
            assign(i) = downsampled_assign(I(1)); 
        else
            assign(i) = -downsampled_assign(I(1));
        end
%         fprintf('\b\b\b\b\b\b\b\b\b\b%10d',i); drawnow;
    end
%     fprintf('\n');
    all_assign{file_ind} = assign;

    for i=1:size(adj,1)
        if sum(abs(assign)==i)==0
            node_median(file_ind,i,:)=NaN;
        else
            node_median(file_ind,i,:)=reshape(median(data(:,abs(assign)==i) ,2) , 1,1,size(node_median,3));
        end
    end
    save(FlowSPD_output_upsample_filename,'node_median','all_assign');
    toc
end

matlabpool close
return


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


