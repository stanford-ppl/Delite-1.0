function parfor_3_Prepare_DataForSPD(num_cells,PooledDataFilename,FlowSPD_input_filename,FileInds_used_in_SPD)

if ~exist('PooledDataFilename')
    PooledDataFilename = 'PooledDownsampledData.mat';
end
if ~exist('FlowSPD_input_filename')
    FlowSPD_input_filename = 'DataForSPD.mat';
end

load(PooledDataFilename); 

if exist('FileInds_used_in_SPD') && ~isempty(FileInds_used_in_SPD)
    is_keep = zeros(1,size(data,2));
    for i=1:length(FileInds_used_in_SPD)
        is_keep(data(end,:)==FileInds_used_in_SPD(i))=1;
    end
    data = data(:,is_keep==1);    
end

if size(data,2)<=num_cells
    keep_ind = 1:size(data,2);
else
    keep_ind = sort(randsample(1:size(data,2),num_cells));
end
data = data(:,keep_ind);
local_density = local_density(keep_ind);
save(FlowSPD_input_filename,'data', 'local_density', 'marker_names', 'used_markers');

display(' ')
display(['Prepared SPD input file: ',FlowSPD_input_filename,' (', num2str(size(data,2)),' cells)'])
display(' ')