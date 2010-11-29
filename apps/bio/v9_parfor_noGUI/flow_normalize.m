function data = flow_normalize(data, normalize_weight_factor)

for i=1:size(data,1)
    [ll] = prctile(data(i,:),5);
    [uu] = prctile(data(i,:),95);
    data(i,:) = (data(i,:)-ll)./(uu-ll)*5;
end

if exist('normalize_weight_factor')
    for i=1:size(data,1)
        data(i,:) = data(i,:) * normalize_weight_factor(i);
    end
end
return
