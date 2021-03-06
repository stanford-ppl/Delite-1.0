function [coeff] = DrawTree_subfunc_drawtree(adj,coeff, node_size, marker_name, cluster_median, normalization_max, template, colormap_scheme,nocolorbars) 

hold off; plot(0,'visible','off'); hold on;

if exist('template') && ~isempty(template)
    for i=1:length(template.BoxCenter_x)
        coordinates = [template.BoxCenter_x(i)-template.BoxWidth(i)/2,   template.BoxCenter_x(i)+template.BoxWidth(i)/2,  template.BoxCenter_x(i)+template.BoxWidth(i)/2,   template.BoxCenter_x(i)-template.BoxWidth(i)/2; ...
                       template.BoxCenter_y(i)-template.BoxHeight(i)/2,  template.BoxCenter_y(i)-template.BoxHeight(i)/2, template.BoxCenter_y(i)+template.BoxHeight(i)/2,  template.BoxCenter_y(i)+template.BoxHeight(i)/2];
        patch(coordinates(1,:),coordinates(2,:), reshape(template.BoxColor(:,i),1,3));
    end
end

% % get node size
draw_node_size = flow_arcsinh(node_size,median(node_size)/2);
draw_node_size = ceil(draw_node_size/max(draw_node_size)*10);
draw_node_size(draw_node_size<5)=5;
% get color code
[color_code_vector,normalization_max]= standardize_color_code_here(cluster_median,normalization_max);

pairs = find_matrix_big_element(triu(adj,1),1);
% uncomment next line for green edges
% for k=1:size(pairs,1), line(coeff(1,pairs(k,:)),coeff(2,pairs(k,:)),'color','g'); end
% uncomment next line for gray edges
for k=1:size(pairs,1), line(coeff(1,pairs(k,:)),coeff(2,pairs(k,:)),'color', 0.5*ones(1,3)); end

% draw nodes

if length(unique(color_code_vector))==1
        for k=1:size(coeff,2), 
            plot(coeff(1,k),coeff(2,k),'o','markersize',draw_node_size(k),'markerfacecolor',ones(1,3)*0.2,'color',ones(1,3)*0.2); 
        end
else
% ES Edit -- this allows the colormap name to be passed directly to lbmapv2
        if ~exist('colormap_scheme')
            colormap_scheme = 'JET';
            colormap('jet'); cmap_tmp = colormap;
        end
        if isequal(upper(colormap_scheme),upper('JET'))
            colormap('jet'); cmap_tmp = colormap;
        else
            cmap_tmp = lbmapv2(1000,colormap_scheme);
        end
        for k=1:size(coeff,2), 
            color_tmp = interp1(((1:size(cmap_tmp,1))'-1)/(size(cmap_tmp,1)-1),cmap_tmp,color_code_vector(k));  % blue-red        
            plot(coeff(1,k),coeff(2,k),'o','markersize',draw_node_size(k),'markerfacecolor',color_tmp,'color',color_tmp); 
        end
end           
hold off;
axis_lims = reshape([-max(abs(coeff)');+max(abs(coeff)')],1,4)*1.1;
for i=1:4
    if abs(axis_lims(i))<56
        if axis_lims(i)>=0
            axis_lims(i)=56;
        else
            axis_lims(i)= -56;
        end
    end
end
axis(axis_lims);axis off;

if length(unique(color_code_vector))==1
    return
end

if exist('nocolorbars') && nocolorbars==1
    return
end

colorbar_half_len = 9;
x_shift = 50-colorbar_half_len; y_shift = -50+colorbar_half_len;
patch([-colorbar_half_len,colorbar_half_len,colorbar_half_len,-colorbar_half_len]+x_shift,[0,0,5,5]+y_shift,[1 1 1]); hold on; 
for i=-colorbar_half_len:(colorbar_half_len)/100:colorbar_half_len
    % color code is standardized between 0 and 1, according to max(abs(cluster_median))
    color_tmp = interp1(((1:size(cmap_tmp,1))'-1)/(size(cmap_tmp,1)-1),cmap_tmp,i/colorbar_half_len/2+0.5);
    patch([i,i+(colorbar_half_len)/50,i+(colorbar_half_len)/50,i]+x_shift,[0,0,5,5]+y_shift,color_tmp,'edgecolor',color_tmp); 
end
text(-colorbar_half_len+x_shift-3, 0+y_shift-3, num2str(round(-normalization_max*10)/10),'fontsize',7) 
text(+colorbar_half_len+x_shift,   0+y_shift-3, num2str(round(normalization_max*10)/10),'fontsize',7) 
% text(max(abs(cluster_median))+x_shift-2, 5+y_shift+2, num2str(round(max(abs(cluster_median))*10)/10),'fontsize',7) 
marker_name(marker_name=='_')=' ';
title(marker_name,'color','b')
return



%%%%%%%%%%%%%%%%
function [color_code_vector,normalization_max] = standardize_color_code_here(color_code_vector,normalization_max)

        color_code_vector(isinf(color_code_vector)) = min(color_code_vector(~isinf(color_code_vector)));
        if sum(isnan(color_code_vector))~=0  % this will never happen
            color_code_vector(isnan(color_code_vector)) = nanmedian(color_code_vector);
        end
        if exist('normalization_max') && ~isempty(normalization_max)
            color_code_vector = ((color_code_vector / normalization_max + 1)/2);
        else
            [Y,I] = sort(abs(color_code_vector),'descend');
            normalization_max = Y(2);
            if normalization_max==0
                normalization_max=1;
            end
            color_code_vector = ((color_code_vector / normalization_max + 1)/2);
        end        
        color_code_vector(color_code_vector<=0)=0;
        color_code_vector(color_code_vector>=1)=1;
return

