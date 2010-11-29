function [coeff] = DrawTree_subfunc_drawtree_HybridTissueMarker(adj,coeff, node_size, marker_name, node_color, template) 

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


hold off; plot(0); hold on;
pairs = find_matrix_big_element(triu(adj,1),1);
for k=1:size(pairs,1), line(coeff(1,pairs(k,:)),coeff(2,pairs(k,:)),'color',[0.9 0.9 0.9]); end
% draw nodes
    for k=1:size(coeff,2), 
        plot(coeff(1,k),coeff(2,k),'o','markersize',draw_node_size(k),'markerfacecolor',node_color(k,:),'color',node_color(k,:)); 
    end
hold off;
% axis(reshape([-max(abs(coeff)');+max(abs(coeff)')],1,4)*1.1); axis off;
axis_lims = reshape([-max(abs(coeff)');+max(abs(coeff)')],1,4)*1.1;
for i=1:4
    if abs(axis_lims(i))<55
        if axis_lims(i)>=0
            axis_lims(i)=55;
        else
            axis_lims(i)= -55;
        end
    end
end
axis(axis_lims);axis off;
marker_name(marker_name=='_')=' ';
title(marker_name)
return

