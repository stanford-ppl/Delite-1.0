function show_2D_clustering(x,y,idx)

colormap('default'); cmap_tmp = colormap;
hold off;
for i=1:max(idx) 
    color_tmp = interp1(((1:size(cmap_tmp,1))'-1)/(size(cmap_tmp,1)-1),cmap_tmp,i/max(idx));  % blue-red        
    plot(x(idx==i),y(idx==i),'.','markerfacecolor',color_tmp,'color',color_tmp);
    if i==1
        hold on;
    end
end

