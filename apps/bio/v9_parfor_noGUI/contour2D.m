function contour2D(x,y)

[F,XI,hx] = ksdensity(x); [F,YI,hy] = ksdensity(y);
Z = zeros(length(XI),length(YI));
dist_XI_x = exp(-(repmat(XI(:),1,length(x))-repmat(x(:)',length(XI),1)).^2/2/hx^2);
dist_YI_y = exp(-(repmat(YI(:),1,length(y))-repmat(y(:)',length(YI),1)).^2/2/hy^2);
Z = (dist_XI_x*dist_YI_y')';
Z = Z./(sqrt(2*pi*hx^2*2*pi*hy)*length(x));
contour(XI,YI,Z,prctile(Z(:),[10:10:90,90:0.5:95,95:0.1:100]));
