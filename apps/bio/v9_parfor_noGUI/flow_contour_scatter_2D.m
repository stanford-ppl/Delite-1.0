function flow_contour_scatter_2D(x,y)

% remove NaNs
remove_flag = (isnan(x) | isnan(y));
x(remove_flag) = [];
y(remove_flag) = [];

% biexponential transformation
scale_factor_x = 10^round(log10(median(x)));
scale_factor_y = 10^round(log10(median(y)));
x1 = asinh(x/scale_factor_x);
y1 = asinh(y/scale_factor_y);

x1 = x1(x1>0);
y1 = y1(y1>0);

if length(x1)>100000
    x1 = x1(1:10000);
end
if length(y1)>100000
    y1 = y1(1:10000);
end

contour2D(x1,y1)





%%%%%%%%%%%%%%
function contour2D(x,y)

[F,XI,hx] = ksdensity(x); [F,YI,hy] = ksdensity(y);
Z = zeros(length(XI),length(YI));
dist_XI_x = exp(-(repmat(XI(:),1,length(x))-repmat(x(:)',length(XI),1)).^2/2/hx^2);
dist_YI_y = exp(-(repmat(YI(:),1,length(y))-repmat(y(:)',length(YI),1)).^2/2/hy^2);
Z = (dist_XI_x*dist_YI_y')';
Z = Z./(sqrt(2*pi*hx^2*2*pi*hy)*length(x));
contour(XI,YI,Z,prctile(Z(:),[80:1:90,90:0.5:95,95:0.1:100]));
