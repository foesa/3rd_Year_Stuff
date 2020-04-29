myMatrix = [25 5 4; 10 8 16;8 12 22];
disp(LUdecompgauss(myMatrix));
matrix = [0,0,0,1,0,0;1,0,1,0,1,1;0,1,0,0,1,0;1,1,0,0,1,0;1,1,1,0,0,1;1,0,0,0,1,0];
[V,D] = eig(matrix);


pop = [400,557,825,981,1135,1266,1370];
year = [1900,1950,1970,1980,1990,2000,2010];
[a1,a0]= LinearRegression(year,log(pop));

function [L,U] = LUdecompgauss(A)
[m, n] = size(A);
L = zeros(m);
for i = 1:m
   L(i,i) = 1;
end
U = A;

lvl = 1;
for i = 1:m
    for j = 1:n
        if i == j
            cur = lvl;
            while cur < m
                cur = cur + 1;
                multi = U(cur,j)/U(i,j);
                for x = 1:m
                    U(cur,x) = U(cur,x) - (multi*U(i,x));
                end
                L(cur,j) = multi;
            end
        end
    end
    lvl = lvl + 1;
end 

disp(L);
disp(U);
end

function [al,aO] = LinearRegression(x, y) 
% LinearRegression calculates the coefficients al and aO of the linear 
% equation y = al*x + aO that best fits n data points. 
% Input variables: % x A vector with the coordinates x of the data points. 
% y A vector with the coordinates y of the data points. 
% Output variables: 
nx=length(x); 
ny=length(y); 
if nx ~= ny
   disp('ERROR: The number of elements in x must be the same as in y. ') 
   al='Error'; 
   aO='Error'; 
else
   Sx=sum(x); 
   Sy=sum(y); 
   Sxy=sum(x.*y);
   Sxx=sum(x.^2); 
   al=(nx*Sxy-Sx*Sy)/(nx*Sxx-Sx^2); 
   aO=(Sxx*Sy-Sxy*Sx)/(nx*Sxx-Sx^2); 
end
end
