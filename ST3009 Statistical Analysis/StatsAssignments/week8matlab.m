vals = zeros(100);
for i=1:10000
    p=0.1;
    A=(rand(10)<p);
    meanc = mean(A,'all');
    vals(i) = meanc;
    disp(vals(i));
end
disp(vals);
histogram(vals);

