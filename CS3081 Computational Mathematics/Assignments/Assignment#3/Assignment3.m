function N = InfinityNorm(A)
  [m,n] = size(A)
  max = 0;
  cur = 0;
  for i in 1:m
    for j in 1:n
      cur = cur+abs(A(i,j));
    end
    if(temp> max)
      max = temp;
    end
    temp = 0;
  end
  N = max;
end
