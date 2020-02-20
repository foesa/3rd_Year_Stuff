myMatrix  = [-1,2,1;2,2,-4;0.2,1,0.5];
disp(Inverse(myMatrix));

function val = twobytwo(matrix)
    val = (matrix(1,1)*matrix(2,2)) - (matrix (1,2)*matrix(2,1));
end

function val2 = threebythree(matrix)
    first = matrix(1,1)*twobytwo([matrix(2,2),matrix(2,3);matrix(3,2),matrix(3,3)]);
    second = matrix(1,2)* twobytwo([matrix(2,1),matrix(2,3);matrix(3,1),matrix(3,3)]);
    third = matrix(1,3)*twobytwo([matrix(2,1),matrix(2,2);matrix(3,1),matrix(3,2)]);
    val2 = (first-second)+third;
end

function val3 = fourbyfour(matrix)
    tempMat = [0,0,0,0,0,0,0,0,0];
    incr = 1;
    curAns = 0;
    for s = 1:4 
        for i = 1:4
            for j = 1:4
                if (i ~= 1 && j ~= s)
                    tempMat(incr) = matrix(i,j);
                    incr = incr +1;
                end
            end
        end
        sendMat = reshape(tempMat,[3,3]);
        if s == 1
            curAns = matrix(1,s)*threebythree(sendMat);l
        elseif mod(s,2) == 0
            curAns = curAns - (matrix(1,s)*threebythree(sendMat));
        else
            curAns = curAns + (matrix(1,s)*threebythree(sendMat));
        end
        incr = 1;
    end
    val3 = curAns
end

function Ainv = Inverse (A)
    [n, m]=size(A); 
    if n ~= m 
        Ainv ='The matrix must be square';
        return
    end
    if n == 0 
        Ainv ='Matrix cant be empty';
        return
    end
    Ainv = eye(n);
    for r = 1 : n
        for c = r : n
            if A(c,r) ~= 0
                t = 1/A(r,r);
                for i = 1 : n
                    A(r,i) = t * A(r,i);
                    Ainv(r,i) = t * Ainv(r,i); 
                end
                for i = 1 : n
                    if i ~= r 
                        t = -A(i,r);
                        for j = 1 : n
                            A(i,j) = A(i,j) + t * A(r,j);
                            Ainv(i,j) = Ainv(i,j) + t * Ainv(r,j);
                        end
                    end
                end
            end
            break
        end
    end
end
        