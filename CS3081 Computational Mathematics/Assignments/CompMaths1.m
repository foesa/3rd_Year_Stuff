myMatrix  = [6,1,1,2;4,-2,5,7;2,8,7,3;3,-5,9,1];
disp(fourbyfour(myMatrix));

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