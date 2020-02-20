locArr = [0.05,0.1,0.05,0.05,
0.05,0.1,0.05,0.05,
0.05,0.05,0.1,0.05,
0.05,0.05,0.1,0.05];
obsArr = [0.75,0.95,0.75,0.05,
0.05,0.75,0.95,0.75,
0.01,0.05,0.75,0.95,
0.01,0.01,0.05,0.75];
LgOArr = zeros(1,16);
for i = 1:length(LgOArr)
    curProbOL = obsArr(i);
    curProbL = locArr(i);
    curProbO = (curProbOL * curProbL) + ((1-curProbOL)*(1-curProbL));
    LgOArr(i) = (curProbOL*curProbL)/curProbO;
end
disp(LgOArr);