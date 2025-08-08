function [alldis shortdis] = soldismat(mat,maxdist)
len = size(mat,1); 
alldis = 1:(len^2)/2-len;
shortdis = 1:len;
count = 0;
for i=1:len-1
    for j=i+1:len
        count = count + 1;
        alldis(count) = mat(i,j);
        mat(j,i) = mat(i,j);
    end
    mat(i,i) = maxdist;
end
mat(len,len) = maxdist;
for i=1:len
    shortdis(i) = min(mat(i,:));
end

        