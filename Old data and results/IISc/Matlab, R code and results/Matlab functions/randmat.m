function [mat] = randmat(m,n,p)
mat = zeros(m,n);
for i=1:m
    for j=1:n
        if rand<p
            mat(i,j) = 1;
        end
        if rand>=p
            mat(i,j) = 0;
        end
    end
end