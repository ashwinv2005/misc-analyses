function [t,s] = convertij(temp,point)
for t=1:size(temp,1)
    for s=1:size(temp,2)
        if size(temp,1)*(s-1)+t == point
            break
        end
    end
    if size(temp,1)*(s-1)+t == point
        break
    end
end