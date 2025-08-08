biom = zeros((size(bw4,1)-100)*(size(bw4,2)-100),11);
%biotemp = zeros((size(bw4,1)-100)*(size(bw4,2)-100),1);
for k = linspace(100,500,11)
    k
    for i = 1:(size(bw4,1)-k+1)
        i
        for j = 1:(size(bw4,2)-k+1)
            bwt = imcrop(bw4,[i,j,(i+k-1),(j+k-1)]);
            biom(i*k+j,((k-100)/40)+1) = sum(sum(bwt))/k^2;
            %biotemp(count1) = sum(sum(bwt))/k^2;
        end
    end
end       
imshow(bwt)
plot(biom(:,11))