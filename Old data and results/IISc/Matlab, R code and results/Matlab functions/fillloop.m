function [bw] = fillloop(bw,n,i,j)
    if i<n+2 && j<n+2
        for s=1:i+n+1
            for p=1:j+n+1
                if s==1 || s==i+n+1 || p==1 || p==j+n+1                                
                    bw = fillone(bw,s,p,i,j);
                end
            end
        end
    end
    if i<n+2 && j>size(bw,2)-(n+1)
        for s=1:i+n+1
            for p=j-(n+1):size(bw,2)
                if s==1 || s==i+n+1 || p==j-(n+1) || p==size(bw,2)                                
                    bw = fillone(bw,s,p,i,j);
                end
            end
        end
    end
    if i>size(bw,1)-(n+1) && j<n+2
        for s=i-(n+1):size(bw,1)
            for p=1:j+n+1
                if s==i-(n+1) || s==size(bw,1) || p==1 || p==j+n+1                                
                    bw = fillone(bw,s,p,i,j);
                end
            end
        end
    end
    if i>size(bw,1)-(n+1) && j>size(bw,2)-(n+1)
        for s=i-(n+1):size(bw,1)
            for p=j-(n+1):size(bw,2)
                if s==i-(n+1) || s==size(bw,1) || p==j-(n+1) || p==size(bw,2)                                
                    bw = fillone(bw,s,p,i,j);
                end
            end
        end
    end
    if i>=n+2 && j>=n+2 && i<=size(bw,1)-(n+1) && j<=size(bw,2)-(n+1)
        for s=i-(n+1):i+n+1
            for p=j-(n+1):j+n+1
                if s==i-(n+1) || s==i+n+1 || p==j-(n+1) || p==j+n+1                                
                    bw = fillone(bw,s,p,i,j);
                end
            end
        end
    end
    if n==1
        if i==1 && j==1
            if bw(i+1,j+1)==1 && bw(i,j+1)==0 && bw(i+1,j)==0
                bw(i,j+1)=1;
                bw(i+1,j)=1;
            end
        end
        if i==1 && j~=1 && j~=size(bw,2)
            if bw(i+1,j+1)==1 && bw(i,j+1)==0 && bw(i+1,j)==0
                bw(i,j+1)=1;
                bw(i+1,j)=1;
            end
            if bw(i+1,j-1)==1 && bw(i,j-1)==0 && bw(i+1,j)==0
                bw(i,j-1)=1;
                bw(i+1,j)=1;
            end
        end
        if i==1 && j==size(bw,2)
            if bw(i+1,j-1)==1 && bw(i,j-1)==0 && bw(i+1,j)==0
                bw(i,j-1)=1;
                bw(i+1,j)=1;
            end
        end
        if i==size(bw,1) && j==1
            if bw(i-1,j+1)==1 && bw(i-1,j)==0 && bw(i,j+1)==0
                bw(i-1,j)=1;
                bw(i,j+1)=1;
            end
        end
        if i==size(bw,1) && j~=1 && j~=size(bw,2)
            if bw(i-1,j+1)==1 && bw(i,j+1)==0 && bw(i-1,j)==0
                bw(i,j+1)=1;
                bw(i-1,j)=1;
            end
            if bw(i-1,j-1)==1 && bw(i,j-1)==0 && bw(i-1,j)==0
                bw(i,j-1)=1;
                bw(i-1,j)=1;
            end
        end
        if i==size(bw,1) && j==size(bw,2)
            if bw(i-1,j-1)==1 && bw(i,j-1)==0 && bw(i-1,j)==0
                bw(i,j-1)=1;
                bw(i-1,j)=1;
            end
        end
        if i~=1 && i~=size(bw,1) && j==1
            if bw(i-1,j+1)==1 && bw(i-1,j)==0 && bw(i,j+1)==0
                bw(i-1,j)=1;
                bw(i,j+1)=1;
            end
            if bw(i+1,j+1)==1 && bw(i+1,j)==0 && bw(i,j+1)==0
                bw(i+1,j)=1;
                bw(i,j+1)=1;
            end
        end
        if i~=1 && i~=size(bw,1) && j==size(bw,2)
            if bw(i-1,j-1)==1 && bw(i,j-1)==0 && bw(i-1,j)==0
                bw(i,j-1)=1;
                bw(i-1,j)=1;
            end
            if bw(i+1,j-1)==1 && bw(i,j-1)==0 && bw(i+1,j)==0
                bw(i,j-1)=1;
                bw(i+1,j)=1;
            end
        end
        if i~=1 && i~=size(bw,1) && j~=1 && j~=size(bw,2)
            if bw(i-1,j-1)==1 && bw(i,j-1)==0 && bw(i-1,j)==0
                bw(i,j-1)=1;
                bw(i-1,j)=1;
            end
            if bw(i-1,j+1)==1 && bw(i,j+1)==0 && bw(i-1,j)==0
                bw(i,j+1)=1;
                bw(i-1,j)=1;
            end
            if bw(i+1,j-1)==1 && bw(i,j-1)==0 && bw(i+1,j)==0
                bw(i,j-1)=1;
                bw(i+1,j)=1;
            end
            if bw(i+1,j+1)==1 && bw(i,j+1)==0 && bw(i+1,j)==0
                bw(i,j+1)=1;
                bw(i+1,j)=1;
            end
        end
    end
end
