function [bw] = fillone(bw,s,p,i,j)
    if bw(s,p) == 1 && (s~=i || p~=j)
        if s<=i && p<=j
            fl = 0;
            for q=s:i
                for r=p:j
                    if (q==s || q==i) && (r==p || r==j)
                        continue
                    end
                    if bw(q,r) == 1 
                        fl = 1;
                    end
                end
            end
            if (fl == 0)
                for q=s:i
                    for r=p:j
                        if (q==s || q==i) && (r==p || r==j)
                            continue
                        end
                        bw(q,r) = 1;
                    end
                end
            end
        end
        if s>=i && p<=j
            fl = 0;
            for q=i:s
                for r=p:j
                    if (q==s || q==i) && (r==p || r==j)
                        continue
                    end
                    if bw(q,r) == 1 
                        fl = 1;
                    end
                end
            end
            if (fl == 0)
                for q=i:s
                    for r=p:j
                        if (q==s || q==i) && (r==p || r==j)
                            continue
                        end
                        bw(q,r) = 1;                                                     
                    end
                end
            end
        end
        if s<=i && p>=j
            fl = 0;
            for q=s:i
                for r=j:p
                    if (q==s || q==i) && (r==p || r==j)
                        continue
                    end
                    if bw(q,r) == 1 
                        fl = 1;
                    end
                end
            end
            if (fl == 0)
                for q=s:i
                    for r=j:p
                        if (q==s || q==i) && (r==p || r==j)
                            continue
                        end
                        bw(q,r) = 1;                                                     
                    end
                end
            end
        end
        if s>=i && p>=j
            fl = 0;
            for q=i:s
                for r=j:p
                    if (q==s || q==i) && (r==p || r==j)
                        continue
                    end
                    if bw(q,r) == 1 
                        fl = 1;
                    end
                end
            end
            if (fl == 0)
                for q=i:s
                    for r=j:p
                        if (q==s || q==i) && (r==p || r==j)
                            continue
                        end
                        bw(q,r) = 1;                                                     
                    end
                end
            end
        end            
    end
end
