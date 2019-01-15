def generate_hierarchy(A):
    for a in A:
        a.setparent(None)
        a.child = False
    for a in A:
        for b in A:
            if not a.equals(b):
                if a.contains(b):
                    if b.parent == None:
                        b.parent = a
                        a.child = True
                        
                    elif b.parent != None: 
                        if b.parent.getarea() >  a.getarea():
                            b.parent = a
                            a.child = True