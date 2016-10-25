import numpy as np

def gaussian_in_spherical_confinement(N, l, t, b, r, save_interval=0):
    """
    Simulate a gaussian chain with N links of characteristic length "l"
    apart and kuhn length b through discrete times t, with a spherical 
    confinement of radius r.

    For now, the initialization is a naive laying of the N beads at their
    characteristic distance apart perfectly circularly in a circle in the
    xy-plane of radius r/2. For this reason, l < r/2 is required.
    """
    x = init_gaussian_in_sphere_naive(N, l, r)
    dt = np.diff(t)
    for i,dti in enumerate(dt):
        x = 
        for j in range(1, len(x)-1):
            x(j) = 4 #todo
        if dti > prev_save_time + save_interval:
            save_x(x)


            
