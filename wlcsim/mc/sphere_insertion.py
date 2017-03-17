"""Monte Carlo of inserting stuff inside of a sphere."""
from enum import Enum
from numba import jit
import numpy as np
import math
from .. import plot as wplot
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import seaborn as sns

SMALL_NUM = 10e-8

# @jit(nopython=True)
def is_inside(x, xr, r):
    """||x|| + xr < r ?"""
    return x[0]*x[0] + x[1]*x[1] + x[2]*x[2] < (r - xr)*(r - xr)

# @jit(nopython=True)
def norm3_squared(x):
    return x[0]*x[0] + x[1]*x[1] + x[2]*x[2]

# @jit(nopython=True)
def weeks_chandler_anderson(r, sigma, epsilon=1):
    """WCA Interaction Potential (a rescaled "repulsive part of lennard jones"
    potential) with epsilon defaulting to 1, so that you can fix beta*epsilon
    elsewhere in the simulation."""
    # r = x2 - x1
    r2 = norm3_squared(r)
    if r2 < SMALL_NUM:
        return np.inf
    sigma2 = sigma*sigma
    if math.pow(2, 1/3)*sigma2 < r2:
        return 0
    sigr = sigma2/r2
    sigr = sigr*sigr*sigr
    return 4*epsilon*(sigr*(sigr - 1) + 1/4)

# # turns out this can be made into a clever weeks_chandler_anderson call
# @jit(nopython=True)
# def confined_in_sphere(x, r_x, r_conf, epsilon=1):
#     """E_confinement = E_WCA(r_conf - ||x||)."""
#     r = r_conf - np.linalg.norm(x)


def initial_locations(num_spheres, sphere_radii, confinement_radius):
    """Get uniformly random initial locations for Metropolis Algorithms to
    start at."""
    num_successful = 0
    sphere_centers = np.zeros((num_spheres, 3))
    while num_successful < num_spheres:
        new_pos = 2*confinement_radius*(np.random.random_sample((3,)) - 1/2)
        if is_inside(new_pos, sphere_radii, confinement_radius):
            sphere_centers[num_successful, :] = new_pos
            num_successful += 1
    return sphere_centers

# @jit(nopython=True)
def volume_of_sphere(radius):
    return (4/3)*math.pi*(radius**3)

def norm_step_mc(num_steps, sphere_centers, num_spheres, sphere_radii, confinement_radius=1,
                 step_size=None, beta_epsilon=0.665):
    """Peform num_steps monte carlo steps on the set of spheres with
    sphere_centers, sphere_radii in a confinement centered at the origin of
    radius confinement_radius. At each step, move one bead by a gaussian amount
    with std dev step_size(default->sphere_radii). The beads are assumed to
    have a weeks_chandler_anderson potential between them with sigma=sphere_radii, and
    epsilon and the temperature are determined by beta_epsilon. The confinment
    sphere is also weeks_chandler_anderson."""
    # default step size to be on average the size of the bead
    step_size = sphere_radii if step_size is None else step_size
    tot_energy_change = 0
    energies = np.zeros((num_spheres,))
    for i in range(num_steps):
        si = np.random.randint(num_spheres)
        # new positions energy calculation, and exit early if possible
        new_pos = sphere_centers[si,:] + step_size*np.random.standard_normal((3,))
        new_dist_to_bdry = confinement_radius - np.linalg.norm(new_pos)
        if new_dist_to_bdry < 0: # then you're outisde the bdry
            continue
        for j in range(num_spheres):
            if j == si:
                energies[j] = weeks_chandler_anderson(np.array([new_dist_to_bdry,0,0]), 2*sphere_radii)
            else:
                energies[j] = weeks_chandler_anderson(new_pos - sphere_centers[j,:], 2*sphere_radii)
        new_potential = np.sum(energies)
        # old position energy calculations
        old_dist_to_bdry = confinement_radius - np.linalg.norm(sphere_centers[si,:])
        for j in range(num_spheres):
            if j == si:
                # # no self-energy contribution
                # energies[j] = 0
                # instead, account here for confinement energy
                energies[j] = weeks_chandler_anderson(np.array([old_dist_to_bdry,0,0]), 2*sphere_radii)
            else:
                energies[j] = weeks_chandler_anderson(sphere_centers[si,:] - sphere_centers[j,:], 2*sphere_radii)
        old_potential = np.sum(energies)
        pot_diff = new_potential - old_potential
        # MH acceptance rule, most short-circuitable form
        if pot_diff > 0 or np.log(np.random.rand()) >= -beta_epsilon*pot_diff:
            continue
        # if we get here, the monte carlo step is accepted.
        sphere_centers[si,:] = new_pos
        tot_energy_change += pot_diff
    return sphere_centers, tot_energy_change

# @jit(nopython=True)
def total_wca_energy(sphere_centers, num_spheres, sphere_radii, confinement_radius=1):
    energy = 0
    for si in range(num_spheres):
        dist_to_bdry = confinement_radius - np.linalg.norm(sphere_centers[si,:])
        energy += weeks_chandler_anderson(np.array([dist_to_bdry,0,0]), sphere_radii)
        for sj in range(si):
            energy += weeks_chandler_anderson(sphere_centers[si,:] - sphere_centers[sj,:], sphere_radii)
    return energy

def sphere_dispersal_mc(num_steps, target_density, sphere_radii, confinement_radius=1,
                        steps_per_check=1000, step_size=None, beta_epsilon=0.665):
    """Perform MCMC for num_steps after uniform position initialization of
    spheres with effective hard repulsive size sphere_radii (accomplished by
    weeks_chandler_anderson potential with barker-henderson mean collision
    diameter set to equal sigma) at target_density inside of a sphere with size
    confinement_radius.

    For now, beta_epsilon fixed to 0.665, per what Tom did in his thesis."""
    # num_spheres*sphere_volume = target_density*confinement_volume
    sphere_volume = volume_of_sphere(sphere_radii)
    confinement_volume = volume_of_sphere(confinement_radius)
    num_spheres = math.floor(target_density*confinement_volume/sphere_volume)
    sphere_centers = initial_locations(num_spheres, sphere_radii, confinement_radius)
    # break run into shorter sprints of 1000 steps, report energy change after
    # each 1000 steps
    num_checks = math.floor(num_steps/steps_per_check)
    energy = total_wca_energy(sphere_centers, num_spheres, sphere_radii, confinement_radius)
    for i in range(num_checks):
        # run MC, reports energy change
        sphere_centers, d_energy = norm_step_mc(steps_per_check, sphere_centers, num_spheres,
                                                sphere_radii, confinement_radius, step_size, beta_epsilon)
        print(energy + d_energy)
        energy = total_wca_energy(sphere_centers, num_spheres, sphere_radii, confinement_radius)
        print(energy)

    return sphere_centers

def plot_spheres(sphere_centers, radii):
    fig = plt.figure()
    ax = fig.gca(projection='3d')
    palette = sns.color_palette('hls', 12)
    for i,c in enumerate(sphere_centers):
        color = palette[i % len(palette)]
        wplot.draw_sphere(c, radii, colors=color, axes=ax)
    return ax