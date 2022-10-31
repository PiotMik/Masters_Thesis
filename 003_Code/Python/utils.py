import pandas as pd
import numpy as np
from scipy.stats import norm, t, beta, expon, gamma, lognorm, multivariate_normal, multivariate_t

import copulas as cp
import pyvinecopulib as pv
from pyvinecopulib import Bicop

from statsmodels.graphics import tsaplots
from statsmodels.tsa.stattools import adfuller
from statsmodels.distributions.empirical_distribution import ECDF
from arch import arch_model

import plotly.graph_objects as go
from plotly.subplots import make_subplots

from mpl_toolkits.mplot3d import axes3d
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from matplotlib import cm
import seaborn as sns

import os, shutil
from typing import Callable
from datetime import date

plt.style.use(['science', 'ggplot-ubs.mplstyle'])
sns.set_context('talk')
palette = sns.color_palette(as_cmap=True)
fmt = 'pdf'

DATA = "../../001_Data"
OUTFIGS = "../Outputs/Figs"
TEXFIGS = "../../002_LaTeX/Inputs/Figs/"
OUTTABS = "../Outputs/Tables/"
TEXTABS = "../../002_LaTeX/Inputs/Tables/"

def savefig(name):
    plt.savefig(os.path.join(OUTFIGS, f'{name}.{fmt}'))
    plt.savefig(os.path.join(TEXFIGS, f'{name}.{fmt}'))
    
def writeimage(fig, name):
    fig.write_image(f"{OUTFIGS}/{name}.{fmt}", scale=3)
    
def savetab(name, df):
    df.to_csv(os.path.join(OUTTABS, f'{name}.csv'))
    df.to_csv(os.path.join(TEXTABS, f'{name}.csv'))
    
def clear_folders(clear_figs=True, clear_tables=True):
    figs_folders, tabs_folders = [OUTFIGS, TEXFIGS], [OUTTABS, TEXTABS]
    folders_to_clear = []
    
    if clear_figs:
        folders_to_clear += figs_folders
    if clear_tables:
        folders_to_clear += tabs_folders

    def clear_dir(dir_path):
        for filename in os.listdir(dir_path):
            file_path = os.path.join(dir_path, filename)
            if os.path.isfile(file_path) or os.path.islink(file_path):
                os.unlink(file_path)
            elif os.path.isdir(file_path):
                shutil.rmtree(file_path)
    
    for directory in folders_to_clear:
        clear_dir(directory)
    
    
def plot_copula_density(copula, zmax = None, eps = 0.0, nx = 10,
                        alpha = .9, shade=False, levels = 10,
                        cmap = 'copper', antialiased =True, rot=(30, 290), **kwargs):
    fig = plt.figure()
    ax = plt.axes(projection='3d', fc='white')

    u = np.linspace(eps, 1-eps, nx)
    u, v = np.meshgrid(u, u)
    u_, v_ = u.flatten(), v.flatten()
    
    if isinstance(copula, Bicop):
        z_ = copula.pdf(np.vstack([u_, v_]).T)
        z = copula.pdf(np.vstack([u.ravel(), v.ravel()]).T).reshape(u.shape)
    elif isinstance(copula, Callable):
        z_ = copula(u_, v_)
        z = copula(u, v)
    else:
        raise NotImplementedError()
    zmax = np.max(z_) if zmax is None else zmax
    
    ax.plot_trisurf(u_, v_, z_, vmin=0.0, vmax=zmax, cmap=cmap, shade=shade, alpha = alpha, antialiased=antialiased, **kwargs)
    ax.contour(u, v, z, colors='black', levels=np.linspace(0, zmax, levels))

    ax.set_xlim(0, 1)
    ax.set_ylim(0, 1)
    ax.set_zlim(0, zmax)
    plt.tight_layout()
    ax.view_init(*rot)
    return fig, ax

def plot_copula_contour(copula, nx=30, levels=10, eps=0.0, cmap='copper', normalize=False, zmax=None, **kwargs):
    fig = plt.figure()
    ax = plt.axes()
    
    if not normalize:
        u = np.linspace(eps, 1-eps, nx)
        u, v = np.meshgrid(u, u)
        if isinstance(copula, Bicop):
            z = copula.pdf(np.vstack([u.ravel(), v.ravel()]).T).reshape(u.shape).T
        elif isinstance(copula, Callable):
            z = copula(u, v)
        else:
            raise NotImplementedError()
        zmax = np.max(z) if zmax is None else zmax
        cset = ax.contour(u, v, z, cmap=cmap, levels=np.linspace(0.0, zmax, levels), **kwargs)
        ax.clabel(cset, inline=True, colors='black', fontsize=12)
        ax.set_xlim(0, 1)
        ax.set_ylim(0, 1)
    else:
        x = np.linspace(-3+eps, 3-eps, 100)
        X, Y = np.meshgrid(x,x)
        NX = norm.cdf(X)
        NY = norm.cdf(Y)
        NGRID = np.stack((np.ravel(NX),np.ravel(NY)), axis = 1)

        nx = norm.pdf(x).reshape((1,len(x)))
        adj = (nx.transpose()*nx).ravel()
        
        if isinstance(copula, Bicop):
            z = copula.pdf(NGRID)
        elif isinstance(copula, Callable):
            z = copula(NX, NY)
        else:
            raise NotImplementedError()
        z = np.reshape(z*adj, X.shape)
        zmax = np.max(z) if zmax is None else zmax
        cset = ax.contour(X, Y, z, cmap=cmap, levels=np.linspace(0.0, zmax, levels), **kwargs)   
        ax.clabel(cset, inline=True, colors='black', fontsize=12)
        ax.set_xlim(-3, 3)
        ax.set_ylim(-3, 3)

    ax.set_aspect('equal', 'box')
    return fig, ax

if __name__ == "__main__":
    main()