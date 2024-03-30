import numpy as np
#import pandas as pd
from scipy.integrate import odeint
import matplotlib.pyplot as plt
import math


def result(N, total_days, alpha, beta, incubation, recovered, death, v, p_i, p_e, sigma):
    ## initial conditions
    def init_vals(S_0, V_0, E_0, I_0, Q_0, R_0, D_0):
        y_0 = S_0, V_0, E_0, I_0, Q_0, R_0, D_0
        return y_0

    ## SVEIQRD model implementation
    def sveiqrd_model(y, t, N, alpha, beta, incubation, recovered, death, v, p_i, p_e, sigma):
        S, V, E, I, Q, R, D = y
        dS_dt = alpha * N - (beta * S * I / N) - (v * S / N) - alpha * S
        dE_dt = beta * S * I / N - incubation * E - p_e * E + sigma * V - alpha * E
        dI_dt = incubation * E - p_i * I - alpha * I
        dR_dt = recovered * Q - alpha * R
        dD_dt = death * Q
        dQ_dt = p_i * I + p_e * E - (recovered + death) * Q - alpha * Q
        dV_dt = (v * S) - sigma * V - alpha * V
        return dS_dt, dV_dt, dE_dt, dI_dt, dQ_dt, dR_dt, dD_dt
    
    t = np.linspace(0, total_days, total_days)
    y_0 = init_vals(N-1, 0, 1, 0, 0, 0, 0)
    res = odeint(sveiqrd_model, y_0, t, args=(N, alpha, beta, incubation, recovered, death, v, p_i, p_e, sigma))
    
    S, V, E, I, Q, R, D = res.T

    return S, V, E, I, Q, R, D

## Basic parameters
N = 328000000.0      # total population number
alpha = 0.02         # natural birth and death rate
total_days = 100
beta = 0.5801809020828108
sigma = 0.05                                 # vaccine inefficacy
v = 0.7/total_days                           # rate of people getting vaccinated per day
incubation_days = 13.112221321200105         # average length of incubation period
incubation = 1.0/incubation_days             # incubation rate
mortality_rate = 0.15                        # mortality rate of infected people dying
recovery_days = 19.698286514369016           # average length of infection (recovered)
recovered = (1.0/recovery_days) * (1 - mortality_rate)           # recovered rate
death_days = 7.0                             # average length of infection (death)
death = (1.0/death_days) * mortality_rate    # death rate
p_i = 0.8/total_days
p_e = 0.2/total_days

