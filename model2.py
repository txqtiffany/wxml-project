import numpy as np
import pandas as pd
from scipy.integrate import odeint

def result(target_age):
    C = pd.read_csv('contact_matrix.csv').set_index(['estimated_age_group']).to_numpy()
    pop_dist = pd.read_csv('pop_dist_1.csv').drop(['Unnamed: 0'], axis=1)
    pop_dist['total'] = pop_dist['male'] + pop_dist['female']
    N = pop_dist.drop(['male', 'female'], axis=1).to_numpy().flatten()

    i = 13.112221321200105    # average length of incubation period
    D = 14                    # average length of infection (recovered)
    m = 0.15                  # infected death rate
    d = 7.0                   # average length of infection (dead)
    beta = 0.7                # num of people getting infected by one person before social-distancing
    time_steps = 200
    t = np.linspace(1, time_steps, time_steps) # total number of days
    sigma = 1.0/i             # incubation rate
    gamma = 1.0/D             # recovery rate
    p = 1.0/d                 # mortality rate

    age_groups = 19

    def init_vals(S_0, E_0, I_0, R_0, D_0, age_groups):
        '''
        Initialize initial conditions for ODEs
        Params:
            S_0, E_0, I_0, R_0, D_0: dim = [age_groups] each
            age_groups: np.int
        Returns:
            y_0: dim = [5, age_groups]
        '''
        y_0 = np.zeros((5, age_groups))
        y_0[0,:] = S_0
        y_0[1,:] = E_0
        y_0[2,:] = I_0
        y_0[3,:] = R_0
        y_0[4,:] = D_0
        return y_0

    def seird_model(y, t, beta, sigma, gamma, p, m):
        '''
        seird model implementation
        Params:
            y: dim = [5, age_groups]
            t: dim = [t]
            args: N: population
                beta: transmission rate
                sigma: incubation rate
                gamma: recovery rate
                p: mortality rate
                m: infected death rate
            age_groups: np.int
        Returns:
            y_0: dim = [5, age_groups]
        '''
        y = y.reshape((5, age_groups))
        S, E, I, R, D = y

        dS_dt = -beta * S * (C @ I) / N
        dE_dt = beta * S * (C @ I) / N - sigma * E
        dI_dt = sigma * E - (1 - m) * gamma * I - p * m * I
        dR_dt = (1 - m) * gamma * I
        dD_dt = m * p * I
        
        dy_dt = np.array([dS_dt,dE_dt,dI_dt,dR_dt,dD_dt])
        
        return dy_dt.flatten()

    y_0 = init_vals(N-2, 1, 1, 0, 0, age_groups)
    y_0 = y_0.reshape((5*age_groups))
    solve = odeint(seird_model, y_0, t, args=(beta, sigma, gamma, p, m))
    solve = solve.reshape((200, 5, age_groups))
    solve = solve.transpose([1, 0, 2])
    S, E, I, R, D = solve[:, :, target_age]
    S = S.reshape(200, 1)
    E = E.reshape(200, 1)
    I = I.reshape(200, 1)
    R = R.reshape(200, 1)
    D = D.reshape(200, 1)
    return S, E, I, R, D
