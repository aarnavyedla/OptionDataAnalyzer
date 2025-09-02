import streamlit as st
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
from scipy import stats
from scipy.stats import norm
import yfinance as yf
import datetime
import pymysql

def CallBlackCalc(s,x,r,t,v):
    d1 = (np.log(s/x)+t*(r+v**2/2))/(v*np.sqrt(t))
    d2 = d1-(v*np.sqrt(t))
    cprice = norm.cdf(d1)*s-norm.cdf(d2)*x*np.exp(-r*t)
    return str(f'{round(cprice,2):.2f}')

def PutBlackCalc(s,x,r,t,v):
    cprice = float(CallBlackCalc(s,x,r,t,v))
    pprice = cprice + x*np.exp(-r*t) - s
    return str(f'{round(pprice,2):.2f}')

def MonteCarloCall(s, x, r, t, v, n, m, plotshow):
    dt = t / n
    nudt = (r - 0.5 * v ** 2) * dt
    volsdt = v * np.sqrt(dt)
    lns = np.log(s)

    z = np.random.normal(size=(n, m))
    delta_lnst = nudt + volsdt * z
    lnst = lns + np.cumsum(delta_lnst, axis=0)
    lnst = np.concatenate((np.full(shape=(1, m), fill_value=lns), lnst))

    stp = np.exp(lnst)
    ct = np.maximum(0, stp - x)
    c0 = np.exp(-r * t) * np.sum(ct[-1]) / m

    sigma = np.sqrt(np.sum((ct[-1] - c0) ** 2) / (m - 1))
    se = sigma / np.sqrt(m)

    call = str(np.round(c0, 2))
    if len(call) < len(str(np.round(c0))) + 1:
        call = call + "0"

    se = str(np.round(se, 2))
    # return "Call value is ${0} with SE +/- {1}".format(call,np.round(se,2))

    if plotshow:
        stp = stp.T

        j = []
        for i in range(n + 1):
            j.append(i)

        for i in range(stp.shape[0]):
            plt.plot(j, stp[i])

        return [call, se, plt]
        #st.pyplot(plt)



    return [call, se]

def MonteCarloPut(s,x,r,t,v,n,m):
    call = float(MonteCarloCall(s,x,r,t,v,n,m,False)[0])
    pvstrike = x*np.exp(-r*t)
    put = call+pvstrike-s

    return "{:20,.2f}".format(put).strip()

def putfromcall(call, s, x, r, t):
    pvstrike = x*np.exp(-r*t)
    put = call+pvstrike-s
    return "{:20,.2f}".format(put).strip()

def getpossibleexpiry(ticker):
    return yf.Ticker(ticker).options

def reducewhitespace():
    st.markdown(
        """
        <style>
        section[data-testid="stSidebar"] {
            width: 400px !important;  # Set your desired width here
        }
        section[data-testid="st-emotion-cache-10p9htt"] {
            height: 3rem 
        }
        
        .block-container {
            padding-top: 2rem;
            padding-bottom: 0rem;
            padding-left: 5rem;
            padding-right: 5rem;
        }
        </style>
        """,
        unsafe_allow_html=True,
    )

def main():
    '''
    stock = 62
    strike = 60
    interest = 0.04
    time = 40/365
    vol = 0.32
    print(CallBlackCalc(stock,strike,interest,time,vol),PutBlackCalc(stock,strike,interest,time,vol))'''


    st.title('Option Data Collection')

    reducewhitespace()

    with st.sidebar:

        st.subheader('Manual Option Calculator')
        col1, col2 = st.columns(2)
        with col1:
            stock = st.number_input("Current asset price", key="stock")
            strike = st.number_input("Strike price", key="strike")
            time = st.number_input("Days till expiry", key="time") / 365


        with col2:
            interest = st.number_input("Risk free interest rate (%)", key="interest") / 100
            vol = st.number_input("Volatility", key="vol")
            option = st.selectbox("Pricing Model", ("Black Scholes", "Monte Carlo"), index = None, placeholder = "Choose pricing model")


        if strike != 0 and stock != 0 and interest != 0 and time != 0 and vol != 0:
           if option == "Black Scholes":
                with col1:
                    st.write('The Black Scholes call price is: $' + CallBlackCalc(stock, strike, interest, time, vol))
                with col2:
                    st.write('The Black Scholes put price is: $' + PutBlackCalc(stock, strike, interest, time, vol))
           if option == "Monte Carlo":
                mcdata =  MonteCarloCall(stock, strike, interest, time, vol, 10, 1000, True)
                with col1:
                    st.write("The Monte Carlo call price is: $" + mcdata[0])
                with col2:
                    st.write("The Monte Carlo put price is: $" + putfromcall(float(mcdata[0]), stock, strike, interest, time))
                st.write("With 10 steps and 1000 simulations, the standard deviation is: +/-" + mcdata[1])
                st.pyplot(mcdata[2])


if __name__=="__main__":
    main()