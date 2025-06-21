import streamlit as st
import pandas as pd
import numpy as np
from scipy import stats
from scipy.stats import norm

def CallBlackCalc(s,x,r,t,v):
    d1 = (np.log(s/x)+t*(r+v**2/2))/(v*np.sqrt(t))
    d2 = d1-(v*np.sqrt(t))
    cprice = norm.cdf(d1)*s-norm.cdf(d2)*x*np.exp(-r*t)
    return cprice

def PutBlackCalc(s,x,r,t,v):
    cprice = CallBlackCalc(s,x,r,t,v)
    pprice = cprice + x*np.exp(-r*t) - s
    return pprice

def main():
    '''
    stock = 62
    strike = 60
    interest = 0.04
    time = 40/365
    vol = 0.32
    print(CallBlackCalc(stock,strike,interest,time,vol),PutBlackCalc(stock,strike,interest,time,vol))'''
    st.title('Black Scholes Price Calculator')
    stock = st.number_input("Current asset price", key="stock")
    strike = st.number_input("Strike price", key="strike")
    interest = st.number_input("Risk free interest rate (%)", key="interest")/100
    time = st.number_input("Days till expiry", key="time")/365
    vol = st.number_input("Volatility", key="vol")
    if strike != 0 and stock != 0 and interest != 0 and time != 0 and vol != 0:
        st.write('The call price is: ' + str(CallBlackCalc(stock, strike, interest, time, vol)))
        st.write('The put price is: ' + str(PutBlackCalc(stock, strike, interest, time, vol)))


if __name__=="__main__":
    main()