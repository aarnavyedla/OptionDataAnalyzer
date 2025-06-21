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
    '''stock = 40
    strike = 50
    interest = 0.04
    time = 40/365
    vol = 0.32
    print(CallBlackCalc(stock,strike,interest,time,vol),PutBlackCalc(stock,strike,interest,time,vol))'''
    st.title('Black Scholes Price Calculator')
    st.text_input("Strike price", key="strike")
    st.text_input("Current asset price", key="stock")
    st.text_input("Risk free interest rate", key="interest")
    st.text_input("Time till expiry", key="time")
    st.text_input("Volatility", key="vol")
    st.write('The call price is: ' + str(
        CallBlackCalc(int(st.session_state.strike), int(st.session_state.stock), int(st.session_state.interest),
                      int(st.session_state.time),
                      int(st.session_state.vol))))
    st.write('The put price is: ' + str(
        PutBlackCalc(int(st.session_state.strike), int(st.session_state.stock), int(st.session_state.interest),
                      int(st.session_state.time),
                      int(st.session_state.vol))))


if __name__=="__main__":
    main()