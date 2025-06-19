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
    stock = 40
    strike = 50
    interest = 0.04
    time = 40/365
    vol = 0.32
    print(CallBlackCalc(stock,strike,interest,time,vol),PutBlackCalc(stock,strike,interest,time,vol))
    st.title('Uber pickups in NYC')

if __name__=="__main__":
    main()