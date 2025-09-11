import streamlit as st
from streamlit import session_state
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
from scipy import stats
from scipy.stats import norm
import yfinance as yf
import datetime
import pymysql
import tempfile
from PIL import Image
import warnings
'''import rpy2.robjects as ro
from rpy2.robjects import pandas2ri, conversion
from rpy2.robjects.packages import importr'''
import subprocess

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
        call = call + '0'

    se = str(np.round(se, 2))
    # return 'Call value is ${0} with SE +/- {1}'.format(call,np.round(se,2))

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

    return '{:20,.2f}'.format(put).strip()

def putfromcall(call, s, x, r, t):
    pvstrike = x*np.exp(-r*t)
    put = call+pvstrike-s
    return '{:20,.2f}'.format(put).strip()

def getpossibleexpiry(ticker):
    return list(yf.Ticker(ticker).options)

def writeoptionprice(stockticker, date):
    conn = init_connection()
    ticker = yf.Ticker(stockticker)
    opt_chain = ticker.option_chain(date)
    numoptions = 0
    placeholder = st.empty()

    calls = opt_chain.calls

    for i, row in calls.iterrows():
        stock = float(ticker.history(start=row['lastTradeDate'], period='1d')['Close'].iloc[0])
        strike = float(row['strike'])
        interest = float((yf.Ticker('^TNX').history(start=row['lastTradeDate'], period='1d')['Close'] / 100).iloc[0])
        time = int((datetime.datetime.strptime(date, '%Y-%m-%d').timestamp() - row['lastTradeDate'].timestamp()) // 86400 + ((datetime.datetime.strptime(date,'%Y-%m-%d').timestamp() - row['lastTradeDate'].timestamp()) % 86400 > 0)) / 365
        volatility = row['impliedVolatility']

        calldata = {
            'tickerid': str(row['contractSymbol']),
            'company': stockticker,
            'date': date,
            'is_call': True,
            'stockprice': stock,
            'strikeprice': strike,
            'timetoexpiry': time*365,
            'impliedvol': volatility,
            'actprice': float(row['lastPrice']),
            'bsprice': float(CallBlackCalc(stock, strike, interest, time, volatility)),
            'mcprice': float(MonteCarloCall(stock, strike, interest, time, volatility, 10, 100000, False)[0])
        }

        if row['lastPrice'] > 0.1 and calldata['bsprice']>0.1 and calldata['mcprice']>0.1 and calldata['impliedvol']>0.0001:
            with conn.cursor() as cursor:
                sql = 'INSERT IGNORE INTO option_pricer_data (tickerid, company, date, is_call, stockprice, strikeprice, timetoexpiry, impliedvol, actprice, bsprice, mcprice) VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)'
                cursor.execute(sql, (calldata['tickerid'], calldata['company'].upper(), calldata['date'], calldata['is_call'], calldata['stockprice'], calldata['strikeprice'],
                                     calldata['timetoexpiry'], calldata['impliedvol'],calldata['actprice'], calldata['bsprice'], calldata['mcprice']))
            conn.commit()
            numoptions += 1
            placeholder.markdown('Number of options added: '+str(numoptions))

    puts = opt_chain.puts
    for i, row in puts.iterrows():
        stock = float(ticker.history(start=row['lastTradeDate'], period='1d')['Close'].iloc[0])
        strike = float(row['strike'])
        interest = float((yf.Ticker('^TNX').history(start=row['lastTradeDate'], period='1d')['Close'] / 100).iloc[0])
        time = int((datetime.datetime.strptime(date, '%Y-%m-%d').timestamp() - row['lastTradeDate'].timestamp()) // 86400 + ((datetime.datetime.strptime(date,'%Y-%m-%d').timestamp() - row['lastTradeDate'].timestamp()) % 86400 > 0)) / 365
        volatility = row['impliedVolatility']
        putdata = {
            'tickerid': str(row['contractSymbol']),
            'company': stockticker,
            'date': date,
            'is_call': False,
            'stockprice': stock,
            'strikeprice': strike,
            'timetoexpiry': time*365,
            'impliedvol': volatility,
            'actprice': float(row['lastPrice']),
            'bsprice': float(PutBlackCalc(stock, strike, interest, time, volatility)),
            'mcprice': float(MonteCarloPut(stock, strike, interest, time, volatility, 10, 100000))
        }

        if row['lastPrice'] > 0.1 and putdata['bsprice']>0.1 and putdata['mcprice']>0.1 and putdata['impliedvol']>0.0001:
            with conn.cursor() as cursor:
                sql = 'INSERT IGNORE INTO option_pricer_data (tickerid, company, date, is_call, stockprice, strikeprice, timetoexpiry, impliedvol, actprice, bsprice, mcprice) VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)'
                cursor.execute(sql, (putdata['tickerid'], putdata['company'].upper(), putdata['date'], putdata['is_call'], putdata['stockprice'], putdata['strikeprice'],
                                     putdata['timetoexpiry'], putdata['impliedvol'],putdata['actprice'], putdata['bsprice'], putdata['mcprice']))
            conn.commit()
            numoptions += 1
            placeholder.markdown('Number of options added: '+str(numoptions))

    conn.close()
    return

def getalldata():
    conn = init_connection()
    query = 'SELECT * FROM option_pricer_data LIMIT 10000'
    df = pd.read_sql(query, conn)
    conn.close()
    return df

def getwrittencompanies():
    conn = init_connection()
    query = 'SELECT DISTINCT company FROM option_pricer_data'
    df = pd.read_sql(query, conn)
    conn.close()
    return sorted(df['company'].tolist())

def getwrittencompanydates(stockticker):
    ticker = stockticker.upper()
    conn = init_connection()
    query = 'SELECT DISTINCT date FROM option_pricer_data WHERE company=%s'
    df = pd.read_sql(query, conn, params = [ticker])
    conn.close()
    return sorted(df['date'].tolist())

def getoptiondata(stockticker, date):
    ticker = stockticker.upper()
    conn = init_connection()
    query = 'SELECT * FROM option_pricer_data WHERE company=%s AND date=%s'
    df = pd.read_sql(query, conn, params = [ticker, date])
    conn.close()
    return df

def get_ca_file():
    ca_text = st.secrets['tidb']['SSL_CA']
    with tempfile.NamedTemporaryFile(delete=False, suffix='.pem') as f:
        f.write(ca_text.encode())
        return f.name

def init_connection():
    ca_path = get_ca_file()
    return pymysql.connect(
        host=st.secrets['tidb']['DB_HOST'],
        port=st.secrets['tidb']['DB_PORT'],
        user=st.secrets['tidb']['DB_USER'],
        password=st.secrets['tidb']['DB_PASSWORD'],
        database=st.secrets['tidb']['DB_DATABASE'],
        ssl_verify_identity = st.secrets['tidb']['SSL_VERIFY_IDENTITY'],
        ssl_verify_cert = st.secrets['tidb']['SSL_VERIFY_CERT'],
        ssl_ca=ca_path
    )

'''@st.cache_resource
def loadscript():
    ro.r['source']['analyzeoptiondata.R']
    return ro
'''
def reducewhitespace():
    st.markdown(
        '''
        <style>
        section[data-testid='stSidebar'] {
            width: 400px !important;  # Set your desired width here
        }
        section[data-testid='st-emotion-cache-10p9htt'] {
            height: 3rem 
        }
        
        .block-container {
            padding-top: 2rem;
            padding-bottom: 0rem;
            padding-left: 5rem;
            padding-right: 5rem;
        }
        </style>
        ''',
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
    warnings.filterwarnings('ignore', category=UserWarning)
    reducewhitespace()

    #r_env = loadscript()

    with st.sidebar:

        st.subheader('Manual Option Calculator')
        col1, col2 = st.columns(2)
        with col1:
            stock = st.number_input('Current asset price', key='stock')
            strike = st.number_input('Strike price', key='strike')
            time = st.number_input('Days till expiry', key='time') / 365


        with col2:
            interest = st.number_input('Risk free interest rate (%)', key='interest') / 100
            vol = st.number_input('Volatility', key='vol')
            option = st.selectbox('Pricing Model', ('Black Scholes', 'Monte Carlo'), index = None, placeholder = 'Choose pricing model')


        if strike != 0 and stock != 0 and interest != 0 and time != 0 and vol != 0:
            if option == 'Black Scholes':
                with col1:
                    st.write('The Black Scholes call price is: $' + CallBlackCalc(stock, strike, interest, time, vol))
                with col2:
                    st.write('The Black Scholes put price is: $' + PutBlackCalc(stock, strike, interest, time, vol))
            if option == 'Monte Carlo':
                mcdata =  MonteCarloCall(stock, strike, interest, time, vol, 10, 1000, True)
                with col1:
                    st.write('The Monte Carlo call price is: $' + mcdata[0])
                with col2:
                    st.write('The Monte Carlo put price is: $' + putfromcall(float(mcdata[0]), stock, strike, interest, time))
                st.write('With 10 steps and 1000 simulations, the standard deviation is: +/-' + mcdata[1])
                st.pyplot(mcdata[2])

    if 'mode' not in st.session_state:
        st.session_state['mode'] = None

    left, middle, right = st.columns(3)
    if left.button('Write Data', width = 'stretch'):
        st.session_state['mode'] = 'write'
    if middle.button('Read Data', width = 'stretch'):
        st.session_state['mode'] = 'read'
    if right.button('Analyze Data', width = 'stretch'):
        st.session_state['mode'] = 'analyze'

    if st.session_state['mode'] == 'write':
        st.write('Please wait until the text "All options added" is shown before switching to Read Data or Analyze Full')
        ticker = st.text_input('Ticker (if no dates are showing up, check your spelling): ')
        if ticker:
            expiryoptions = getpossibleexpiry(ticker)
            if expiryoptions:
                expiryoptions.insert(0, 'No date selected')
                expiry = st.selectbox('Please select an expiry date', expiryoptions)
                if expiry!='No date selected':
                    writeoptionprice(ticker, expiry)
                    with st.empty():
                        st.write('All options added')

    if st.session_state['mode'] == 'read':
        try:
            del company
        except:
            pass

        col1, col2 = st.columns(2)
        with col1:
            st.session_state['company_list'] = getwrittencompanies()
            company = st.radio('Select a company', st.session_state['company_list'], index = None, key = 'company')
        with col2:
            if company:
                st.session_state['date_list'] = getwrittencompanydates(company)
                date = st.radio('Select a date for '+company, st.session_state['date_list'], index = None, key = 'date')
        if company and date:
            st.empty().dataframe(getoptiondata(company,date))

    if st.session_state['mode'] == 'analyze':
        data = getalldata()
        '''with conversion.localconverter(ro.default_converter + pandas2ri.converter):
            grdevices = importr('grDevices')
            ro.r['source']("analyzeoptiondata.R")
            ro.globalenv['data'] = ro.conversion.py2rpy(data)
            grdevices.png(file="multi_plot.png", width=1200, height=800)
            ro.r('skewsmirk(data)')
            grdevices.dev_off()
        st.image(Image.open("multi_plot.png"), caption="Multi-Plot from R")'''
        process1 = subprocess.Popen(['Rscript', 'analyzeoptiondata.R', 'test',], stdout=subprocess.PIPE, stderr=subprocess.PIPE, text = True)
        result1 = process1.communicate()
        st.write(result1)

if __name__=='__main__':
    main()